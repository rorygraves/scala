package scala.tools.nsc.backend.jvm

import java.util.concurrent.BlockingQueue

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.Duration
import scala.reflect.internal.util.{NoPosition, Statistics}
import scala.tools.asm
import scala.tools.asm.tree.ClassNode
import scala.util.control.NonFatal

// support classes for GenBcode
// seperated here to allow for different imports, and to better ensure that there is not tree access
trait BCodeParallel {
  genBCode: GenBCode =>

  case class Item1(arrivalPos: Int, cd: global.ClassDef, cunit: global.CompilationUnit, workflow :Workflow)

  class Workflow extends AsyncReporter {
    val optimize = Promise[Item2]
    val item2 = Promise[Item2]
    val item3 = Promise[Item3]

    override def toString: String = s"Workflow optimizeComplete: ${optimize.isCompleted} item2Complete: ${item2.isCompleted} item2Complete: ${item2.isCompleted}"
  }

  case class Item2(arrivalPos: Int,
                   mirror: asm.tree.ClassNode,
                   plain: asm.tree.ClassNode,
                   bean: asm.tree.ClassNode,
                   sourceFilePath: String,
                   outFolder: scala.tools.nsc.io.AbstractFile)

  /*
 *  An item of queue-3 (the last queue before serializing to disk) contains three of these
 *  (one for each of mirror, plain, and bean classes).
 *
 *  @param jclassName  internal name of the class
 *  @param jclassBytes bytecode emitted for the class SubItem3 represents
 */
  case class SubItem3(
                       jclassName: String,
                       jclassBytes: Array[Byte]
                     )

  case class Item3(arrivalPos: Int,
                   mirror: SubItem3,
                   plain: SubItem3,
                   bean: SubItem3,
                   outFolder: scala.tools.nsc.io.AbstractFile)

  abstract class ParallelWorker[I <: AnyRef, O](val id: Int, val queue: BlockingQueue[Workflow], val timer: Statistics.Timer) extends Runnable {

    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration.Duration
    import scala.util.{Success, Failure}

    def getWork(workflow: Workflow): Future[I]

    def process(input: I): O

    def nextStageSuccess(workflow: Workflow, result: O): Unit

    def nextStageFailed(workflow: Workflow, ex: Throwable): Unit

    var currentWork: Workflow = _

    @tailrec final def run(): Unit = {
      currentWork = queue.poll()
      if (currentWork ne null) {
        withReporterOverride(currentWork) {
          val work = getWork(currentWork)
          Await.ready(work, Duration.Inf)
          work.value.get match {
            case Success(item) =>
              val start = timer.start()
              try {
                process(item)
                nextStageSuccess(currentWork, process(item))
              } catch {
                case t: Throwable =>
                  nextStageFailed(currentWork, t)
              }
              timer.stop(start)
            case Failure(f) => //TODO
              nextStageFailed(currentWork, f)
          }
        }
        run()
      }
    }
  }

  class OptimisationWorkflow(allData: ArrayBuffer[Item1]) extends Runnable {

    import scala.util.{Try, Success, Failure}

    val compilerSettings = bTypes.compilerSettings
    val byteCodeRepository = bTypes.byteCodeRepository
    val callGraph = bTypes.callGraph

    val optAddToBytecodeRepository = compilerSettings.optAddToBytecodeRepository
    val optBuildCallGraph = compilerSettings.optBuildCallGraph
    val hasGlobalOptimisations = compilerSettings.optInlinerEnabled || compilerSettings.optClosureInvocations

    override def run(): Unit = {
      try {
        val downstreams = allData map { item1 : Item1 =>
          val workflow = item1.workflow
          Await.ready(workflow.optimize.future, Duration.Inf)
          trace("start optimise")
          val upstream = workflow.optimize.future.value.get
          try {
            upstream match {
              case Success(item) =>
                // add classes to the bytecode repo before building the call graph: the latter needs to
                // look up classes and methods in the code repo.
                if (optAddToBytecodeRepository) {
                  val someSourceFilePath = Some(item.sourceFilePath)
                  //byteCodeRepository.add is threadsafe and doesnt access tree
                  if (item.mirror != null) byteCodeRepository.add(item.mirror, someSourceFilePath)
                  if (item.plain != null) byteCodeRepository.add(item.plain, someSourceFilePath)
                  if (item.bean != null) byteCodeRepository.add(item.bean, someSourceFilePath)
                }
                if (optBuildCallGraph) {
                  // skip call graph for mirror / bean: wd don't inline into them, and they are not used in the plain class
                  if (item.plain != null) callGraph.addClass(item.plain)
                }
              case _ =>
            }
            if (!hasGlobalOptimisations) {
              trace("push to Worker2")
              workflow.item2.complete(upstream)
            }
            upstream
          } catch {
            case NonFatal(t) =>
              val downstream = Failure(t)
              if (!hasGlobalOptimisations) {
                trace("push to Worker2")
                workflow.item2.complete(downstream)
              }
              downstream
          }
        }
        if (hasGlobalOptimisations) withAstTreeLock {
          if (compilerSettings.optInlinerEnabled)
            bTypes.inliner.runInliner()
          if (compilerSettings.optClosureInvocations)
            bTypes.closureOptimizer.rewriteClosureApplyInvocations()
          trace("push all to Worker2")
          for (i <- 0 to allData.size) {
            allData(i).workflow.item2.complete(downstreams(i))
          }
        }

      } catch {
        case t: Throwable =>
          val fail = Failure(t)
          allData map {
            _.workflow.item2.tryComplete(fail)
          }
          throw t
      }
    }
  }

  /*
 *  Pipeline that takes ClassNodes from queue-2. The unit of work depends on the optimization level:
 *
 *    (a) no optimization involves:
 *          - converting the plain ClassNode to byte array and placing it on queue-3
 */
  class Worker2(id: Int, q2: BlockingQueue[Workflow]) extends ParallelWorker[Item2, Item3](id, q2, BackendStats.bytesGenStat) {
    val localOpt = bTypes.localOpt
    val backendUtils = bTypes.backendUtils

    override def getWork(workflow: Workflow): Future[Item2] = workflow.item2.future

    override def nextStageSuccess(workflow: Workflow, result: Item3): Unit = {
      trace("push to Worker3")
      workflow.item3.success(result)
    }

    override def nextStageFailed(workflow: Workflow, ex: Throwable): Unit = {
      trace("push to Worker3")
      workflow.item3.failure(ex)
    }

    def localOptimizations(classNode: ClassNode): Unit = withAstTreeLock{
      BackendStats.timed(BackendStats.methodOptTimer)(localOpt.methodOptimizations(classNode))
    }

    def setInnerClasses(classNode: ClassNode): Unit = if (classNode != null) {
      classNode.innerClasses.clear()
      addInnerClasses(classNode, backendUtils.collectNestedClasses(classNode))
    }

    override def process(item: Item2): Item3 = {
      try {
        trace("start Worker2")
        localOptimizations(item.plain)
        setInnerClasses(item.plain)
        val lambdaImplMethods = bTypes.getIndyLambdaImplMethods(item.plain.name)
        if (lambdaImplMethods.nonEmpty)
          backendUtils.addLambdaDeserialize(item.plain, lambdaImplMethods)
        setInnerClasses(item.mirror)
        setInnerClasses(item.bean)
        makeItem3(item)
      } catch {
        case e: java.lang.RuntimeException if e.getMessage != null && (e.getMessage contains "too large!") =>
          reporter.error(NoPosition, s"Could not write class ${item.plain.name} because it exceeds JVM code size limits. ${e.getMessage}")
          throw e
        case ex: Throwable =>
          ex.printStackTrace()
          reporter.error(NoPosition, s"Error while emitting ${item.plain.name}\n${ex.getMessage}")
          throw ex
      }
    }

    private def makeItem3(item: Item2) = {

      def getByteArray(cn: asm.tree.ClassNode): Array[Byte] = {
        val cw = new CClassWriter(extraProc)
        cn.accept(cw)
        cw.toByteArray
      }

      val Item2(arrivalPos, mirror, plain, bean, _, outFolder) = item

      val mirrorC = if (mirror == null) null else SubItem3(mirror.name, getByteArray(mirror))
      val plainC = SubItem3(plain.name, getByteArray(plain))
      val beanC = if (bean == null) null else SubItem3(bean.name, getByteArray(bean))

      if (AsmUtils.traceSerializedClassEnabled && plain.name.contains(AsmUtils.traceSerializedClassPattern)) {
        if (mirrorC != null) AsmUtils.traceClass(mirrorC.jclassBytes)
        AsmUtils.traceClass(plainC.jclassBytes)
        if (beanC != null) AsmUtils.traceClass(beanC.jclassBytes)
      }

      Item3(arrivalPos, mirrorC, plainC, beanC, outFolder)

    }

  }

  // end of class BCodePhase.Worker2


  /* Pipeline that writes classfile representations to disk. */
  class Worker3(id: Int, q3: BlockingQueue[Workflow], bytecodeWriter: BytecodeWriters#BytecodeWriter) extends ParallelWorker[Item3, Unit](id, q3, BackendStats.bcodeWriteTimer) {
    val localOpt = bTypes.localOpt
    val backendUtils = bTypes.backendUtils

    override def getWork(workflow: Workflow): Future[Item3] = workflow.item3.future

    override def nextStageSuccess(workflow: Workflow, result: Unit): Unit = {
      trace("done Worker3")
    }

    override def nextStageFailed(workflow: Workflow, ex: Throwable): Unit = {
      trace("done Worker3")
    }

    def sendToDisk(cfr: SubItem3, outFolder: scala.tools.nsc.io.AbstractFile) {
      if (cfr != null) {
        val SubItem3(jclassName, jclassBytes) = cfr
        try {
          val outFile =
            if (outFolder == null) null
            else getFileForClassfile(outFolder, jclassName, ".class")
          bytecodeWriter.writeClass(jclassName, jclassName, jclassBytes, outFile)
        }
        catch {
          case e: FileConflictException =>
            reporter.error(NoPosition, s"error writing $jclassName: ${e.getMessage}")
        }
      }
    }

    override def process(item: Item3): Unit = {
      trace("start Worker3")
      val outFolder = item.outFolder
      sendToDisk(item.mirror, outFolder)
      sendToDisk(item.plain, outFolder)
      sendToDisk(item.bean, outFolder)
    }
  }
  def trace(s:String) = println (s)
}
