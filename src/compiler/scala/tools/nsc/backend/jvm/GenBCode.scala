/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala
package tools.nsc
package backend
package jvm

import java.util.concurrent.BlockingQueue

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Future, Promise}
import scala.reflect.internal.util.Statistics
import scala.tools.asm
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.opt.ByteCodeRepository

/*
 *  Prepare in-memory representations of classfiles using the ASM Tree API, and serialize them to disk.
 *
 *  Three pipelines are at work, each taking work items from a queue dedicated to that pipeline:
 *
 *  (There's another pipeline so to speak, the one that populates queue-1 by traversing a CompilationUnit until ClassDefs are found,
 *   but the "interesting" pipelines are the ones described below)
 *
 *    (1) In the first queue, an item consists of a ClassDef along with its arrival position.
 *        This position is needed at the time classfiles are serialized to disk,
 *        so as to emit classfiles in the same order CleanUp handed them over.
 *        As a result, two runs of the compiler on the same files produce jars that are identical on a byte basis.
 *        See `ant test.stability`
 *
 *    (2) The second queue contains items where a ClassDef has been lowered into:
 *          (a) an optional mirror class,
 *          (b) a plain class, and
 *          (c) an optional bean class.
 *
 *    (3) The third queue contains items ready for serialization.
 *        It's a priority queue that follows the original arrival order,
 *        so as to emit identical jars on repeated compilation of the same sources.
 *
 *  Plain, mirror, and bean classes are built respectively by PlainClassBuilder, JMirrorBuilder, and JBeanInfoBuilder.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
abstract class GenBCode extends BCodeSyncAndTry {
  import global._

  import bTypes._
  import coreBTypes._

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)

  final class PlainClassBuilder(cunit: CompilationUnit) extends SyncAndTryBuilder(cunit)

  class BCodePhase(prev: Phase) extends StdPhase(prev) {

    override def name = phaseName
    override def description = "Generate bytecode from ASTs using the ASM library"
    override def erasedTypes = true

    private var bytecodeWriter  : BytecodeWriter   = null
    private var mirrorCodeGen   : JMirrorBuilder   = null
    private var beanInfoCodeGen : JBeanInfoBuilder = null

    class Workflow {
      val item2 = Promise[Item2]
      val item3 = Promise[Item3]
    }
    /* ---------------- q1 ---------------- */

    case class Item1(arrivalPos: Int, cd: ClassDef, cunit: CompilationUnit, workflow :Workflow)

    private val q1 = new ArrayBuffer[Item1]

    /* ---------------- q2 ---------------- */

    case class Item2(arrivalPos:     Int,
                     mirror:         asm.tree.ClassNode,
                     plain:          asm.tree.ClassNode,
                     bean:           asm.tree.ClassNode,
                     sourceFilePath: String,
                     outFolder:      scala.tools.nsc.io.AbstractFile)
    private val q2 = new java.util.concurrent.LinkedBlockingQueue[Workflow]

    /* ---------------- q3 ---------------- */

    /*
     *  An item of queue-3 (the last queue before serializing to disk) contains three of these
     *  (one for each of mirror, plain, and bean classes).
     *
     *  @param jclassName  internal name of the class
     *  @param jclassBytes bytecode emitted for the class SubItem3 represents
     */
    case class SubItem3(
      jclassName:  String,
      jclassBytes: Array[Byte]
    )

    case class Item3(arrivalPos: Int,
                     mirror:     SubItem3,
                     plain:      SubItem3,
                     bean:       SubItem3,
                     outFolder:  scala.tools.nsc.io.AbstractFile){
    }
    private val q3 = new java.util.concurrent.LinkedBlockingQueue[Workflow]

    /*
     *  Pipeline that takes ClassDefs from queue-1, lowers them into an intermediate form, placing them on queue-2
     *  This runs in the main thread as it requires access to the tree
     */
     private class Worker1(needsOutFolder: Boolean) {
      //TODO should be a scalac param
      val checkCaseInsensitively = true
      val optAddToBytecodeRepository = settings.optAddToBytecodeRepository
      val optBuildCallGraph = settings.optBuildCallGraph

      val caseInsensitively = mutable.Map.empty[String, Symbol]

      import scala.util.{Try, Success, Failure}

      def run() = {
        val globalOptimisations = hasGlobalOptimisations
        val pendingStarts = new ArrayBuffer[(Item1, Try[Item2])](q1.size)
        q1 foreach { item1 =>
          val item2Try = Try {
            withCurrentUnitNoLog(item1.cunit)(visit(item1))
          }
          item2Try match {
            case Success(item2) =>
              runOptimizations(item2)
            case Failure(ex: Throwable) =>
              ex.printStackTrace()
              reporter.error(NoPosition, s"Error while emitting ${item1.cunit.source}\n${ex.getMessage}")
          }
          if (globalOptimisations) pendingStarts += ((item1, item2Try))
          else startPipeline(item1,item2Try)
        }
        runGlobalOptimizations()
        if (globalOptimisations)
          pendingStarts foreach {
            case (item1, item2try) => startPipeline (item1,item2try)
          }
      }
      def startPipeline(item1:Item1, item2:Try[Item2]) = {
        item1.workflow.item2.complete(item2)
      }

      def runOptimizations(item:Item2): Unit = {
        // add classes to the bytecode repo before building the call graph: the latter needs to
        // look up classes and methods in the code repo.
        if (optAddToBytecodeRepository) {
            val someSourceFilePath = Some(item.sourceFilePath)
            if (item.mirror != null) byteCodeRepository.add(item.mirror, someSourceFilePath)
            if (item.plain != null) byteCodeRepository.add(item.plain, someSourceFilePath)
            if (item.bean != null) byteCodeRepository.add(item.bean, someSourceFilePath)
        }
        if (optBuildCallGraph) {
          // skip call graph for mirror / bean: wd don't inline into them, and they are not used in the plain class
          if (item.plain != null) callGraph.addClass(item.plain)
        }
      }
      def hasGlobalOptimisations = settings.optInlinerEnabled || settings.optClosureInvocations
      def runGlobalOptimizations(): Unit = {
        if (settings.optInlinerEnabled)
          bTypes.inliner.runInliner()
        if (settings.optClosureInvocations)
          closureOptimizer.rewriteClosureApplyInvocations()
      }


      /*
       *  Checks for duplicate internal names case-insensitively,
       *  builds ASM ClassNodes for mirror, plain, and bean classes;
       *  returns an Item2
       *
       */
      def visit(item: Item1) = {
        val Item1(arrivalPos, cd, cunit, _) = item
        val claszSymbol = cd.symbol

        if (checkCaseInsensitively) {
          // GenBCode checks this before classfiles are emitted, https://github.com/scala/scala/commit/e4d1d930693ac75d8eb64c2c3c69f2fc22bec739
          val lowercaseJavaClassName = claszSymbol.javaClassName.toLowerCase
          caseInsensitively.put(lowercaseJavaClassName, claszSymbol).foreach {
            dupClassSym =>
              reporter.warning(
                claszSymbol.pos,
                s"Class ${claszSymbol.javaClassName} differs only in case from ${dupClassSym.javaClassName}. " +
                  "Such classes will overwrite one another on case-insensitive filesystems."
              )
          }
        }

        // shim for SBT, see https://github.com/sbt/sbt/issues/2076
        // TODO put this closer to classfile writing once we have closure elimination
        // TODO create a nicer public API to find out the correspondence between sourcefile and ultimate classfiles
        currentUnit.icode += new icodes.IClass(cd.symbol)

        // -------------- mirror class, if needed --------------
        val mirrorC =
          if (isTopLevelModuleClass(claszSymbol)) {
            if (claszSymbol.companionClass == NoSymbol) {
              mirrorCodeGen.genMirrorClass(claszSymbol, cunit)
            } else {
              log(s"No mirror class for module with linked class: ${claszSymbol.fullName}")
              null
            }
          } else null

        // -------------- "plain" class --------------
        val pcb = new PlainClassBuilder(cunit)
        pcb.genPlainClass(cd)
        val outF = if (needsOutFolder) getOutFolder(claszSymbol, pcb.thisBType.internalName, cunit) else null
        val plainC = pcb.cnode

        // -------------- bean info class, if needed --------------
        val beanC =
          if (claszSymbol hasAnnotation BeanInfoAttr) {
            beanInfoCodeGen.genBeanInfoClass(
              claszSymbol, cunit,
              fieldSymbols(claszSymbol),
              methodSymbols(cd)
            )
          } else null

        // ----------- hand over to pipeline-2

        Item2(arrivalPos,
          mirrorC, plainC, beanC,
          cunit.source.file.canonicalPath,
          outF)

      } // end of method visit(Item1)

    } // end of class BCodePhase.Worker1

    abstract class ParallelWorker[I <: AnyRef,O](val id: Int, val queue: BlockingQueue[Workflow], val timer:Statistics.Timer) extends Runnable {

      import scala.concurrent.{Await, Future}
      import scala.concurrent.duration.Duration
      import scala.util.{Success, Failure}

      def getWork(workflow: Workflow): Future[I]

      def process(input: I): O

      def nextStageSuccess(workflow: Workflow, result: O): Unit

      def nextStageFailed(workflow: Workflow, ex: Throwable): Unit

      @tailrec final def run() : Unit = {
        val currentWork = queue.poll()
        if (currentWork ne null) {
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

          run()
        }
      }
    }

    val ec = scala.concurrent.ExecutionContext.fromExecutorService(
      java.util.concurrent.Executors.newCachedThreadPool(), onWorkerError
    )
    def onWorkerError(workerFailed:Throwable) : Unit = {
      workerFailed.printStackTrace()
      reporter.error(NoPosition, workerFailed.toString)
    }
    object Worker2 {
      import scala.concurrent.Future
      def startWorkers()= for (i <- 1 to 3) yield {
        val worker = new Worker2(i)
        Future(worker)(ec)
      }
    }
    /*
     *  Pipeline that takes ClassNodes from queue-2. The unit of work depends on the optimization level:
     *
     *    (a) no optimization involves:
     *          - converting the plain ClassNode to byte array and placing it on queue-3
     */
    class Worker2(id:Int) extends ParallelWorker[Item2, Item3](id, q2, BackendStats.bytesGenStat) {

      override def getWork(workflow: Workflow): Future[Item2] = workflow.item2.future

      override def nextStageSuccess(workflow: Workflow, result: Item3): Unit =
        workflow.item3.success(result)

      override def nextStageFailed(workflow: Workflow, ex: Throwable): Unit =
        workflow.item3.failure(ex)


      def localOptimizations(classNode: ClassNode): Unit = {
        BackendStats.timed(BackendStats.methodOptTimer)(localOpt.methodOptimizations(classNode))
      }

      def setInnerClasses(classNode: ClassNode): Unit = if (classNode != null) {
        classNode.innerClasses.clear()
        addInnerClasses(classNode, bTypes.backendUtils.collectNestedClasses(classNode))
      }

      override def process(item: Item2): Item3 = {
        try {
          localOptimizations(item.plain)
          setInnerClasses(item.plain)
          val lambdaImplMethods = getIndyLambdaImplMethods(item.plain.name)
          if (lambdaImplMethods.nonEmpty)
            backendUtils.addLambdaDeserialize(item.plain, lambdaImplMethods)
          setInnerClasses(item.mirror)
          setInnerClasses(item.bean)
          makeItem3(item)
        } catch {
          case e: java.lang.RuntimeException if e.getMessage != null && (e.getMessage contains "too large!") =>
            reporter.error(NoPosition,
              s"Could not write class ${item.plain.name} because it exceeds JVM code size limits. ${e.getMessage}")
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
        val plainC  = SubItem3(plain.name, getByteArray(plain))
        val beanC   = if (bean == null)   null else SubItem3(bean.name, getByteArray(bean))

        if (AsmUtils.traceSerializedClassEnabled && plain.name.contains(AsmUtils.traceSerializedClassPattern)) {
          if (mirrorC != null) AsmUtils.traceClass(mirrorC.jclassBytes)
          AsmUtils.traceClass(plainC.jclassBytes)
          if (beanC != null) AsmUtils.traceClass(beanC.jclassBytes)
        }

        Item3(arrivalPos, mirrorC, plainC, beanC, outFolder)

      }

    } // end of class BCodePhase.Worker2

    var arrivalPos = 0

    /**
     * The `run` method is overridden because the backend has a different data flow than the default
     * phase: the backend does not transform compilation units one by one, but on all units in the
     * same run. This allows cross-unit optimizations and running some stages of the backend
     * concurrently on multiple units.
     *
     *  A run of the BCodePhase phase comprises:
     *
     *    (a) set-up steps (most notably supporting maps in `BCodeTypes`,
     *        but also "the" writer where class files in byte-array form go)
     *
     *    (b) building of ASM ClassNodes, their optimization and serialization.
     *
     *    (c) tear down (closing the classfile-writer and clearing maps)
     *
     */
    override def run() {
      val bcodeStart = Statistics.startTimer(BackendStats.bcodeTimer)

      val initStart = Statistics.startTimer(BackendStats.bcodeInitTimer)
      arrivalPos = 0 // just in case
      scalaPrimitives.init()
      bTypes.initializeCoreBTypes()
      bTypes.javaDefinedClasses.clear()
      bTypes.javaDefinedClasses ++= currentRun.symSource collect {
        case (sym, _) if sym.isJavaDefined => sym.javaBinaryNameString
      }
      Statistics.stopTimer(BackendStats.bcodeInitTimer, initStart)

      // initBytecodeWriter invokes fullName, thus we have to run it before the typer-dependent thread is activated.
      bytecodeWriter  = initBytecodeWriter(cleanup.getEntryPoints)
      mirrorCodeGen   = new JMirrorBuilder
      beanInfoCodeGen = new JBeanInfoBuilder

      val needsOutfileForSymbol = bytecodeWriter.isInstanceOf[ClassBytecodeWriter]
      buildAndSendToDisk(needsOutfileForSymbol)

      // closing output files.
      bytecodeWriter.close()
      Statistics.stopTimer(BackendStats.bcodeTimer, bcodeStart)

      /* TODO Bytecode can be verified (now that all classfiles have been written to disk)
       *
       * (1) asm.util.CheckAdapter.verify()
       *       public static void verify(ClassReader cr, ClassLoader loader, boolean dump, PrintWriter pw)
       *     passing a custom ClassLoader to verify inter-dependent classes.
       *     Alternatively,
       *       - an offline-bytecode verifier could be used (e.g. Maxine brings one as separate tool).
       *       - -Xverify:all
       *
       * (2) if requested, check-java-signatures, over and beyond the syntactic checks in `getGenericSignature()`
       *
       */
    }

    /*
     *  Sequentially:
     *    (a) place all ClassDefs in queue-1
     *    (b) dequeue one at a time from queue-1, convert it to ASM ClassNode, place in queue-2
     *    (c) dequeue one at a time from queue-2, convert it to byte-array,    place in queue-3
     *    (d) serialize to disk by draining queue-3.
     */
    private def buildAndSendToDisk(needsOutFolder: Boolean) {
      val workers2 = Worker2.startWorkers()
      val workers3 = Worker3.startWorkers()

      feedPipeline1()
      val genStart = Statistics.startTimer(BackendStats.bcodeGenStat)
      (new Worker1(needsOutFolder)).run()
      Statistics.stopTimer(BackendStats.bcodeGenStat, genStart)

      (workers2 ++ workers3) foreach {
        // check for any exception during the operation of the backgroud threads
        f => scala.concurrent.Await.result(f, scala.concurrent.duration.Duration.Inf)
      }
      assert(ec.shutdownNow().isEmpty)

      // we're done
      assert(q1.isEmpty, s"Some ClassDefs remained in the first queue: $q1")
      assert(q2.isEmpty, s"Some classfiles remained in the second queue: $q2")
      assert(q3.isEmpty, s"Some classfiles weren't written to disk: $q3")

    }

    /* Feed pipeline-1: place all ClassDefs on q1, recording their arrival position. */
    private def feedPipeline1() {
      super.run()
    }

    object Worker3 {
      import scala.concurrent.Future
      def startWorkers()= for (i <- 1 to 3) yield {
        val worker = new Worker3(i)
        Future(worker)(ec)
      }
    }
    /* Pipeline that writes classfile representations to disk. */
    class Worker3(id:Int) extends ParallelWorker[Item3, Unit](id, q3,BackendStats.bcodeWriteTimer) {
      override def getWork(workflow: Workflow): Future[Item3] = workflow.item3.future

      override def nextStageSuccess(workflow: Workflow, result: Unit): Unit = ()

      override def nextStageFailed(workflow: Workflow, ex: Throwable): Unit = ()

      def sendToDisk(cfr: SubItem3, outFolder: scala.tools.nsc.io.AbstractFile) {
        if (cfr != null){
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
        val outFolder = item.outFolder
        sendToDisk(item.mirror, outFolder)
        sendToDisk(item.plain,  outFolder)
        sendToDisk(item.bean,   outFolder)
      }

    }

    override def apply(cunit: CompilationUnit): Unit = {

      def gen(tree: Tree) {
        tree match {
          case EmptyTree            => ()
          case PackageDef(_, stats) => stats foreach gen
          case cd: ClassDef         =>
            val workflow = new Workflow
            val item1 = Item1(arrivalPos, cd, cunit, workflow)
            q1 += item1

            arrivalPos += 1
        }
      }

      gen(cunit.body)
    }

  } // end of class BCodePhase

} // end of class GenBCode

object GenBCode {
  def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

  final val PublicStatic      = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC
  final val PublicStaticFinal = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL

  val CLASS_CONSTRUCTOR_NAME    = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"
}
