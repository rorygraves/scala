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
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.reflect.internal.util.Statistics
import scala.tools.asm
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.opt.ByteCodeRepository
import scala.util.control.NonFatal

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
abstract class GenBCode extends BCodeSyncAndTry with BCodeParallel  {
  import global.{reporter => _, _}

  import bTypes._
  import coreBTypes._

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)

  final class PlainClassBuilder(cunit: CompilationUnit) extends SyncAndTryBuilder(cunit)

  class BCodePhase(prev: Phase) extends StdPhase(prev) {

    override def name = phaseName
    override def description = "Generate bytecode from ASTs using the ASM library"
    override def erasedTypes = true

    protected[this] var bytecodeWriter  : BytecodeWriter   = null
    protected[this] var mirrorCodeGen   : JMirrorBuilder   = null
    protected[this] var beanInfoCodeGen : JBeanInfoBuilder = null

    /* ---------------- q1 ---------------- */

    private val allData = new ArrayBuffer[Item1]

    /* ---------------- q2 ---------------- */

    private val q2 = new java.util.concurrent.LinkedBlockingQueue[Workflow]

    /* ---------------- q3 ---------------- */

    private val q3 = new java.util.concurrent.LinkedBlockingQueue[Workflow]

    /*
     *  Pipeline that takes ClassDefs from queue-1, lowers them into an intermediate form, placing them on queue-2
     *  This runs in the main thread as it requires access to the tree
     */
     private class Worker1(needsOutFolder: Boolean) {
      //TODO should be a scalac param
      val checkCaseInsensitively = true

      val caseInsensitively = mutable.Map.empty[String, Symbol]

      import scala.util.{Try, Success, Failure}

      def run() = {
        allData foreach { item1 =>
          val item2Try = Try {
            withAstTreeLock(withCurrentUnitNoLog(item1.cunit)(visit(item1)))
          }
          item2Try match {
            case Failure(ex: Throwable) =>
              ex.printStackTrace()
              //we can report directly to the reporter - this is not parallelised
              reporter.error(NoPosition, s"Error while emitting ${item1.cunit.source}\n${ex.getMessage}")
            case _ =>
          }
          startPipeline(item1,item2Try)
        }
      }
      def startPipeline(item1:Item1, item2:Try[Item2]) = {
        trace("push to optimise")
        item1.workflow.optimize.complete(item2)
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
      allData.clear // just in case
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
    def onWorkerError(workerFailed:Throwable) : Unit = {
      workerFailed.printStackTrace()
      reporter.error(NoPosition, workerFailed.toString)
    }
    /*
     *  Sequentially:
     *    (a) place all ClassDefs in queue-1
     *    (b) dequeue one at a time from queue-1, convert it to ASM ClassNode, place in queue-2
     *    (c) dequeue one at a time from queue-2, convert it to byte-array,    place in queue-3
     *    (d) serialize to disk by draining queue-3.
     */
    private def buildAndSendToDisk(needsOutFolder: Boolean) {
      val ec = scala.concurrent.ExecutionContext.fromExecutorService(
        java.util.concurrent.Executors.newCachedThreadPool(), onWorkerError
      )
      import scala.concurrent.Future
      feedPipeline1()

      val workerOpt:Future[Unit] = Future(new OptimisationWorkflow(allData).run)(ec)
      val workers2:Seq[Future[Unit]] = (1 to 3) map {i => Future(new Worker2(i,q2).run)(ec)}
      val workers3:Seq[Future[Unit]] = (1 to (if (bytecodeWriter.isSingleThreaded) 1 else 3)) map {i => Future(new Worker3(i,q3, bytecodeWriter).run)(ec)}

      val genStart = Statistics.startTimer(BackendStats.bcodeGenStat)
      (new Worker1(needsOutFolder)).run()
      Statistics.stopTimer(BackendStats.bcodeGenStat, genStart)

      def checkWorker(worker:Future[Unit]) = {
        scala.concurrent.Await.result(worker, scala.concurrent.duration.Duration.Inf)
        worker.value.get.get
      }

      // check for any exception during the operation of the background threads
      checkWorker(workerOpt)
      workers2 foreach checkWorker
      workers3 foreach checkWorker

      assert(ec.shutdownNow().isEmpty)

      // we're done
      assert(q2.isEmpty, s"Some classfiles remained in the second queue: $q2")
      assert(q3.isEmpty, s"Some classfiles weren't written to disk: $q3")


      //report any deferred messages
      val globalReporter = reporter
      allData foreach {
        data => data.workflow.relayReports(globalReporter)
      }
      allData.clear()

    }

    /* Feed pipeline-1: place all ClassDefs on q1, recording their arrival position. */
    private def feedPipeline1() {
      super.run()
    }


    override def apply(cunit: CompilationUnit): Unit = {

      def gen(tree: Tree) {
        tree match {
          case EmptyTree            => ()
          case PackageDef(_, stats) => stats foreach gen
          case cd: ClassDef         =>
            val workflow = new Workflow
            val item1 = Item1(allData.length, cd, cunit, workflow)
            allData += item1
            q2 add workflow
            q3 add workflow
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
