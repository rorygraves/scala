package scala.tools.nsc.profile

import java.io.{FileWriter, PrintWriter}
import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.tools.nsc.util.RunnableInPhase
import scala.tools.nsc.{Phase, Settings}

object Profiler {
  def apply(settings: Settings):Profiler =
    if (!settings.YprofileEnabled) NoOpProfiler
    else {
      val reporter = if(settings.YprofileDestination.isSetByUser)
        new StreamProfileReporter(new PrintWriter(new FileWriter(settings.YprofileDestination.value, true)))
      else ConsoleProfileReporter
      new RealProfiler(reporter, settings)
    }
}

//TODO separate the main thread wall clock time from the background threads times
case class ProfileCounters(wallClockTimeNanos : Long, idleTimeNanos:Long, cpuTimeNanos: Long, userTimeNanos: Long, allocatedBytes:Long, retainedHeapBytes:Long, gcTimeMillis:Long) {
  def - (that :ProfileCounters) = {
    ProfileCounters(
      this.wallClockTimeNanos - that.wallClockTimeNanos,
      this.idleTimeNanos - that.idleTimeNanos,
      this.cpuTimeNanos - that.cpuTimeNanos,
      this.userTimeNanos - that.userTimeNanos,
      this.allocatedBytes - that.allocatedBytes,
      this.retainedHeapBytes - that.retainedHeapBytes,
      this.gcTimeMillis - that.gcTimeMillis)
  }
  def + (that :ProfileCounters) = {
    ProfileCounters(
      this.wallClockTimeNanos + that.wallClockTimeNanos,
      this.idleTimeNanos + that.idleTimeNanos,
      this.cpuTimeNanos + that.cpuTimeNanos,
      this.userTimeNanos + that.userTimeNanos,
      this.allocatedBytes + that.allocatedBytes,
      this.retainedHeapBytes + that.retainedHeapBytes,
      this.gcTimeMillis + that.gcTimeMillis)
  }
  def updateHeap(heapDetails:ProfileCounters) = {
    copy(retainedHeapBytes = heapDetails.retainedHeapBytes)
  }
  private def toMillis(ns: Long) = ns/1000000.0D
  private def toMegaBytes(bytes: Long) = bytes/1000000.0D

  def wallClockTimeMillis = toMillis(wallClockTimeNanos)
  def idleTimeMillis = toMillis(idleTimeNanos)
  def cpuTimeMillis = toMillis(cpuTimeNanos)
  def userTimeMillis = toMillis(userTimeNanos)
  def allocatedMB = toMegaBytes(allocatedBytes)
  def retainedHeapMB = toMegaBytes(retainedHeapBytes)

}

sealed trait Profiler {
  def registerRunnable(runnable: RunnableInPhase): Unit
  /** record a runnable. The runnable may be in the main thread or more typically in a background thread
    */
  def beforeRunnable(runnable: RunnableInPhase): Unit

  /** called after a Runnable completes work
    */
  def afterRunnable(runnable: RunnableInPhase, idleTime:Long): Unit

  def beforeInit() : ProfileCounters
  def afterInit(profileBefore : ProfileCounters)

  def finished() :Unit

  def after(phase: Phase, profileBefore: ProfileCounters) : Unit

  def before(phase: Phase) : ProfileCounters

  val emptySnap = ProfileCounters(0,0,0,0,0,0,0)

}
private [profile] object NoOpProfiler extends Profiler {

  override def beforeInit() = emptySnap

  override def afterInit(profileBefore: ProfileCounters): Unit = ()

  override def before(phase: Phase): ProfileCounters = emptySnap

  override def after(phase: Phase, profileBefore: ProfileCounters): Unit = ()

  override def finished(): Unit = ()

  override def registerRunnable(runnable: RunnableInPhase) = ()

  override def beforeRunnable(runnable: RunnableInPhase) = ()

  override def afterRunnable(runnable: RunnableInPhase, idleTime:Long) = ()
}
private [profile] object RealProfiler {
  import scala.collection.JavaConverters._
  val runtimeMx = ManagementFactory.getRuntimeMXBean
  val memoryMx = ManagementFactory.getMemoryMXBean
  val gcMx = ManagementFactory.getGarbageCollectorMXBeans.asScala.toList
  val classLoaderMx = ManagementFactory.getClassLoadingMXBean
  val compileMx = ManagementFactory.getCompilationMXBean
  val threadMx = ExtendedThreadMxBean.proxy
  if (threadMx.isThreadCpuTimeSupported) threadMx.setThreadCpuTimeEnabled(true)
  private val idGen = new AtomicInteger()
}

private [profile] class RealProfiler(reporter : ProfileReporter, val settings: Settings) extends Profiler {
  def outDir = settings.outputDirs.getSingleOutput.getOrElse(settings.outputDirs.outputs.head._2.file).toString

  val id = RealProfiler.idGen.incrementAndGet()

  private var snapBeforeInit: ProfileCounters = _
  private val mainThread = Thread.currentThread()

  override def beforeInit() = snap

  override def afterInit(profileBefore: ProfileCounters): Unit = {
    reporter.reportInit(this, snap - profileBefore - overhead)
  }


  private def snap: ProfileCounters = {
    import RealProfiler._
    ProfileCounters(System.nanoTime(), 0, threadMx.getCurrentThreadCpuTime, threadMx.getCurrentThreadUserTime,
      threadMx.getThreadAllocatedBytes(Thread.currentThread().getId), memoryMx.getHeapMemoryUsage.getUsed,
      gcMx.foldLeft(0L) { case (sum, bean) => bean.getCollectionTime + sum })
  }

  private def snapBackground: ProfileCounters = {
    import RealProfiler._
    ProfileCounters(0, 0, threadMx.getCurrentThreadCpuTime, threadMx.getCurrentThreadUserTime,
      threadMx.getThreadAllocatedBytes(Thread.currentThread().getId), 0, 0)
  }

  private val overhead = {
    val s1 = snap
    val s2 = snap
    s2 - s1
  }

  private def doGC: Unit = {
    System.gc()
    System.runFinalization()
  }

  reporter.header(this)

  override def finished(): Unit = reporter.close(this)

  var total = emptySnap

  override def after(phase: Phase, profileBefore: ProfileCounters): Unit = {
    assert(mainThread eq Thread.currentThread())
    val initialSnap = snap
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook stop")
      ExternalToolHook.after()
    }
    val finalSnap = if (settings.YprofileRunGcBetweenPhases.containsPhase(phase)) {
      doGC
      initialSnap.updateHeap(snap)
    } else initialSnap
    val mainThreadUsage = finalSnap - profileBefore - overhead
    threadInfo.synchronized {
      total += mainThreadUsage
      threadInfo.get(phase) match {
        case None =>
          reporter.report(this, phase, EventType.SINGLE, -1, "--", mainThreadUsage)
        case Some(info) =>
          info.mainThreadUsage(mainThreadUsage)
      }
    }
  }

  override def before(phase: Phase): ProfileCounters = {
    assert(mainThread eq Thread.currentThread())
    if (settings.YprofileRunGcBetweenPhases.containsPhase(phase))
      doGC
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook start")
      ExternalToolHook.before()
    }
    snap
  }

  private val threadInfo = mutable.Map[Phase, ThreadInfo]()

  override def registerRunnable(runnable: RunnableInPhase) = {
    threadInfo.synchronized {
      threadInfo.getOrElseUpdate(runnable.phase, new ThreadInfo(runnable.phase)).registerRunnable(runnable)
    }
  }

  override def beforeRunnable(runnable: RunnableInPhase): Unit = {
    threadInfo.synchronized {
      threadInfo.getOrElseUpdate(runnable.phase, new ThreadInfo(runnable.phase)).startRunnable(runnable)
    }
  }

  override def afterRunnable(runnable: RunnableInPhase, idleTime: Long): Unit = {
    threadInfo.synchronized {
      threadInfo(runnable.phase).endRunnable(runnable)
    }
  }

  class ThreadInfo(phase: Phase) {
    private var otherThreadsTotalUsage = emptySnap
    private var mainThreadUsage: ProfileCounters = _
    private val pendingThreads = mutable.Map[Int, ProfileCounters]()

    def registerRunnable(runnable: RunnableInPhase): Unit = this.synchronized {
      pendingThreads(runnable.id) = emptySnap
    }

    def startRunnable(runnable: RunnableInPhase): Unit = this.synchronized {
      pendingThreads(runnable.id) = snapBackground
    }

    def endRunnable(runnable: RunnableInPhase): Unit = this.synchronized {
      val profileBefore = pendingThreads.remove(runnable.id)
      val thisThreadUsage = snapBackground - profileBefore.get - overhead
      if (mainThread != Thread.currentThread()) {
        otherThreadsTotalUsage += thisThreadUsage
        reporter.report(RealProfiler.this, phase, EventType.TASK, runnable.id, runnable.comment, thisThreadUsage)
        if ((pendingThreads isEmpty) && (mainThreadUsage ne null)) {
          reporter.report(RealProfiler.this, phase, EventType.TOTAL, -1, "--", mainThreadUsage + otherThreadsTotalUsage)
        }
      } else {
        reporter.report(RealProfiler.this, phase, EventType.TASK, runnable.id, runnable.comment, thisThreadUsage)
      }
    }

    def mainThreadUsage(mainThreadUsage: ProfileCounters): Unit = this.synchronized {
      this.mainThreadUsage = mainThreadUsage
      reporter.report(RealProfiler.this, phase, EventType.MAIN, -1, "--", mainThreadUsage)

      if (pendingThreads isEmpty) {
        reporter.report(RealProfiler.this, phase, EventType.TOTAL, -1, "--", mainThreadUsage + otherThreadsTotalUsage)
        total += otherThreadsTotalUsage
      } else {
        println("late reporting for " + phase)
      }
    }
  }
}

object EventType extends Enumeration {
  // only one report for a phase
  val SINGLE = Value("single")
  //main thread with other tasks
  val MAIN = Value("main")
  //other task ( background thread)
  val TASK = Value("task")
  //total for phase
  val TOTAL = Value("total")
  //total for compile
  val ALL = Value("all")
}
sealed trait ProfileReporter {
  def report(profiler: RealProfiler, phase: Phase, eventType:EventType.Value, id:Int, desc:String, diff: ProfileCounters) : Unit
  def reportInit(profiler: RealProfiler, diff: ProfileCounters) : Unit

  def header(profiler: RealProfiler) :Unit
  def close(profiler: RealProfiler) :Unit
}

object ConsoleProfileReporter extends ProfileReporter {
  override def report(profiler: RealProfiler, phase: Phase, eventType:EventType.Value, id:Int, desc:String, diff: ProfileCounters): Unit =
    println(f"Profiler compile ${profiler.id} after phase ${phase.id}%2d:${phase.name}%20s ${eventType}%10s ${desc}%20s wallClockTime: ${diff.wallClockTimeMillis}%12.4fms, idleTime: ${diff.idleTimeMillis}%12.4fms, cpuTime ${diff.cpuTimeMillis}%12.4fms, userTime ${diff.userTimeMillis}%12.4fms, allocatedBytes ${diff.allocatedMB}%12.4fMB, retainedHeapBytes ${diff.retainedHeapMB}%12.4fMB, gcTime ${diff.gcTimeMillis}%6.0fms")
  override def reportInit(profiler: RealProfiler, diff: ProfileCounters): Unit =
    println(f"Profiler compile ${profiler.id} after phase ${0}%2d:${"<<INIT>>"}%20s ${"single"}%10s wallClockTime: ${diff.wallClockTimeMillis}%12.4fms, idleTime: ${diff.idleTimeMillis}%12.4fms, cpuTime ${diff.cpuTimeMillis}%12.4fms, userTime ${diff.userTimeMillis}%12.4fms, allocatedBytes ${diff.allocatedMB}%12.4fMB, retainedHeapBytes ${diff.retainedHeapMB}%12.4fMB, gcTime ${diff.gcTimeMillis}%6.0fms")

  override def close(profiler: RealProfiler): Unit = ()

  override def header(profiler: RealProfiler): Unit = {
    println(s"Profiler start (${profiler.id}) ${profiler.outDir}")
  }
}

class StreamProfileReporter(out:PrintWriter) extends ProfileReporter {
  override def header(profiler: RealProfiler): Unit = {
    out.println(s"info, ${profiler.id}, ${profiler.outDir}")
    out.println(s"header,id,phaseId,phaseName,type,id,comment,wallClockTimeMs,idleTimeMs,cpuTimeMs,userTimeMs,allocatedMB,retainedHeapMB,gcTimeMs")
  }
  override def report(profiler: RealProfiler, phase: Phase, eventType:EventType.Value, id:Int, desc:String, diff: ProfileCounters): Unit = {
    out.println(s"data,${profiler.id},${phase.id},${phase.name},${eventType},$id,$desc, ${diff.wallClockTimeMillis},${diff.idleTimeMillis},${diff.cpuTimeMillis},${diff.userTimeMillis},${diff.allocatedMB},${diff.retainedHeapMB},${diff.gcTimeMillis}")
  }
  override def reportInit(profiler: RealProfiler, diff: ProfileCounters): Unit = {
    out.println(s"data,${profiler.id},${0},${"<<INIT>>"},single,-1,--,${diff.wallClockTimeMillis},${diff.idleTimeMillis},${diff.cpuTimeMillis},${diff.userTimeMillis},${diff.allocatedMB},${diff.retainedHeapMB},${diff.gcTimeMillis}")
  }

  override def close(profiler: RealProfiler): Unit = {
    out.flush
    out.close
  }
}

