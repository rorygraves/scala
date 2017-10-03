import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import java.util.zip.{CRC32, ZipEntry, ZipOutputStream}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

object Compare extends App {

  def time(name: String, threads: Int, size: Int, iteration: Int)(f: => Unit): Unit = {
    val start = System.nanoTime()
    f
    val end = System.nanoTime()
    val time: Double = end - start

    val timeMS = {
      time / 1000 / 1000
    }
    println(s"$name,$threads,$size,$iteration,$timeMS")
//    Thread.sleep(5000)
  }


  val root = Paths.get(args(0))
  println(s"running in $root")

  def clear(): Unit = if (Files.exists(root)) {
    val filesPattern = "data_[0-9]+.txt".r
    Files.walk(root).toArray.foreach{
      case file:Path =>
        val name = file.getFileName.toString
        if (Files.isRegularFile(file) && (name == "archive.zip" || filesPattern.pattern.matcher(name).matches())) Files.delete(file)
    }
  }

  def rawIO(data: Seq[(Int, Array[Byte])]): Unit = {
    for ((i, bytes) <- data) {
      val file = new File(root.toFile, s"data_$i.txt")
      //      println(s"$file")
      file.createNewFile()
      val os = new FileOutputStream(file)
      os.write(bytes)
      os.close()
    }
  }

  def parIO(n: Int, data: Seq[(Int, Array[Byte])]): Unit = {
    implicit val e = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(n))
    val futures = for ((i, bytes) <- data) yield Future {
      val file = new File(root.toFile, s"data_$i.txt")
      //      println(s"$file")
      file.createNewFile()
      val os = new FileOutputStream(file)
      os.write(bytes)
      os.close()
    }
    futures foreach (Await.result(_, Duration.Inf))
  }

  def parNIO(n: Int, data: Seq[(Int, Array[Byte])]): Unit = {
    implicit val e = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(n))
    val futures = for ((i, bytes) <- data) yield Future {
      val newPath = root.resolve(s"data_$i.txt")
      val os = Files.newOutputStream(newPath)
      os.write(bytes)
      os.close()
    }
    futures foreach (Await.result(_, Duration.Inf))
  }

  def parNIO2(n: Int, data: Seq[(Int, Array[Byte])]): Unit = {
    implicit val e = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(n))
    val futures = for ((i, bytes) <- data) yield Future {
      val newPath = root.resolve(s"data_$i.txt")
      val os = Files.write(newPath, bytes)
    }
    futures foreach (Await.result(_, Duration.Inf))
  }

  def parNIO3(n: Int, data: Seq[(Int, Array[Byte])]): Unit = {
    val ex = Executors.newFixedThreadPool(n)

    object comp extends CompletionHandler[Integer, AsynchronousFileChannel] {
      val counter = new AtomicInteger(1)
      val result = Promise[Unit]()

      private def close(channel: AsynchronousFileChannel): Unit = {
        try {
          channel.close()
        } catch {
          case e: Throwable => result.tryFailure(e)
        }
      }

      override def failed(exc: Throwable, channel: AsynchronousFileChannel) = {
        result.tryFailure(exc)
        close(channel)
      }

      override def completed(r: Integer, channel: AsynchronousFileChannel) = {
        close(channel)
        if (counter.decrementAndGet() == 0) result.trySuccess(())
      }
    }
    val openOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    data map { case (i, bytes) =>
      val newPath = root.resolve(s"data_$i.txt")
      comp.counter.incrementAndGet()
      val os = AsynchronousFileChannel.open(newPath, openOptions, ex)
      os.write(ByteBuffer.wrap(bytes), 0L, os, comp)
    }

    if (comp.counter.decrementAndGet() == 0) comp.result.trySuccess(())
    Await.result(comp.result.future, Duration.Inf)
  }

  def parNIO4(n: Int, data: Seq[(Int, Array[Byte])]): Unit = {
    val ex = Executors.newFixedThreadPool(n)
    implicit val e = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(n))

    object comp extends CompletionHandler[Integer, AsynchronousFileChannel] {
      val counter = new AtomicInteger(1)
      val result = Promise[Unit]()

      private def close(channel: AsynchronousFileChannel): Unit = {
        try {
          channel.close()
        } catch {
          case e: Throwable => result.tryFailure(e)
        }
      }

      override def failed(exc: Throwable, channel: AsynchronousFileChannel) = {
        result.tryFailure(exc)
        close(channel)
      }

      override def completed(r: Integer, channel: AsynchronousFileChannel) = {
        close(channel)
        if (counter.decrementAndGet() == 0) result.trySuccess(())
      }
    }
    val openOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    data.map { case (i, bytes) =>
      val newPath = root.resolve(s"data_$i.txt")
      comp.counter.incrementAndGet()
      Future {
        val os = AsynchronousFileChannel.open(newPath, openOptions, ex)
        os.write(ByteBuffer.wrap(bytes), 0L, os, comp)
      }
    }.map {
      Await.result(_, Duration.Inf)
    }

    if (comp.counter.decrementAndGet() == 0) comp.result.trySuccess(())
    Await.result(comp.result.future, Duration.Inf)
  }

  def parNIO5(n: Int, data: Seq[(Int, Array[Byte])]): Unit = {
    val ex = Executors.newFixedThreadPool(n)
    implicit val e = ExecutionContext.fromExecutor(ex)

    val openOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    data.map { case (i, bytes) =>
      val newPath = root.resolve(s"data_$i.txt")
      Future {
        val os = AsynchronousFileChannel.open(newPath, openOptions, ex)
        (os, os.write(ByteBuffer.wrap(bytes), 0L))
      }.map {
        case (os, _) =>
          os.close()
      }
    }.foreach {
      f =>
        Await.result(f, Duration.Inf)
    }
  }

  def parNIO6(n: Int, data: Seq[(Int, Array[Byte])]): Unit = {
    val ex = Executors.newFixedThreadPool(n)
    implicit val e = ExecutionContext.fromExecutor(ex)

    val openOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.DSYNC, StandardOpenOption.SYNC)
    data.map { case (i, bytes) =>
      val newPath = root.resolve(s"data_$i.txt")
      Future {
        val os = AsynchronousFileChannel.open(newPath, openOptions, ex)
        (os, os.write(ByteBuffer.wrap(bytes), 0L))
      }.map {
        case (os, _) =>
          os.close()
      }
    }.foreach {
      f =>
        Await.result(f, Duration.Inf)
    }
  }

  def zip(data: Seq[(Int, Array[Byte])]): Unit = {
    val zip = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(root.resolve("archive.zip").toFile), 1024 * 1024))
    data.foreach { case (id, fileData) =>
      zip.putNextEntry(new ZipEntry(s"file$id"))
      zip.write(fileData)
      zip.closeEntry()
    }
    zip.close()
  }

  def zipStored(data: Seq[(Int, Array[Byte])]): Unit = {
    val zip = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(root.resolve("archive.zip").toFile), 1024 * 1024))
    zip.setMethod(ZipEntry.STORED)
    val crc = new CRC32
    data.foreach { case (id, fileData) =>
      val e = new ZipEntry(s"file$id")
      e.setSize(fileData.length)
      e.setCompressedSize(fileData.length)
      crc.reset()
      crc.update(fileData)
      e.setCrc(crc.getValue)
      zip.putNextEntry(e)
      zip.write(fileData)
      zip.closeEntry()
    }
    zip.close()
  }

  val count = 100

  val ioThreads = List(1, 2, 3, 4, 6, 8, 12, 16, 32, 64, 96, 128)

  for (size <- List(1000, 10000, 100000)) {
    val raw = new Array[Byte](size)
    val data = (1 to 1000) map (i => (i, raw))

    for (iter <- 1 to count) {
      clear()
      time("raw", 1, size, iter)(rawIO(data))
    }
    for (thread <- ioThreads; iter <- 1 to count) {
      clear()
      time("par", thread, size, iter)(parIO(thread, data))
    }
    for (thread <- ioThreads; iter <- 1 to count) {
      clear()
      time("parNIO", thread, size, iter)(parNIO(thread, data))
    }
    for (thread <- ioThreads; iter <- 1 to count) {
      clear()
      time("parNIO2", thread, size, iter)(parNIO2(thread, data))
    }
    for (thread <- ioThreads; iter <- 1 to count) {
      clear()
      time("parNIO3", thread, size, iter)(parNIO3(thread, data))
    }
    for (thread <- ioThreads; iter <- 1 to count) {
      clear()
      time("parNIO4", thread, size, iter)(parNIO4(thread, data))
    }
    for (thread <- ioThreads; iter <- 1 to count) {
      clear()
      time("parNIO5", thread, size, iter)(parNIO5(thread, data))
    }
    for (thread <- ioThreads; iter <- 1 to count) {
      clear()
      time("parNIO6", thread, size, iter)(parNIO6(thread, data))
    }

    for (iter <- 1 to count) {
      clear()
      time("zip", 1, size, iter)(zip(data))
    }
    for (iter <- 1 to count) {
      clear()
      time("zipStored", 1, size, iter)(zipStored(data))
    }

  }
}
