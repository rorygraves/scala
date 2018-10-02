package scala.tools.nsc

import java.util.concurrent.ConcurrentHashMap

import scala.reflect.internal.util.{NoFile, NoSourceFile, Parallel}
import scala.reflect.internal.{LockManagement, SymbolTable}
import scala.reflect.io.{AbstractFile, NoAbstractFile}
import scala.collection.JavaConverters._
import scala.reflect.internal.util.Parallel.{AbstractLock, RealLock}

trait RealLockManagement extends LockManagement {
  self: Global =>

  override val symbolTableLock:LockType  = lockManager.rootLock("symbolTableLock", false)
  override type LockType = RealLock
  override type LockManagerType = Parallel.RealLockManager
  override lazy val lockManager: Parallel.RealLockManager = new Parallel.RealLockManager()

  private val fileReadLock = new ConcurrentHashMap[AbstractFile, RealLock]()
  private val fileWriteLock = new ConcurrentHashMap[AbstractFile, RealLock]()

  override def readLockFor(file: AbstractFile): RealLock = {
    fileReadLock.computeIfAbsent(file, f => lockManager.childLock(self.symbolTableLock, s"read for $file", false))
  }

  override def writeLockFor(file: AbstractFile): RealLock = {
    fileWriteLock.computeIfAbsent(file, f => lockManager.childLock(self.symbolTableLock, s"write for $file", false))
  }

  override def readLockFor(sym: Symbol): RealLock = {
    sym.originalEnclosingTopLevelClassOrDummy match {
      case noSym: NoSymbol=>
        //cant use == NoSymbol because of stack overflow
        //same with toString
        lockManager.childLock(readLockFor(NoAbstractFile), "NoSymbol", false)
      case normal =>
        val enclosingCu = currentRun.symSource.get(normal)
        val parent = readLockFor(enclosingCu.getOrElse(NoAbstractFile))
        lockManager.childLock(parent, sym.toString, false)
    }

  }


}
