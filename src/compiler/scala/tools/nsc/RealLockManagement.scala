package scala.tools.nsc

import java.util.concurrent.ConcurrentHashMap

import scala.reflect.internal.util.{NoFile, NoSourceFile, Parallel}
import scala.reflect.internal.{LockManagement, SymbolTable}
import scala.reflect.io.AbstractFile
import scala.collection.JavaConverters._

trait RealLockManagement extends LockManagement {
  self: Global =>

  override val lockManager = new Parallel.RealLockManager()

  private val fileReadLock = new ConcurrentHashMap[AbstractFile, lockManager.LockType]()
  private val fileWriteLock = new ConcurrentHashMap[AbstractFile, lockManager.LockType]()
  private val fileReadLockScala = fileReadLock asScala

  override def readLockFor(file: AbstractFile): lockManager.LockType = {
    fileReadLock.computeIfAbsent(file, f => lockManager.childLock(self.symbolTableLock, s"read for $file", false))
  }

  override def writeLockFor(file: AbstractFile): lockManager.LockType = {
    fileWriteLock.computeIfAbsent(file, f => lockManager.childLock(self.symbolTableLock, s"write for $file", false))
  }

  override def readLockFor(sym: Symbol): lockManager.LockType = {
    val top = sym.originalEnclosingTopLevelClassOrDummy
    val enclosingCu = if (top == NoSymbol) None else currentRun.symSource.get(top)
    val parent = enclosingCu map fileReadLockScala getOrElse symbolTableLock
    lockManager.childLock(parent, sym.toString, false)
    //maybe
//    val source = sym.pos.source
//    ...
  }


}
