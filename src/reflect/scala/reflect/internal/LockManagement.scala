package scala.reflect.internal

import scala.reflect.internal.util.Parallel
import scala.reflect.io.AbstractFile


trait LockManagement {
  self : SymbolTable =>

  val lockManager: Parallel.LockManager = Parallel.noopLockManager

  val singleLock = lockManager.childLock(self.symbolTableLock, "Single lock", false)
  def readLockFor(file: AbstractFile): lockManager.LockType = singleLock
  def writeLockFor(file: AbstractFile): lockManager.LockType = singleLock
  def readLockFor(sym: Symbol): lockManager.LockType = singleLock
}

