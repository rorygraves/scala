package scala.reflect.internal

import scala.reflect.internal.util.Parallel
import scala.reflect.internal.util.Parallel.{AbstractLock, Lock, NoLockManager}
import scala.reflect.io.AbstractFile

class LockManagementBase {

  type LockType <: AbstractLock
  type LockManagerType <: Parallel.LockManager[_ <: AbstractLock]
}

trait LockManagement extends LockManagementBase{
  self : SymbolTable =>

  def lockManager: LockManagerType = NoLockManager.asInstanceOf[LockManagerType]
  private lazy val singleLock:LockType = lockManager.rootLock("Single lock", false).asInstanceOf[LockType]
  val symbolTableLock:LockType  = singleLock
  def readLockFor(file: AbstractFile): LockType = singleLock
  def writeLockFor(file: AbstractFile): LockType = singleLock
  def readLockFor(sym: Symbol): LockType = singleLock
}

