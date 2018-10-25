package scala.io

import java.io._
import java.lang.SecurityManager
import java.security.Permission

import org.junit._
import org.junit.Assert._
import sun.security.util.SecurityConstants

object TopLevelTestObject extends Serializable

class ObjectSerialiationTest {

  def viaSerialisation[T <: Serializable](testObject: T): T = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(testObject)
    val bais = new ByteArrayInputStream(baos.toByteArray)
    val ois = new ObjectInputStream(bais)
    val res = ois.readObject()
    assertEquals("expected end of input", 0L, ois.available())
    res.asInstanceOf[T]
  }

  @Test def singleObject: Unit = {
    val expected = TopLevelTestObject
    val actual = viaSerialisation(expected)
    assertSame(expected, actual)
  }

  @Test def testSecurityManager: Unit = {
    //warmup
    singleObject

    object sm extends SecurityManager {
      val permissions = List.newBuilder[(Permission, Exception)]
      var recording = true

      override def checkPermission(perm: Permission): Unit = {
        if (recording) permissions += ((perm, new Exception(s"$perm requested ")))
      }

      override def checkPermission(perm: Permission, context: scala.Any): Unit = {
        if (recording) permissions += ((perm, new Exception(s"$perm requested ")))
      }

      checkPermission(new FilePermission("abc", "read"))
      permissions.clear()

    }
    object loader extends ClassLoader(getClass.getClassLoader) {

      val parent = getClass.getClassLoader

      override def loadClass(name: String, resolve: Boolean): Class[_] = {
        if (name contains "InSeperateLoader") {
          val in = parent.getResourceAsStream(name.replace(".", "/") + ".class")
          val baos = new ByteArrayOutputStream()
          var read = in.read()
          while (read != -1) {
            baos.write(read)
            read = in.read()
          }
          defineClass(name, baos.toByteArray, 0, baos.size())
        } else
          super.loadClass(name, resolve)
      }

      override def findClass(name: String): Class[_] = super.findClass(name)
    }


    val old = System.getSecurityManager
    val oldccl = Thread.currentThread().getContextClassLoader

    val testRun = Class.forName(classOf[InSeperateLoader].getCanonicalName, true, loader).newInstance().asInstanceOf[Runnable]
    assertTrue(testRun.getClass.getClassLoader eq loader)
    testRun.run()

    Thread.currentThread().setContextClassLoader(loader)
    System.setSecurityManager(sm)

    val checks = try {
      testRun.run()
      sm.permissions.result()
    } finally {
      sm.recording = false
      System.setSecurityManager(old)
      Thread.currentThread().setContextClassLoader(oldccl)
    }
    checks match {
      case (SecurityConstants.CHECK_MEMBER_ACCESS_PERMISSION, _) :: Nil => //expected
      case errors =>
        errors.foreach { case (_, t) => t.printStackTrace()}
        fail (s"expected a single ${SecurityConstants.CHECK_MEMBER_ACCESS_PERMISSION} but got ${errors map {_._1} mkString}")
    }

  }

}
class InSeperateLoader extends Runnable{
  def run(): Unit = {
    val expected = TopLevelTestObject
    val actual = new ObjectSerialiationTest().viaSerialisation(expected)
    assertSame(expected, actual)
    expected.getClass.getDeclaredField("MODULE$").get(null)
    expected.getClass.getDeclaredField("MODULE$").get(null)
  }
}
