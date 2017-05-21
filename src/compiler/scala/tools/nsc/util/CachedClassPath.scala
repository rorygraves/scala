package scala.tools.nsc.util

import java.net.URL
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scala.concurrent.{ExecutionContext, Future}
import scala.tools.nsc.classpath._
import scala.tools.nsc.io.AbstractFile
import scala.collection.{mutable, Set => SSet}


/**
  * factory for classpath where some result are cached
  */
object CachedClassPath {
  def apply(classPath:ClassPath): ClassPath = {
    classPath match {
      case cachedAlready: CachedClassPath => cachedAlready
      case noSources: ClassPath with NoSourcePaths => CachedNoSourcesClassPath(noSources)
      case noClasses: ClassPath with NoClassPaths => CachedNoClassesClassPath(noClasses)
      case other: ClassPath => CachedAnyClassPath(other)
    }
  }
  private case class CachedNoSourcesClassPath(underlying:ClassPath with NoSourcePaths) extends
    ClassesCachedClassPath with NoSourcePaths

  private case class CachedNoClassesClassPath(underlying:ClassPath with NoClassPaths) extends
    SourcesCachedClassPath with NoClassPaths

  private case class CachedAnyClassPath(underlying:ClassPath) extends
    ClassesCachedClassPath with SourcesCachedClassPath

  sealed trait CachedClassPath extends ClassPath {
    protected val underlying: ClassPath

    override def hashCode(): Int = underlying.hashCode() * 31

    override def equals(obj: scala.Any): Boolean = obj match {
      case cached : CachedClassPath => (cached eq this) || (cached.getClass == getClass && cached.underlying == underlying)
      case _ => false
    }

    override final def findClass(className: String) = super.findClass(className)

    override final lazy val asClassPathString = underlying.asClassPathString
    override final lazy val asClassPathStrings: Seq[String] = underlying.asClassPathStrings
    override final lazy val asURLs: Seq[URL] = underlying.asURLs

    private val packagesCache = new CachedLazyIndexedMapping(underlying.packages)
    override private[nsc] final def packages(inPackage: String) = packagesCache(inPackage)

    private val listCache = new CachedLazyMapping(underlying.list, ClassPathEntries.empty)
    override private[nsc] final def list(inPackage: String) = listCache(inPackage)

    private val prefetchStarted = new AtomicBoolean
    def startPrefetch(implicit exec:ExecutionContext) : Option[Future[Unit]] = if (prefetchStarted.compareAndSet(false, true)) {
      Some(Future{
        val all = mutable.Set[String]()
        def load(names:Iterable[PackageEntry]) : Unit = {
          names foreach { p =>
            all += p.name
            load(packages(p.name))
          }
        }
        load(packages(""))
        prefetchFromPackages(all, exec)
      })
    } else None
    protected def prefetchFromPackages(packages: Iterable[String], exec:ExecutionContext) = {
      packagesCache.makeComplete(packages.iterator)
      listCache.makeComplete(packages.iterator)
    }

  }
  private sealed trait ClassesCachedClassPath extends CachedClassPath {

    private val clasesCache = new CachedLazyIndexedMapping(underlying.classes)
    override private[nsc] final def classes(inPackage: String) = clasesCache(inPackage)

    override def findClassFile(className: String): Option[AbstractFile] = underlying.findClassFile(className)

    override protected def prefetchFromPackages(packages: Iterable[String], exec: ExecutionContext): Unit = {
      super.prefetchFromPackages(packages, exec)
      //not sure we wait to load all of the classes - need to profile
      clasesCache.makeComplete(packages.iterator)
    }
  }
  private sealed trait SourcesCachedClassPath  extends CachedClassPath{

    private val sourcesCache = new CachedLazyIndexedMapping(underlying.sources)
    override private[nsc] def sources(inPackage: String) = sourcesCache(inPackage)

    override lazy val asSourcePathString: String = underlying.asSourcePathString
    override protected def prefetchFromPackages(packages: Iterable[String], exec: ExecutionContext): Unit = {
      super.prefetchFromPackages(packages, exec)
      //not sure we wait to load all of the sources - need to profile
      sourcesCache.makeComplete(packages.iterator)
    }

  }

}

/**
  * a Cached implementation of a mapping. This has to be threadsafe as a classpath may be cached and reused in a JVM
  * @tparam V
  */
abstract class BaseCachedLazyMapping[V <: AnyRef] {

  @volatile protected var complete = false

  protected val hits = new AtomicInteger
  protected val misses = new AtomicInteger
  protected val defaulted = new AtomicInteger

  def apply(key:String): V
  def makeComplete(keyRange: Iterator[String]): Unit = {
    keyRange foreach apply
    //all the keys are mapped
    complete = true
    optimise()
  }
  protected def optimise()
}
class CachedLazyMapping[V <: AnyRef] (miss : (String => V), defaultValue: V ) extends BaseCachedLazyMapping[V] {
  protected val cache =  new ConcurrentHashMap[String,V]

  def apply(key:String): V = {
    val cached = cache.get(key)
    if (cached ne null) {
      hits.incrementAndGet()
      cached
    } else if (complete) {
      defaulted.incrementAndGet()
      defaultValue
    } else {
      misses.incrementAndGet()
      val missed = miss(key)
      val existing = cache.putIfAbsent(key, missed)
      if (existing ne null) existing else missed
    }
  }
  override def optimise : Unit = {
    //remove the default values
    //cant use cache.values.remove(All), they only remove a single instance
    val it = cache.entrySet().iterator()
    while (it.hasNext()) {
      val curr = it.next()
      if (curr.getValue == defaultValue) it.remove()
    }
  }
}
class CachedLazyIndexedMapping[V <: Named] (miss : (String => Iterable[V]) )
  extends BaseCachedLazyMapping[Seq[V]] {
  protected val cache =  new ConcurrentHashMap[String,Map[String,V]]

  def getRaw(key:String): Map[String,V] = {
    val cached = cache.get(key)
    if (cached ne null) {
      hits.incrementAndGet()
      cached
    } else if (complete) {
      defaulted.incrementAndGet()
      Map.empty
    } else {
      misses.incrementAndGet()
      val missed = miss(key)
      val rawMap:Map[String,V] = if (missed.isEmpty) Map.empty
      else {
        val builder = Map.newBuilder[String,V]
        missed foreach { v:V =>
          builder += ((v.name, v))
        }
        builder.result()
      }

      val existing = cache.putIfAbsent(key, rawMap)
      if (existing ne null) existing else rawMap
    }
  }

  def apply(key:String) : Seq[V] = {
    getRaw(key).values.toSeq
  }

  def named(key:String, name:String) : Option[V] = {
    getRaw(key).get(name)
  }

  override def optimise : Unit = {
    //remove the default values
    //cant use cache.values.remove(All), they only remove a single instance
    val it = cache.entrySet().iterator()
    while (it.hasNext()) {
      val curr = it.next()
      if (curr.getValue.isEmpty) it.remove()
    }
  }


}
