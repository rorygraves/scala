import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.util.jar.{JarEntry, JarFile, JarOutputStream}
import java.util.zip.{CRC32, ZipEntry}

import org.kohsuke.args4j.{CmdLineException, CmdLineParser}
import org.kohsuke.args4j.spi.StringArrayOptionHandler
import org.objectweb.asm._
import org.objectweb.asm.signature._

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.util.matching.Regex

trait JarShrinkCommandLine{ self: App =>
  import org.kohsuke.args4j.Option
  import scala.collection.breakOut

  //basic options
  @Option(name="-in", aliases=Array("--input"), required=true,
    usage="input jar file to process"  )
  private var inputFileV:File = _
  def inputFile = inputFileV

  @Option(name="-out", aliases=Array("--output"),
    usage="output jar file to process - default is to use the same name as the input filename, but in the local directory"  )
  private var outputFileV:File = _
  lazy val outputFile = scala.Option(outputFileV).getOrElse(new File(inputFile.getName))

  @Option(name="-over",aliases=Array("--overwriteOutput"),
    usage="overwrite the output file if it exists"  )
  private var overwriteV:Boolean = _
  def overwrite = overwriteV

  //what to strip
  @Option(name="-sd", aliases=Array("--stripDeprecated"),
    usage=" strip deprecated classes, methods and fields"  )
  private var stripDeprecatedV:Boolean = _
  def stripDeprecated = stripDeprecatedV

  @Option(name="-si", aliases=Array("--stripInner"),
    usage=" strip inner classes that are not referenced in the outer class"  )
  private var stripInnerV:Boolean = _
  def stripInner = stripInnerV

  @Option(name="-ss", aliases=Array("--stripScala"),
    usage="reduce to the minimum, classes compiled withthe scalac compiler, making use of the scala special attributes"  )
  private var stripScalaV:Boolean = _
  def stripScala = stripScalaV

  @Option(name="-sp", aliases=Array("--stripPackageRegex"), handler = classOf[StringArrayOptionHandler],
    usage=""" strip class/field/method which are java package protected, where th class matches these regex. This can take a list of regex values, e.g. -sp "sun\\..*  """)
  private var stripPackageRegexV:Array[String] = Array()
  lazy val stripPackageRegex:List[Regex] = stripPackageRegexV.map (_.r)(breakOut)

  @Option(name="-sr", aliases=Array("--stripRegex"), handler = classOf[StringArrayOptionHandler],
    usage=" strip classes which match regex. This can take a list of regex values"  )
  private var stripRegexV:Array[String] = Array()
  lazy val stripRegex:List[Regex] = stripRegexV.map (_.r)(breakOut)

  //admin
  @Option(name="-h", aliases=Array("-?", "--help", "-help"),
    usage="print help"  )
  private var helpV:Boolean = _
  def help = helpV

  @Option(name="-v", aliases=Array("--verbose"),
    usage="be verbose"  )
  private var verboseV:Boolean = _
  def verbose = verboseV


  def init(args:Array[String]): Unit = {
    val parser = new CmdLineParser(this)
    parser.getProperties.withShowDefaults(true).withUsageWidth(120)

    def displayHelp(exitCode: Int = 0) = {
      parser.printUsage(System.out)
      System.exit(exitCode)
    }

    try parser.parseArgument(self.args: _*) catch {
      case e: CmdLineException =>
        if (help) {
          displayHelp()
        } else {
          println(e.getMessage)
          displayHelp(1)
        }
    }
  }
}

/**
  * Created by Mike Skells on 03/10/2016.
  */
object JarShrink extends App with JarShrinkCommandLine{

  init(args)

  shrink
  /** shrinks a file so that it contains only the inplementation that scalac needs
    */
  def shrink {

    require(inputFile exists)
    require(overwrite || !outputFile.exists)
    val in = new JarFile(inputFile)
    val mf = in.getManifest()
    val out = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(outputFile), 65536), mf)
    out.setMethod(ZipEntry.STORED)

    val allScanners = new mutable.HashMap[String, LocalClassScanner]

    val crc = new CRC32
    //we sort the entry by name as this ensures that outer classes are visited before inner classes
    for (entry <- in.entries().toVector.sortBy(_.getName)) {
      val name = entry.getName
      if (!name.endsWith(".class"))
        trace(s"eliminated file $name")
      else {
        val data = readAll(in, entry)

        val reader = new LocalClassScanner(name)
        val cr = new ClassReader(data)
        cr.accept(reader, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
        allScanners(reader.internalClassName) = reader
      }
    }
    for (inner <- allScanners.values if inner.isInnerClass; outer = allScanners(inner.outerClass))
      outer.trueInnerClasses += inner.internalClassName

    def markInnerClasses(outerIsNeeded:Boolean,scanner:LocalClassScanner): Unit = {
      for ((inner, needed) <- scanner.innerClasses if scanner.trueInnerClasses(inner); innerScanner = allScanners(inner)) {
        if ((!needed  || !outerIsNeeded) && innerScanner.shouldWrite) {
          if (inner == "java/util/regex/Pattern$Node")
            trace("XXXX")
          trace(s"marked $inner as not needed")
          innerScanner.shouldWrite = false
        }
        markInnerClasses(outerIsNeeded && needed, innerScanner)
      }
    }
    if (stripInner) allScanners.values.filter(!_.isInnerClass).foreach {markInnerClasses(true, _)}


    val pending = new mutable.Queue[String]
    for (scanner <- allScanners.values) {
      if (scanner.shouldWrite) pending += scanner.internalClassName
    }
    val allClasses = new mutable.HashSet[String]()
    for (reached <- pending) {
      allClasses += reached
      val scanner = allScanners(reached)
      val toAdd = scanner.referencedClasses.keySet.filter(allScanners.keySet).filterNot(allClasses)
      if (verbose) for (added <- toAdd; addedScanner = allScanners(added) if !addedScanner.shouldWrite) {
        trace(s"class ${addedScanner.internalClassName} is needed - referenced by $reached as ${scanner.referencedClasses(added)}")
      }

      pending ++= toAdd
    }
    val allClassesToExclude = (allScanners.keySet -- allClasses.toSet).toSet
    for (classToWrite <- allClasses) {
      val scanner = allScanners(classToWrite)
      val reader: BaseClassCopier =
        if (stripScala && (scanner.scalaSignature.isDefined || scanner.scalaTopLevel.isDefined))
          new LocalClassCopier(allClassesToExclude, scanner.internalClassName, scanner.javaClassName, scanner.fieldsToKeep.toSet, scanner.methodsToKeep.toSet)
        else new ScalaClassCopier(scanner.internalClassName, scanner.javaClassName)
      val cr = new ClassReader(readAll(in, in.getEntry(scanner.entryName)))
      cr.accept(reader, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
      val result = reader.data
      val zip = new ZipEntry(scanner.entryName)
      zip.setSize(result.length)
      crc.reset()
      crc.update(result)
      zip.setCrc(crc.getValue)
      out.putNextEntry(zip)
      out.write(result)
      out.closeEntry()
    }
    out.flush()
    out.close()
  }

  private def readAll(in: JarFile, entry: ZipEntry): Array[Byte] = {
    val size = entry.getSize.asInstanceOf[Int]
    assert(size == entry.getSize, s"size out of range ${entry.getSize}")
    val data = new Array[Byte](size)
    val str = in.getInputStream(entry)
    @tailrec def readMore(start: Int): Unit = {
      if (start != size) {
        val read = str.read(data, start, size - start)
        readMore(start + read)
      }
    }
    readMore(0)
    data
  }

  //scns the class to determine what classes are reachable from this class
  import Opcodes._
  private abstract class BaseClassScanner extends ClassVisitor(ASM5) {
    def internalClassName:String
    def javaClassName:String
    def isPackage(access: Int): Boolean = (access & (ACC_PRIVATE | ACC_PROTECTED | ACC_PUBLIC)) == 0
  }
  private class LocalClassScanner(val entryName:String) extends BaseClassScanner {
    var shouldWrite = true
    var internalClassName:String = "<<>>"
    var javaClassName:String = "<<>>"
    var stripPackage = false
    def isInnerClass = outerClass ne null
    var outerClass: String = _
    var scalaSignature = Option.empty[(String,Any)]
    var scalaTopLevel = Option.empty[(String,Any)]
    def dontWrite(reason:String) ={
      if (shouldWrite) trace(s"class $internalClassName is not written - $reason")
      shouldWrite = false
    }
    def dontWritePart(part:String, name:String, reason:String) ={
      if (shouldWrite) trace(s"class $internalClassName $part $name, is not written - $reason")
      null
    }
    class CommonAnnotationVisitor(val source:String) extends AnnotationVisitor(ASM5) {
      def seen (value:Any): Unit = {
        value match {
          case array: Array[t] => array foreach seen
          case t:Type => keepTypes(source, t)
          case _ => //ignore primatives
        }
      }
      override def visit(name: String, value: Any) = seen(value)
      override def visitEnum(name: String, desc: String, value: String) {}
      override def visitAnnotation(name: String, desc: String): AnnotationVisitor = this
      override def visitArray(name: String): AnnotationVisitor = this
      override def visitEnd() =()
    }
    class TopAnnotationVisitor(source:String) extends CommonAnnotationVisitor(source) {
      override def visit(name: String, value: Any) = name match {
        case "ScalaSignature" | "ScalaLongSignature" =>
          scalaSignature = Some(name,value)
        case "ScalaSig" =>
          scalaTopLevel = Some(name,value)
        case _ => super.visit(name,value)
      }
    }
    class ClassFieldVisiter(val name:String) extends FieldVisitor(ASM5) {
      override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor =
        new CommonAnnotationVisitor(s"field $name")
      override def visitTypeAnnotation(typeRef: Int, typePath: TypePath, desc: String, visible: Boolean): AnnotationVisitor =
        new CommonAnnotationVisitor(s"field $name")
      override def visitAttribute(attr: Attribute) = ()
      override def visitEnd() =()
    }
    class ClassMethodVisiter(val name:String, val desc:String) extends MethodVisitor(ASM5) {
      def av =new CommonAnnotationVisitor(s"field $name")
      override def visitAnnotationDefault: AnnotationVisitor = av
      override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = av
      override def visitTypeAnnotation(typeRef: Int, typePath: TypePath, desc: String, visible: Boolean): AnnotationVisitor = av
      override def visitParameterAnnotation(parameter: Int, desc: String, visible: Boolean): AnnotationVisitor = av
      override def visitAttribute(attr: Attribute) = ()
    }

    val fieldsToKeep =  mutable.Set.empty[String]
    val methodsToKeep =  mutable.Set.empty[(String,String)]
    val innerClasses =  mutable.Map.empty[String,Boolean]
    val trueInnerClasses=  mutable.Set.empty[String]
    val referencedClasses = mutable.Map.empty[String,String]
    def probablyWant(innerClass:String): Unit = {
      innerClasses += (innerClass -> true)
    }
    def probablyDontWant(innerClass:String): Unit = {
      innerClasses += (innerClass -> false)
    }

    def keepTypes(source:String, tpe:Type) :Unit = {
      // http://asm.ow2.org/doc/tutorial-asm-2.0.html
      import Type._
      tpe.getSort match {
        case VOID =>
        case BOOLEAN =>
        case CHAR =>
        case BYTE =>
        case SHORT =>
        case INT =>
        case FLOAT =>
        case LONG =>
        case DOUBLE =>
        case ARRAY => keepTypes(source, tpe.getElementType)
        case OBJECT => referencedClasses(tpe.getInternalName) = source
      }
    }
    private def keepGenericTypes(source:String, sign:String) {
      if(sign!=null) {
        new SignatureReader(sign).accept(new SignatureVisitor(Opcodes.ASM5) {

          override def visitFormalTypeParameter(s: String): Unit = ()
          override def visitClassType(name: String): Unit = referencedClasses(name) = source
          override def visitExceptionType(): SignatureVisitor = this
          override def visitInnerClassType(name: String): Unit = referencedClasses(name) = source
          override def visitBaseType(descriptor: Char): Unit = ()
          override def visitArrayType(): SignatureVisitor = this
          override def visitInterface(): SignatureVisitor = this
          override def visitParameterType(): SignatureVisitor = this
          override def visitInterfaceBound(): SignatureVisitor = this
          override def visitEnd(): Unit = ()
          override def visitReturnType(): SignatureVisitor = this
          override def visitClassBound(): SignatureVisitor = this
          override def visitSuperclass(): SignatureVisitor = this
          override def visitTypeVariable(name: String): Unit = ()
          override def visitTypeArgument(): Unit = ()
          override def visitTypeArgument(c: Char): SignatureVisitor = this
        })
      }
    }
    // we can ignore the class if it is private, and maybe deprecated
    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
      if (superName ne null) referencedClasses(superName) = s"$name superclass"
      if (interfaces ne null) interfaces foreach {referencedClasses(_) = s"$name superclass"}
      internalClassName = name
      javaClassName = Type.getObjectType(name).getClassName
      if (stripDeprecated && (access & ACC_DEPRECATED) != 0)
        dontWrite(" deprecated")
      if ((access & ACC_PRIVATE) != 0)
        dontWrite(" private")
      stripRegex.find { r =>
        r.pattern.matcher(javaClassName).matches()
      } foreach { r=>
        dontWrite(s" matches regex '${r.regex}'")
      }
      stripPackageRegex.find { r =>
        r.pattern.matcher(javaClassName).matches()
      } foreach { r=>
        if (isPackage(access)) dontWrite(s" package protected matches regex '${r.regex}'")
        else {
          trace(s"strip package protected content for $javaClassName as it  matches regex '${r.regex}'")
          stripPackage = true
        }
      }
    }
    override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = new TopAnnotationVisitor("class annotations")

    override def visitField(access: Int, name: String, desc: String, signature: String, value: scala.Any): FieldVisitor = {
      if (stripDeprecated && (access & ACC_DEPRECATED) != 0)
        dontWritePart("field", name, "deprecated")
      else if ((access & ACC_PRIVATE) != 0)
        dontWritePart("field", name, "private")
      else if (stripPackage && isPackage(access))
        dontWritePart("field", name, "package")
      else {
        fieldsToKeep += name
        keepTypes(s"field $name", Type.getObjectType(desc))
        keepGenericTypes(s"field $name", signature)
      }
      new ClassFieldVisiter(name)
    }
    override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      //we cant filter deprecated methods as hat may cause the wrong binding
      //e.g.
      // def foo(n:Number)
      // @deprecated ("not safe with Int")
      // def foo(i:Int)
      //if we remove the deprecated method then it woul bind to th wring methods

      if ((access & ACC_PRIVATE) != 0)
        dontWritePart("method", name, "private")
      else if (stripPackage && isPackage(access))
        dontWritePart("method", name, "package")
      else {
        methodsToKeep += ((name,desc))
        keepTypes(s"method $name $desc", Type.getReturnType(desc))
      }
      new ClassMethodVisiter(name,desc)
    }
    override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int): Unit = {
      if (name != internalClassName) {
        if ((access & ACC_PRIVATE) != 0) {
          dontWritePart("inner class", name, "private")
          probablyDontWant(name)
        } else if (innerName == null) {
          dontWritePart("inner class", name, "anonymous")
          probablyDontWant(name)
        } else {
          probablyWant(name)
        }
      }
    }
    override def visitOuterClass(owner: String, name: String, desc: String): Unit = {
      outerClass = owner
    }
  }
  private abstract class BaseClassCopier extends BaseClassScanner  {
    protected val writer: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    def data = writer.toByteArray

    // we can ignore the class if it is private, and maybe deprecated
    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
      require (internalClassName == name)
      writer.visit(version,access,name,signature,superName, interfaces)
    }
    override def visitSource(source: String, debug: String): Unit = {
      //dont care about source so strip it
    }
    override def visitEnd(): Unit = writer.visitEnd()

  }
  private class ScalaClassCopier(val internalClassName:String, val javaClassName:String) extends BaseClassCopier {
    override def visitAttribute(attribute: Attribute): Unit = writer.visitAttribute(attribute)
  }
  private class LocalClassCopier(val classesToExclude:Set[String], val internalClassName:String, val javaClassName:String, val fieldsToKeep:Set[String],  val methodsToKeep:Set[(String, String)]) extends BaseClassCopier{

    override def visitOuterClass(owner: String, name: String, desc: String): Unit = writer.visitOuterClass(owner,name,desc)

    override def visitAttribute(attribute: Attribute): Unit = writer.visitAttribute(attribute)
    override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = writer.visitAnnotation(desc,visible)

    override def visitField(access: Int, name: String, desc: String, signature: String, value: scala.Any): FieldVisitor = {
      if (fieldsToKeep.contains(name))
        writer.visitField(access,name,desc,signature, value)
      else null
    }
    override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      if (methodsToKeep((name,desc)))
        writer.visitMethod(access,name,desc,signature, exceptions)
      else null
    }
    override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int): Unit = {
      if (!classesToExclude(name))
        writer.visitInnerClass(name,outerName,innerName,access)
    }
  }

  def trace(s: String): Unit = {
    if (verbose) println(s)
  }
}

