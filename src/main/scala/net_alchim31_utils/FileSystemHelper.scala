package net_alchim31_utils

import java.io.Closeable
import java.io.OutputStreamWriter
import java.io.InputStreamReader
import java.io.{File, InputStream, OutputStream, FileInputStream, FileOutputStream, Reader, Writer, BufferedOutputStream, BufferedInputStream}
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import reflect.Manifest
//import resource._
import java.io.{File, InputStream, OutputStream, FileInputStream, FileOutputStream, Reader, Writer, BufferedOutputStream, BufferedInputStream}
import java.util.zip.GZIPOutputStream
import java.util.regex.Pattern
import java.net.URL
import java.nio.channels.FileChannel
import java.util.logging.Logger

object JFile {
  def apply(pathName : String) : File = new File(pathName).getCanonicalFile

  def apply(parent : File, subPaths : String*) : File = new File(parent,  subPaths.mkString(File.separator)).getCanonicalFile
}

class FileSystemHelper(logger : MiniLogger) {


  def changeSuffix(fileName : String, newSuffix : String) = fileName.substring(0, fileName.lastIndexOf('.') + 1) + newSuffix

//  def createTempDirectory() : File = {
//    val tmpFile = File.createTempFile( "tmpBuild" , ".dir" )
//    if (!tmpFile.delete()){
//      log.debug("delete failed on {}", tmpFile)
//    }
//    if(!tmpFile.mkdirs()){
//      log.warn("mkdirs failed on {}", tmpFile)
//    }
//    tmpFile
//  }

  def scan[T](root : File, includes : List[String] = Nil, excludes : List[String] = Nil)(f : (File, String, File) => Option[T]) : List[T] = {
    val excludesP = excludes.map(Regexps.globToRegexPattern)
    val includesP = includes.map(Regexps.globToRegexPattern)

    def accept(file : File, rpath : String) : Boolean = {
        val isExcluded = excludesP.foldLeft(false)((r, pattern) => r || pattern.matcher(rpath).matches())
        val isIncluded = includesP match {
            case Nil => true
            case l => l.foldLeft(false)((r, pattern) => r || pattern.matcher(rpath).matches())
        }
        val b = !isExcluded && isIncluded
        logger.trace("check %s = %s under %s", b, rpath, file)
        b
    }

    def scanFiles(rootDir : File, rpath : String) : List[T] = {
      val dir = new File(rootDir, rpath)
      var back : List[T] = Nil
      for (child <- dir.listFiles(); if !child.isHidden && child.canRead) {
        val rpathChild = rpath + "/" + child.getName
        if (child.isDirectory) {
          back = scanFiles(rootDir, rpathChild) ::: back
        } else {
          accept(child, rpathChild) match {
            case true => f(rootDir, rpathChild, child) match {
              case Some(file) => back = file :: back
              case None => back
            }
            case false => back
          }
        }
      }
      back
    }

    val b = scanFiles(root, "")
    logger.debug("nb files found under %s : %d with includes[%s] excludes[%s]", root, b.size, includes, excludes)
    b
  }

  def findFiles(root : File, includes : List[String] = Nil, excludes : List[String] = Nil) : List[File] = {
    scan(root, includes, excludes){(rootDir, rpath, file) => Some(file.getCanonicalFile)}
  }
  
  def getExtension(fileName : String) : String = {
    val idx = fileName.lastIndexOf('.')
    var back = fileName.substring(idx + 1)
    if ((back == "gz") || (back == "bz2")) {
      val idx2 = fileName.lastIndexOf('.', idx-1)
      if (idx2 > 0) {
        back = fileName.substring(idx2 + 1)
      }
    }
    back
  }

  def getBaseName(fileName : String) = {
    val ext = getExtension(fileName)
    fileName.substring(0, if (ext.length == 0) fileName.length else (fileName.length - ext.length -1))
  }

  def deleteRecursively(file : File) : Unit = {
    if (file.exists) {
      deleteContent(file)
      file.delete()
    }
  }

  def deleteContent(file : File) : Unit = {
    if (file.isDirectory()) {
      file.listFiles().foreach( f => deleteRecursively(f) )
    }
  }

  def jar(dest : File, dir : File, paths : Set[String]) {
	 import java.util.jar.{JarOutputStream, JarEntry}

    val tmpFileSuffix = ".part-" + java.lang.Long.toHexString(System.nanoTime())
    val tmpfile = new File(dest.getAbsolutePath + tmpFileSuffix)
    dest.getParentFile.mkdirs()
    using(new JarOutputStream(new FileOutputStream(tmpfile))){ os =>
      paths.foreach { path =>
        using(new FileInputStream(new File(dir, path))) {is =>
          val entry = new JarEntry(path)
          os.putNextEntry(entry)
          copy(is, os)
        }
      }
    }
    move(tmpfile, dest)
  }

  def jar0gz(dest : File, dir : File, paths : Set[String]) {
    import java.util.jar.{JarOutputStream, JarEntry}

    val tmpFileSuffix = ".part-" + java.lang.Long.toHexString(System.nanoTime())
    val tmpfile = new File(dest.getAbsolutePath + tmpFileSuffix)
    dest.getParentFile.mkdirs()
    using(new JarOutputStream(new GZIPOutputStream(new FileOutputStream(tmpfile)))){ os =>
      os.setLevel(0) //STORE only
      paths.foreach { path =>
        using(new FileInputStream(new File(dir, path))) {is =>
          val entry = new JarEntry(path)
          os.putNextEntry(entry)
          copy(is, os)
        }
      }
    }
    move(tmpfile, dest)
  }
  
  def move(src : File, dest : File) {
    if (src.equals(dest)) return
    if (!src.exists) {
      throw new IllegalArgumentException("file not found :" + src)
    }
    if (src.exists()) {
      if (dest.exists) {
        deleteRecursively(dest)
      }
      if (!src.renameTo(dest)) {
        copy(src, dest)
        deleteRecursively(src)
      }
    }
  }

  /**
   * if includes and excludes is Nil, and you don't care about the result, then use copy(srcDir, destDir) (less operations => better performance)
   */
  def copy(srcDir : File, destDir : File, includes : List[String] = Nil, excludes : List[String] = Nil) : List[String] = {
    scan(srcDir, includes, excludes){(srcDir, rpath, file) =>
      val dest = new File(destDir, rpath)
      val p = dest.getParentFile
      // TODO optimize to avoid check parent directory for every files
      if (!p.isDirectory && !p.mkdirs()) {
        throw new IllegalStateException("directory not available/makable :" + p)
      }
      copy(file, dest)
      Some(rpath)
    }
  }
  
  def copy(input : File, output : File) : Long = {
    if (input.equals(output)) return output.length
    if (!input.exists) {
      throw new IllegalArgumentException("file not found :" + input)
    }
    if (input.isDirectory) {
      output.mkdirs
      input.listFiles().foldLeft(0l){(back, file) => back + copy(file, new File(output, file.getName))}
    } else {
      // see http://www.javalobby.org/java/forums/t17036.html
      // for larger files (20Mb) use streams
      val fileSize = input.length
      if ( fileSize > 20971520l ) {
        using(new FileInputStream(input)) { is =>
          using(new FileOutputStream(output)) {os =>
           copy(is, os)
          }
        }
      } else {
        using(new FileInputStream(input).getChannel) { is =>
          using(new FileOutputStream(output).getChannel) {os =>
           copy(is, os, fileSize)
          }
        }
      }
      fileSize
    }
  }

  def copy(input : FileChannel, output : FileChannel, length : Long ) : Long = {
    var offset = 0L
    val copyCnt = math.min(65536, length)
    while (offset < length) {
      offset += input.transferTo(offset, copyCnt, output)
    }
    offset
  }

  /**
   * Copy bytes from a large (over 2GB) <code>InputStream</code> to an
   * <code>OutputStream</code>.
   * <p>
   * This method buffers the input internally, so there is no need to use a
   * <code>BufferedInputStream</code>.
   *
   * @param input  the <code>InputStream</code> to read from
   * @param output  the <code>OutputStream</code> to write to
   * @return the number of bytes copied
   * @throws NullPointerException if the input or output is null
   * @throws IOException if an I/O error occurs
   * @basedon commons-io (IOUtils)
   */
  def copy(input : InputStream, output : OutputStream) : Long = {
    val buffer = new Array[Byte](1024 * 4)
    var count = 0L
    var n = input.read(buffer)
    while (n != -1) {
      output.write(buffer, 0, n)
      count += n
      n = input.read(buffer)
    }
    count
  }

  def copy(input : Reader, output : Writer) : Long = {
    val buffer = new Array[Char](1024 * 4)
    var count = 0L
    var n = input.read(buffer)
    while (n != -1) {
      output.write(buffer, 0, n)
      count += n
      n = input.read(buffer)
    }
    count
  }

  def copy(input : InputStream, length : Int , output : OutputStream) : Long = {
    val bufferSize = 1024 * 4
    val buffer = new Array[Byte]( bufferSize )
    var count = 0
    var n = input.read(buffer , 0 , math.min( bufferSize , length ) )
    while (n != -1) {
      output.write(buffer , 0 , n )
      count += n
      n = if( count == length ) {
        -1
      }
      else {
        input.read( buffer , 0 , math.min( length - count , bufferSize ) )
      }
    }
    count
  }

  def toString(file : File) : String = toString(file, "UTF-8")

  def toString(file : File, encoding : String) : String = toString(new FileInputStream(file), encoding)

  def toString(stream : InputStream, encoding : String) : String = {
    import java.io.InputStreamReader
    using(new InputStreamReader(stream, encoding)) { in =>
      toString(in)
    }
  }

  def toString(in : Reader) : String = {
    val out = new java.io.StringWriter()
    copy(in, out)
    out.toString()
  }

  def using[A, R <: Closeable](r : R)(f : R => A) : A = {
    try {
      f(r)
    } finally {
	    try {
	      r.close()
	    } catch {
	      case e =>
	    }
    }
  }

}
