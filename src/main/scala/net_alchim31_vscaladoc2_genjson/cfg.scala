/**
 * Copyright (C) 2010 Alchim31 http://alchim31.net/
 *
 * http://github.com/davidB/vscaladoc2_genjson
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net_alchim31_vscaladoc2_genjson

import net_alchim31_utils.MiniLogger
import org.jsoup.safety.Whitelist
import org.jsoup.Jsoup
import net_alchim31_utils.FileSystemHelper
import scala.collection.SortedSet
import scala.collection.generic.SortedSetFactory
import java.io.FileInputStream
import java.util.jar.JarInputStream
import org.codehaus.jackson.JsonParser
import scala.collection.mutable.ListBuffer
import org.codehaus.jackson.JsonToken
import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.doc.DocFactory
import java.io.File
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.FatalError
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }
import scala.tools.nsc.util.FakePos //{Position}
import scala.tools.nsc.doc
import scala.tools.nsc.doc.model
import scala.reflect.NameTransformer



class Cfg {
  var groupId = ""
  var artifactId = "undef"
  var version = "0.0.0"
  var description = ""
  var sources : List[Source] = Nil
  var dependencies : List[Dependency] = Nil
  var logo : Option[String] = None
  var license : Option[String] = None
  var additionnalArgs : List[String] = Nil
  var apidocdir : File = new File(System.getProperty("user.home"), ".config/vscaladoc2/apis")

  val timestamp = System.currentTimeMillis
  def apidocdirTmp = new File(apidocdir, "tmp-" + timestamp)

  def sourcefiles : Seq[File] = sources.flatMap(_.files)
  def find(rpath : String) : Option[File] = sources.flatMap(_.find(rpath)).headOption
  def findDependency(packageName : String) : Option[Dependency] = None
  def scaladoc2Args : List[String] = {
    val l = new ListBuffer[String]()
    l ++= additionnalArgs

    if (!dependencies.isEmpty) {
      l += "-classpath"
      l += dependencies.map(_.file.getCanonicalPath).mkString(File.pathSeparator)
    }

    val d = new File(apidocdirTmp, artifactId + "/" + version)
    d.mkdirs()
    l += "-d"
    l += d.getCanonicalPath

    if (!sources.isEmpty) {
      l += "-sourcepath"
      l += sources(0).dir.getCanonicalPath
      for (source <- sources; file <- source.files) {
        l += file.getCanonicalPath
      }
    }

    l.toList
  }
}

class CfgHelper(logger : MiniLogger, val fs : FileSystemHelper) {
  def apply(json : String) : Cfg = apply(new org.codehaus.jackson.JsonFactory().createJsonParser(json))
  def apply(json : File) : Cfg = apply(new org.codehaus.jackson.JsonFactory().createJsonParser(json))

  def apply(jp : JsonParser) : Cfg = {
    val b = new Cfg()
    try {
      if (jp.nextToken() != JsonToken.START_OBJECT) {
        throw new Exception("Expected data to start with an Object not by : " + jp.getCurrentToken + " ~~ " + jp.getText)
      }
      while (jp.nextToken() != JsonToken.END_OBJECT) {
        val fieldName = jp.getCurrentName()
        // Let's move to value
        jp.nextToken()
        fieldName match {
          case "loglevel" => logger.setLevel(jp.getText)
          case "groupId" => b.groupId = jp.getText
          case "artifactId" => b.artifactId = jp.getText
          case "version" => b.version = jp.getText
          case "description" => b.description = jp.getText
          case "logo" => b.logo = Some(Jsoup.clean(jp.getText, Whitelist.basicWithImages))
          case "license" => b.license = Some(Jsoup.clean(jp.getText, Whitelist.basicWithImages))
          case "dependencies" if jp.getCurrentToken == JsonToken.START_ARRAY => b.dependencies = parseDependencies(jp)
          case "dependencies" => logger.warn("dependencies should be an array of array")
          case "sources" if jp.getCurrentToken == JsonToken.START_ARRAY => b.sources = parseSources(jp)
          case "sources" => logger.warn("sources should be an array of array")
          case "apidocdir" => b.apidocdir = new File(jp.getText).getCanonicalFile
          case x => logger.warn("unsupported field :" + x)
        }
      }
      completeDescriptionWithOverview(b)
    } finally {
      jp.close()
    }
    b
  }

  private def completeDescriptionWithOverview(cfg : Cfg) {
    val buf = new StringBuilder(cfg.description)
    for (src <- cfg.sources) {
      val ohtml = new File(src.dir, "overview.html")
      if (ohtml.exists) {
        if (buf.length != 0) {
          buf.append("\n<hr/>\n")
        }
        buf.append(fs.toString(ohtml))
      }
    }
    cfg.description = Jsoup.clean(buf.toString, Whitelist.relaxed())
  }

  private def parseDependencies(jp : JsonParser) : List[Dependency] = {
    val l = new ListBuffer[Dependency]()
    while (jp.nextToken() != JsonToken.END_ARRAY) {
      if (jp.getCurrentToken == JsonToken.START_ARRAY) {
        jp.nextToken
        val depPath = new File(jp.getText)
        jp.nextToken
        val dep = (jp.getCurrentToken == JsonToken.END_ARRAY) match {
          case true => Dependency(depPath, depPath.getName, "0.0.1") // TODO extract artifactId + version from filename
          case false => Dependency(depPath, jp.getText, { jp.nextToken; jp.getText })
        }
        // ignore other value until end of array
        while (jp.nextToken() != JsonToken.END_ARRAY) {}
        l += dep
      }
    }
    l.toList
  }

  private def parseSources(jp : JsonParser) : List[Source] = {
    val l = new ListBuffer[Source]()
    while (jp.nextToken() != JsonToken.END_ARRAY) {
      val source = if (jp.getCurrentToken == JsonToken.START_ARRAY) {
        jp.nextToken
        val source = new Source(new File(jp.getText), fs)
        jp.nextToken
        if (jp.getCurrentToken() != JsonToken.END_ARRAY) {
          if (jp.getCurrentToken == JsonToken.START_ARRAY) {
            val ls = new ListBuffer[String]()
            while (jp.nextToken() != JsonToken.END_ARRAY) {
              ls += jp.getText
            }
            source.excludes = ls.toList
            jp.nextToken
          }
        }
        if (jp.getCurrentToken() != JsonToken.END_ARRAY) {
          if (jp.getCurrentToken == JsonToken.START_ARRAY) {
            val ls = new ListBuffer[String]()
            while (jp.nextToken() != JsonToken.END_ARRAY) {
              ls += jp.getText
            }
            source.includes = ls.toList
            jp.nextToken
          }
        }
        // ignore other value until end of array
        while (jp.getCurrentToken() != JsonToken.END_ARRAY) { jp.nextToken() }
        source
      } else {
        new Source(new File(jp.getText), fs)
      }
      l += source
    }
    l.toList
  }
}

class Source(val dir : File, val fs : FileSystemHelper) {
  var includes : List[String] = List("**/*.scala", "**/*.java")
  var excludes : List[String] = Nil
  def files : Seq[File] = fs.findFiles(dir, includes, excludes)

  /**
   * search an existing file (ignore includes/excludes)
   */
  def find(rpath : String) : Option[File] = {
    val f = new File(dir, rpath)
    f.exists match {
      case true => Some(f)
      case false => None
    }
  }
}

case class Dependency(file : File, artifactId : String, version : String) {
  private lazy val _jarEntries : SortedSet[String] = {
    val jarFile = new JarInputStream(new FileInputStream(file))
    try {
      val b = SortedSet.newBuilder[String]
      var jarEntry = jarFile.getNextJarEntry()
      while (jarEntry != null) {
          b += jarEntry.getName().replace('\\', '/')
          jarEntry = jarFile.getNextJarEntry()
      }
      b.result
    } finally {
      jarFile.close()
    }
  }

  def contains(packageName : Option[String], typeName : Option[String]) : Boolean = {
    val path = packageName.map(_.replace('.', '/') + "/").getOrElse("") +  typeName.map(x => NameTransformer.encode(x) + ".class")
    if (file.isDirectory) {
        new File(file, path).exists
    } else {
        _jarEntries.contains(path)
    }
  }
}