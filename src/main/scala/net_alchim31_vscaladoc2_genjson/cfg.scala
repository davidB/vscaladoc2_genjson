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

import java.io.FileInputStream
import java.util.jar.JarInputStream
import org.codehaus.jackson.JsonParser
import scala.collection.mutable.ListBuffer
import org.codehaus.jackson.JsonToken
import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.doc.DocFactory
import java.io.File
import net_alchim31_utils.Files
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.FatalError
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }
import scala.tools.nsc.util.FakePos //{Position}
import scala.tools.nsc.doc
import scala.tools.nsc.doc.model
import scala.reflect.NameTransformer



class Cfg {
  var artifactId = "undef"
  var version = "0.0.1"
  var description = ""
  var sources : List[Source] = Nil
  var dependencies : List[Dependency] = Nil
  var title : Option[String] = None
  var copyright : Option[String] = None
  var additionnalArgs : List[String] = Nil
  var apidocdir : File = new File("./apidoc")

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

    l += "-d"
    l += apidocdir.getCanonicalPath

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

object Cfg {
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
          case "artifactId" => b.artifactId = jp.getText
          case "version" => b.version = jp.getText
          case "description" => b.description = jp.getText
          case "title" => b.title = Some(jp.getText)
          case "copyright" => b.copyright = Some(jp.getText)
          case "dependencies" if jp.getCurrentToken == JsonToken.START_ARRAY => b.dependencies = parseDependencies(jp)
          case "dependencies" => println("dependencies should be an array of array") //TODO use logger
          case "sources" if jp.getCurrentToken == JsonToken.START_ARRAY => b.sources = parseSources(jp)
          case "sources" => println("dependencies should be an array of array") //TODO use logger
          case "apidocdir" => b.apidocdir = new File(jp.getText).getCanonicalFile
          case x => println("unsupported field :" + x)
        }
      }
    } finally {
      jp.close()
    }
    b
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
        val source = new Source(new File(jp.getText))
        jp.nextToken
        if (jp.getCurrentToken() != JsonToken.END_ARRAY) {
          if (jp.getCurrentToken == JsonToken.START_ARRAY) {
            val ls = new ListBuffer[String]()
            while (jp.nextToken() != JsonToken.END_ARRAY) {
              ls += jp.getText
            }
            source.includes = ls.toList
          }
        }
        if (jp.getCurrentToken() != JsonToken.END_ARRAY) {
          if (jp.getCurrentToken == JsonToken.START_ARRAY) {
            val ls = new ListBuffer[String]()
            while (jp.nextToken() != JsonToken.END_ARRAY) {
              ls += jp.getText
            }
            source.excludes = ls.toList
          }
        }
        // ignore other value until end of array
        while (jp.getCurrentToken() != JsonToken.END_ARRAY) { jp.nextToken() }
        source
      } else {
        new Source(new File(jp.getText))
      }
      l += source
    }
    l.toList
  }
}

class Source(val dir : File) {
  var includes : List[String] = List("**/*.scala", "**/*.java")
  var excludes : List[String] = Nil
  def files : Seq[File] = Files.findFiles(dir, includes, excludes)

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
  private lazy val _jarEntries : List[String] = {
    val jarFile = new JarInputStream(new FileInputStream(file))
    try {
      val b = new ListBuffer[String]()
      var jarEntry = jarFile.getNextJarEntry()
      while (jarEntry != null) {
          b += jarEntry.getName().replace('\\', '/')
          jarEntry = jarFile.getNextJarEntry()
      }
      b.toList
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