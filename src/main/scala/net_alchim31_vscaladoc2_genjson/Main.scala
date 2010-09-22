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

import net_alchim31_utils.FileSystemHelper
import net_alchim31_utils.FileSystemHelper
import java.io.File
import scala.tools.nsc.{doc, FatalError, CompilerCommand}
import scala.tools.nsc.doc.{model, DocFactory}
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }

/**
 * @author david.bernard
 *
 */
object Main {

  /** The main class for scaladoc, a front-end for the Scala compiler 
   *  that generates documentation from source files.
   */

  private val versionMsg : String = "VScaladoc2_genjson 0.1"

  private lazy val _fs = new FileSystemHelper()
  
  def main(args : Array[String]) {
    val jsonCfg = args(0)
    println(jsonCfg)
    val reporter = process(new CfgHelper(_fs).apply(new File(jsonCfg)))
    //exit(if (reporter.hasErrors) 1 else 0)
  }

  //  def mkScaladoc2Args() : Array[String] = {
  //      var back = new ArrayBuffer[String]()
  //      
  //      val outputDir = new File(System.getProperty("output", System.getProperty("user.dir") + "/api"))
  //      outputDir.mkdirs()
  //      back + ("-d", outputDir.getCanonicalPath)
  //      
  //      val inputDir = new File(System.getProperty("input", System.getProperty("user.dir") + "/src"))
  //      if (!inputDir.exists) {
  //          throw new IllegalArgumentException("no 'input' dir defined")
  //      }
  //      scalaFiles
  //      back.toArray
  //  }

  def process(cfg : Cfg) : ConsoleReporter = {
    println("docSettings.outdir", cfg.apidocdir)
    cfg.apidocdir.mkdirs()

    def error(msg : String) : Unit = {
      //reporter.error(FakePos("scalac"), msg + "\n  scalac -help  gives more information")
      println("ERROR: " + msg)
    }

    val docSettings : doc.Settings = new doc.Settings(error)
    val reporter = new ConsoleReporter(docSettings) //ConsoleReporter doesn't use settings (param 1) but AbstractReposter use it

    //    docSettings.d.
    val l = cfg.scaladoc2Args
    println(l)
    val command = new CompilerCommand(l, docSettings)

    //    if (!reporter.hasErrors) { // No need to continue if reading the command generated errors

    if (docSettings.version.value)
      reporter.info(null, versionMsg, true)
    else if (docSettings.help.value) {
      reporter.info(null, command.usageMsg, true)
    } else if (docSettings.Xhelp.value)
      reporter.info(null, command.xusageMsg, true)
    else if (docSettings.Yhelp.value)
      reporter.info(null, command.yusageMsg, true)
    else if (docSettings.showPlugins.value)
      reporter.warning(null, "Plugins are not available when using Scaladoc")
    else if (docSettings.showPhases.value)
      reporter.warning(null, "Phases are restricted when using Scaladoc")
    else try {
      val docProcessor = new MyDocFactory(reporter, docSettings, cfg, _fs)
      docProcessor.document(command.files)

    } catch {
      case ex@FatalError(msg) =>
        if (docSettings.debug.value) ex.printStackTrace();
        reporter.error(null, "fatal error: " + msg)
    }
    finally {
      reporter.printSummary()
    }
    //    }
    reporter
  }

}

class MyDocFactory(reporter : Reporter, settings : doc.Settings, cfg : Cfg, val fs : FileSystemHelper) extends DocFactory(reporter, settings) {

  /**
   * Creates a scaladoc site for all symbols defined in this call's `files`, as well as those defined in `files` of
   * previous calls to the same processor.
   * @param files The list of paths (relative to the compiler's source path, or absolute) of files to document.
   */
  override def document(files : List[String]) : Unit = {
    println("analyzing sources...")  
    (new compiler.Run()) compile files
    compiler.addSourceless
    if (!reporter.hasErrors) {
      val modelFactory = (new model.ModelFactory(compiler, settings) with model.comment.CommentFactory)
      val docModel = modelFactory.makeModel
      println("... model contains " + modelFactory.templatesCount + " documentable top entity")
      println("generating json into "+ cfg.apidocdir +"...")
      val uoaHelper = new UriOfApiHelper(cfg)
      val htmlHelper = new HtmlHelper(uoaHelper)
      //(new html.HtmlFactory(docModel)).generate
      new JsonDocFactory(cfg, uoaHelper, htmlHelper).generate(docModel.rootPackage, fs)
      println("...DONE")  
    } else {
      println("...failed")
    }
  }
}

