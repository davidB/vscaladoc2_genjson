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

/**
 *
 */
package net_alchim31_vscaladoc2_genjson

import net_alchim31_utils.MiniLogger
import net_alchim31_utils.FileSystemHelper
import java.io.File
import junit.framework.Assert
import org.junit.Test
/**
 * @author david.bernard
 *
 */
class CfgTest {
  val logger = new MiniLogger("test")
  val fs = new FileSystemHelper(logger)

  @Test
  def parseEmpty() {
    val mini = """
          {
            }
        """
    val cfg = new CfgHelper(logger, fs)(mini)
    Assert.assertEquals("", cfg.groupId)
    Assert.assertEquals("undef", cfg.artifactId)
    Assert.assertEquals("0.0.0", cfg.version)
    Assert.assertEquals("", cfg.description)
    Assert.assertEquals(None, cfg.logo)
    Assert.assertEquals(None, cfg.license)
    Assert.assertEquals(Nil, cfg.sources)
    Assert.assertEquals(Nil, cfg.dependencies)
  }

  @Test
  def parseInfoMaxi() {
    val mini = """
          {
              "artifactId" : "demo1",
              "version" : "0.1-SNAPSHOT",
              "groupId" : "my.groupId",
              "logo" : "<a href=\"http://mysite/\"><img src=\"http://mysite/logo.png\" alt=\"My Site\"/></a>",
              "description" : "My <i>Description</i>",
              "license" : "ASL 2.0"
            }
        """
    val cfg = new CfgHelper(logger, fs)(mini)
    Assert.assertEquals("my.groupId", cfg.groupId)
    Assert.assertEquals("demo1", cfg.artifactId)
    Assert.assertEquals("0.1-SNAPSHOT", cfg.version)
    Assert.assertEquals("My <i>Description</i>", cfg.description)
    Assert.assertEquals(Some("""<a href="http://mysite/" rel="nofollow"><img src="http://mysite/logo.png" alt="My Site" /></a>"""), cfg.logo)
    Assert.assertEquals(Some("ASL 2.0"), cfg.license)
  }

  @Test
  def parseSources() {
    val mini = """
          {
            "sources" : [
              ["/tmp/src0"],
              ["/tmp/src1", [], []],
              ["/tmp/src2", ["Toto.java"]],
              ["/tmp/src3", ["Foo.java", "Bar.scala"]],
              ["/tmp/src4", ["Bar.scala"], ["**/*.scala"]]
            ]
            }
        """
    val cfg = new CfgHelper(logger, fs)(mini)

    Assert.assertEquals(5, cfg.sources .length)

    val src0 = cfg.sources(0)
    Assert.assertEquals("/tmp/src0", src0.dir.getAbsolutePath)
    Assert.assertEquals(Nil, src0.excludes)
    Assert.assertEquals(List("**/*.scala", "**/*.java"), src0.includes)

    val src1 = cfg.sources(1)
    Assert.assertEquals("/tmp/src1", src1.dir.getAbsolutePath)
    Assert.assertEquals(Nil, src1.excludes)
    Assert.assertEquals(Nil, src1.includes)

    val src2 = cfg.sources(2)
    Assert.assertEquals("/tmp/src2", src2.dir.getAbsolutePath)
    Assert.assertEquals(List("Toto.java"), src2.excludes)
    Assert.assertEquals(List("**/*.scala", "**/*.java"), src2.includes)

    val src3 = cfg.sources(3)
    Assert.assertEquals("/tmp/src3", src3.dir.getAbsolutePath)
    Assert.assertEquals(List("Foo.java", "Bar.scala"), src3.excludes)
    Assert.assertEquals(List("**/*.scala", "**/*.java"), src3.includes)

    val src4 = cfg.sources(4)
    Assert.assertEquals("/tmp/src4", src4.dir.getAbsolutePath)
    Assert.assertEquals(List("Bar.scala"), src4.excludes)
    Assert.assertEquals(List("**/*.scala"), src4.includes)
}

}