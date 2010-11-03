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
  def parseStringField() {
    val cfg = new CfgHelper(logger, fs)("""{
            		"artifactId" : "xArtifactId",
            		"version" : "xVersion",
            		"apidocdir" : "/tmp"
        }""")
    Assert.assertEquals("xArtifactId", cfg.artifactId)
    Assert.assertEquals("xVersion", cfg.version)
    Assert.assertEquals(new File("/tmp").getCanonicalFile, cfg.apidocdir)
  }

  @Test
  def parseInfoMini() {
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
  }

  @Test
  def parseInfoMaxi() {
    val mini = """
          {
              "artifactId" : "demo1",
              "version" : "0.1-SNAPSHOT",
              "groupId" : "my.groupId",
              "logo" : "<a href=\"http://mysite/\"><img src=\"http://mysite/logo.png\" alt=\"My Site\"/></a>",
              "description" : "My <i>Description</i>"
            }
        """
    val cfg = new CfgHelper(logger, fs)(mini)
    Assert.assertEquals("my.groupId", cfg.groupId)
    Assert.assertEquals("demo1", cfg.artifactId)
    Assert.assertEquals("0.1-SNAPSHOT", cfg.version)
    Assert.assertEquals("My <i>Description</i>", cfg.description)
    println(cfg.logo)
    Assert.assertEquals(Some("""<a href="http://mysite/" rel="nofollow"><img src="http://mysite/logo.png" alt="My Site" /></a>"""), cfg.logo)
  }

  @Test
  def parseOfSources() {
    //        val cfg = Cfg(""")
  }

}