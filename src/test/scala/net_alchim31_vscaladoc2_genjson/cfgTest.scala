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
  def parseDemo1() {
    val txt = """
          {
              "artifactId" : "demo1",
              "version" : "0.1-SNAPSHOT",
              "apidocdir" : "/xxx/work/oss/demo-1/target/site/scaladocs",
              "dependencies" : [
                ["/xxx/.m2/repository/org/scala-lang/scala-library/2.8.0/scala-library-2.8.0.jar", "scala-library", "2.8.0"]
              ],
              "sources" : [
                ["/xxx/work/oss/demo-1/src/main/scala"]
              ]
            }
        """
    val cfg = new CfgHelper(logger, fs)(txt)
    Assert.assertEquals("demo1", cfg.artifactId)
    Assert.assertEquals("0.1-SNAPSHOT", cfg.version)
    Assert.assertEquals(new File("/xxx/work/oss/demo-1/target/site/scaladocs").getCanonicalFile, cfg.apidocdir)
  }

  @Test
  def parseOfSources() {
    //        val cfg = Cfg(""")
  }

}