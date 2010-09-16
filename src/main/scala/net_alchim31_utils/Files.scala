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

package net_alchim31_utils

import java.io.{File}
import java.util.regex.{ Pattern }

object Files {

  def findFiles(root : File, includes : List[String] = Nil, excludes : List[String] = Nil) : List[File] = {
    val excludesP = excludes.map(Regexps.globToRegexPattern)
    val includesP = includes.map(Regexps.globToRegexPattern)

    def accept(file : File, rpath : String) : Boolean = {
        val isExcluded = excludesP.foldLeft(false)((r, pattern) => r || pattern.matcher(rpath).matches())
        val isIncluded = includesP match {
            case Nil => true
            case l => l.foldLeft(false)((r, pattern) => r || pattern.matcher(rpath).matches())
        }
        val b = !isExcluded && isIncluded
        println("check", b, rpath, file)
        b
    }

    def findFiles(rootDir : File, rpath : String) : List[File] = {
      val dir = new File(rootDir, rpath)
      var back : List[File] = Nil
      for (child <- dir.listFiles(); if !child.isHidden && child.canRead) {
        val rpathChild = rpath + "/" + child.getName
        if (child.isDirectory) {
          back = findFiles(rootDir, rpathChild) ::: back
        } else {
          accept(child, rpathChild) match {
            case true => back = child.getCanonicalFile :: back
            case false => back
          }
        }
      }
      back
    }

    val b = findFiles(root, "")
    println("nb files found under ", root, b.size, includes, excludes)
    b
  }

}