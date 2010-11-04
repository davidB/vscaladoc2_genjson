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

import scala.tools.nsc.doc.model.comment.Text
import net_alchim31_utils.MiniLogger
import scala.tools.nsc.util.Position
import scala.tools.nsc.doc.model.ModelFactory
import scala.tools.nsc.doc.model.comment.CommentFactory
import scala.tools.nsc.doc.model.comment.Body
import scala.tools.nsc.doc.model.comment.Comment

import scala.collection._
import scala.util.matching.Regex
import scala.annotation.switch

trait MyComment extends Comment {
  def docTags : Iterable[(String, List[String], Option[String])]
}

/**
 * Override CommentFactory : 
 * * to retrieve all tags and return a MyComment instead of Comment
 *
 * @param reporter The reporter on which user messages (error, warnings) should be printed.
 */
trait MyCommentFactory extends CommentFactory { thisFactory: ModelFactory with CommentFactory =>
  
  import global.reporter
  
  /** Parses a raw comment string into a `Comment` object.
    * @param comment The raw comment string (including start and end markers) to be parsed.
    * @param pos     The position of the comment in source. */
  override protected def parse(comment: String, pos: Position): Comment = {

    /** The cleaned raw comment as a list of lines. Cleaning removes comment start and end markers, line start markers
      * and unnecessary whitespace. */
    val cleaned: List[String] = {
      def cleanLine(line: String): String = {
        //replaceAll removes trailing whitespaces       
        line.replaceAll("""\s+$""", "") match {
          case CleanCommentLine(ctl) => ctl
          case tl => tl
        }
      }
      val strippedComment = comment.trim.stripPrefix("/*").stripSuffix("*/")
      val safeComment = DangerousTags.replaceAllIn(strippedComment, { htmlReplacement(_) })
      val markedTagComment =
        SafeTags.replaceAllIn(safeComment, { mtch =>
          java.util.regex.Matcher.quoteReplacement(safeTagMarker + mtch.matched + safeTagMarker)
        })
      markedTagComment.lines.toList map (cleanLine(_))
    }
    
    /** Parses a comment (in the form of a list of lines) to a Comment instance, recursively on lines. To do so, it
      * splits the whole comment into main body and tag bodies, then runs the `WikiParser` on each body before creating
      * the comment instance.
      *
      * @param docBody     The body of the comment parsed until now.
      * @param tags        All tags parsed until now.
      * @param lastTagKey  The last parsed tag, or `None` if the tag section hasn't started. Lines that are not tagged
      *                    are part of the previous tag or, if none exists, of the body.
      * @param remaining   The lines that must still recursively be parsed.
      * @param inCodeBlock Whether the next line is part of a code block (in which no tags must be read). */
    def parse0(docBody: String, tags: Map[TagKey, List[String]], lastTagKey: Option[TagKey], remaining: List[String], inCodeBlock: Boolean): Comment = {
      remaining match {

        case CodeBlockStart(before, after) :: ls if (!inCodeBlock) =>
          if (before.trim != "")
            parse0(docBody, tags, lastTagKey, before :: ("{{{" + after) :: ls, false)
          else if (after.trim != "")
            parse0(docBody, tags, lastTagKey, "{{{" :: after :: ls, true)
          else
            parse0(docBody + endOfLine + "{{{", tags, lastTagKey, ls, true)

        case CodeBlockEnd(before, after) :: ls =>
          if (before.trim != "")
            parse0(docBody, tags, lastTagKey, before :: ("}}}" + after) :: ls, true)
          else if (after.trim != "")
            parse0(docBody, tags, lastTagKey, "}}}" :: after :: ls, false)
          else
            parse0(docBody + endOfLine + "}}}", tags, lastTagKey, ls, false)

        case SymbolTag(name, sym, body) :: ls if (!inCodeBlock) =>
          val key = SymbolTagKey(name, sym)
          val value = body :: tags.getOrElse(key, Nil)
          parse0(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)

        case SimpleTag(name, body) :: ls if (!inCodeBlock) =>
          val key = SimpleTagKey(name)
          val value = body :: tags.getOrElse(key, Nil)
          parse0(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)

        case line :: ls if (lastTagKey.isDefined) =>
          val key = lastTagKey.get
          val value =
            ((tags get key): @unchecked) match {
              case Some(b :: bs) => (b + endOfLine + line) :: bs
              case None => oops("lastTagKey set when no tag exists for key")
            }
          parse0(docBody, tags + (key -> value), lastTagKey, ls, inCodeBlock)

        case line :: ls =>
          val newBody = if (docBody == "") line else docBody + endOfLine + line
          parse0(newBody, tags, lastTagKey, ls, inCodeBlock)

        case Nil =>

          val bodyTags: mutable.Map[TagKey, List[Body]] =
            mutable.Map(tags mapValues (_ map (parseWiki(_, pos))) toSeq: _*)

          def oneTag(key: SimpleTagKey): Option[Body] =
            ((bodyTags remove key): @unchecked) match {
              case Some(r :: rs) =>
                if (!rs.isEmpty) reporter.warning(pos, "Only one '@" + key.name + "' tag is allowed")
                Some(r)
              case None => None
            }

          def allTags(key: SimpleTagKey): List[Body] =
            (bodyTags remove key) getOrElse Nil

          def allSymsOneTag(key: TagKey): Map[String, Body] = {
            val keys: Seq[SymbolTagKey] =
              bodyTags.keys.toSeq flatMap {
                case stk: SymbolTagKey if (stk.name == key.name) => Some(stk)
                case stk: SimpleTagKey if (stk.name == key.name) =>
                  reporter.warning(pos, "Tag '@" + stk.name + "' must be followed by a symbol name")
                  None
                case _ => None
              }
            val pairs: Seq[(String, Body)] =
              for (key <- keys) yield {
                val bs = (bodyTags remove key).get
                if (bs.length > 1)
                  reporter.warning(pos, "Only one '@" + key.name + "' tag for symbol " + key.symbol + " is allowed")
                (key.symbol, bs.head)
              }
            Map.empty[String, Body] ++ pairs
          }

          /**
           * revert parsing
           */
          def toDocTags(tags : Map[TagKey, List[String]]) : Iterable[(String, List[String], Option[String])] = {
            for((key, bodys) <- tags) yield {
              key match {
                case SimpleTagKey(name) => (name, bodys, None)
                case SymbolTagKey(name, symbol) => (name, bodys, Some(symbol))
              }
            }
          }
          
          val com = new MyComment {
            val body        = parseWiki(docBody, pos)
            val docTags     = toDocTags(tags)
            val authors     = allTags(SimpleTagKey("author"))
            val see         = allTags(SimpleTagKey("see"))
            val result      = oneTag(SimpleTagKey("return"))
            val throws      = allSymsOneTag(SimpleTagKey("throws"))
            val valueParams = allSymsOneTag(SimpleTagKey("param"))
            val typeParams  = allSymsOneTag(SimpleTagKey("tparam"))
            val version     = oneTag(SimpleTagKey("version"))
            val since       = oneTag(SimpleTagKey("since"))
            val todo        = allTags(SimpleTagKey("todo"))
            val deprecated  = oneTag(SimpleTagKey("deprecated"))
            val note        = allTags(SimpleTagKey("note"))
            val example     = allTags(SimpleTagKey("example"))
            val short       = body.summary getOrElse Text("no summary matey")
          }
//
//          for ((key, _) <- bodyTags)
//            reporter.warning(pos, "Tag '@" + key.name + "' is not recognised")

          com

      }
    }

    parse0("", Map.empty, None, cleaned, false)

  }
}

