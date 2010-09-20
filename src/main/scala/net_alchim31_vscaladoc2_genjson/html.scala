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

import scala.xml.NodeSeq
// copy from scala.tools.nsc.doc.html.HtmlPage
class HtmlHelper(val uoaHelper : UriOfApiHelper) {
  import scala.tools.nsc.doc.model.comment._    
  
  /**
   * Transforms an optional comment into an styled HTML tree representing its body if it is defined, or into an empty
   * node sequence if it is not.
   */
   def commentToHtml(comment: Option[Comment]): NodeSeq =
     (comment map (commentToHtml(_))) getOrElse NodeSeq.Empty
  
   /**
    * Transforms a comment into an styled HTML tree representing its body.
    */
   def commentToHtml(comment: Comment): NodeSeq =
     bodyToHtml(comment.body)
 
   def bodyToHtml(body: Body): NodeSeq =
     body.blocks flatMap (blockToHtml(_))
 
   def blockToHtml(block: Block): NodeSeq = block match {
     case Title(in, 1) => <h3>{ inlineToHtml(in) }</h3>
     case Title(in, 2) => <h4>{ inlineToHtml(in) }</h4>
     case Title(in, 3) => <h5>{ inlineToHtml(in) }</h5>
     case Title(in, _) => <h6>{ inlineToHtml(in) }</h6>
     case Paragraph(in) => <p>{ inlineToHtml(in) }</p>
     case Code(data) => <pre>{ xml.Text(data) }</pre>
     case UnorderedList(items) =>
       <ul>{ listItemsToHtml(items) }</ul>
     case OrderedList(items, listStyle) =>
       <ol class={ listStyle }>{ listItemsToHtml(items) }</ol>
     case DefinitionList(items) =>
       <dl>{items map { case (t, d) => <dt>{ inlineToHtml(t) }</dt><dd>{ blockToHtml(d) }</dd> } }</dl>
     case HorizontalRule() =>
       <hr/>
   }
  
   def listItemsToHtml(items: Seq[Block]) =
     items.foldLeft(xml.NodeSeq.Empty){ (xmlList, item) =>
       item match {
         case OrderedList(_, _) | UnorderedList(_) =>  // html requires sub ULs to be put into the last LI
           xmlList.init ++ <li>{ xmlList.last.child ++ blockToHtml(item) }</li>
         case Paragraph(inline) =>
           xmlList :+ <li>{ inlineToHtml(inline) }</li>  // LIs are blocks, no need to use Ps
         case block =>
           xmlList :+ <li>{ blockToHtml(block) }</li>
       }
   }
  
   def inlineToHtml(inl: Inline): NodeSeq = inl match {
     case Chain(items) => items flatMap (inlineToHtml(_))
     case Italic(in) => <i>{ inlineToHtml(in) }</i>
     case Bold(in) => <b>{ inlineToHtml(in) }</b>
     case Underline(in) => <u>{ inlineToHtml(in) }</u>
     case Superscript(in) => <sup>{ inlineToHtml(in) }</sup>
     case Subscript(in) => <sub>{ inlineToHtml(in) }</sub>
     case Link(raw, title) => <a href={ raw }>{ inlineToHtml(title) }</a>
     case EntityLink(entity) => <a href={"api:" + uoaHelper.toRefPath(entity) } title={entity.qualifiedName}>{ entity.name }</a>
     case Monospace(text) => <code>{ xml.Text(text) }</code>
     case Text(text) => xml.Text(text)
     case Summary(in) => inlineToHtml(in)
     case HtmlTag(tag) => xml.Unparsed(tag)
   }
}
