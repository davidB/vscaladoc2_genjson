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

import scala.tools.nsc.doc.model.ValueParam
import scala.tools.nsc.doc.model.AbstractType
import scala.tools.nsc.doc.model.AliasType
import scala.tools.nsc.doc.model.Val
import scala.tools.nsc.doc.model.Def
import scala.tools.nsc.doc.model.Constructor
import scala.tools.nsc.doc.model.ProtectedInTemplate
import scala.tools.nsc.doc.model.Public
import scala.tools.nsc.doc.model.ProtectedInInstance
import scala.tools.nsc.doc.model.PrivateInTemplate
import scala.tools.nsc.doc.model.PrivateInInstance
import scala.tools.nsc.doc.model.Visibility
import scala.tools.nsc.doc.model.TypeParam
import scala.xml.NodeSeq
import scala.tools.nsc.doc.model.MemberEntity
import scala.tools.nsc.doc.model.TypeEntity
import scala.collection.mutable.ListBuffer
import scala.reflect.NameTransformer
import scala.tools.nsc.doc.Universe
import scala.tools.nsc.doc.model.NoDocTemplate
import scala.tools.nsc.doc.model.Entity
import scala.tools.nsc.doc.model.TemplateEntity
import scala.tools.nsc.doc.model.DocTemplateEntity
import scala.tools.nsc.doc.model.{ Trait, Class, Object, Package }
import org.codehaus.jackson.JsonEncoding
import org.codehaus.jackson.JsonGenerator

import java.io.{ File => JFile }

//see http://www.cowtowncoder.com/blog/archives/2009/01/entry_132.html
/**
 * A class that can generate api as json files to some fixed root folder.
 * 
 * @author David Bernard
 */
class JsonDocFactory(val cfg : Cfg, val uoaHelper : UriOfApiHelper, val htmlHelper : HtmlHelper) {

  type SplitStringWithRef = List[(String, Option[String])]
  
  private val _encoding : String = "UTF-8"

  /*universe.settings.outdir.value*/
  private val _siteRoot : JFile = cfg.apidocdir

  private val _jacksonFactory = new org.codehaus.jackson.JsonFactory()

  /**
   * @param rootPackage The model to generate in the form of a sequence of packages.
   */
  def generate(rootPackage : DocTemplateEntity) : Unit = {
    import scala.collection.mutable

    def writeMembers(uoa : UriOfApi, v : List[MemberEntity], written : mutable.HashSet[UriOfApi]) : Unit = {
      if (!(written contains uoa)) {
        write(uoa, v)
        written += uoa
        v match {
        	case tpl : DocTemplateEntity =>{
        		val groups = tpl.members.filter(_.inheritedFrom.isEmpty).filter(! _.isInstanceOf[Constructor]).groupBy(x => uoaHelper(x))
        		for( (uoa, ms) <- groups) {
        			writeMembers(uoa, ms, written)
        		}
        	}
        	case _ =>()
        }
        
      }
    }
    println("generate to " + _siteRoot)
    writeArtifactVersionInfo()
    writeMembers(uoaHelper(rootPackage), List(rootPackage), mutable.HashSet.empty[UriOfApi])
  }

  def writeArtifactVersionInfo() {
    val f = new JFile(_siteRoot, cfg.artifactId + "/" + cfg.version + ".json")
    f.getParentFile.mkdirs()
    val jg = _jacksonFactory.createJsonGenerator(f, JsonEncoding.UTF8);
    jg.useDefaultPrettyPrinter() // enable indentation just to make debug/testing easier
    try {
      jg.writeStartObject()
      jg.writeStringField("title", cfg.title.getOrElse(""))
      jg.writeStringField("artifactId", cfg.artifactId)
      jg.writeStringField("version", cfg.version)
      jg.writeStringField("description", cfg.description)
      jg.writeStringField("copyright", cfg.copyright.getOrElse(""))
      jg.writeArrayFieldStart("dependencies")
      for (dep <- cfg.dependencies) {
          jg.writeStartArray()
          jg.writeString(dep.artifactId)
          jg.writeString(dep.version)
          jg.writeEndArray()
      }
      jg.writeEndArray()
      jg.writeEndObject()
    } finally {
      jg.flush()
      jg.close()
    }
  }
  
  def write(uoa : UriOfApi, v : List[MemberEntity]) {
    val f = new JFile(_siteRoot, uoaHelper.toRefPath(uoa))
    f.getParentFile.mkdirs()
    val jg = _jacksonFactory.createJsonGenerator(f, JsonEncoding.UTF8);
    jg.useDefaultPrettyPrinter() // enable indentation just to make debug/testing easier
    jg.writeStringField("uoa", uoaHelper.toRefPath(uoa))
    jg.writeArrayFieldStart("e")
    try {
      for( m <- v) {
    	write(m, jg)  
      }
    } finally {
      jg.writeEndArray()
      jg.flush()
      jg.close()
    }
  }
  
  def write[T <: MemberEntity](v : T, jg : JsonGenerator) {
    try {
      jg.writeStartObject()
      
      val kind = v match {
        // Class is a Trait => case should be before
        case v : Class => writeClassData(v, jg) ; "class"
        case v : Trait => writeTraitData(v, jg) ; "trait"
        // Package is an Object => case should be before
        case v : Package => writePackageData(v, jg) ; "package"
        case v : Object => writeObjectData(v, jg) ; "object"
        // Package is an Object => case should be before
        case v : Constructor => throw new Exception("should not append here")
        case v : Def => "def"
        case v : Val => "val"
        case v : AliasType => "aliasType"
        case v : AbstractType => "abstractType"
        case v => "undef_" + v.getClass
      }
      jg.writeStringField("kind", kind)        

    } finally {  
      jg.writeEndObject()
    }
  }

  def writeFieldEntityList[T <: Entity](fieldName : String, l : List[T], jg : JsonGenerator) {
    jg.writeArrayFieldStart(fieldName)
    for (e <- l) {
       jg.writeString(uoaHelper.toRefPath(e)) 
    }
    jg.writeEndArray()
  }

  def writeSplitStringWithRef(fieldName : Option[String], v : SplitStringWithRef, jg : JsonGenerator) {
      fieldName.foreach{ x => jg.writeFieldName(x) }
      jg.writeStartArray()
      for ((str, oRef) <- v) {
          jg.writeStartArray()
          jg.writeString(str)
          oRef.foreach { x => jg.writeString(x) }
          jg.writeEndArray()
      }
      jg.writeEndArray()
  }
  
  def tentityToSplitStringWithRef(v : TypeEntity) : SplitStringWithRef = {
	  var lastFrag = 0
	  val b = new ListBuffer[(String, Option[String])]()
      for ((start, (entity, end)) <- v.refEntity) {
    	  if (start > lastFrag) {
    	 	  b += Tuple2( v.name.substring(lastFrag, start), None )
    	 	  lastFrag = start
    	  }
          b += Tuple2( v.name.substring(start, end), Some(uoaHelper.toRefPath(entity)) )
          lastFrag = end
      }
	  b.toList
  }

  def tparamsToSplitStringWithRef(v : List[TypeParam]) : SplitStringWithRef = {
    if (v.isEmpty) {
    	Nil
    } else {
      def tparam0(tp: TypeParam): SplitStringWithRef =
         (tp.variance + tp.name, None) +: boundsToSplitStringWithRef(tp.hi, tp.lo)
	  def tparams0(tpss: List[TypeParam]) : SplitStringWithRef = (tpss: @unchecked) match {
	    case tp :: Nil => tparam0(tp)
	    case tp :: tps => tparam0(tp) ::: ((", ", None) +: tparams0(tps))
	  }
	  ("[", None) +: tparams0(v) :+ ("]", None)
    }
  }
  
  def boundsToSplitStringWithRef(hi: Option[TypeEntity], lo: Option[TypeEntity]): SplitStringWithRef = {
	def bound0(bnd: Option[TypeEntity], pre: String): SplitStringWithRef = bnd match {
	  case None => Nil
	  case Some(tpe) => (pre, None) +: tentityToSplitStringWithRef(tpe)
	}
	bound0(hi, "<:") ::: bound0(lo, ">:")
  }
  
  def visibilityToSplitStringWithRef(mbr: MemberEntity) : SplitStringWithRef = {
    mbr.visibility match {
      case PrivateInInstance() => List(("private[this]", None))
      case PrivateInTemplate(owner) if (owner == mbr.inTemplate) =>  List(("private", None))
      case PrivateInTemplate(owner) => List(("private[", None), (owner.name, Some(uoaHelper.toRefPath(owner))), ("]", None))
      case ProtectedInInstance() => List(("protected[this]", None))
      case ProtectedInTemplate(owner) if (owner == mbr.inTemplate) => List(("protected", None))
      case ProtectedInTemplate(owner) => List(("protected[", None), (owner.name, Some(uoaHelper.toRefPath(owner))), ("]", None))
      case Public() => Nil
    }
  }

  def vparamsToSplitStringWithRef(v : List[List[ValueParam]]) : SplitStringWithRef = {
	  v.map {params =>
	   val l = params.foldLeft[SplitStringWithRef](Nil) { (r, param) =>
	  	 var b : SplitStringWithRef = r :+ (",", None)
	  	 if (param.isImplicit) b = b :+ ("implicit", None) 
	  	 b = b ++ tentityToSplitStringWithRef(param.resultType)
	  	 param.defaultValue match {
	  		 case Some(s) => b = b :+ (s, None)
	  		 case None => ()
	  	 }
	  	 b
	   }
	   ("(", None) +: l.tail :+ (")", None)
	  }.flatten
  }
// TreeEntity since 2.8.1
//  def defaultValueToSplitStringWithRef(defVal : TreeEntity) : SplitStringWithRef = {
//	    var index = 0
//	    val str = defVal.expression
//    val length = str.length
//	    var myXml : SplitStringWithRef  = (" =", None) :: Nil
//    for( x <- defVal.refs) {
//	      val from = x._1
//      val to = x._2._2
//	      if (index < from) {
//	        myXml ++= (str.substring(index,from), None)
//	        index = from
//	      }
//	     
//      if (index == from) {           
//        val member:Entity = x._2._1
//        member match {
//          case mbr: DocTemplateEntity =>
//            myXml ++= (str.substring(from, to), Some(uoaHelper.toRefPath(mbr)))
//          case mbr: MemberEntity => 
//            myXml ++= (str.substring(from, to), Some(uoaHelper.toRefPath(mbr)))
//          case _ => assert(false, "unexpected case in defaultValueToHtml")
//        }
//	        index = to
//      }
//    }
//    if (index <= length-1) myXml ++= (str.substring(index, length ), None)
//    myXml
//  }

  def writeMemberEntityData(v : MemberEntity, jg : JsonGenerator) {
    jg.writeStringField("name", v.name)
    jg.writeStringField("qualifiedName", v.qualifiedName)
    jg.writeStringField("definitionName", v.definitionName)
    jg.writeStringField("visibility", v.visibility.toString)
    v.comment.foreach { x => jg.writeStringField("description", htmlHelper.commentToHtml(x).toString) }
    jg.writeStringField("flags", v.visibility.toString)
    v.deprecation.foreach { x => jg.writeStringField("deprecation", htmlHelper.bodyToHtml(x).toString) }
    writeFieldEntityList("inheritedFrom", v.inheritedFrom, jg)
    writeSplitStringWithRef(Some("visilibility"), visibilityToSplitStringWithRef(v), jg)
  	writeSplitStringWithRef(Some("resultType"), tentityToSplitStringWithRef( v.resultType), jg)
  }
  
  def writeDocTemplateEntityData(v : DocTemplateEntity, jg : JsonGenerator) {
    writeMemberEntityData(v, jg)  
    v.inSource.foreach { s => 
      jg.writeArrayFieldStart("sourceStartPoint")
      jg.writeString(s._1.path)
      jg.writeNumber(s._2)
      jg.writeEndArray()
    }
    writeFieldEntityList("subClassesK", v.subClasses, jg)
    writeFieldEntityList("members", v.members, jg)
    writeFieldEntityList("templates", v.templates, jg)
    writeFieldEntityList("methods", v.methods, jg)
    writeFieldEntityList("values", v.values, jg)
    writeFieldEntityList("abstractTypes", v.abstractTypes, jg)
    writeFieldEntityList("aliasTypes", v.aliasTypes, jg)
    v.companion.foreach { v => jg.writeStringField("companion", uoaHelper.toRefPath(v)) }
    // signature
    
    v.parentType.foreach { x => writeSplitStringWithRef(Some("parentType"), tentityToSplitStringWithRef(x), jg) }
//76    def linearization: List[(TemplateEntity, TypeEntity)]
//77    def linearizationTemplates: List[TemplateEntity]
    jg.writeArrayFieldStart("linearization")
    for( te <- v.linearization) {
      jg.writeString(uoaHelper.toRefPath(te))
    }
    jg.writeEndArray()
  }

  def writeTraitData(v : Trait, jg : JsonGenerator) {
    writeDocTemplateEntityData(v, jg)
    writeSplitStringWithRef(Some("typeParams"), tparamsToSplitStringWithRef(v.typeParams), jg)
  }

  def writeClassData(v : Class, jg : JsonGenerator) {
    writeTraitData(v, jg)
    jg.writeArrayFieldStart("constructors")
    for( x <- v.constructors) {
       writeMemberEntityData(x, jg)
       writeSplitStringWithRef(Some("valueParams"), vparamsToSplitStringWithRef(v.valueParams), jg)
    }
    jg.writeEndArray()
  }

  def writeObjectData(v : Object, jg : JsonGenerator) {
    writeDocTemplateEntityData(v, jg)
  }

  def writePackageData(v : Package, jg : JsonGenerator) {
    writeObjectData(v, jg)
    writeFieldEntityList("packages", v.packages, jg)
  }
}

//  private void write(JsonGenerator jg, TwitterEntry entry) throws IOException
//  {
//    jg.writeStartObject();
//    // can either do "jg.writeFieldName(...) + jg.writeNumber()", or this:
//   jg.writeNumberField("id", entry.getId());
//   jg.writeStringField("text", entry.getText());
//   jg.writeNumberField("fromUserId", entry.getFromUserId());
//   jg.writeNumberField("toUserId", entry.getToUserId());
//   jg.writeStringField("langugeCode", entry.getLanguageCode());
//   jg.writeEndObject();
//   jg.close();
//  }

   