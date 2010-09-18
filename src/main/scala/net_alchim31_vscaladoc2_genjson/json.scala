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

  private val _encoding : String = "UTF-8"

  /*universe.settings.outdir.value*/
  private val _siteRoot : JFile = cfg.apidocdir

  private val _jacksonFactory = new org.codehaus.jackson.JsonFactory()

  /**
   * @param model The model to generate in the form of a sequence of packages.
   */
  def generate(rootPackage : DocTemplateEntity) : Unit = {
    import scala.collection.mutable

    def writeTemplate(tpl : DocTemplateEntity, written : mutable.HashSet[DocTemplateEntity]) : Unit = {
      if (!(written contains tpl)) {
        write(tpl)
        written += tpl
        tpl.templates map (writeTemplate(_, written))
      }
    }
    println("generate to " + _siteRoot)
    writeArtifactVersionInfo()
    writeTemplate(rootPackage, mutable.HashSet.empty[DocTemplateEntity])
  }

  //  def findPath[T <: Entity](v : T) = findPath(v, v.qualifiedName) 
  //
  //  def findPath[T <: Entity](v : T, qualifiedName : String) : List[String] = {
  //    v match {
  //        case x : TemplateEntity => List(x.qualifiedName, qualifiedName.substring(x.qualifiedName.length + 1))
  //        case _ => findPackageName(v.inTemplate, qualifiedName)
  //    }
  //  }


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
  
  def write[T <: DocTemplateEntity](v : T) {

    val f = new JFile(_siteRoot, uoaHelper.toRefPath(v))
    f.getParentFile.mkdirs()
    val jg = _jacksonFactory.createJsonGenerator(f, JsonEncoding.UTF8);
    jg.useDefaultPrettyPrinter() // enable indentation just to make debug/testing easier
    try {
      jg.writeStartObject()
      v match {
        // Class is a Trait => case should be before
        case v : Class => writeClassData(v, jg)
        case v : Trait => writeTraitData(v, jg)
        // Package is an Object => case should be before
        case v : Package => writePackageData(v, jg)
        case v : Object => writeObjectData(v, jg)
      }
      jg.writeEndObject()
    } finally {
      jg.flush()
      jg.close()
    }
  }

  def writeFieldEntityList[T <: Entity](fieldName : String, l : List[T], jg : JsonGenerator) {
    jg.writeArrayFieldStart(fieldName)
    for (e <- l) {
       jg.writeString(uoaHelper.toRefPath(e)) 
    }
    jg.writeEndArray()
  }

  def writeEntityData(v : Entity, jg : JsonGenerator) {
    jg.writeStringField("name", v.name)
    jg.writeStringField("qualifiedName", v.qualifiedName)
  }
  
  def writeTypeEntity(fieldName : Option[String], v : TypeEntity, jg : JsonGenerator) {
      fieldName.foreach{ x => jg.writeFieldName(x) }
      jg.writeStartObject()
      jg.writeStringField("name", v.name)
      jg.writeArrayFieldStart("ref")
      for ((start, (entity, end)) <- v.refEntity) {
          jg.writeStartArray()
          jg.writeString(uoaHelper.toRefPath(entity))
          jg.writeNumber(start)
          jg.writeNumber(end)
          jg.writeEndArray()
      }
      jg.writeEndArray()
      jg.writeEndObject()
  }
  
  def writeMemberEntity(v : MemberEntity, jg : JsonGenerator) {
    jg.writeStringField("definitionName", v.definitionName)
    jg.writeStringField("visibility", v.visibility.toString)
    v.comment.foreach { x => jg.writeStringField("description", htmlHelper.commentToHtml(x).toString) }
    jg.writeStringField("flags", v.visibility.toString)
    v.deprecation.foreach { x => jg.writeStringField("deprecation", htmlHelper.bodyToHtml(x).toString) }
    writeFieldEntityList("inheritedFrom", v.inheritedFrom, jg)
    jg.writeStringField("resultType", v.visibility.toString)
    writeTypeEntity(Some("resultType"), v.resultType, jg)
  }
  
  def writeDocTemplateEntityData(v : DocTemplateEntity, jg : JsonGenerator) {
    writeEntityData(v, jg)  
    jg.writeStringField("uoa", uoaHelper.toRefPath(v))
    jg.writeStringField("kind", if (v.isTrait) "trait" else if (v.isClass) "class" else if (v.isObject) "object" else if (v.isPackage) "package" else "undef")
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
    
    v.parentType.foreach { x => writeTypeEntity(Some("parentType"), x, jg) }
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
  }

  def writeClassData(v : Class, jg : JsonGenerator) {
    writeTraitData(v, jg)
//def primaryConstructor: Option[Constructor]
//95    def constructors: List[Constructor]
//96    def valueParams: List[List[ValueParam]]    
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

   