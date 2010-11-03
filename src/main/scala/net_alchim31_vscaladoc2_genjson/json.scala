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

import org.jsoup.safety.Whitelist
import org.jsoup.Jsoup
import net_alchim31_utils.MiniLogger
import net_alchim31_utils.FileSystemHelper
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
class JsonDocFactory(val logger: MiniLogger, val cfg: Cfg, val uoaHelper: UriOfApiHelper, val htmlHelper: HtmlHelper, val fs : FileSystemHelper, val commentPlus : CommentPlus) {

  case class StringWithRef(s : String, ref : Option[String])
  implicit def toStringWithRef(x : Tuple2[String, Option[String]]) = StringWithRef(x._1, x._2)

  type SplitStringWithRef = List[StringWithRef]

  private val _encoding = "UTF-8"
  private val _extension = "_.json"

  /*universe.settings.outdir.value*/
  private val _siteRoot: JFile = cfg.apidocdirTmp

  private val _jacksonFactory = new org.codehaus.jackson.JsonFactory()

  /**
   * @param rootPackage The model to generate in the form of a sequence of packages.
   */
  def generate(rootPackage: DocTemplateEntity): Unit = {
    import scala.collection.mutable

    def writeMembers(uoa: UriOfApi, v: List[MemberEntity], written: mutable.HashSet[UriOfApi]): mutable.HashSet[UriOfApi] = {
      if (!(written contains uoa)) {
        write(uoa, v)
        written += uoa
        v.foreach {
          case tpl: DocTemplateEntity => {
            val groups = tpl.members.filter{x =>
              val notInherited = x.inheritedFrom.isEmpty || x.inheritedFrom.contains(tpl)
                x.isInstanceOf[DocTemplateEntity] || ( notInherited && !x.isInstanceOf[Constructor])
            }.groupBy(x => uoaHelper(x))
            for ((uoa, ms) <- groups) {
              writeMembers(uoa, ms, written)
            }
          }
          case _ => () // don't go deep
        }
      }
      written
    }
    logger.info("generate to %s", _siteRoot)
    _siteRoot.mkdirs()
    logger.info("generate artifact info... ")
    val f0 = writeArtifactVersionInfo()
    logger.info("generate code info... ")
    val fn = writeMembers(uoaHelper(rootPackage), List(rootPackage), mutable.HashSet.empty[UriOfApi]).toSet.map { x : UriOfApi => uoaHelper.toRefPath(x) + _extension}
    logger.info("generate archive... ")
    val archive = new JFile(_siteRoot, cfg.artifactId + "-" + cfg.version + "-apidoc.jar")
    fs.jar(archive, _siteRoot, fn + f0)
    logger.info("move (overwrite) generated to %s ...", cfg.apidocdir)
    commit(archive.getName, (cfg.artifactId + "/" + cfg.version), (cfg.artifactId + "/" + cfg.version + _extension))
  }

  private def commit(filenames : String*) {
    for (fname <- filenames) {
      val dest = new JFile(cfg.apidocdir, fname)
      if (dest.exists) {
        fs.deleteRecursively(dest)
      }
      fs.move(new JFile(_siteRoot, fname), dest)
    }
    fs.deleteRecursively(_siteRoot)
    if (_siteRoot.exists) {
      logger.info("can't delete workingd dir : %s", _siteRoot)
    }
  }

  def writeArtifactVersionInfo() : String = {
    val b = cfg.artifactId + "/" + cfg.version + _extension
    val f = new JFile(_siteRoot, b)
    f.getParentFile.mkdirs()
    val jg = _jacksonFactory.createJsonGenerator(f, JsonEncoding.UTF8);
    jg.useDefaultPrettyPrinter() // enable indentation just to make debug/testing easier
    try {
      jg.writeStartObject()
      jg.writeStringField("groupId", cfg.groupId)
      jg.writeStringField("artifactId", cfg.artifactId)
      jg.writeStringField("version", cfg.version)
      jg.writeStringField("description", cfg.description)
      jg.writeStringField("logo", cfg.logo.getOrElse(""))
      jg.writeStringField("license", cfg.logo.getOrElse(""))
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
    b
  }

  def write(uoa: UriOfApi, v: List[MemberEntity]) {
    val rpath = uoaHelper.toRefPath(uoa)
    logger.debug("writing ", rpath)
    val f = new JFile(_siteRoot, rpath + _extension)
    if (!f.getParentFile.isDirectory && !f.getParentFile.mkdirs()) {
      logger.warn("can't create directory : %s", f.getParentFile)
    }
    val jg = _jacksonFactory.createJsonGenerator(f, JsonEncoding.UTF8);
    jg.useDefaultPrettyPrinter() // enable indentation just to make debug/testing easier
    try {
      jg.writeStartObject()
      jg.writeNumberField("v", 2)
      jg.writeStringField("uoa", rpath)
      jg.writeArrayFieldStart("e")
      for (m <- v) {
        write(m, jg)
      }
      jg.writeEndArray()
      jg.writeEndObject()
    } finally {
      jg.flush()
      jg.close()
    }
  }

  def write[T <: MemberEntity](v: T, jg: JsonGenerator) {

    jg.writeStartObject()

    val kind = v match {
      // Class is a Trait => case should be before
      case v: Class => writeClassData(v, jg); "class"
      case v: Trait => writeTraitData(v, jg); "trait"
      // Package is an Object => case should be before
      case v: Package => writePackageData(v, jg); "package"
      case v: Object => writeObjectData(v, jg); "object"
      // Non top element
      case v: Constructor => throw new Exception("should not append here")
      case v: Def => writeDefData(v, jg); "def"
      case v: Val => writeValData(v, jg); if (v.isLazyVal) "lazy val" else if (v.isVar) "var" else "val"
      case v: AliasType => writeAliasTypeData(v, jg); "aliasType"
      case v: AbstractType => writeAbstractTypeData(v, jg); "abstractType"
      case v => "undef_" + v.getClass
    }
    jg.writeStringField("kind", kind)
    jg.writeEndObject()
  }

  def writeFieldEntityList[T <: Entity](fieldName: String, l: List[T], jg: JsonGenerator) {
    jg.writeArrayFieldStart(fieldName)
    l.map(x => uoaHelper.toRefPath(x)).sortWith(_ < _).distinct.foreach{ ref =>
      jg.writeString(ref)
    }
    jg.writeEndArray()
  }

  def writeSplitStringWithRef(fieldName: Option[String], v: SplitStringWithRef, jg: JsonGenerator) {
    fieldName.foreach { x => jg.writeFieldName(x) }
    jg.writeStartArray()
    for (swr <- v) {
      jg.writeStartArray()
      jg.writeString(swr.s)
      swr.ref.foreach { x => jg.writeString(x) }
      jg.writeEndArray()
    }
    jg.writeEndArray()
  }

  def tentityToSplitStringWithRef(v: TypeEntity): SplitStringWithRef = {
    var lastFrag = 0
    val b = new ListBuffer[StringWithRef]()
    for ((start, (entity, end)) <- v.refEntity) {
      if (start > lastFrag) {
        b += StringWithRef(v.name.substring(lastFrag, start), None)
        lastFrag = start
      }
      b += StringWithRef(v.name.substring(start, start+end), Some(uoaHelper.toRefPath(entity)))
      lastFrag = start + end
    }
    if (lastFrag < v.name.length ) {
      b += StringWithRef(v.name.substring(lastFrag), None)
    }
    b.toList
  }

  def tparamsToSplitStringWithRef(v: List[TypeParam]): SplitStringWithRef = {
    if (v.isEmpty) {
      Nil
    } else {
      def tparam0(tp: TypeParam): SplitStringWithRef =
        StringWithRef(tp.variance + tp.name, None) +: boundsToSplitStringWithRef(tp.hi, tp.lo)
      def tparams0(tpss: List[TypeParam]): SplitStringWithRef = (tpss: @unchecked) match {
        case tp :: Nil => tparam0(tp)
        case tp :: tps => tparam0(tp) ::: (StringWithRef(", ", None) +: tparams0(tps))
      }
      StringWithRef("[", None) +: tparams0(v) :+ StringWithRef("]", None)
    }
  }

  def boundsToSplitStringWithRef(hi: Option[TypeEntity], lo: Option[TypeEntity]): SplitStringWithRef = {
    def bound0(bnd: Option[TypeEntity], pre: String): SplitStringWithRef = bnd match {
      case None => Nil
      case Some(tpe) => StringWithRef(pre, None) +: tentityToSplitStringWithRef(tpe)
    }
    bound0(hi, "<:") ::: bound0(lo, ">:")
  }

  def visibilityToSplitStringWithRef(mbr: MemberEntity): SplitStringWithRef = {
    mbr.visibility match {
      case PrivateInInstance() => List(("private[this]", None))
      case PrivateInTemplate(owner) if (owner == mbr.inTemplate) => List(("private", None))
      case PrivateInTemplate(owner) => List(("private[", None), (owner.name, Some(uoaHelper.toRefPath(owner))), ("]", None))
      case ProtectedInInstance() => List(("protected[this]", None))
      case ProtectedInTemplate(owner) if (owner == mbr.inTemplate) => List(("protected", None))
      case ProtectedInTemplate(owner) => List(("protected[", None), (owner.name, Some(uoaHelper.toRefPath(owner))), ("]", None))
      case Public() => Nil
    }
  }

  def vparamsToSplitStringWithRef(v: List[List[ValueParam]]): SplitStringWithRef = {
    v.map { params =>
      val l = params.foldLeft[SplitStringWithRef](Nil) { (r, param) =>
        var b: SplitStringWithRef = r :+ StringWithRef(", ", None)
        if (param.isImplicit) b = b :+ StringWithRef("implicit", None)
        b = b :+ StringWithRef(param.name + " : ", None)
        b = b ++ tentityToSplitStringWithRef(param.resultType)
        param.defaultValue match {
          case Some(s) => b = b :+ StringWithRef(s, None)
          case None => ()
        }
        b
      }
      l match {
          case Nil => List(StringWithRef("( )", None))
          case comma :: tail => StringWithRef("( ", None) +: tail :+ StringWithRef(" )", None)
      }

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

  def writeMemberEntityData(v: MemberEntity, jg: JsonGenerator) {
    jg.writeStringField("name", v.name)
    jg.writeStringField("qualifiedName", v.qualifiedName)
    //jg.writeStringField("definitionName", v.definitionName)
    jg.writeStringField("description", commentPlus.findAllDescription(cfg.sources, v, htmlHelper.commentToHtml(v.comment).toString))
//    v.comment.foreach { x =>
//      val comments =
//      val sb = new StringBuilder
//      jg.writeFieldName("docTags")
//      jg.writeStartArray()
//      // TODO extracts tags
//      jg.writeEndArray()
//    }
    jg.writeStringField("flags", v.visibility.toString)
    v.deprecation.foreach { x => jg.writeStringField("deprecation", htmlHelper.bodyToHtml(x).toString) }
    writeFieldEntityList("inheritedFrom", v.inheritedFrom, jg)
    writeSplitStringWithRef(Some("visibility"), visibilityToSplitStringWithRef(v), jg)
    writeSplitStringWithRef(Some("resultType"), tentityToSplitStringWithRef(v.resultType), jg)
  }



  def writeDocTemplateEntityData(v: DocTemplateEntity, jg: JsonGenerator) {
    writeMemberEntityData(v, jg)
    try {
      v.inSource.foreach { s =>
        jg.writeArrayFieldStart("sourceStartPoint")
        jg.writeString(s._1.path)
        jg.writeNumber(s._2)
        jg.writeEndArray()
      }
    } catch {
      case e => logger.warn("failed to extract sourceStartPoint : %s <= %s", v.qualifiedName, e.getClass.getName + " : " + e.getMessage) //ignore
    }
    writeFieldEntityList("subClassesK", v.subClasses, jg)
    //writeFieldEntityList("members", v.members, jg)
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
    for (te <- v.linearization) {
      jg.writeString(uoaHelper.toRefPath(te))
    }
    jg.writeEndArray()
  }

  def writeTraitData(v: Trait, jg: JsonGenerator) {
    writeDocTemplateEntityData(v, jg)
    writeSplitStringWithRef(Some("typeParams"), tparamsToSplitStringWithRef(v.typeParams), jg)
  }

  def writeClassData(v: Class, jg: JsonGenerator) {
    writeTraitData(v, jg)
    jg.writeBooleanField("isCaseClass", v.isCaseClass)
    jg.writeArrayFieldStart("constructors")
    for (x <- v.constructors) {
      jg.writeStartObject()
      writeMemberEntityData(x, jg)
      writeSplitStringWithRef(Some("valueParams"), vparamsToSplitStringWithRef(v.valueParams), jg)
      jg.writeBooleanField("isPrimary", x.isPrimary)
      jg.writeStringField("kind", "def")
      jg.writeEndObject()
    }
    jg.writeEndArray()
  }

  def writeObjectData(v: Object, jg: JsonGenerator) {
    writeDocTemplateEntityData(v, jg)
  }

  def writePackageData(v: Package, jg: JsonGenerator) {
    writeObjectData(v, jg)
    writeFieldEntityList("packages", v.packages, jg)
  }

  def writeValData(v: Val, jg: JsonGenerator) {
    writeMemberEntityData(v, jg)
    jg.writeBooleanField("isUseCase", v.isUseCase)
  }

  def writeDefData(v: Def, jg: JsonGenerator) {
    writeMemberEntityData(v, jg)
    jg.writeBooleanField("isUseCase", v.isUseCase)
    writeSplitStringWithRef(Some("typeParams"), tparamsToSplitStringWithRef(v.typeParams), jg)
    writeSplitStringWithRef(Some("valueParams"), vparamsToSplitStringWithRef(v.valueParams), jg)
  }

  def writeAliasTypeData(v: AliasType, jg: JsonGenerator) {
    writeMemberEntityData(v, jg)
    jg.writeBooleanField("isUseCase", v.isUseCase)
    writeSplitStringWithRef(Some("alias"), tentityToSplitStringWithRef(v.alias), jg)
  }

  def writeAbstractTypeData(v: AbstractType, jg: JsonGenerator) {
    writeMemberEntityData(v, jg)
    jg.writeBooleanField("isUseCase", v.isUseCase)
    writeSplitStringWithRef(Some("bounds"), boundsToSplitStringWithRef(v.lo, v.hi), jg)
  }
}

