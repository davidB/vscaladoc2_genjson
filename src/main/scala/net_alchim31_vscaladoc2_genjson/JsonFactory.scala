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
class JsonFactory(val cfg : Cfg, val universe : Universe) {

  private val _encoding : String = "UTF-8"

  /*universe.settings.outdir.value*/
  private val _siteRoot : JFile = cfg.apidocdir

  private val _jacksonFactory = new org.codehaus.jackson.JsonFactory()

  /**
   * @param model The model to generate in the form of a sequence of packages.
   */
  def generate() : Unit = {
    import scala.collection.mutable

    def writeTemplate(tpl : DocTemplateEntity, written : mutable.HashSet[DocTemplateEntity]) : Unit = {
      if (!(written contains tpl)) {
        write(tpl)
        written += tpl
        tpl.templates map (writeTemplate(_, written))
      }
    }
    println("generate to " + _siteRoot)
    writeTemplate(universe.rootPackage, mutable.HashSet.empty[DocTemplateEntity])
  }

  //  def findPath[T <: Entity](v : T) = findPath(v, v.qualifiedName) 
  //
  //  def findPath[T <: Entity](v : T, qualifiedName : String) : List[String] = {
  //    v match {
  //        case x : TemplateEntity => List(x.qualifiedName, qualifiedName.substring(x.qualifiedName.length + 1))
  //        case _ => findPackageName(v.inTemplate, qualifiedName)
  //    }
  //  }

  case class UriOfApi(val artifactId : String, val version : String, val packageName : String, val typeName : Option[String] = None, val memberName : Option[String] = None) {
    def toRefPath : String = {
      artifactId + "/" + version + "/" +
        packageName +
        typeName.map(x => "/" + UriOfApi.encode(x)).getOrElse("") +
        memberName.map(x => "/" + UriOfApi.encode(x)).getOrElse("") +
        ".json"
    }
  }

  object UriOfApi {
    def encode(v : String) = NameTransformer.encode(v).replace("$u002E", ".")

    private val _currentArtifact = (Some(cfg.artifactId), Some(cfg.version))
    private val _cache = new scala.collection.mutable.HashMap[Entity, UriOfApi]()

    def findTypeEntity[T <: Entity](v : T) : Option[TemplateEntity] = v match {
      case t : TemplateEntity if t.isPackage => None
      case t : TemplateEntity => Some(t)
      case m => findTypeEntity(m.inTemplate)
    }

    // TODO test with type in root/default package
    def findPackageEntity[T <: Entity](v : Entity) : Option[Package] = v match {
      case x : Package => Some(x)
      case x : TemplateEntity if x.isRootPackage => None
      case _ => findPackageEntity(v.inTemplate)
    }

    def apply[T <: Entity](v : T) : UriOfApi = _cache.getOrElseUpdate(v, {
      var packageName : Option[String] = None
      var typeName : Option[String] = None
      var memberName : Option[String] = None

      def findArtifactVersion(te : TemplateEntity) : (Option[String], Option[String]) = te match {
        case t : NoDocTemplate => {
          cfg.dependencies.find(_.contains(packageName, typeName)).map { x =>
            (Some(x.artifactId), Some(x.version))
          } orElse {
            for (p <- packageName; if p.startsWith("java.") || p.startsWith("javax.")) yield (Some("jse"), Some(System.getProperty("java.version")))
          } getOrElse {
            (None, None)
          }
        }
        case t : DocTemplateEntity => _currentArtifact
      }

      val typeEntity = findTypeEntity(v)
      val packageEntity = findPackageEntity(v)

      packageName = packageEntity.map(_.qualifiedName)
      typeName = typeEntity.map { t =>
        t.qualifiedName.substring(packageName.map(_.length + 1).getOrElse(0)) +
          (if (t.isObject) "$object" else "")
      }
      memberName = typeEntity.flatMap { t =>
        (v.qualifiedName.length > t.qualifiedName.length) match {
          case true => Some(v.qualifiedName.substring(t.qualifiedName.length + 1))
          case false => None
        }
      }
      val (artifactId, version) = typeEntity.orElse(packageEntity).map(t => findArtifactVersion(t)).getOrElse((None, None))
      UriOfApi(
        artifactId.getOrElse("undef"),
        version.getOrElse("0.0.0"),
        packageName.getOrElse("_root_"),
        typeName,
        memberName)
    })
  }

  //  def findPackageName[T <: TemplateEntity](v : T) : List[String] = findPackageName(v, v.qualifiedName)
  //
  //  def findPackageName[T <: TemplateEntity](v : T, qualifiedName : String) : List[String] = {
  //    v match {
  //      case x if x.isRootPackage => List(qualifiedName)
  //      case x : Package => List(x.qualifiedName, qualifiedName.substring(x.qualifiedName.length + 1))
  //      case _ => findPackageName(v.inTemplate, qualifiedName)
  //    }
  //  }

  def write[T <: DocTemplateEntity](v : T) {

    val uoa = UriOfApi(v)
    val f = new JFile(_siteRoot, uoa.toRefPath)
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

  def writeDocTemplateEntityData(v : DocTemplateEntity, jg : JsonGenerator) {
    jg.writeStringField("name", v.name)
    jg.writeStringField("qualifiedName", v.qualifiedName)
    jg.writeStringField("uoa", UriOfApi(v).toRefPath)
    jg.writeStringField("kind", if (v.isTrait) "trait" else if (v.isClass) "class" else if (v.isObject) "object" else if (v.isPackage) "package" else "undef")
    //      jg.writeBooleanField("isCaseClass", v.isCaseClass)
  }

  def writeTraitData(v : Trait, jg : JsonGenerator) {
    writeDocTemplateEntityData(v, jg)
  }

  def writeClassData(v : Class, jg : JsonGenerator) {
    writeTraitData(v, jg)
  }

  def writeObjectData(v : Object, jg : JsonGenerator) {
    writeDocTemplateEntityData(v, jg)
  }

  def writePackageData(v : Package, jg : JsonGenerator) {
    writeObjectData(v, jg)
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
