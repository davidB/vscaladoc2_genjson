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

import scala.tools.nsc.doc.model.DocTemplateEntity
import scala.tools.nsc.doc.model.NoDocTemplate
import scala.tools.nsc.doc.model.Entity
import scala.tools.nsc.doc.model.MemberEntity
import scala.tools.nsc.doc.model.TemplateEntity
import scala.tools.nsc.doc.model.Package
import scala.reflect.NameTransformer

case class UriOfApi(val artifactId: String, val version: String, val packageName: String, val typeName: Option[String] = None, val memberName: Option[String] = None)

class UriOfApiHelper(val cfg: Cfg) {
  def encode(v: String) = NameTransformer.encode(v).replace("$u002E", ".")

  private val _currentArtifact = (Some(cfg.artifactId), Some(cfg.version))
  private val _cache = new scala.collection.mutable.HashMap[Entity, UriOfApi]()
  private lazy val _scalaLib = cfg.dependencies.find(x => x.artifactId == "scala-library" || x.file.getName.startsWith("scala-library"))
  private val _jseAV = (Some("jse"), Some(System.getProperty("java.version")))

  def toRefPath(uoa : UriOfApi): String = {
    uoa.artifactId + "/" + uoa.version + "/" +
    uoa.packageName +
    uoa.typeName.map(x => "/" + encode(x)).getOrElse("") +
    uoa.memberName.map(x => "/" + encode(x)).getOrElse("")
  }

  def toRefPath[T <: Entity](v: T): String = toRefPath(apply(v))
  
  def findTypeEntity[T <: Entity](v: T): Option[TemplateEntity] = v match {
    case t: TemplateEntity if t.isPackage => None
    case t: TemplateEntity => Some(t)
    case m: MemberEntity => findTypeEntity(m.inDefinitionTemplates.headOption.getOrElse(m.inTemplate))
    case m => findTypeEntity(m.inTemplate)
  }

  // TODO test with type in root/default package
  def findPackageEntity[T <: Entity](v: Entity): Option[Package] = v match {
    case x: Package => Some(x)
    case x: TemplateEntity if x.isRootPackage => None
    case m: MemberEntity => findPackageEntity(m.inDefinitionTemplates.headOption.getOrElse(m.inTemplate))
    case _ => findPackageEntity(v.inTemplate)
  }

  def apply[T <: Entity](v: T): UriOfApi = {
    def findOrCreate(e: Entity) = {
      _cache.getOrElseUpdate(e, {
        var packageName: Option[String] = None
        var typeName: Option[String] = None
        var memberName: Option[String] = None

        def findArtifactVersion(te: TemplateEntity): (Option[String], Option[String]) = te match {
          case t: NoDocTemplate => {
            cfg.dependencies.find(_.contains(packageName, typeName)).map { x =>
              (Some(x.artifactId), Some(x.version))
            } orElse {
              // for performance should be done first, but several artifact (have scala.* package) : scala-compiler,...  
              for (p <- packageName; if (p + ".").startsWith("scala."); dep <- _scalaLib) yield (Some(dep.artifactId), Some(dep.version))
            } orElse {
              for (p <- packageName; if p.startsWith("java.") || p.startsWith("javax.")) yield _jseAV
            } getOrElse {
              (None, None)
            }
          }
          case t: DocTemplateEntity => _currentArtifact
        }

        val typeEntity = findTypeEntity(v)
        val packageEntity = findPackageEntity(v)

        packageName = packageEntity.map(_.qualifiedName)
        typeName = typeEntity.map { t =>
          t.qualifiedName.substring(packageName.map(_.length + 1).getOrElse(0)) //+ if (t.isObject) "$object" else "")
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
    //      def declaredEntity(v : T) = v match {
    //        case m : MemberEntity if !m.inheritedFrom.isEmpty => m.inheritedFrom.head.members.find( _.name == m.name ).getOrElse(m)
    //        case x => x
    //      }
    findOrCreate(v)
  }
}
