package pup

import pup.{FSSyntax => F, PuppetSyntax => P}

/*
 * Assume that the manifest is deterministic and that the order in which resources appear is the
 * order in which they should be evaluated. We use this as an assumption to simplify compilation but
 * it should be fairly trivial given Rehearsal to convert manifests to this form.
 */
object PuppetCompiler {
  import FSEmbeddedDSL._
  import PuppetCompilerInitialization._

  type IdEnv = Map[String, String]
  type ResEnv = Map[String, (P.Arguments, Seq[Int], P.Manifest)]
  type Envs = (IdEnv, ResEnv)

  var names: Map[String, Int] = Map()
  def freshName(str: String): String = {
    val num = names.getOrElse(str, 0) + 1
    names += (str -> num)
    s"$str!$num"
  }

  def compileUnOp(op: P.UnOp): F.UnOp = op match {
    case P.UNot => F.UNot
    case P.UNeg => F.UNeg
  }

  def compileBinOp(op: P.BinOp): F.BinOp = op match {
    case P.BAnd => F.BAnd
    case P.BOr => F.BOr
    case P.BEq => F.BEq
    case P.BLt => F.BLt
    case P.BGt => F.BGt
  }

  def compileExpr(expr: P.Expr)(implicit envs: Envs): F.Expr = expr match {
    case P.EUndef => undef
    case P.EVar(id) => envs._1.get(id) match {
      case Some(newId) => $(newId)
      case None => $(id)
    }
    case P.EConst(c) => F.EConst(c)
    case P.EStrInterp(terms) => terms.map(compileExpr).reduce[F.Expr]({
      case (lhs, rhs) => lhs +# rhs
    })
    case P.EUnOp(op, operand) => F.EUnOp(compileUnOp(op), compileExpr(operand))
    case P.EBinOp(op, lhs, rhs) => F.EBinOp(compileBinOp(op), compileExpr(lhs), compileExpr(rhs))
  }

  def compileResource(
    typ: String, title: P.Expr, attrs: P.Attributes
  )(implicit envs: Envs): F.Statement = typ match {
    // Handle special case for file resources.
    case "file" => {
      val attrMap = attrs.flatMap(P.Attribute.unapply).toMap
      val directPath = attrMap.get("path").map(compileExpr).getOrElse(undef)
      val path = if (directPath == undef) {
        compileExpr(title)
      } else {
        directPath
      }
      val mode = attrMap.get("mode").map(compileExpr).getOrElse(undef)
      val content = attrMap.get("content").map(compileExpr).getOrElse(undef)
      val owner = attrMap.get("owner").map(compileExpr).getOrElse(undef)
      val ensure = attrMap.get("ensure").map(compileExpr).getOrElse(undef)

      _if (ensure =? "present") {
        create(path, content)
      } .else_if (ensure =? "directory") {
        mkdir(path)
      } .else_if (ensure =? "absent") {
        rm(path)
      } >>
      _if (is_defined(mode)) {
        chmod(path, mode)
      } >>
      _if (is_defined(owner)) {
        chown(path, owner)
      }
    }

    // Handle special case for package resources.
    case "package" => {
      val attrMap = attrs.flatMap(P.Attribute.unapply).toMap
      val directName = attrMap.get("name").map(compileExpr).getOrElse(undef)
      val name = if (directName == undef) {
        compileExpr(title)
      } else {
        directName
      }
      val ensure = attrMap.get("ensure").map(compileExpr).getOrElse(undef)
      val provider = attrMap.get("provider").map(compileExpr).getOrElse(s("dpkg"))

      // TODO: call out to package server.
      _if (ensure =? "present") {
        create(s("/") +# provider +# s("/") +# name, "package installed")
      } .else_if (ensure =? "absent") {
        rm(s("/") +# provider +# s("/") +# name)
      }
    }

    // This case corresponds to applying a define type.
    case _ => {
      val renv = envs._2
      val (paramsOrig, locs, body) = renv(typ)

      // Map parameters into fresh names (because of multiple instantiations).
      val params = paramsOrig.map {
        case P.Argument(P.EVar(id), _) => freshName(id)
      }

      // Update the environment with the new name bindings.
      val updatedEnv = envs._1 ++ paramsOrig.map(_.vari.id).zip(params)

      // Build the default attribute mapping for the define type.
      val defaultAttrMap = paramsOrig.flatMap(P.Argument.unapply).flatMap {
        case (P.EVar(key), Some(value)) => Some(updatedEnv(key) -> value)
        case (_, None) => None
      }.toMap

      // Build the attribute mapping using the updated name environment.
      val attrMap = defaultAttrMap ++ attrs.flatMap(P.Attribute.unapply).map {
        case (key, value) => updatedEnv(key) -> value
      }.toMap

      // Build the FS++ expression by adding a series of let bindings around the original body.
      params.zip(locs).foldRight(compileManifest(body)(updatedEnv -> envs._2)._1) {
        case ((param, loc), body) => {
          val arg = attrMap.get(param).map(compileExpr(_)(updatedEnv -> envs._2)).getOrElse(undef)

          let (s"$param" := arg or loc) {
            body
          }
        }
      }
    }
  }

  def compileManifest(mani: P.Manifest)(implicit envs: Envs): (F.Statement, ResEnv) = mani match {
    case P.MEmpty => (skip, envs._2)
    case P.MAssign(id, value, body) => {
      val cmd =
        let (s"$id" := compileExpr(value) or mani.label) {
          compileManifest(body)._1
        }
      (cmd, envs._2)
    }
    case P.MResource(typ, title, attrs) => (compileResource(typ, title, attrs), envs._2)
    case P.MDefine(typ, args, body) => (skip, envs._2 + (typ -> (args, mani.labels, body)))
    case P.MSeq(lhs, rhs) => {
      val (c1, renv1) = compileManifest(lhs)
      val (c2, renv2) = compileManifest(rhs)(envs._1 -> renv1)
      (c1 >> c2, renv2)
    }
    case P.MIf(pred, cons, alt) => {
      val cmd =
        _if (compileExpr(pred)) {
          compileManifest(cons)._1
        } _else {
          compileManifest(alt)._1
        }
      (cmd, envs._2)
    }
  }

  def compile(mani: P.Manifest): F.Statement = compileManifest(mani)(Map() -> initialResEnv)._1
}

object PuppetCompilerInitialization {
  import PuppetCompiler._
  import PuppetEmbeddedDSL._
  import PuppetLabeler._

  lazy val initialResEnv: ResEnv = compileManifest {
    label {
      define ("user") ("name", "managehome") {
        resource("file")(s("/etc/users/", $("name")),
          ensure ~> present
        ) >>
        _if ($("managehome") /= undef) {
          resource("file")(s("/home/", $("name")),
            ensure ~> directory
          )
        }
      }
    }
  }(Map() -> Map())._2
}
