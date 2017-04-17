package pup

import pup.{FSSyntax => F, PuppetSyntax => P}
import pup.FSEmbeddedDSL._

/*
 * Assume that the manifest is deterministic and that the order in which resources appear is the
 * order in which they should be evaluated. We use this as an assumption to simplify compilation but
 * it should be fairly trivial given Rehearsal to convert manifests to this form.
 */
object PuppetCompiler {
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
      case (lhs, rhs) => lhs + rhs
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
      val contents = attrMap.get("contents").map(compileExpr).getOrElse(undef)
      val ensure = attrMap.get("ensure").map(compileExpr).getOrElse(undef)

      _if (ensure =? "present") {
        create(path, contents)
      } .else_if (ensure =? "directory") {
        mkdir(path)
      } .else_if (ensure =? "absent") {
        rm(path)
      } >>
      _if (is_defined(mode)) {
        chmod(path, mode)
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

      // Build the attribute mapping using the updated name environment.
      val attrMap = attrs.flatMap(P.Attribute.unapply).map {
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
}
