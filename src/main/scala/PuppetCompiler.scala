package pup

import pup.{FSSyntax => F, PuppetSyntax => P}
import pup.FSEmbeddedDSL._

/*
 * Assume that the manifest is deterministic and that the order in which resources appear is the
 * order in which they should be evaluated. We use this as an assumption to simplify compilation but
 * it should be fairly trivial given Rehearsal to convert manifests to this form.
 */
object PuppetCompiler {
  type ResEnv = Map[String, (P.Arguments, Seq[Int], P.Manifest)]

  var loc = 0
  def freshLoc(): Int = {
    loc += 1
    loc
  }

  def nLocs(n: Int): Seq[Int] = 0.to(n).map(_ => freshLoc())

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

  def compileExpr(expr: P.Expr): F.Expr = expr match {
    case P.EUndef => undef 
    case P.EVar(id) => $(id) 
    case P.EConst(c) => F.EConst(c)
    case P.EStrInterp(terms) => terms.map(compileExpr).reduce[F.Expr]({
      case (lhs, rhs) => lhs + rhs 
    })
    case P.EUnOp(op, operand) => F.EUnOp(compileUnOp(op), compileExpr(operand))
    case P.EBinOp(op, lhs, rhs) => F.EBinOp(compileBinOp(op), compileExpr(lhs), compileExpr(rhs))
  }

  def compileResource(
    typ: String, title: P.Expr, attrs: P.Attributes
  )(implicit renv: ResEnv): F.Statement = typ match {
    // Handle special case for file resources.
    case "file" => {
      val attrMap = attrs.flatMap(P.Attribute.unapply).toMap
      val path = attrMap.get("path").map(compileExpr).getOrElse(undef)
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
      val attrMap = attrs.flatMap(P.Attribute.unapply).toMap
      val (params, locs, body) = renv(typ)
      params.zip(locs).foldRight(compileManifest(body)._1) {
        case ((parameter, loc), body) => {
          val param = parameter.vari.id
          val arg = attrMap.get(param).map(compileExpr).getOrElse(undef)

          let (s"$param" := arg or loc) {
            body
          }
        }
      }
    }
  }

  def compileManifest(mani: P.Manifest)(implicit renv: ResEnv): (F.Statement, ResEnv) = mani match {
    case P.MEmpty => (skip, renv)
    case P.MResource(typ, title, attrs) => (compileResource(typ, title, attrs), renv)
    case P.MDefine(typ, args, body) => (skip, renv + (typ -> (args, nLocs(args.length), body)))
    case P.MSeq(lhs, rhs) => {
      val (c1, renv1) = compileManifest(lhs)
      val (c2, renv2) = compileManifest(rhs)(renv1)
      (c1 >> c2, renv2)
    }
    case P.MIf(pred, cons, alt) => {
      val cmd =
        _if (compileExpr(pred)) {
          compileManifest(cons)._1
        } _else {
          compileManifest(alt)._1
        }
      (cmd, renv)
    }
  }
}
