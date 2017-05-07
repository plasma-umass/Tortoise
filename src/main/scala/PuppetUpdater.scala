package pup

import CommonSyntax._
import PuppetSyntax._
import SymbolicFS.Substitution

/**
  * This object contains the implementation of update application for Puppet manifests. After a
  * substitution has been produced from the synthesis procedure, we need to transform the original
  * manifest AST into an updated form to be pretty-printed as the final result. In the simplest
  * case, this transformation is obvious (if the variable is being assigned a constant string), but
  * in other cases, this can be a lot trickier (for example, in the case of string interpolation).
  */
object PuppetUpdater {
  type LocEnv = Map[String, Int]
  case class UpdateContext(env: LocEnv, subst: Substitution) {
    def apply(str: String): String = subst(env(str))
    def contains(str: String): Boolean = env.contains(str) && subst.contains(env(str))

    def +(pair: (String, Int)): UpdateContext = UpdateContext(env + pair, subst)
    def ++(pairs: Seq[(String, Int)]): UpdateContext = UpdateContext(env ++ pairs, subst)
  }

  // Replaces an expression immediately based on a string (used for let bindings).
  def replaceExpr(expr: Expr, value: String): Expr = expr match {
    case EUndef | EConst(CStr(_)) | EVar(_) => EConst(CStr(value))
    case EStrInterp(_) => EConst(CStr(value))
    case EConst(_) | EUnOp(_, _) | EBinOp(_, _, _) => throw UpdateError(expr)
  }

  // Updates an expression under the specified update context (used for resource instantiations).
  def updateExpr(expr: Expr)(implicit cxt: UpdateContext): Expr = expr match {
    case EUndef | EConst(_) => expr
    case EVar(id) if cxt.contains(id) => EConst(CStr(cxt(id)))
    case EVar(_) => expr
    case EStrInterp(terms) => EStrInterp(terms.map(updateExpr))
    case EUnOp(op, operand) => EUnOp(op, updateExpr(operand))
    case EBinOp(op, lhs, rhs) => EBinOp(op, updateExpr(lhs), updateExpr(rhs))
  }

  def updateAttribute(attr: Attribute)(implicit cxt: UpdateContext): Attribute = attr match {
    case Attribute(name, value) => Attribute(name, updateExpr(value))
  }

  def updateAttributes(attrs: Attributes)(implicit cxt: UpdateContext): Attributes = {
    attrs.map(updateAttribute)
  }

  def updateManifest(mani: Manifest)(implicit cxt: UpdateContext): Manifest = mani match {
    case MEmpty => MEmpty
    case MAssign(id, expr, body) => cxt.subst.get(mani.label) match {
      case Some(replacement) => {
        MAssign(id, replaceExpr(expr, replacement), updateManifest(body)).setLabel(mani.label)
      }
      case None => MAssign(id, expr, updateManifest(body)).setLabel(mani.label)
    }
    case MResource(typ, title, attrs) => MResource(typ, updateExpr(title), updateAttributes(attrs))
    case MDefine(typ, args, body) => MDefine(
      typ, args, updateManifest(body)(cxt ++ args.map(_.vari.id).zip(mani.labels))
    ).setLabels(mani.labels)
    case MSeq(lhs, rhs) => MSeq(updateManifest(lhs), updateManifest(rhs))
    case MIf(pred, cons, alt) => MIf(pred, updateManifest(cons), updateManifest(alt))
  }

  def update(mani: Manifest, subst: Substitution): Manifest ={
    updateManifest(mani)(UpdateContext(Map(), subst))
  }
}
