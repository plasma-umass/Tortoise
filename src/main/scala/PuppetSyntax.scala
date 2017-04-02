package pup

object PuppetSyntax {
  // Attributes for resource instantiations.
  case class Attribute(name: String, value: Expr)
  type Attributes = Seq[Attribute]
  // Arguments to define types.
  // NOTE: we may wish to include types later.
  case class Argument(vari: EVar, default: Option[Expr])
  type Arguments = Seq[Argument]

  sealed trait Const
  case class CStr(str: String) extends Const
  case class CNum(n: Int) extends Const
  case class CBool(bool: Boolean) extends Const

  sealed trait UnOp
  case object UNot extends UnOp
  case object UNeg extends UnOp

  sealed trait BinOp
  case object BAnd extends BinOp
  case object BOr extends BinOp
  case object BEq extends BinOp
  case object BLt extends BinOp
  case object BGt extends BinOp

  sealed trait Expr
  type Exprs = Seq[Expr]
  case object EUndef extends Expr
  case class EVar(id: String, loc: Int) extends Expr
  case class EConst(c: Const, loc: Int) extends Expr
  case class ERef(typ: String, title: Expr) extends Expr
  case class EStrInterp(terms: Exprs) extends Expr
  case class EUnOp(op: UnOp, operand: Expr) extends Expr
  case class EBinOp(op: BinOp, lhs: Expr, rhs: Expr) extends Expr

  sealed trait Manifest
  case object MEmpty extends Manifest
  case class MResource(typ: String, title: Expr, attrs: Attributes) extends Manifest
  case class MDefine(typ: String, args: Arguments, body: Manifest) extends Manifest
  case class MSeq(lhs: Manifest, rhs: Manifest) extends Manifest
  case class MIf(pred: Expr, cons: Manifest, alt: Manifest) extends Manifest
}
