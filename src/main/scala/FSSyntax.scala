package pup

import CommonSyntax._
import PrettyFS._

object FSSyntax {
  sealed trait UnOp
  case object UNot extends UnOp
  case object UNeg extends UnOp
  case object UFile extends UnOp
  case object UDir extends UnOp
  case object UDefined extends UnOp

  sealed trait BinOp
  case object BAnd extends BinOp
  case object BOr extends BinOp
  case object BEq extends BinOp
  case object BLt extends BinOp
  case object BGt extends BinOp
  case object BConcat extends BinOp

  sealed trait Expr {
    lazy val pretty: String = prettyExpr(this)
  }

  case object EUndef extends Expr
  case class EVar(id: String) extends Expr
  case class EConst(const: Const) extends Expr
  case class EUnOp(op: UnOp, operand: Expr) extends Expr
  case class EBinOp(op: BinOp, lhs: Expr, rhs: Expr) extends Expr

  sealed trait Statement {
    lazy val pretty: String = prettyStatement(this)
    lazy val partialed: Statement = FSPartialEvaluator.eval(this)
  }

  case object SSkip extends Statement
  case class SMkdir(path: Expr) extends Statement
  case class SCreate(path: Expr, contents: Expr) extends Statement
  case class SRm(path: Expr) extends Statement
  case class SCp(src: Expr, dst: Expr) extends Statement
  case class SChmod(path: Expr, mode: Expr) extends Statement
  case class SSeq(lhs: Statement, rhs: Statement) extends Statement
  // Let statements are special in that they are optionally update sites via synthesis.
  case class SLet(id: String, expr: Expr, index: Option[Int], body: Statement) extends Statement
  case class SIf(pred: Expr, cons: Statement, alt: Statement) extends Statement
}
