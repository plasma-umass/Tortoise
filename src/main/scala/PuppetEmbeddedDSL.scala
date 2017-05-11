package pup

import scala.language.implicitConversions

import CommonSyntax._
import PuppetSyntax._

object PuppetEmbeddedDSL {
  lazy val empty = MEmpty

  // Define types
  def define(typ: String)(args: (String, Option[Expr])*)(body: Manifest): Manifest = MDefine(
    typ, args.map(pair => Argument(EVar(pair._1), pair._2)), body
  )

  implicit def stringToArgPair(string: String): (String, Option[Expr]) = (string, None)

  implicit class RichStr(str: String) {
    def :=(expr: Expr): (String, Option[Expr]) = (str, Some(expr))
  }

  // Resources
  def resource(typ: String)(title: Expr, attrs: Attribute*): Manifest = MResource(typ, title, attrs)
  implicit class RichString(str: String) {
    def ~>(expr: Expr): Attribute = Attribute(str, expr)
  }

  // Sequencing
  implicit class RichManifest(mani: Manifest) {
    def >>(other: Manifest): Manifest = (mani, other) match {
      case (MEmpty, _) => other
      case (_, MEmpty) => mani
      case _ => MSeq(mani, other)
    }
  }

  // If statements
  def _if(pred: Expr)(cons: Manifest): MIf = MIf(pred, cons, MEmpty)

  implicit class RichMIf(_if: MIf) {
    def _else(alt: Manifest): MIf = _if match {
      case MIf(pred, cons, MEmpty) => MIf(pred, cons, alt)
      case MIf(pred1, cons1, if2@MIf(_, _, _)) => MIf(pred1, cons1, if2._else(alt))
      case _ => throw MisusedEDSL(_if)
    }

    def else_if(pred: Expr)(body: Manifest): MIf = _if match {
      case MIf(pred1, cons1, MEmpty) => MIf(pred1, cons1, MIf(pred, body, empty))
      case MIf(pred1, cons1, if2@MIf(_, _, _)) => MIf(pred1, cons1, if2.else_if(pred)(body))
      case _ => throw MisusedEDSL(_if)
    }
  }

  // Common keywords
  lazy val ensure = "ensure"
  lazy val content = "content"
  lazy val present = "present"
  lazy val directory = "directory"

  // Expressions
  lazy val undef = EUndef

  def $(str: String): EVar = EVar(str)
  def s(exprs: Expr*): EStrInterp = EStrInterp(exprs)

  implicit def stringToExpr(str: String): Expr = EConst(CStr(str))
  implicit def booleanToExpr(bool: Boolean): Expr = EConst(CBool(bool))
  implicit def intToExpr(int: Int): Expr = EConst(CNum(int))

  implicit class RichExpr(lhs: Expr) {
    def unary_!(expr: Expr): Expr = EUnOp(UNot, expr)
    def unary_-(expr: Expr): Expr = EUnOp(UNeg, expr)

    def &&(rhs: Expr): Expr = EBinOp(BAnd, lhs, rhs)
    def ||(rhs: Expr): Expr = EBinOp(BOr, lhs, rhs)
    def =?(rhs: Expr): Expr = EBinOp(BEq, lhs, rhs)
    def <(rhs: Expr): Expr = EBinOp(BLt, lhs, rhs)
    def >(rhs: Expr): Expr = EBinOp(BGt, lhs, rhs)

    // Special operators
    def <=(rhs: Expr): Expr = lhs =? rhs || lhs < rhs
    def >=(rhs: Expr): Expr = lhs =? rhs || lhs > rhs
    def /=(rhs: Expr): Expr = unary_!(lhs =? rhs)
  }
}
