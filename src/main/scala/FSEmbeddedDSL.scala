package pup

import scala.language.implicitConversions

import CommonSyntax._
import FSSyntax._

object FSEmbeddedDSL {
  // Atomic statements
  lazy val skip = SSkip
  def mkdir(path: Expr): Statement = SMkdir(path)
  def create(path: Expr, contents: Expr): Statement = SCreate(path, contents)
  def rm(path: Expr): Statement = SRm(path)
  def cp(src: Expr, dst: Expr): Statement = SCp(src, dst)
  def chmod(path: Expr, mode: Expr): Statement = SChmod(path, mode)
  def chown(path: Expr, owner: Expr): Statement = SChown(path, owner)

  // Variables
  def $(str: String): EVar = EVar(str)

  // Strings
  def s(str: String): Expr = EConst(CStr(str))

  // Sequencing
  implicit class RichStatement(stmt: Statement) {
    def >>(other: Statement): Statement = (stmt, other) match {
      case (SSkip, _) => other
      case (_, SSkip) => stmt
      case _ => SSeq(stmt, other)
    }
  }

  // Let bindings
  type LetTuple = (String, Expr, Option[Int])
  implicit class RichString(id: String) {
    def :=(tup: (Expr, Int)): LetTuple = (id, tup._1, Some(tup._2))
    def :=(expr: Expr): LetTuple = (id, expr, None)
  }

  def let(tup: LetTuple)(body: Statement): Statement = tup match {
    case (id, expr, index) => SLet(id, expr, index, body)
  }

  // If statements
  def _if(pred: Expr)(cons: Statement): SIf = SIf(pred, cons, SSkip)

  implicit class RichSIf(_if: SIf) {
    def _else(alt: Statement): SIf = _if match {
      case SIf(pred, cons, SSkip) => SIf(pred, cons, alt)
      case SIf(pred1, cons1, if2@SIf(_, _, _)) => SIf(pred1, cons1, if2._else(alt))
      case _ => throw MisusedEDSL(_if)
    }

    def else_if(pred: Expr)(body: Statement): SIf = _if match {
      case SIf(pred1, cons1, SSkip) => SIf(pred1, cons1, SIf(pred, body, skip))
      case SIf(pred1, cons1, if2@SIf(_, _, _)) => SIf(pred1, cons1, if2.else_if(pred)(body))
      case _ => throw MisusedEDSL(_if)
    }
  }

  // Expressions
  lazy val undef = EUndef

  implicit def stringToExpr(str: String): Expr = EConst(CStr(str))
  implicit def booleanToExpr(bool: Boolean): Expr = EConst(CBool(bool))
  implicit def intToExpr(int: Int): Expr = EConst(CNum(int))

  def is_file(expr: Expr): Expr = EUnOp(UFile, expr)
  def is_dir(expr: Expr): Expr = EUnOp(UDir, expr)
  def is_defined(expr: Expr): Expr = EUnOp(UDefined, expr)

  implicit class RichExpr(lhs: Expr) {
    // Used for let syntax.
    def or(n: Int): (Expr, Int) = (lhs, n)

    def unary_!(expr: Expr): Expr = EUnOp(UNot, expr)
    def unary_-(expr: Expr): Expr = EUnOp(UNeg, expr)

    def &&(rhs: Expr): Expr = EBinOp(BAnd, lhs, rhs)
    def ||(rhs: Expr): Expr = EBinOp(BOr, lhs, rhs)
    def =?(rhs: Expr): Expr = EBinOp(BEq, lhs, rhs)
    def <(rhs: Expr): Expr = EBinOp(BLt, lhs, rhs)
    def >(rhs: Expr): Expr = EBinOp(BGt, lhs, rhs)
    def +#(rhs: Expr): Expr = EBinOp(BConcat, lhs, rhs)

    // Special operators
    def <=(rhs: Expr): Expr = lhs =? rhs || lhs < rhs
    def >=(rhs: Expr): Expr = lhs =? rhs || lhs > rhs
    def !=(rhs: Expr): Expr = unary_!(lhs =? rhs)
  }
}
