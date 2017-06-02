package tortoise

import org.bitbucket.inkytonik.kiama.output._

import CommonSyntax._
import FSSyntax._

object PrettyFS extends ParenPrettyPrinter {

  override val defaultIndent = 2

  sealed trait PrettyExpr extends PrettyExpression

  case class PAtom(doc: Doc) extends PrettyExpr

  case class PNot(expr: PrettyExpr) extends PrettyExpr with PrettyUnaryExpression {
    def fixity = Prefix
    def priority = 2
    def op = "!"
    def exp = expr
  }

  case class PNeg(expr: PrettyExpr) extends PrettyExpr with PrettyUnaryExpression {
    def fixity = Prefix
    def priority = 2
    def op = "-"
    def exp = expr
  }

  case class PFile(expr: PrettyExpr) extends PrettyExpr with PrettyUnaryExpression {
    def fixity = Prefix
    def priority = 2
    def op = "file?"
    def exp = expr
  }

  case class PDir(expr: PrettyExpr) extends PrettyExpr with PrettyUnaryExpression {
    def fixity = Prefix
    def priority = 2
    def op = "dir?"
    def exp = expr
  }

  case class PDefined(expr: PrettyExpr) extends PrettyExpr with PrettyUnaryExpression {
    def fixity = Prefix
    def priority = 2
    def op = "defined?"
    def exp = expr
  }

  case class PAnd(e1: PrettyExpr, e2: PrettyExpr) extends PrettyExpr with PrettyBinaryExpression {
    def fixity = Infix(LeftAssoc)
    def priority = 7
    def op = "&&"
    def left = e1
    def right = e2
  }

  case class POr(e1: PrettyExpr, e2: PrettyExpr) extends PrettyExpr with PrettyBinaryExpression {
    def fixity = Infix(LeftAssoc)
    def priority = 8
    def op = "||"
    def left = e1
    def right = e2
  }

  case class PEq(e1: PrettyExpr, e2: PrettyExpr) extends PrettyExpr with PrettyBinaryExpression {
    def fixity = Infix(LeftAssoc)
    def priority = 6
    def op = "=?"
    def left = e1
    def right = e2
  }

  case class PLt(e1: PrettyExpr, e2: PrettyExpr) extends PrettyExpr with PrettyBinaryExpression {
    def fixity = Infix(LeftAssoc)
    def priority = 5
    def op = "<"
    def left = e1
    def right = e2
  }

  case class PGt(e1: PrettyExpr, e2: PrettyExpr) extends PrettyExpr with PrettyBinaryExpression {
    def fixity = Infix(LeftAssoc)
    def priority = 5
    def op = ">"
    def left = e1
    def right = e2
  }

  case class PConcat(e1: PrettyExpr, e2: PrettyExpr) extends PrettyExpr with PrettyBinaryExpression {
    def fixity = Infix(LeftAssoc)
    def priority = 4
    def op = "+"
    def left = e1
    def right = e2
  }

  def convert(expr: Expr): PrettyExpr = expr match {
    case EUnOp(UNot, op) => PNot(convert(op))
    case EUnOp(UNeg, op) => PNeg(convert(op))
    case EUnOp(UFile, op) => PFile(convert(op))
    case EUnOp(UDir, op) => PDir(convert(op))
    case EUnOp(UDefined, op) => PDefined(convert(op))
    case EBinOp(BAnd, lhs, rhs) => PAnd(convert(lhs), convert(rhs))
    case EBinOp(BOr, lhs, rhs) => POr(convert(lhs), convert(rhs))
    case EBinOp(BEq, lhs, rhs) => PEq(convert(lhs), convert(rhs))
    case EBinOp(BLt, lhs, rhs) => PLt(convert(lhs), convert(rhs))
    case EBinOp(BGt, lhs, rhs) => PGt(convert(lhs), convert(rhs))
    case EBinOp(BConcat, lhs, rhs) => PConcat(convert(lhs), convert(rhs))
    case _ => PAtom(showExpr(expr))
  }

  override def toParenDoc(e : PrettyExpression): Doc = e match {
    case PAtom(doc) => doc
    case PFile(e) => "file?" <> parens(toParenDoc(e))
    case PDir(e) => "dir?" <> parens(toParenDoc(e))
    case PDefined(e) => "defined?" <> parens(toParenDoc(e))
    case e: PrettyExpr => super.toParenDoc(e)
  }

  def showExpr(expr: Expr): Doc = expr match {
    case EUndef => "undef"
    case EVar(id) => dollar <> id
    case EConst(CNum(n)) => value(n)
    case EConst(CStr(s)) => dquotes(s)
    case EConst(CBool(b)) => value(b)
    case EUnOp(_, _) | EBinOp(_, _, _) => toParenDoc(convert(expr))
  }

  def parindent(doc: Doc): Doc = lparen <@> indent(doc) <@> rparen

  def showStatement(stmt: Statement): Doc = stmt match {
    case SSkip => "skip"
    case SMkdir(path) => "mkdir" <> parens(showExpr(path))
    case SCreate(path, contents) => "create" <> parens(
      showExpr(path) <> comma <+> showExpr(contents)
    )
    case SRm(path) => "rm" <> parens(showExpr(path))
    case SCp(src, dst) => "cp" <> parens(showExpr(src) <> comma <+> showExpr(dst))
    case SChmod(path, mode) => "chmod" <> parens(
      showExpr(path) <> comma <+> showExpr(mode)
    )
    case SChown(path, owner) => "chown" <> parens(
      showExpr(path) <> comma <+> showExpr(owner)
    )
    case SSeq(lhs@SIf(_, _, _), rhs@SIf(_, _, _)) =>
      parindent(showStatement(lhs)) <> semi <@> parindent(showStatement(rhs))
    case SSeq(lhs@SIf(_, _, _), rhs) => parindent(showStatement(lhs)) <> semi <@> showStatement(rhs)
    case SSeq(lhs, rhs@SIf(_, _, _)) => showStatement(lhs) <> semi <@> parindent(showStatement(rhs))
    case SSeq(lhs, rhs) => showStatement(lhs) <> semi <@> showStatement(rhs)
    case SLet(id, expr, Some(idx), body) => "let" <+> dollar <> id <+> equal <+> showExpr(expr) <+>
        "or" <+> brackets(value(idx)) <@> "in" <+> showStatement(body)
    case SLet(id, expr, None, body) => "let" <+> dollar <> id <+> equal <+> showExpr(expr) <@>
        "in" <+> showStatement(body)
    case SIf(pred, cons, alt@SIf(_, _, _)) => "if" <+> showExpr(pred) <+> "then" <@>
        indent(showStatement(cons)) <@> "else" <+> showStatement(alt)
    case SIf(pred, cons, alt) => "if" <+> showExpr(pred) <+> "then" <@>
        indent(showStatement(cons)) <@> "else" <@> indent(showStatement(alt))
  }

  def prettyExpr(expr: Expr): String = super.pretty(showExpr(expr)).layout
  def prettyStatement(stmt: Statement): String = super.pretty(showStatement(stmt)).layout
}
