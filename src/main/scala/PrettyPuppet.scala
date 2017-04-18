package pup

import org.bitbucket.inkytonik.kiama.output._

import CommonSyntax._
import PuppetSyntax._

object PrettyPuppet extends ParenPrettyPrinter {
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

  def convert(expr: Expr): PrettyExpr = expr match {
    case EUnOp(UNot, op) => PNot(convert(op))
    case EUnOp(UNeg, op) => PNeg(convert(op))
    case EBinOp(BAnd, lhs, rhs) => PAnd(convert(lhs), convert(rhs))
    case EBinOp(BOr, lhs, rhs) => POr(convert(lhs), convert(rhs))
    case EBinOp(BEq, lhs, rhs) => PEq(convert(lhs), convert(rhs))
    case EBinOp(BLt, lhs, rhs) => PLt(convert(lhs), convert(rhs))
    case EBinOp(BGt, lhs, rhs) => PGt(convert(lhs), convert(rhs))
    case _ => PAtom(showExpr(expr))
  }

  override def toParenDoc(e : PrettyExpression): Doc = e match {
    case PAtom(doc) => doc
    case e: PrettyExpr => super.toParenDoc(e)
  }

  val knownKeywords = Set(
   "present", "absent", "directory"
  )

  def showExpr(expr: Expr): Doc = expr match {
    case EUndef => "undef"
    case EVar(id) => dollar <> id
    case EConst(CNum(n)) => value(n)
    case EConst(CStr(s)) if knownKeywords contains s => s
    case EConst(CStr(s)) => dquotes(s)
    case EConst(CBool(b)) => value(b)
    case EStrInterp(terms) => {
      // Kiama only likes explicitly immutable Seqs :(
      val docs = scala.collection.immutable.Seq(terms.map(showExpr): _*)
      dquotes(hcat(docs))
    }
    case EUnOp(_, _) | EBinOp(_, _, _) => toParenDoc(convert(expr))
  }

  def showAttribute(attr: Attribute): Doc = attr match {
    case Attribute(name, value) => name <+> equal <> rangle <+> showExpr(value)
  }

  def showAttributes(attrs: Attributes): Doc = {
    val docs = scala.collection.immutable.Seq(attrs.map(showAttribute): _*)
    vsep(docs, comma)
  }

  def showArgument(arg: Argument): Doc = arg match {
    case Argument(vari, Some(default)) => showExpr(vari) <+> equal <+> showExpr(default)
    case Argument(vari, None) => showExpr(vari)
  }

  def showArguments(args: Arguments): Doc = {
    val docs = scala.collection.immutable.Seq(args.map(showArgument): _*)
    hsep(docs, comma)
  }

  def scope(doc: Doc): Doc =
    lbrace <@> indent(doc) <@> rbrace

  def showManifest(mani: Manifest): Doc = mani match {
    case MEmpty => emptyDoc
    case MAssign(id, expr, body) => dollar <> id <+> equal <+> showExpr(expr) <@> showManifest(body)
    case MResource(typ, EUndef, Seq(attr)) => typ <+> lbrace <+> showAttribute(attr) <+> rbrace
    case MResource(typ, EUndef, attrs) => typ <+> scope(showAttributes(attrs))
    case MResource(typ, title, attrs) => {
      typ <+> lbrace <> showExpr(title) <> colon <@> indent(showAttributes(attrs)) <@> rbrace
    }
    case MDefine(typ, args, body) => {
      "define" <+> typ <> parens(showArguments(args)) <+> scope(showManifest(body))
    }
    case MSeq(lhs, rhs) => showManifest(lhs) <@> showManifest(rhs)
    case MIf(pred, cons, alt) => {
      "if" <+> parens(showExpr(pred)) <+> scope(showManifest(cons)) <+> "else" <+>
        scope(showManifest(alt))
    }
  }

  def prettyExpr(expr: Expr): String = super.pretty(showExpr(expr)).layout
  def prettyManifest(mani: Manifest): String = super.pretty(showManifest(mani)).layout
}
