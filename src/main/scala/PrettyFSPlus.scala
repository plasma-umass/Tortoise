package pup

import org.bitbucket.inkytonik.kiama.output.PrettyPrinter
import FSPlusSyntax._

object PrettyFSPlus {
  import pup.{FSPlusPretty => P}

  // Without location labels.
  def prettyProg(stmt: Statement): String = P.layout(P.prettyStmt(stmt)(false))
  def prettyValueConstraint(c: ValueConstraint): String = P.layout(P.prettyValueConstraint(c))

  // With location labels.
  def prettyStmt(stmt: Statement): String = P.layout(P.prettyStmt(stmt)(true))
  def prettyExpr(expr: Expr): String = P.layout(P.prettyExpr(expr)(true))
  def prettyConst(const: Const): String = P.layout(P.prettyConst(const)(true))
  def prettyPred(pred: Pred): String = P.layout(P.prettyPred(pred)(true))
}

private object FSPlusPretty extends PrettyPrinter {

  override val defaultIndent = 0

  def prettyStmt(stmt: Statement)(implicit showLoc: Boolean): Doc = stmt match {
    case SError => "error"
    case SSkip => "skip"
    case SLet(id, e, b@SLet(_, _, _)) => {
      "let" <+> id <+> equal <+> prettyExpr(e) <@> "in" <+> prettyStmt(b)
    }
    case SLet(id, e, b) => {
      "let" <+> id <+> equal <+> prettyExpr(e) <@> "in" <+> indent(prettyStmt(b))
    }
    case SIf(p, s1, s2) => {
      "if" <+> prettyPred(p) <@> "then" <+> indent(prettyStmt(s1)) <@> "else" <+>
      indent(prettyStmt(s2))
    }
    case SSeq(s1, s2) => prettyStmt(s1) <> semi <@> prettyStmt(s2)
    case SMkdir(p) => "mkdir" <> parens(prettyExpr(p))
    case SCreateFile(p, c) => "mkfile" <> parens(prettyExpr(p) <> comma <+> prettyExpr(c))
    case SRm(p) => "rm" <> parens(prettyExpr(p))
    case SCp(src, dst) => "cp" <> parens(prettyExpr(src) <> comma <+> prettyExpr(dst))
  }

  def prettyExpr(expr: Expr)(implicit showLoc: Boolean): Doc = expr match {
    case EId(id) => id
    case EPath(path) => prettyConst(path)
    case EString(str) => prettyConst(str)
    case EParent(e) => "parent" <> parens(prettyExpr(e))
    case EConcat(lhs, rhs) => prettyExpr(lhs) <+> "+" <+> prettyExpr(rhs)
    case EIf(p, e1, e2) => {
      "if" <+> prettyPred(p) <@> "then" <+> indent(prettyExpr(e1)) <@> "else" <+>
      indent(prettyExpr(e2))
    }
  }

  def prettyConst(const: Const)(implicit showLoc: Boolean): Doc = const match {
    case CPath(path, loc) if showLoc => angles(path.path.toString) <> brackets(loc.toString)
    case CPath(path, _) => angles(path.path.toString)
    case CString(str, loc) if showLoc => dquotes(str.toString) <> brackets(loc.toString)
    case CString(str, _) => dquotes(str.toString)
  }

  sealed trait PredCxt
  case object AndCxt extends PredCxt
  case object OrCxt extends PredCxt
  case object NotCxt extends PredCxt

  def prettyPred(pred: Pred)(implicit showLoc: Boolean): Doc = prettyPred(NotCxt, pred)

  def prettyPred(cxt: PredCxt, pred: Pred)(implicit showLoc: Boolean): Doc = pred match {
    case PTrue => "true"
    case PFalse => "false"
    case PTestFileState(p, st) => prettyFileState(st) <> question <> parens(prettyExpr(p))
    case PTestFileContains(p, cts) => "contains" <> question <> parens(
      prettyExpr(p) <> comma <+> prettyExpr(cts)
    )
    case PNot(prd@PTestFileState(_, _)) => exclamation <> prettyPred(NotCxt, prd)
    case PNot(prd@PTestFileContains(_, _)) => exclamation <> prettyPred(NotCxt, prd)
    case PNot(p) => exclamation <> parens(prettyPred(NotCxt, p))
    case PAnd(lhs, rhs) => {
      val (ls, rs) = (prettyPred(AndCxt, lhs), prettyPred(AndCxt, rhs))
      cxt match {
        case AndCxt | NotCxt => ls <+> "&&" <+> rs
        case OrCxt => parens(ls <+> "&&" <+> rs)
      }
    }
    case POr(lhs, rhs) => {
      val (ls, rs) = (prettyPred(OrCxt, lhs), prettyPred(OrCxt, rhs))
      cxt match {
        case OrCxt | NotCxt => ls <+> "||" <+> rs
        case AndCxt => parens(ls <+> "||" <+> rs)
      }
    }
  }

  def prettyFileState(st: FileState): Doc = st match {
    case IsFile(_) => "file"
    case IsDir => "dir"
    case DoesNotExist => "dne"
  }

  def prettyFileStateConstraint(st: FileState): Doc = st match {
    case IsFile(_) => "file"
    case IsDir => "dir"
    case DoesNotExist => "null"
  }

  def prettyValueConstraint(c: ValueConstraint): Doc = c match {
    case PathConstraint(p, st) => angles(p.toString) <+> "->" <+> prettyFileStateConstraint(st)
    case StringConstraint(p, cts) => angles(p.toString) <+> "=>" <+> cts
  }
}
