package rehearsal

import org.bitbucket.inkytonik.kiama.output.PrettyPrinter
import FSPlusTrace._
import FSPlusSyntax.{FileState, IsFile, IsDir, DoesNotExist}

object PrettyFSPlusTrace {
  import rehearsal.{FSPlusTracePretty => P}

  def prettyStmt(stmt: Statement): String = P.layout(P.prettyStmt(stmt))
  def prettyExpr(expr: Expr): String = P.layout(P.prettyExpr(expr))
  def prettyPred(pred: Pred): String = P.layout(P.prettyPred(pred))
  def prettyConst(const: Const): String = P.layout(P.prettyConst(const))
}

private object FSPlusTracePretty extends PrettyPrinter {

  override val defaultIndent = 0

  def prettyStmt(stmt: Statement): Doc = stmt match {
    case SError => "error"
    case SSkip => "skip"
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

  def prettyExpr(expr: Expr): Doc = expr match {
    case EHole(_, loc, _) => "\u2022" <> brackets(loc.toString)
    case EParent(e) => "parent" <> parens(prettyExpr(e))
    case EConcat(lhs, rhs) => prettyExpr(lhs) <+> "+" <+> prettyExpr(rhs)
    case EIf(p, e1, e2) => {
      "if" <+> prettyPred(p) <@> "then" <+> indent(prettyExpr(e1)) <@> "else" <+>
      indent(prettyExpr(e2))
    }
  }

  def prettyConst(const: Const): Doc = const match {
    case CPath(path) => angles(path.path.toString)
    case CString(str) => dquotes(str.toString)
  }

  sealed trait PredCxt
  case object AndCxt extends PredCxt
  case object OrCxt extends PredCxt
  case object NotCxt extends PredCxt

  def prettyPred(pred: Pred): Doc = prettyPred(NotCxt, pred)

  def prettyPred(cxt: PredCxt, pred: Pred): Doc = pred match {
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
}
