package rehearsal

import FSPlusSyntax.{FileState, DoesNotExist, IsDir, IsFile}
import FSPlusTrace._

private[rehearsal] object PrettyFSPlusTrace {
  def prettyStmt(stmt: Statement): String = stmt match {
    case SError => "error"
    case SSkip => "skip"
    case SIf(p, s1, s2) => s"if ${prettyPred(p)} then ${prettyStmt(s1)} else ${prettyStmt(s2)}"
    case SSeq(s1, s2) => s"${prettyStmt(s1)}; ${prettyStmt(s2)}"
    case SMkdir(p) => s"mkdir(${prettyExpr(p)})"
    case SCreateFile(p, c) => s"mkfile(${prettyExpr(p)}, ${prettyExpr(c)})"
    case SRm(p) => s"rm(${prettyExpr(p)})"
    case SCp(src, dst) => s"cp(${prettyExpr(src)}, ${prettyExpr(dst)})"
  }

  sealed trait ExprCxt
  case object ConcatCxt extends ExprCxt
  case object DefaultCxt extends ExprCxt

  def prettyExpr(expr: Expr): String = prettyExpr(DefaultCxt, expr)

  def prettyExpr(cxt: ExprCxt, expr: Expr): String = expr match {
    case EHole(_, loc) => s"\u2022[$loc]"
    case EParent(e) => s"parent(${prettyExpr(e)})"
    case EConcat(lhs, rhs) => cxt match {
      case ConcatCxt => s"(${prettyExpr(ConcatCxt, lhs)} + ${prettyExpr(ConcatCxt, rhs)})"
      case DefaultCxt => s"${prettyExpr(ConcatCxt, lhs)} + ${prettyExpr(ConcatCxt, rhs)}"
    }
    case EIf(p, e1, e2) => s"if ${prettyPred(p)} then ${prettyExpr(e1)} else ${prettyExpr(e2)}"
  }

  sealed trait PredCxt
  case object AndCxt extends PredCxt
  case object OrCxt extends PredCxt
  case object NotCxt extends PredCxt

  def prettyPred(pred: Pred): String = prettyPred(NotCxt, pred)

  def prettyPred(cxt: PredCxt, pred: Pred): String = pred match {
    case PTrue => "true"
    case PFalse => "false"
    case PTestFileState(p, st) => s"${prettyFileState(st)}?(${prettyExpr(p)})"
    case PTestFileContains(p, cnts) => s"contains?(${prettyExpr(p)}, ${prettyExpr(cnts)})"
    case PNot(PTrue) => "!true"
    case PNot(PFalse) => "!false"
    case PNot(PTestFileState(p, st)) => s"!${prettyFileState(st)}?(${prettyExpr(p)})"
    case PNot(p) => s"!(${prettyPred(NotCxt, p)})"
    case PAnd(lhs, rhs) => {
      val (ls, rs) = (prettyPred(AndCxt, lhs), prettyPred(AndCxt, rhs))
      cxt match {
        case AndCxt | NotCxt => s"$ls && $rs"
        case OrCxt => s"($ls && $rs)"
      }
    }
    case POr(lhs, rhs) => {
      val (ls, rs) = (prettyPred(OrCxt, lhs), prettyPred(OrCxt, rhs))
      cxt match {
        case OrCxt | NotCxt => s"$ls || $rs"
        case AndCxt => s"($ls || $rs)"
      }
    }
  }

  def prettyFileState(st: FileState): String = st match {
    case IsFile(_) => "file"
    case IsDir => "dir"
    case DoesNotExist => "dne"
  }
}

