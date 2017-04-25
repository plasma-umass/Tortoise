package pup

import CommonSyntax._
import FSSyntax._

/**
  * Collection of recursive descent functions that collect information by visiting nodes in the FS++
  * AST.
  */
object FSVisitors {
  // Collects labels from all let bindings.
  def collectLabels(prog: Statement): Set[Int] = prog match {
    case SSkip => Set()
    case SMkdir(_) | SCreate(_, _) | SRm(_) | SCp(_, _) | SChmod(_, _) | SChown(_, _) => Set()
    case SSeq(lhs, rhs) => collectLabels(lhs) ++ collectLabels(rhs)
    case SIf(_, cons, alt) => collectLabels(cons) ++ collectLabels(alt)
    case SLet(_, _, None, body) => collectLabels(body)
    case SLet(_, _, Some(n), body) => collectLabels(body) + n
  }

  // Collect all strings that are used as paths.
  def collectPaths(prog: Statement): Set[String] = {
    type Env = Map[String, Set[String]]

    def visitConst(const: Const): Set[String] = const match {
      case CNum(_) | CBool(_) => Set()
      case CStr(str) => Set(str)
    }

    def visitExpr(expr: Expr)(implicit env: Env): Set[String] = expr match {
      case EUndef | EUnOp(_, _) => Set()
      case EVar(id) => env.getOrElse(id, Set())
      case EConst(const) => visitConst(const)
      case EBinOp(BConcat, lhs, rhs) => visitExpr(lhs).flatMap {
        leftStr => visitExpr(rhs).map {
          rightStr => leftStr + rightStr
        }
      }
      case EBinOp(_, _, _) => Set()
    }

    def visit(stmt: Statement)(implicit env: Env): Set[String] = stmt match {
      case SSkip => Set()
      case SMkdir(path) => visitExpr(path)
      case SCreate(path, _) => visitExpr(path)
      case SRm(path) => visitExpr(path)
      case SCp(src, dst) => visitExpr(src) ++ visitExpr(dst)
      case SChmod(path, _) => visitExpr(path)
      case SChown(path, _) => visitExpr(path)
      case SSeq(lhs, rhs) => visit(lhs) ++ visit(rhs)
      case SLet(id, expr, _, body) => visit(body)(env + (id -> visitExpr(expr)))
      case SIf(_, cons, alt) => visit(cons) ++ visit(alt)
    }

    visit(prog)(Map())
  }
}
