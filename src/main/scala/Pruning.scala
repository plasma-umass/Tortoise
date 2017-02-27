package pup

import FSPlusSyntax._
import Implicits._

object Pruning {
  def prune(stmt: Statement)(implicit touchedPaths: Set[Path]): Statement = stmt match {
    case SError | SSkip => stmt
    case SLet(id, e, body) => if (shouldPrune(stmt)) {
      SLet(id, e, prune(body))
    } else {
      SSkip
    }
    case SIf(p, s1, s2) => {
      val (set1, set2) = (
        containsSet(s1) intersect touchedPaths, containsSet(s2) intersect touchedPaths
      )
      if (set1.nonEmpty || set2.nonEmpty) {
        val set = containsSet(stmt) union touchedPaths
        ite(p, prune(s1)(set), prune(s2)(set))
      } else {
        SSkip
      }
    }
    case SSeq(s1, s2) => seq(prune(s1), prune(s2))
    case SMkdir(_) | SCreateFile(_, _) | SRm(_) | SCp(_, _) => if (shouldPrune(stmt)) {
      stmt
    } else {
      SSkip
    }
  }

  def shouldPrune(stmt: Statement)(implicit touchedPaths: Set[Path]): Boolean = {
    (containsSet(stmt) intersect touchedPaths).nonEmpty
  }

  var cache: Map[Statement, Set[Path]] = Map()
  def containsSet(stmt: Statement): Set[Path] = {

    type Env = Map[String, Expr]

    def genPred(pred: Pred)(implicit env: Env): Set[Path] = pred match {
      case PTrue | PFalse => Set()
      case PAnd(lhs, rhs) => genPred(lhs) union genPred(rhs)
      case POr(lhs, rhs) => genPred(lhs) union genPred(rhs)
      case PNot(pred) => genPred(pred)
      case PTestFileState(path, _) => genExpr(path)
      case PTestFileContains(path, contents) => genExpr(path) union genExpr(contents)
    }

    def genExpr(expr: Expr)(implicit env: Env): Set[Path] = expr match {
      case EId(id) => genExpr(env.getOrElse(id, throw UnboundIdentifier(id)))
      case EPath(CPath(p, _)) => p.path.ancestors union Set(p.path)
      case EPath(_) => throw Unreachable
      case EString(CString(s, _)) => Set()
      case EString(_) => throw Unreachable
      case EParent(e) => genExpr(e)
      case EConcat(lhs, rhs) => {
        val (lhsRes, rhsRes) = (genExpr(lhs), genExpr(rhs))
        val concatPaths = for (p1 <- lhsRes; p2 <- rhsRes) yield (p1 concat p2)
        lhsRes union rhsRes union concatPaths
      }
      case EIf(p, e1, e2) => genPred(p) union genExpr(e1) union genExpr(e2)
    }

    def genStmt(stmt: Statement)(implicit env: Env): Set[Path] = stmt match {
      case SError => Set()
      case SSkip => Set()
      case SLet(id, e, s) => genExpr(e) union cached(s)(env + (id -> e))
      case SIf(p, s1, s2) => genPred(p) union cached(s1) union cached(s2)
      case SSeq(s1, s2) => cached(s1) union cached(s2)
      case SMkdir(path) => genExpr(path)
      case SCreateFile(path, str) => genExpr(path) union genExpr(str)
      case SRm(path) => genExpr(path)
      case SCp(src, dst) => genExpr(src) union genExpr(dst)
    }

    def cached(stmt: Statement)(implicit env: Env): Set[Path] = cache.get(stmt) match {
      case Some(paths) => paths
      case None => {
        val res = genStmt(stmt)
        cache = cache + (stmt -> res)
        res
      }
    }

    cached(stmt)(Map())
  }
}
