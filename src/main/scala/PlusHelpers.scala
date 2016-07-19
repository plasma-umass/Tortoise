package rehearsal

import java.nio.file.Paths

import FSPlusSyntax._
import Implicits._

private[rehearsal] object PlusHelpers {
  def predPaths(pred: Pred): Set[Path] = pred match {
    case PTrue | PFalse => Set()
    case PAnd(lhs, rhs) => predPaths(lhs) union predPaths(rhs)
    case POr(lhs, rhs) => predPaths(lhs) union predPaths(rhs)
    case PNot(pred) => predPaths(pred)
    case PTestFileState(path, _) => exprPaths(path)
  }

  def exprPaths(expr: Expr): Set[Path] = expr match {
    case EId(_) => Set()
    case EPath(CPath(p, _)) => p.path.ancestors union Set(p.path)
    case EPath(_) => Set()
    case EString(_) => Set()
    case EParent(e) => exprPaths(e)
    case EConcat(lhs, rhs) => exprPaths(lhs) union exprPaths(rhs) union exprPaths(lhs).flatMap {
      p1 => exprPaths(rhs).map {
        p2 => p1 resolve p2
      }
    }
    case EIf(p, e1, e2) => predPaths(p) union exprPaths(e1) union exprPaths(e2)
  }

  def stmtPaths(stmt: Statement): Set[Path] = stmt match {
    case SError => Set()
    case SSkip => Set()
    case SLet(_, e, s) => exprPaths(e) union stmtPaths(s)
    case SIf(p, s1, s2) => predPaths(p) union stmtPaths(s1) union stmtPaths(s2)
    case SSeq(s1, s2) => stmtPaths(s1) union stmtPaths(s2)
    case SMkdir(path) => exprPaths(path)
    case SCreateFile(path, _) => exprPaths(path)
    case SRm(path) => exprPaths(path)
    case SCp(src, dst) => exprPaths(src) union exprPaths(dst)
  }

  def generateSoftPathConstraints(stmt: Statement, paths: Set[Path]): Seq[PathConstraint] = {
    FSPlusEval.eval(stmt) match {
      case Some(state) => {
        val changed = state.keys.toSet
        val unchanged = paths -- changed

        val changedCs = state.map {
          case (path, st) => PathConstraint(path, st.toFileState())
        }.toSeq

        val unchangedCs = unchanged.map {
          path => PathConstraint(path, DoesNotExist)
        }.toSeq

        changedCs ++ unchangedCs
      }
      case None => Seq()
    }
  }

   def generateSoftLocationConstraints(stmt: Statement): Seq[LocationConstraint] = {

    def genStmt(stmt: Statement): Set[LocationConstraint] = stmt match {
      case SError | SSkip => Set()
      case SLet(_, e, body) => genExpr(e) union genStmt(body)
      case SIf(pred, s1, s2) => genPred(pred) union genStmt(s1) union genStmt(s2)
      case SSeq(s1, s2) => genStmt(s1) union genStmt(s2)
      case SMkdir(path) => genExpr(path)
      case SCreateFile(path, contents) => genExpr(path) union genExpr(contents)
      case SRm(path) => genExpr(path)
      case SCp(src, dst) => genExpr(src) union genExpr(dst)
    }

    def genExpr(expr: Expr): Set[LocationConstraint] = expr match {
      case EId(_) => Set()
      case EPath(path) => genConst(path)
      case EString(str) => genConst(str)
      case EParent(e) => genExpr(e)
      case EConcat(lhs, rhs) => genExpr(lhs) union genExpr(rhs)
      case EIf(pred, e1, e2) => genPred(pred) union genExpr(e1) union genExpr(e2)
    }

    def genPred(pred: Pred): Set[LocationConstraint] = pred match {
      case PTrue | PFalse => Set()
      case PAnd(lhs, rhs) => genPred(lhs) union genPred(rhs)
      case POr(lhs, rhs) => genPred(lhs) union genPred(rhs)
      case PNot(pred) => genPred(pred)
      case PTestFileState(path, _) => genExpr(path)
    }

    def genConst(const: Const): Set[LocationConstraint] = const match {
      case CPath(p, loc) => Set(LocationConstraint(loc, p.path))
      case CString(_, _) => Set()
    }

    genStmt(stmt).toSeq
  }

  // This probably needs to be improved.
  def stringifyPath(path: Path): String = path.toString.replace('/', '$')

  def destringifyPath(str: String): Path = Paths.get(str.replace('$', '/'))
}
