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

  type Constraint = (Const, FileState)

  def constraintPaths(cs: Seq[Constraint]): Set[Path] = cs.foldRight[Set[Path]](Set()) {
    case ((CPath(p, _), _), acc) => acc + p.path
    case (_, acc) => acc
  }

  // This probably needs to be improved.
  def stringifyPath(path: Path): String = path.toString.replace('/', '$')

  def destringifyPath(str: String): Path = Paths.get(str.replace('$', '/'))
}
