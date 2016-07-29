package rehearsal

import java.nio.file.Paths

import FSPlusSyntax._
import Implicits._

private[rehearsal] object PlusHelpers {
  def calculateConsts(stmt: Statement): (Set[Path], Set[String]) = {
    type Result = (Set[Path], Set[String])

    implicit class RichTuple(tup: Result) {
      def union(other: Result): Result = (tup._1 union other._1, tup._2 union other._2)
    }

    def genPred(pred: Pred): Result = pred match {
      case PTrue | PFalse => (Set(), Set())
      case PAnd(lhs, rhs) => genPred(lhs) union genPred(rhs)
      case POr(lhs, rhs) => genPred(lhs) union genPred(rhs)
      case PNot(pred) => genPred(pred)
      case PTestFileState(path, _) => genExpr(path)
      case PTestFileContains(path, contents) => genExpr(path) union genExpr(contents)
    }

    def genExpr(expr: Expr): Result = expr match {
      case EId(_) => (Set(), Set())
      case EPath(CPath(p, _)) => (p.path.ancestors union Set(p.path), Set())
      case EPath(_) => (Set(), Set())               // Should not trigger
      case EString(CString(s, _)) => (Set(), Set(s))
      case EString(_) => (Set(), Set())             // Should not trigger
      case EParent(e) => genExpr(e)
      case EConcat(lhs, rhs) => genExpr(lhs) union genExpr(rhs) union (genExpr(lhs)._1.flatMap {
        p1 => genExpr(rhs)._1.map {
          p2 => p1 resolve p2
        }
      }, Set())
      case EIf(p, e1, e2) => genPred(p) union genExpr(e1) union genExpr(e2)
    }

    def genStmt(stmt: Statement): Result = stmt match {
      case SError => (Set(), Set())
      case SSkip => (Set(), Set())
      case SLet(_, e, s) => genExpr(e) union genStmt(s)
      case SIf(p, s1, s2) => genPred(p) union genStmt(s1) union genStmt(s2)
      case SSeq(s1, s2) => genStmt(s1) union genStmt(s2)
      case SMkdir(path) => genExpr(path)
      case SCreateFile(path, str) => genExpr(path) union genExpr(str)
      case SRm(path) => genExpr(path)
      case SCp(src, dst) => genExpr(src) union genExpr(dst)
    }
    
    genStmt(stmt)
  }

  def generateSoftValueConstraints(stmt: Statement, paths: Set[Path]): Seq[ValueConstraint] = {
    FSPlusEval.eval(stmt) match {
      case Some(state) => {
        val changed = state.keys.toSet
        val unchanged = paths -- changed

        val changedCs = state.map {
          case (path, st) => PathConstraint(path, st)
        }.toSeq

        val unchangedCs = unchanged.map {
          path => PathConstraint(path, DoesNotExist)
        }.toSeq

        val strCs = state.toSeq.map {
          case (path, IsFile(str)) => Some(StringConstraint(path, str))
          case _ => None
        }.flatten

        changedCs ++ unchangedCs ++ strCs
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
      case PTestFileContains(path, _) => genExpr(path)
    }

    def genConst(const: Const): Set[LocationConstraint] = const match {
      case CPath(p, loc) => Set(PathLocationConstraint(loc, p.path))
      case CString(str, loc) => Set(StringLocationConstraint(loc, str))
    }

    genStmt(stmt).toSeq
  }

  val rootValue = "__ROOT_1337_H4x0R__"

  def stringifyPath(path: Path): String = if (path == Paths.get("/")) {
    rootValue
  } else {
    path.toString
  }

  def destringifyPath(str: String): Path = if (str == rootValue) {
    Paths.get("/") 
  } else {
    Paths.get(str)
  }

  case class StringBiMap private (forward: Map[String, String], inverse: Map[String, String]) {
    def rep(left: String): String = forward(left)
    def original(right: String): String = inverse(right)
    
    def getRep(left: String): Option[String] = forward.get(left)
    def getOriginal(right: String): Option[String] = inverse.get(right)

    def +(pair: (String, String)): StringBiMap = StringBiMap(forward + pair, inverse + pair.swap)
  }

  object StringBiMap {
    def apply(): StringBiMap = StringBiMap(Map[String, String](), Map[String, String]())

    def apply(map: Map[String, String]): StringBiMap = StringBiMap(map, map.map(_.swap))

    def apply(pairs: (String, String)*): StringBiMap = StringBiMap(pairs.toMap)
  }
}
