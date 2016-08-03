package rehearsal

import java.nio.file.Paths

import FSPlusSyntax._
import Implicits._

case object Unreachable extends RuntimeException("This code branch should be unreachable.")

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
      case EPath(_) => throw Unreachable
      case EString(CString(s, _)) => (Set(), Set(s))
      case EString(_) => throw Unreachable
      case EParent(e) => genExpr(e)
      case EConcat(lhs, rhs) => {
        val (lhsRes, rhsRes) = (genExpr(lhs), genExpr(rhs))
        val concatPaths = for (p1 <- lhsRes._1; p2 <- rhsRes._1) yield (p1 resolve p2)
        val concatStrings = for (s1 <- lhsRes._2; s2 <- rhsRes._2) yield (s1 + s2)
        lhsRes union rhsRes union (concatPaths, concatStrings)
      }
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

  def size(stmt: Statement) = {
    def stmtSize(stmt: Statement): Int = stmt match {
      case SError | SSkip => 1
      case SMkdir(_) | SCp(_, _) | SCreateFile(_, _) | SRm(_) => 1
      case SSeq(s1, s2) => 1 + stmtSize(s1) + stmtSize(s2)
      case SIf(p, s1, s2) => 1 + predSize(p) + stmtSize(s1) + stmtSize(s2)
      case SLet(_, expr, body) => 1 + exprSize(expr) + stmtSize(body)
    }

    def predSize(pred: Pred): Int = pred match {
      case PTrue | PFalse => 1
      case PTestFileContains(path, cnts) => 1 + exprSize(path) + exprSize(cnts)
      case PTestFileState(path, _) => 1 + exprSize(path)
      case PNot(p) => 1 + predSize(p)
      case PAnd(p1, p2) => 1 + predSize(p1) + predSize(p2)
      case POr(p1, p2) => 1 + predSize(p1) + predSize(p2)
    }

    def exprSize(expr: Expr): Int = expr match {
      case EId(_) | EPath(_) | EString(_) => 1
      case EParent(e) => 1 + exprSize(e)
      case EConcat(l, r) => 1 + exprSize(l) + exprSize(r)
      case EIf(pred, e1, e2) => 1 + predSize(pred) + exprSize(e1) + exprSize(e2)
    }

    stmtSize(stmt)
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

  def getLocationMap(manifest: PuppetSyntax.Manifest): Map[String, Int] = {
    type Result = Map[String, Int]
    import PuppetSyntax._

    def genManifest(m: Manifest): Result = m match {
      case MEmpty => Map()
      case MSeq(s1, s2) => genManifest(s1) ++ genManifest(s2)
      case MResources(resLst) => resLst.map({
        res => genResource(res)
      }).foldRight[Result](Map())(_ ++ _)
      case MDefine(_, params, body) => params.map(
        param => param.default.map(genExpr(_))
      ).flatten.foldRight[Result](Map())(_ ++ _) ++ genManifest(body)
      case MClass(_, params, _, body) => params.map(
        param => param.default.map(genExpr(_))
      ).flatten.foldRight[Result](Map())(_ ++ _) ++ genManifest(body)
      case MSet(_, e) => genExpr(e)
      case MCase(e, cases) => genExpr(e) ++
        cases.map(genCase(_)).foldRight[Result](Map())(_ ++ _)
      case MIte(pred, m1, m2) => genExpr(pred) ++ genManifest(m1) ++ genManifest(m2)
      case MInclude(es) => es.map(genExpr(_)).foldRight[Result](Map())(_ ++ _)
      case MRequire(e) => genExpr(e)
      case MApp(_, args) => args.map(genExpr(_)).foldRight[Result](Map())(_ ++ _)
      case MResourceDefault(_, attrs) => attrs.map({
        case Attribute(e1, e2) => genExpr(e1) ++ genExpr(e2)
      }).foldRight[Result](Map())(_ ++ _)
    }

    def genResource(r: Resource): Result = r match {
      case ResourceDecl(_, rs) => rs.map({
        case (e, attrs) => genExpr(e) ++ attrs.map({
          case Attribute(e1, e2) => genExpr(e1) ++ genExpr(e2)
        }).foldRight[Result](Map())(_ ++ _)
      }).foldRight[Result](Map())(_ ++ _)
      case ResourceRef(_, e, attrs) => genExpr(e) ++ attrs.map({
        case Attribute(e1, e2) => genExpr(e1) ++ genExpr(e2)
      }).foldRight[Result](Map())(_ ++ _)
      case RCollector(_, r) => genRExpr(r)
    }

    def genExpr(e: Expr): Result = e match {
      case EUndef | ENum(_) | EVar(_) | EBool(_) | ERegex(_) => Map()
      case EStr(s) => Map(s -> e.loc())
      case EStrInterp(terms) => terms.map(genExpr(_)).foldRight[Result](Map())(_ ++ _)
      case ENot(e) => genExpr(e)
      case EAnd(e1, e2) => genExpr(e1) ++ genExpr(e2)
      case EOr(e1, e2) => genExpr(e1) ++ genExpr(e2)
      case EEq(e1, e2) => genExpr(e1) ++ genExpr(e2)
      case ELT(e1, e2) => genExpr(e1) ++ genExpr(e2)
      case EMatch(e1, e2) => genExpr(e1) ++ genExpr(e2)
      case EIn(e1, e2) => genExpr(e1) ++ genExpr(e2)
      case EArray(es) => es.map(genExpr(_)).foldRight[Result](Map())(_ ++ _)
      case EApp(_, es) => es.map(genExpr(_)).foldRight[Result](Map())(_ ++ _)
      case ECond(test, t, f) => genExpr(test) ++ genExpr(f) ++ genExpr(t)
      case EResourceRef(_, title) => genExpr(title)
    }

    def genRExpr(rexpr: RExpr): Result = rexpr match {
      case REAttrEqual(_, e) => genExpr(e)
      case REAnd(r1, r2) => genRExpr(r1) ++ genRExpr(r2)
      case REOr(r1, r2) => genRExpr(r1) ++ genRExpr(r2)
      case RENot(r) => genRExpr(r)
    }

    def genCase(c: Case) = c match {
      case CaseDefault(m) => genManifest(m)
      case CaseExpr(e, m) => genExpr(e) ++ genManifest(m)
    }

    genManifest(manifest)
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
