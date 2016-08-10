package rehearsal

import PrettyFSPlus._
import Implicits.RichPath

case class FileNotFoundException(msg: String) extends RuntimeException(msg)

object FSPlusSyntax {
  type Substitution = Map[Int, Const]

  sealed trait Constraint
  sealed trait LocationConstraint extends Constraint
  sealed trait ValueConstraint extends Constraint

  case class PathConstraint(path: Path, state: FileState) extends ValueConstraint
  case class StringConstraint(path: Path, contents: String) extends ValueConstraint
  case class PathLocationConstraint(loc: Int, path: Path) extends LocationConstraint
  case class StringLocationConstraint(loc: Int, string: String) extends LocationConstraint

  sealed trait LangPath {
    def path(): Path = this match {
      case JavaPath(p) => p
      case Parent(p) => p.path.getParent
      case Concat(lhs, rhs) => lhs.path concat rhs.path
    }
  }

  case class JavaPath(p: Path) extends LangPath
  case class Parent(p: LangPath) extends LangPath
  case class Concat(lhs: LangPath, rhs: LangPath) extends LangPath

  object JavaPath {
    import java.nio.file.Paths

    def apply(s: String): JavaPath = JavaPath(Paths.get(s))
  }

  sealed trait FileState
  case class IsFile(str: String) extends FileState
  case object IsDir extends FileState
  case object DoesNotExist extends FileState


  // Given file systems Fi and Ff, Ff - Fi returns a sequence of constrains that the represent
  // the path changes from Fi to Ff.
  case class FileSystem(paths: Map[Path, FileState]) {
    def -(other: FileSystem): Seq[ValueConstraint] = {
      val filtered = paths.toSeq.filter { case (path, state) =>
          val oldSt = other.paths.getOrElse(path, throw FileNotFoundException(s"path $path is missing."))
          oldSt != state
      }

      filtered.flatMap { case (path, state) => state match {
          case IsFile(str) => Seq(PathConstraint(path, state), StringConstraint(path, str))
          case _ => Seq(PathConstraint(path, state))
          }
        }
    }
  }

  sealed trait Pred {
    def pretty: String = prettyPred(this)
    override def toString: String = this.pretty

    def &&(b: => Pred): Pred = (this, b) match {
      case (PTrue, _) => b
      case (_, PTrue) => this
      case (PFalse, _) => PFalse
      case (_, PFalse) => PFalse
      case _ => internPred(PAnd(this, b))
    }

    def ||(b: => Pred): Pred = (this, b) match {
      case (PTrue, _) => PTrue
      case (PFalse, _) => b
      case (_, PTrue) => PTrue
      case (_, PFalse) => this
      case _ => internPred(POr(this, b))
    }

    def unary_!(): Pred = this match {
      case PNot(PTrue) => PTrue
      case PNot(PFalse) => PFalse
      case PNot(a) => a
      case _ => internPred(PNot(this))
    }
  }

  case object PTrue extends Pred
  case object PFalse extends Pred
  case class PAnd private[FSPlusSyntax](lhs: Pred, rhs: Pred) extends Pred
  case class POr private[FSPlusSyntax](lhs: Pred, rhs: Pred) extends Pred
  case class PNot private[FSPlusSyntax](pred: Pred) extends Pred
  case class PTestFileState(path: Expr, state: FileState) extends Pred
  case class PTestFileContains(path: Expr, contents: Expr) extends Pred

  sealed trait Const {
    def pretty: String = prettyConst(this)
    override def toString: String = this.pretty
  }

  case class CPath(path: LangPath, loc: Int) extends Const
  case class CString(str: String, loc: Int) extends Const

  sealed trait Expr {
    def pretty: String = prettyExpr(this)
    override def toString: String = this.pretty
  }

  case class EId(id: String) extends Expr
  case class EPath(path: Const) extends Expr
  case class EString(str: Const) extends Expr
  case class EParent(e: Expr) extends Expr
  case class EConcat(lhs: Expr, rhs: Expr) extends Expr
  case class EIf(p: Pred, e1: Expr, e2: Expr) extends Expr

  sealed trait Statement {
    def pretty: String = prettyStmt(this)
    override def toString: String = this.pretty

    def >>(s2: Statement) = (this, s2) match {
      case (SSkip, _) => s2
      case (_, SSkip) => this
      case (SError, _) => SError
      case (_, SError) => SError
      case (SIf(a, SSkip, SError), SIf(b, SSkip, SError)) => ite(a && b, SSkip, SError)
      case (p, q) => intern(SSeq(p, q))
    }

    lazy val (paths, strings) = PlusHelpers.calculateConsts(this)
    lazy val size = PlusHelpers.size(this)
  }

  case object SError extends Statement
  case object SSkip extends Statement
  case class SLet(id: String, e: Expr, body: Statement) extends Statement
  case class SIf private[FSPlusSyntax](p: Pred, s1: Statement, s2: Statement) extends Statement
  case class SSeq private[FSPlusSyntax](s1: Statement, s2: Statement) extends Statement
  case class SMkdir private[FSPlusSyntax](path: Expr) extends Statement
  case class SCreateFile private[FSPlusSyntax](path: Expr, contents: Expr) extends Statement
  case class SRm private[FSPlusSyntax](path: Expr) extends Statement
  case class SCp private[FSPlusSyntax](src: Expr, dst: Expr) extends Statement

  private val stmtCache = scala.collection.mutable.HashMap[Statement, Statement]()
  private val predCache = scala.collection.mutable.HashMap[Pred, Pred]()

  private def internPred(a: Pred) = predCache.getOrElseUpdate(a, a)

  private def intern(e: Statement): Statement = stmtCache.getOrElseUpdate(e, e)

  def ite(pred: Pred, p: => Statement, q: => Statement): Statement = {
    pred match {
      case PTrue => intern(p)
      case PFalse => intern(q)
      case _ => if (p eq q) p else intern(new SIf(pred, p, q))
    }
  }

  def seq(p: Statement, q: Statement): Statement = p >> q

  def seq(stmts: Statement*): Statement = stmts.foldRight[Statement](SSkip) {
    case (stmt, acc) => stmt >> acc
  }

  def mkdir(e: Expr): Statement = intern(SMkdir(e))

  def mkfile(p: Expr, c: Expr): Statement = intern(SCreateFile(p, c))

  def rm(p: Expr): Statement = intern(SRm(p))

  def cp(src: Expr, dst: Expr): Statement = intern(SCp(src, dst))
}
