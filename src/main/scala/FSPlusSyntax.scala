package rehearsal

import PrettyFSPlus._

object FSPlusSyntax {
  type Substitution = Map[Int, Path]

  sealed trait Constraint

  case class PathConstraint(path: Path, state: FileState) extends Constraint
  case class LocationConstraint(loc: Int, path: Path) extends Constraint

  sealed trait LangPath {
    def path(): Path = this match {
      case JavaPath(p) => p
      case Parent(p) => p.path.getParent
    }
  }

  case class JavaPath(p: Path) extends LangPath
  case class Parent(p: LangPath) extends LangPath

  sealed trait FileState

  case object IsFile extends FileState
  case object IsDir extends FileState
  case object DoesNotExist extends FileState

  sealed trait Pred {
    def pretty: String = prettyPred(this)
    override def toString: String = this.pretty

    def &&(b: Pred): Pred = (this, b) match {
      case (PTrue, _) => b
      case (_, PTrue) => this
      case (PFalse, _) => PFalse
      case (_, PFalse) => PFalse
      case _ => internPred(PAnd(this, b))
    }

    def ||(b: Pred): Pred = (this, b) match {
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
      case _ => seq(this, s2)
    }
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

  def ite(a: Pred, p: Statement, q: Statement): Statement = {
    if (p eq q) p else intern(new SIf(a, p, q))
  }

  def seq(p: Statement, q: Statement): Statement = intern(SSeq(p, q))

  def seq(stmts: Statement*): Statement = stmts.foldRight[Statement](SSkip) {
    case (stmt, acc) => stmt >> acc
  }

  def mkdir(e: Expr): Statement = intern(SMkdir(e))

  def mkfile(p: Expr, c: Expr): Statement = intern(SCreateFile(p, c))

  def rm(p: Expr): Statement = intern(SRm(p))

  def cp(src: Expr, dst: Expr): Statement = intern(SCp(src, dst))
}
