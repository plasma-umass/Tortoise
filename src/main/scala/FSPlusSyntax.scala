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
  }

  case object PTrue extends Pred
  case object PFalse extends Pred
  case class PAnd(lhs: Pred, rhs: Pred) extends Pred
  case class POr(lhs: Pred, rhs: Pred) extends Pred
  case class PNot(pred: Pred) extends Pred
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
  }

  case object SError extends Statement
  case object SSkip extends Statement
  case class SLet(id: String, e: Expr, body: Statement) extends Statement
  case class SIf(p: Pred, s1: Statement, s2: Statement) extends Statement
  case class SSeq(s1: Statement, s2: Statement) extends Statement
  case class SMkdir(path: Expr) extends Statement
  case class SCreateFile(path: Expr, contents: Expr) extends Statement
  case class SRm(path: Expr) extends Statement
  case class SCp(src: Expr, dst: Expr) extends Statement
}
