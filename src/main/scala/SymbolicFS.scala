package tortoise

import edu.umass.cs.smtlib.SMT.Implicits._
import smtlib.parser.Commands._
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.experimental.Strings._

object SymbolicFS {
  /**
    * Definitions for synthesizer
    */

  type Substitution = Map[Int, String]

  sealed trait FileState
  case object File extends FileState
  case object Dir extends FileState
  case object Nil extends FileState

  sealed trait Constraint {
    def paths: Set[String] = this match {
      case StateConstraint(path, _) => Set(path)
      case ContentsConstraint(path, _) => Set(path)
      case ModeConstraint(path, _) => Set(path)
      case OwnerConstraint(path, _) => Set(path)
    }
  }

  case class StateConstraint(path: String, state: FileState) extends Constraint
  case class ContentsConstraint(path: String, contents: String) extends Constraint
  case class ModeConstraint(path: String, mode: String) extends Constraint
  case class OwnerConstraint(path: String, owner: String) extends Constraint

  /**
    * Type definitions for compiler use
    */

  type Commands = Seq[Command]
  type Funs = (FunName, FunName, FunName, FunName)
  type State = Integer

  case class FunName(name: String, num: State) {
    override def toString: String = s"$name$num?"
    def id: QualifiedIdentifier = this.toString.id
    def sym: SSymbol = SSymbol(this.toString)
    def apply(terms: Term*): Term = FunctionApplication(this.id, terms)
    def next(): FunName = FunName(name, num + 1)

    // Cons and Alt versions of the function.
    def cons(): FunName = FunName(name + "Cons", num)
    def alt(): FunName = FunName(name + "Alt", num)
  }

  implicit class RichFuns(funs: Funs) {
    def next(): Funs = funs match {
      case (fun1, fun2, fun3, fun4) => (fun1.next, fun2.next, fun3.next, fun4.next)
    }
  }

  /**
    * stateN? and containsN?
    */

  def stateHuh(arg: Term)(implicit st: State): Term = FunctionApplication(
    FunName("state", st).id, Seq(arg)
  )

  def containsHuh(arg: Term)(implicit st: State): Term = FunctionApplication(
    FunName("contains", st).id, Seq(arg)
  )

  /**
    * File system states and other constants
    */

  val file = "File".id
  val dir = "Dir".id
  val nil = "Null".id
  val defaultOwner = StringLit("root") // root is the default owner in Puppet.
  val defaultMode = StringLit("644") // FIXME: what is the default mode?
  val undef = StringLit("undef")
  val stateSort = Sort(SimpleIdentifier(SSymbol("State")))
  val stringSort = Sort(SimpleIdentifier(SSymbol("String")))
  val intSort = Sort(SimpleIdentifier(SSymbol("Int")))

  /**
    * Holes and hole-tracking utilities
    */

  var holes: Map[Int, Term] = Map()
  def makeHole(label: Int): (Term, Seq[Command]) = holes.get(label) match {
    case Some(term) => (term, Seq())
    case None => {
      val hole = SSymbol(s"loc-$label")
      val holeDef = DeclareConst(hole, stringSort)
      holes += (label -> hole.id)
      (hole.id, Seq(holeDef))
    }
  }

  def resetState() {
    holes = Map()
  }
}
