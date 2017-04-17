package pup

import edu.umass.cs.smtlib._
import edu.umass.cs.smtlib.SMT._
import edu.umass.cs.smtlib.SMT.Implicits._
import smtlib.parser.Commands._
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.theories.experimental.Strings._
import pup.{FSSyntax => F}
import SymbolicFS._
import SymbolicFSCompiler._

object Synthesizer {
  import Implicits._

  def synthesize(prog: F.Statement, constraints: Seq[Constraint]): Option[Substitution] = {
    val progPaths = FSVisitors.collectPaths(prog)
    val constraintPaths = constraints.map(_.paths).reduce(_ ++ _)
    val basePaths = progPaths ++ constraintPaths ++ Settings.assumedDirs

    // Compute all ancestors from each path.
    // NOTE: this may cause bugs because of the conversion to and from Path.
    val paths = basePaths.map(_.ancestors).reduce(_ ++ _).map(_.toString)

    // Create the default file system representation.
    val defaultFS = Settings.assumedDirs.map((_, Dir)).toMap + ("/" -> Dir)

    // Create synthesizer and synthesize!
    val synthesizer = Synthesizer(paths, defaultFS)
    val partialedProg = FSPartialEvaluator.eval(prog)
    synthesizer.synthesize(partialedProg, constraints)
  }
}

case class Synthesizer(paths: Set[String], defaultFS: Map[String, FileState]) {
  val solver = SMT()

  /**
    * Declare State datatype.
    */

  val stateSort = Sort(SimpleIdentifier(SSymbol("State")))

  solver.eval(DeclareDatatypes(Seq(
    SSymbol("State") -> Seq(
      Constructor(SSymbol("Dir"), Seq()), // Directory
      Constructor(SSymbol("File"), Seq()), // File
      Constructor(SSymbol("Null"), Seq()) // Does Not Exist
    )
  )))

  /**
    * Generate initial state? function (state1?)
    */

  val initialStateHuh = FunName("state", 1)

  val initialStateTuples = paths.map {
    path => defaultFS.get(path).map(compileFileState).map(state => (StringLit(path), state))
  }.flatten

  val initialStateHuhBody = initialStateTuples.foldRight[Term](nil) {
    case ((path, state), acc) => ITE(
      Equals("p".id, path),
      state,
      acc
    )
  }

  solver.eval(DefineFun(FunDef(
    initialStateHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stateSort, initialStateHuhBody
  )))

  /**
    * Generate initial contains? function (contains1?)
    */

  val initialContainsHuh = FunName("contains", 1)

  val initialContainsTuples = paths.map {
    path => (StringLit(path), StringLit("")) // FIXME: initial contents for files
  }

  val initialContainsHuhBody = initialContainsTuples.foldRight[Term](StringLit("")) {
    case ((path, str), acc) => ITE(
      Equals("p".id, path),
      str,
      acc
    )
  }

  solver.eval(DefineFun(FunDef(
    initialContainsHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
    initialContainsHuhBody
  )))

  /**
    * Generate initial mode? function (mode1?)
    */

  val initialModeHuh = FunName("mode", 1)

  val initialModeTuples = paths.map {
    path => (StringLit(path), defaultMode) // FIXME: initial mode for files
  }

  val initialModeHuhBody = initialModeTuples.foldRight[Term](defaultMode) {
    case ((path, str), acc) => ITE(
      Equals("p".id, path),
      str,
      acc
    )
  }

  solver.eval(DefineFun(FunDef(
    initialModeHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
    initialModeHuhBody
  )))

  /**
    * Defining a procedure for parsing models into substitutions.
    */

  def parseModel(exprs: List[SExpr]): Substitution = {
    def extractKey(str: String): Int = Integer.parseInt(
      str.substring(str.indexOf('-') + 1)
    )

    exprs.flatMap {
      case DefineFun(FunDef(SSymbol(str), Seq(), sort, body)) if str.startsWith("loc") => {
        val key = extractKey(str)
        body match {
          case StringLit(str) => Some(key -> str)
          case _ => None
        }
      }
      case _ => None
    }.toMap
  }

  /**
    * Defining the synthesis procedure.
    */

  def synthesize(prog: F.Statement, constraints: Seq[Constraint]): Option[Substitution] = {
    val initialFuns = (initialStateHuh, initialContainsHuh, initialModeHuh)
    val nextFuns = initialFuns.next

    val (commands, (lastStateHuh, lastContainsHuh, lastModeHuh)) =
      compileStatement(prog, True(), initialFuns, nextFuns)

    // Evaluate commands compiled from the program in the solver.
    commands.foreach {
      command => solver.eval(command)
    }

    // Convert each constraint into a file system assertion, and then add it to the solver.
    constraints.foreach {
      // state constraints deal with state?
      case StateConstraint(path, state) => {
        val pathTerm = StringLit(path)
        solver.eval(Assert(Equals(lastStateHuh(pathTerm), compileFileState(state))))
      }

      // contents constraints deal with contains?
      case ContentsConstraint(path, contents) => ???

      // mode constraints deal with mode?
      case ModeConstraint(path, mode) => ???
    }

    // Collect labels from all let bindings and compute their sum under the solver.
    val labels = FSVisitors.collectLabels(prog)
    val counts = labels.toSeq.map(label => s"count-$label".id)

    val sum = SSymbol("sum")
    solver.eval(DeclareConst(sum, IntSort()))
    solver.eval(Assert(Equals(sum.id, FunctionApplication("+".id, counts))))

    // Use binary search to find a satisfying model that maximizes the number of unchanged labels.
    var lo = 0
    var hi = counts.length
    var res: Option[List[SExpr]] = None

    while (lo < hi) {
      solver.pushPop {
        solver.eval(Assert(
          GreaterEquals(
            sum.id,
            SNumeral((lo + hi) / 2)
          )
        ))

        if (solver.checkSat()) {
          res = Some(solver.getModel())
          lo = (lo + hi) / 2 + 1
        } else {
          hi = (lo + hi) / 2
        }
      }
    }

    // Parse the maximal satisfying model into a substitution.
    res.map(parseModel)
  }
}
