package pup

import edu.umass.cs.smtlib._
import edu.umass.cs.smtlib.SMT._
import edu.umass.cs.smtlib.SMT.Implicits._
import smtlib.parser.Commands._
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.theories.experimental.Strings._
import pup.{FSSyntax => F, SynthTransformers => T}
import SymbolicFS._
import SymbolicFSCompiler._

object Synthesizer {
  import Implicits._

  private def computePathsAndDefaultFS(
    prog: F.Statement, constraints: Seq[Constraint]
  ): (Set[String], Map[String, FileState]) = {
    val progPaths = FSVisitors.collectPaths(prog)
    val constraintPaths = constraints.map(_.paths).flatten
    val basePaths = progPaths ++ constraintPaths ++ Settings.assumedDirs

    // Compute all ancestors from each path.
    // NOTE: this may cause bugs because of the conversion to and from Path.
    val paths = basePaths.map(_.ancestors).flatten

    // Create the default file system representation.
    val defaultFS = Settings.assumedDirs.map((_, Dir)).toMap + ("/" -> Dir)

    (paths, defaultFS)
  }

  def synthesize(
    inProg: F.Statement, constraints: Seq[Constraint], transformer: T.Transformer = T.identity
  ): Option[Substitution] = {
    val prog = transformer(inProg)
    val (paths, defaultFS) = computePathsAndDefaultFS(prog, constraints)
    synthesizer(paths, defaultFS) {
      synth => synth.synthesize(prog.partialed, constraints)
    }
  }

  def synthesizeAll(
    inProg: F.Statement, constraints: Seq[Constraint], transformer: T.Transformer = T.identity
  ): Seq[Substitution] = {
    val prog = transformer(inProg)
    val (paths, defaultFS) = computePathsAndDefaultFS(prog, constraints)
    synthesizer(paths, defaultFS) {
      synth => synth.synthesizeAll(prog.partialed, constraints)
    }
  }

  def synthesizer[A](
    paths: Set[String], defaultFS: Map[String, FileState]
  )(func: Synthesizer => A): A = {
    val synthesizer = Synthesizer(paths, defaultFS)
    val res = func(synthesizer)
    synthesizer.free()
    res
  }
}

case class Synthesizer(paths: Set[String], defaultFS: Map[String, FileState]) {
  val solver = SMT()

  def free(): Unit = solver.free()

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
    * Generate initial owner? function (owner1?)
    */

  val initialOwnerHuh = FunName("owner", 1)

  val initialOwnerTuples = paths.map {
    path => (StringLit(path), defaultOwner)
  }

  val initialOwnerHuhBody = initialOwnerTuples.foldRight[Term](defaultOwner) {
    case ((path, str), acc) => ITE(
      Equals("p".id, path),
      str,
      acc
    )
  }

  solver.eval(DefineFun(FunDef(
    initialOwnerHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
    initialOwnerHuhBody
  )))

  /**
    * Defining a procedure for parsing models into substitutions.
    */

  def extractKey(str: String): Int = Integer.parseInt(
    str.substring(str.indexOf('-') + 1)
  )

  def parseModel(exprs: List[SExpr]): Substitution = {
    val changedMap = exprs.flatMap {
      case DefineFun(FunDef(SSymbol(str), Seq(), _, body)) if str.startsWith("unchanged") => {
        val key = extractKey(str)
        body match {
          case NumeralLit(n) if n.intValue == 0 => Some(key -> true)
          case NumeralLit(n) => Some(key -> false)
          case _ => None
        }
      }
      case _ => None
    }.toMap

    exprs.flatMap {
      case DefineFun(FunDef(SSymbol(str), Seq(), _, body)) if str.startsWith("loc") => {
        val key = extractKey(str)
        body match {
          case StringLit(str) if changedMap(key) => Some(key -> str)
          case _ => None
        }
      }
      case _ => None
    }.toMap
  }

  /**
    * Defining a procedure to build the symbolic file system in the solver.
    */

  def buildSymbolicFS(prog: F.Statement, constraints: Seq[Constraint]): (Seq[Term], Term) = {
    val initialFuns = (initialStateHuh, initialContainsHuh, initialModeHuh, initialOwnerHuh)
    val nextFuns = initialFuns.next

    // Collect labels from all let bindings and declare them as variables.
    val labels = FSVisitors.collectLabels(prog)

    labels.foreach {
      label => solver.eval(DeclareConst(SSymbol(s"unchanged-$label"), intSort))
    }

    val (commands, (lastStateHuh, lastContainsHuh, lastModeHuh, lastOwnerHuh)) =
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
      case ContentsConstraint(path, contents) => {
        val pathTerm = StringLit(path)
        val contentsTerm = StringLit(contents)
        solver.eval(Assert(Equals(lastContainsHuh(pathTerm), contentsTerm)))
      }

      // mode constraints deal with mode?
      case ModeConstraint(path, mode) => {
        val pathTerm = StringLit(path)
        val modeTerm = StringLit(mode)
        solver.eval(Assert(Equals(lastModeHuh(pathTerm), modeTerm)))
      }

      // owner constraints deal with owner?
      case OwnerConstraint(path, owner) => {
        val pathTerm = StringLit(path)
        val ownerTerm = StringLit(owner)
        solver.eval(Assert(Equals(lastOwnerHuh(pathTerm), ownerTerm)))
      }
    }

    // Define the sum to maximize in terms of counts of unchanged variables.
    val counts = labels.toSeq.map(label => s"unchanged-$label".id)
    val sum = SSymbol("sum")
    solver.eval(DeclareConst(sum, IntSort()))
    solver.eval(Assert(Equals(sum.id, FunctionApplication("+".id, counts))))

    (counts, sum.id)
  }

  /**
    * Defining the synthesis procedure.
    */

  def synthesize(prog: F.Statement, constraints: Seq[Constraint]): Option[Substitution] = {
    // Build the symbolic file system in the theorem prover.
    val (counts, sum) = buildSymbolicFS(prog, constraints)

    // Use binary search to find a satisfying model that maximizes the number of unchanged labels.
    var lo = 0
    var hi = counts.length
    var res: Option[List[SExpr]] = None

    while (lo < hi) {
      solver.pushPop {
        solver.eval(Assert(
          GreaterEquals(
            sum,
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

  def synthesizeAll(prog: F.Statement, constraints: Seq[Constraint]): Seq[Substitution] = {
    val (_, _) = buildSymbolicFS(prog, constraints)

    // Iterate through all possible satisfying models until there are no more possible.
    var res: Seq[List[SExpr]] = Seq()
    val startTime = java.time.LocalTime.now()
    while (solver.checkSat() &&
           java.time.LocalTime.now().isBefore(startTime.plusMinutes(2))) {
      val currentModel = solver.getModel()  
      res = res :+ currentModel

      // Collect all pairs of symbol and their values from the model.
      val modelPairs = currentModel.flatMap {
        case DefineFun(FunDef(sym@SSymbol(_), Seq(), _, body)) => Some(sym -> body)
        case _ => None
      }

      // Build a model term stating which locations were changed and unchanged.
      val modelTerm = modelPairs.filter(_._1.name.startsWith("unchanged-")).foldRight(True()) {
        case ((sym, value), acc) => And(Equals(sym.id, value), acc)
      }

      // Assert that the model cannot match this assignment of changed and unchanged locations.
      solver.eval(Assert(Not(modelTerm)))
    }

    // Parse all satisfying models into a substitution.
    res.map(parseModel)
  }
}
