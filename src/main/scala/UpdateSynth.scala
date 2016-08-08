package rehearsal

import edu.umass.cs.smtlib._
import smtlib.parser.Commands._
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._
import rehearsal.FSPlusSyntax.{FileState, IsFile, IsDir, DoesNotExist}
import rehearsal.FSPlusSyntax.{Constraint, ValueConstraint, LocationConstraint, Substitution}
import rehearsal.FSPlusSyntax.{PathConstraint, PathLocationConstraint}
import rehearsal.FSPlusSyntax.{StringConstraint, StringLocationConstraint}
import rehearsal.FSPlusTrace.{Type, TPath, TString}
import rehearsal.{FSPlusSyntax => FSP}
import rehearsal.{FSPlusTrace => T}
import Implicits.RichPath
import scala.collection.JavaConversions._

case class SynthTypeError(msg: String) extends RuntimeException(msg)

object UpdateSynth {
  import FSP._
  import Implicits._
  import java.nio.file.Paths

  def synthesize(stmtOriginal: Statement, cs: Seq[ValueConstraint]): Option[Substitution] = {
    val pathCs = cs.filter(_.isInstanceOf[PathConstraint]).map(_.asInstanceOf[PathConstraint])
    val strCs = cs.filter(_.isInstanceOf[StringConstraint]).map(_.asInstanceOf[StringConstraint])
    val constraintPaths = {
      pathCs.flatMap(c => Seq(c.path) ++ c.path.ancestors).toSet ++
      strCs.flatMap(c => Seq(c.path) ++ c.path.ancestors).toSet
    }

    val stmt = Pruning.prune(stmtOriginal)(constraintPaths)

    val (paths, strings) = PlusHelpers.calculateConsts(stmt)

    val basePaths = constraintPaths ++ paths ++ Settings.assumedDirs
    val allStrings = strCs.map(_.contents).toSet ++ Set("") ++ strings

    val allPaths: Set[Path] = basePaths.flatMap { path =>
        val conts = path.iterator.toList
        (1 until conts.size).flatMap{ i =>
          conts.toList.sliding(i).toList.map(_.reduce { _ concat  _})
        } ++ basePaths
    }

    // Calculate soft constraints
    val softVals = PlusHelpers.generateSoftValueConstraints(stmt, paths)
    val softLocs = PlusHelpers.generateSoftLocationConstraints(stmt)

    val soft = softVals union softLocs
    val defaultFS = Settings.assumedDirs.map((_, IsDir)).toMap + ((Paths.get("/"), IsDir))
    val impl = new UpdateSynth(allPaths, allStrings, defaultFS)

    val trace = FSPlusEval.tracingEval(stmt)
    println("Synthesis initiated.")
    val res = impl.synthesize(trace, cs, soft)
    println("Syntheseis complete.")
    res
  }
}

class UpdateSynth(paths: Set[Path], strings: Set[String], defaultFS: Map[Path, FileState]) {
  import SMT._
  import SMT.Implicits._

  val strMap = PlusHelpers.StringBiMap(
    strings.toSeq.zipWithIndex.map {
      case (str, i) => (str, s"str_$i")
    }
  : _*)

  val smt = SMT()
  import smt.eval

  // Generate Path datatype.
  val pathSort = Sort(SimpleIdentifier(SSymbol("Path")))
  val pathCtrs = paths.toSeq.map(path => Constructor(
    SSymbol(PlusHelpers.stringifyPath(path)), Seq()
  ))
  val noPathCtr = Constructor(SSymbol("NoPath"), Seq())
  eval(DeclareDatatypes(Seq(
    SSymbol("Path") -> (noPathCtr +: pathCtrs)
  )))

  // Generate String datatype
  val stringSort = Sort(SimpleIdentifier(SSymbol("String")))
  val stringCtrs = strings.toSeq.map(s => Constructor(SSymbol(strMap.rep(s)), Seq()))
  val noStringCtr = Constructor(SSymbol("NoString"), Seq())
  eval(DeclareDatatypes(Seq(
    SSymbol("String") -> (noStringCtr +: stringCtrs)
  )))

  // Declare State datatype.
  val stateSort = Sort(SimpleIdentifier(SSymbol("State")))
  eval(DeclareDatatypes(Seq(
    SSymbol("State") -> Seq(
      Constructor(SSymbol("Dir"), Seq()), // Directory
      Constructor(SSymbol("File"), Seq()), // File
      Constructor(SSymbol("Null"), Seq()) // Does Not Exist
    )
  )))

  // Declare Error var.
  eval(DeclareConst(SSymbol("error"), BoolSort()))
  eval(Assert(Not("error".id)))

  // Generate Parent function.
  val parentTuples = paths.map { path =>
    Option(path.getParent).map {
      parent => (PlusHelpers.stringifyPath(path).id, PlusHelpers.stringifyPath(parent).id)
    }
  }.flatten

  val parentBody = parentTuples.foldRight[Term]("NoPath".id) { case ((path, parent), acc) =>
    ITE(Equals("p".id, path), parent, acc)
  }

  eval(DefineFun(FunDef(SSymbol("parent"), Seq(SortedVar(SSymbol("p"), pathSort)),
    pathSort, parentBody
  )))

  // Generate path concatenation function.
  val concatPaths = "cat-paths".id

  val pathTuples = for (p1 <- paths; p2 <- paths; if paths.contains(p1 concat p2)) yield (p1, p2)

  val concatPathBody =
    pathTuples.foldRight[Term]("NoPath".id) { case ((p1, p2), acc) =>
      ITE(
        And(
          Equals("p1".id, PlusHelpers.stringifyPath(p1).id),
          Equals("p2".id, PlusHelpers.stringifyPath(p2).id)
        ),
        PlusHelpers.stringifyPath(p1 concat p2).id,
        acc
      )
    }

  eval(DefineFun(FunDef(
    SSymbol("cat-paths"), Seq(SortedVar(SSymbol("p1"), pathSort), SortedVar(SSymbol("p2"), pathSort)),
    pathSort, concatPathBody
  )))

  // Generate string concatenation function.
  val concatStrings = "cat-strings".id

  val stringTuples = for (p1 <- strings; p2 <- strings; if paths.contains(p1 + p2)) yield (p1, p2)

  val concatStringBody =
    stringTuples.foldRight[Term]("NoString".id) { case ((s1, s2), acc) =>
      ITE(
        And(
          Equals("s1".id, strMap.rep(s1).id),
          Equals("s2".id, strMap.rep(s2).id)
        ),
        strMap.rep(s1 + s2).id,
        acc
      )
    }
  eval(DefineFun(FunDef(
    SSymbol("cat-strings"), Seq(SortedVar(SSymbol("s1"), stringSort), SortedVar(SSymbol("s2"), stringSort)),
    stringSort, concatStringBody
  )))

  // Generate initial state function (state1?)
  val initialState = FunName("state", 1)

  val initialStateTuples = paths.map { path =>
    defaultFS.get(path).map(convertFileState).map(state => (path, state))
  }.flatten

  val initialStateBody = initialStateTuples.foldRight[Term]("Null".id) { case ((path, state), acc) =>
    ITE(
      Equals("p".id, PlusHelpers.stringifyPath(path).id),
      state,
      acc
    )
  }

  eval(DefineFun(FunDef(
    initialState.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stateSort,
    initialStateBody
  )))

  // Generate initial contains function (contains1?)
  val initialContains = FunName("contains", 1)

  val initialContainsTuples = paths.map { path =>
    defaultFS.get(path).flatMap {
      case IsFile(str) => Some((PlusHelpers.stringifyPath(path).id, strMap.rep(str).id))
      case _ => None
    }
  }.flatten

  val initialContainsBody = initialContainsTuples.foldRight[Term]("NoString".id) {
    case ((path, str), acc) => {
      ITE(
        Equals("p".id, path),
        str,
        acc
      )
    }
  }

  eval(DefineFun(FunDef(
    initialContains.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stringSort,
    initialContainsBody
  )))

  // Hole-tracking utilities
  var holes: Map[Int, Term] = Map()
  def mkHole(loc: Int, typ: Type): Term = holes.get(loc) match {
    case Some(term) => term
    case None if loc < 0 => {
      val fresh = freshName(s"not-present$loc@")
      val sort = typ match {
        case TPath => pathSort
        case TString => stringSort
      }
      val value = PlusHelpers.NotPresentMap(loc) match {
        case FSP.CPath(p, _) => PlusHelpers.stringifyPath(p.path).id
        case FSP.CString(s, _) => strMap.rep(s).id
      }
      // This is actually a constant declaration.
      eval(DefineFun(FunDef(fresh, Seq(), sort, value)))
      holes += (loc -> fresh.id)
      fresh.id
    }
    case None => {
      val hole = freshName(s"loc-$loc@")
      eval(DeclareConst(hole, typ match {
        case TPath => pathSort
        case TString => stringSort
      }))
      typ match {
        case TPath => eval(Assert(Not(Equals(hole.id, "NoPath".id))))
        case TString => eval(Assert(Not(Equals(hole.id, "NoString".id))))
      }
      holes += (loc -> hole.id)
      hole.id
    }
  }

  private[UpdateSynth] case class FunName(name: String, num: Int) {
    override def toString: String = s"$name$num?"
    def id: QualifiedIdentifier = s"$this".id
    def sym: SSymbol = SSymbol(s"$this")
    def next: FunName = FunName(name, num + 1)
    def last: FunName = {
      if (num == 0) throw new RuntimeException(s"No last FunName for $this")
      else FunName(name, num - 1)
    }
  }

  def convertPred(pred: T.Pred)(implicit fs: (FunName, FunName)): Term = pred match {
    case T.PTrue => True()
    case T.PFalse => False()
    case T.PAnd(lhs, rhs) => convertPred(lhs) && convertPred(rhs)
    case T.POr(lhs, rhs) => convertPred(lhs) || convertPred(rhs)
    case T.PNot(p) => Not(convertPred(p))
    case T.PTestFileState(path, state) => Equals(
      FunctionApplication(fs._1.id, Seq(convertExpr(path))),
      convertFileState(state)
    )
    case T.PTestFileContains(path, cts) => Equals(
      FunctionApplication(fs._2.id, Seq(convertExpr(path))),
      convertExpr(cts)
    )
  }

  def convertFileState(st: FileState): Term = st match {
    case IsFile(_) => "File".id
    case IsDir => "Dir".id
    case DoesNotExist => "Null".id
  }

  def convertExpr(expr: T.Expr)(implicit fs: (FunName, FunName)): Term = {

    def convert(expr: T.Expr): (Term, T.Type) = expr match {
      case T.EHole(typ, loc) => (mkHole(loc, typ), typ)
      case T.EParent(e) => (FunctionApplication("parent".id, Seq(convert(e)._1)), T.TPath)
      case T.EConcat(lhs, rhs) => (convert(lhs), convert(rhs)) match {
        case ((lhs, T.TPath), (rhs, T.TPath)) => (FunctionApplication(
          concatPaths, Seq(lhs, rhs)
        ), T.TPath)
        case ((lhs, T.TString), (rhs, T.TString)) => (FunctionApplication(
          concatStrings, Seq(lhs, rhs)
        ), T.TString)
        case ((_, t1), (_, t2)) => throw SynthTypeError(s"Type mismatch: $t1, $t2")
      }
      case T.EIf(p, e1, e2) => (convert(e1), convert(e2)) match {
        case ((et1, t1), (et2, t2)) if t1 == t2 => (ITE(convertPred(p), et1, et2), t1)
        case ((_, t1), (_, t2)) => throw SynthTypeError(s"Type mismatch: $t1, $t2")
      }
    }

    convert(expr)._1
  }

  // Returns the last state function that was defined.
  def compile(
    trace: T.Statement, cond: Term, lastFuns: (FunName, FunName), currFuns: (FunName, FunName)
  ): (FunName, FunName) = trace match {

    case T.SError => {
      eval(Assert(Implies(
        cond,
        Equals("error".id, True())
      )))
      lastFuns
    }

    case T.SSkip => lastFuns

    case T.SMkdir(path) => {
      val pathTerm = convertExpr(path)(lastFuns)
      val (lastStateFun, lastContainsFun) = lastFuns
      val (currStateFun, currContainsFun) = currFuns

      val assertNoError =
        Assert(Implies(
          cond,
          ITE(
            And(
              Equals(FunctionApplication(lastStateFun.id, Seq(pathTerm)), "Null".id),
              Equals(FunctionApplication(lastStateFun.id, Seq(
                FunctionApplication("parent".id, Seq(pathTerm))
              )), "Dir".id)
            ),
            True(),
            Equals("error".id, True())
          )
        ))

      val stateFun =
        DefineFun(FunDef(currStateFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stateSort,
          ITE(
            Equals("p".id, pathTerm),
            "Dir".id,
            FunctionApplication(lastStateFun.id, Seq("p".id))
          )
        ))

      val containsFun =
        DefineFun(FunDef(currContainsFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            "NoString".id,
            FunctionApplication(lastContainsFun.id, Seq("p".id))
          )
        ))

      eval(assertNoError)
      eval(containsFun)
      eval(stateFun)
      currFuns
    }

    case T.SCreateFile(path, cnts) => {
      val pathTerm = convertExpr(path)(lastFuns)
      val contentsTerm = convertExpr(cnts)(lastFuns)
      val (lastStateFun, lastContainsFun) = lastFuns
      val (currStateFun, currContainsFun) = currFuns

      val assertNoError =
        Assert(Implies(
          cond,
          ITE(
            And(
              Equals(FunctionApplication(lastStateFun.id, Seq(pathTerm)), "Null".id),
              Equals(FunctionApplication(lastStateFun.id, Seq(
                FunctionApplication("parent".id, Seq(pathTerm))
              )), "Dir".id)
            ),
            True(),
            Equals("error".id, True())
          )
        ))

      val stateFun =
        DefineFun(FunDef(currStateFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stateSort,
            ITE(
              Equals("p".id, pathTerm),
              "File".id,
              FunctionApplication(lastStateFun.id, Seq("p".id))
            )
        ))

      val containsFun =
        DefineFun(FunDef(currContainsFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            contentsTerm,
            FunctionApplication(lastContainsFun.id, Seq("p".id))
          )
        ))

      eval(assertNoError)
      eval(stateFun)
      eval(containsFun)
      currFuns
    }

    case T.SSeq(s1, s2) => {
      val funPrimes = compile(s1, cond, lastFuns, currFuns)
      compile(s2, cond, funPrimes, (funPrimes._1.next, funPrimes._2.next))
    }

    case T.SIf(pred, cons, alt) => {
      val predTerm = convertPred(pred)(lastFuns)
      val (currStateFun, currContainsFun) = currFuns

      val cName = (FunName(currStateFun + "Cons", 1), FunName(currContainsFun + "Cons", 1))
      val aName = (FunName(currStateFun + "Alt", 1), FunName(currContainsFun + "Alt", 1))

      val (consStateFun, consContainsFun) = compile(cons, cond && predTerm, lastFuns, cName)
      val (altStateFun, altContainsFun) = compile(alt, cond && Not(predTerm), lastFuns, aName)

      val stateFun =
        DefineFun(FunDef(currStateFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stateSort,
          ITE(
            predTerm,
            FunctionApplication(consStateFun.id, Seq("p".id)),
            FunctionApplication(altStateFun.id, Seq("p".id))
          )
        ))

      val containsFun =
        DefineFun(FunDef(currContainsFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stringSort,
          ITE(
            predTerm,
            FunctionApplication(consContainsFun.id, Seq("p".id)),
            FunctionApplication(altContainsFun.id, Seq("p".id))
          )
        ))

      eval(stateFun)
      eval(containsFun)
      currFuns
    }

    case T.SRm(path) => {
      val pathTerm = convertExpr(path)(lastFuns)
      val (lastStateFun, lastContainsFun) = lastFuns
      val (currStateFun, currContainsFun) = currFuns

      val assertNoError =
        Assert(Implies(
          cond,
          ITE(
            Or(
              Equals(FunctionApplication(lastStateFun.id, Seq(pathTerm)), "File".id),
              And(
                Equals(FunctionApplication(lastStateFun.id, Seq(pathTerm)), "Dir".id),
                Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
                  Implies(
                    Equals(FunctionApplication("parent".id, Seq("p".id)), pathTerm),
                    Equals(FunctionApplication(lastStateFun.id, Seq("p".id)), "Null".id)
                  )
                )
              )
            ),
            True(),
            Equals("error".id, True())
          )
        ))

      val stateFun =
        DefineFun(FunDef(currStateFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stateSort,
          ITE(
            Equals("p".id, pathTerm),
            "Null".id,
            FunctionApplication(lastStateFun.id, Seq("p".id))
          )
        ))

      val containsFun =
        DefineFun(FunDef(currContainsFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            "NoString".id,
            FunctionApplication(lastContainsFun.id, Seq("p".id))
          )
        ))

      eval(assertNoError)
      eval(containsFun)
      eval(stateFun)
      currFuns
    }

    case T.SCp(p1, p2) => {
      val srcTerm = convertExpr(p1)(lastFuns)
      val dstTerm = convertExpr(p2)(lastFuns)
      val (lastStateFun, lastContainsFun) = lastFuns
      val (currStateFun, currContainsFun) = currFuns

      val assertNoError =
        Assert(Implies(
          cond,
          ITE(
            And(
              Equals(FunctionApplication(lastStateFun.id, Seq(srcTerm)), "File".id),
              Equals(FunctionApplication(lastStateFun.id, Seq(dstTerm)), "Null".id)
            ),
            True(),
            Equals("error".id, True())
          )
        ))

      val stateFun =
        DefineFun(FunDef(currStateFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stateSort,
          ITE(
            Equals("p".id, dstTerm),
            FunctionApplication(lastStateFun.id, Seq(srcTerm)),
            FunctionApplication(lastStateFun.id, Seq("p".id))
          )
        ))

      val containsFun =
        DefineFun(FunDef(currContainsFun.sym, Seq(SortedVar(SSymbol("p"), pathSort)), stringSort,
            ITE(
              Equals("p".id, dstTerm),
              FunctionApplication(lastContainsFun.id, Seq(srcTerm)),
              FunctionApplication(lastContainsFun.id, Seq("p".id))
            )
        ))

      eval(assertNoError)
      eval(containsFun)
      eval(stateFun)
      currFuns
    }
  }


  def parseModel(exprs: List[SExpr]): Substitution = {
    def extractKey(str: String): Int = Integer.parseInt(
      str.substring(str.indexOf('-') + 1, str.indexOf('@'))
    )

    exprs.map {
      case DefineFun(FunDef(SSymbol(str), Seq(), sort, body)) if str.startsWith("loc") => {
        val key = extractKey(str)
        body match {
          case QualifiedIdentifier(Identifier(SSymbol(id), _), _) => Some(
            key -> {
              if (sort == pathSort) {
                FSP.CPath(FSP.JavaPath(PlusHelpers.destringifyPath(id)), key)
              } else {
                FSP.CString(strMap.original(id), key)
              }
            }
          )
          case _ => None
        }
      }
      case _ => None
    }.flatten.toMap
  }

  def synthesize(trace: T.Statement, cs: Seq[ValueConstraint], soft: Seq[Constraint]) = {
    val initialFuns = (initialState, initialContains)
    val nextFuns = (initialState.next, initialContains.next)
    val lastFuns = compile(trace, True(), initialFuns, nextFuns)
    val (lastStateN, lastContainsN) = (lastFuns._1.id, lastFuns._2.id)

    // Hard constraints...
    cs.foreach {
      // Hard path constraints
      case PathConstraint(path, st) => {
        val pathTerm = PlusHelpers.stringifyPath(path).id
        eval(Assert(Equals(
          FunctionApplication(lastStateN, Seq(pathTerm)),
          convertFileState(st)
        )))
      }

      // Hard string constraints
      case StringConstraint(path, str) => {
        val pathTerm = PlusHelpers.stringifyPath(path).id
        val strTerm = strMap.rep(str).id
        val assertion = Assert(And(
          Equals(FunctionApplication(lastStateN, Seq(pathTerm)),
            "File".id
          ),
          Equals(
            FunctionApplication(lastContainsN, Seq(pathTerm)),
            strTerm
          )
        ))
        eval(assertion)
      }
    }

    // Soft constraints...
    val counts = soft.filter {
      // Filter out "soft" constraints dealing with not-present-in-source locations.
      case PathLocationConstraint(loc, _) => loc > 0
      case StringLocationConstraint(loc, _) => loc > 0
      case _ => true
    }.map {
      // Soft path constraints
      case PathConstraint(path, st) => {
        val pathTerm = PlusHelpers.stringifyPath(path).id
        val count = freshName("count")
        eval(DeclareConst(count, IntSort()))
        eval(Assert(ITE(
          Equals(
            FunctionApplication(lastStateN, Seq(pathTerm)),
            convertFileState(st)
          ),
          Equals(count.id, SNumeral(1)),
          Equals(count.id, SNumeral(0))
        )))
        count.id
      }

      // Soft path location constraints
      case PathLocationConstraint(loc, path) => {
        val pathTerm = PlusHelpers.stringifyPath(path).id
        val count = freshName("count")
        eval(DeclareConst(count, IntSort()))
        eval(Assert(ITE(
          Equals(mkHole(loc, TPath), pathTerm),
          Equals(count.id, SNumeral(1)),
          Equals(count.id, SNumeral(0))
        )))
        count.id
      }

      // Soft string constraints
      case StringConstraint(path, str) => {
        val pathTerm = PlusHelpers.stringifyPath(path).id
        val strTerm = strMap.rep(str).id
        val count = freshName("count")
        eval(DeclareConst(count, IntSort()))
        eval(Assert(ITE(
          And(
            Equals(
              FunctionApplication(lastStateN, Seq(pathTerm)),
              "File".id
            ),
            Equals(
              FunctionApplication(lastContainsN, Seq(pathTerm)),
              strTerm
            )
          ),
          Equals(count.id, SNumeral(1)),
          Equals(count.id, SNumeral(0))
        )))
        count.id
      }

      // Soft string location constraints
      case StringLocationConstraint(loc, str) => {
        val strTerm = strMap.rep(str).id
        val count = freshName("count")
        eval(DeclareConst(count, IntSort()))
        eval(Assert(ITE(
          Equals(mkHole(loc, TString), strTerm),
          Equals(count.id, SNumeral(1)),
          Equals(count.id, SNumeral(0))
        )))
        count.id
      }
    }

    val sum = freshName("sum")
    eval(DeclareConst(sum, IntSort()))
    eval(Assert(Equals(sum.id, FunctionApplication("+".id, counts))))

    // Optimization...
    var lo = 0
    var hi = counts.length
    var res: Option[List[SExpr]] = None

    while (lo < hi) {
      smt.pushPop {
        eval(Assert(
          GreaterThan(
            sum.id,
            SNumeral((lo + hi) / 2)
          )
        ))
        if (smt.checkSat()) {
          res = Some(smt.getModel())
          lo = (lo + hi) / 2 + 1
        } else {
          hi = (lo + hi) / 2
        }
      }
    }

    res.map(model => parseModel(model))
  }
}
