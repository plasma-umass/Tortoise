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

  def synthesize(stmt: Statement, cs: Seq[ValueConstraint]): Option[Substitution] = {

    val (paths, strings) = PlusHelpers.calculateConsts(stmt)
    val pathCs = cs.filter(_.isInstanceOf[PathConstraint]).map(_.asInstanceOf[PathConstraint])
    val stringCs = cs.filter(_.isInstanceOf[StringConstraint]).map(_.asInstanceOf[StringConstraint])
    val constraintPaths = {
      pathCs.flatMap(c => Seq(c.path) ++ c.path.ancestors).toSet ++
      stringCs.flatMap(c => Seq(c.path) ++ c.path.ancestors).toSet
    }

    val basePaths = constraintPaths ++ paths ++ Settings.assumedDirs
    val allStrings = stringCs.map(_.contents).toSet ++ Set("") ++ strings

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
    impl.synthesize(trace, cs, soft)
    println("Syntheseis complete.")
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
  val pathCtrs = paths.toSeq.map(path => Constructor(SSymbol(PlusHelpers.stringifyPath(path)), Seq()))
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
  eval(DeclareFun(SSymbol("parent"), Seq(pathSort), pathSort))
  for (path <- paths) {
    eval(Assert(
      Equals(
        FunctionApplication("parent".id, Seq(PlusHelpers.stringifyPath(path).id)),
        Option(path.getParent) match {
          case Some(parent) => PlusHelpers.stringifyPath(parent).id
          case None => "NoPath".id
        }
      )
    ))
  }

  // Generate path concatenation function.
  val concatPaths = "cat-paths".id
  eval(DeclareFun(SSymbol("cat-paths"), Seq(pathSort, pathSort), pathSort))
  for (p1 <- paths; p2 <- paths) {
    if (paths.contains(p1 concat p2)) {
      eval(Assert(
        Equals(
          FunctionApplication(concatPaths, Seq(
            PlusHelpers.stringifyPath(p1).id, PlusHelpers.stringifyPath(p2).id
          )),
          PlusHelpers.stringifyPath(p1 concat p2).id
        )
      ))
    } else {
      eval(Assert(
        Equals(
          FunctionApplication(concatPaths, Seq(
            PlusHelpers.stringifyPath(p1).id, PlusHelpers.stringifyPath(p2).id
          )),
          "NoPath".id
        )
      ))
    }
  }

  // Generate string concatenation function.
  val concatStrings = "cat-strings".id
  eval(DeclareFun(SSymbol("cat-strings"), Seq(stringSort, stringSort), stringSort))
  for (s1 <- strings; s2 <- strings) {
    if (strings.contains(s1 + s2)) {
      eval(Assert(
        Equals(
          FunctionApplication(concatStrings, Seq(strMap.rep(s1).id, strMap.rep(s2).id)),
          strMap.rep(s1 + s2).id
        )
      ))
    } else {
      eval(Assert(
        Equals(
          FunctionApplication(concatStrings, Seq(strMap.rep(s1).id, strMap.rep(s2).id)),
          "NoString".id
        )
      ))
    }
  }

  // Generate initial state function (state1?)
  val initialState = FunName("state", 1)
  eval(DeclareFun(initialState.sym, Seq(pathSort), stateSort))

  eval(Assert(Equals(FunctionApplication(initialState.id, Seq("NoPath".id)), "Null".id)))
  for (path <- paths) {
    eval(Assert(
      Equals(
        FunctionApplication(initialState.id, Seq(PlusHelpers.stringifyPath(path).id)),
        defaultFS.get(path).map(convertFileState(_)).getOrElse("Null".id)
      )
    ))
  }

  // Generate initial contains function (contains1?)
  val initialContains = FunName("contains", 1)
  eval(DeclareFun(initialContains.sym, Seq(pathSort), stringSort))
  for (path <- paths) {
    eval(Assert(
      Equals(
        FunctionApplication(initialContains.id, Seq(PlusHelpers.stringifyPath(path).id)),
        defaultFS.get(path).map {
          case IsFile(str) => strMap.rep(str).id
          case _ => strMap.rep("").id
        }.getOrElse(strMap.rep("").id)
      )
    ))
  }

  // Hole-tracking utilities
  var holes: Map[Int, Term] = Map()
  def mkHole(loc: Int, typ: Type): Term = holes.get(loc) match {
    case _ if loc == -1 => {
      val fresh = freshName("not-present")
      eval(DeclareConst(fresh, typ match {
        case TPath => pathSort
        case TString => stringSort
      }))
      fresh.id
    }
    case Some(term) => term
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
  def defineFuns(trace: T.Statement, cond: Term,
    lastFuns: (FunName, FunName),
    currFuns: (FunName, FunName)): (FunName, FunName) = trace match {
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
      // Need to do this, otherwise if doesn't work.
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
      val funPrimes = defineFuns(s1, cond, lastFuns, currFuns)
      defineFuns(s2, cond, funPrimes, (funPrimes._1.next, funPrimes._2.next))
    }
    case T.SIf(pred, cons, alt) => {
      val predTerm = convertPred(pred)(lastFuns)
      val (currStateFun, currContainsFun) = currFuns

      val cName = (FunName(currStateFun + "Cons", 1), FunName(currContainsFun + "Cons", 1))
      val aName = (FunName(currStateFun + "Alt", 1), FunName(currContainsFun + "Alt", 1))

      val (consStateFun, consContainsFun) = defineFuns(cons, cond && predTerm, lastFuns, cName)
      val (altStateFun, altContainsFun) = defineFuns(alt, cond && Not(predTerm), lastFuns, aName)

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
      // FIXME(rachit): I think these semantics are wrong. Once the copy operation is complete,
      // there is no relation between src and dst
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
    val lastFuns = defineFuns(trace, True(), initialFuns, nextFuns)
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
