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


  def synthesize(stmt: Statement, cs: Seq[ValueConstraint]): Option[Substitution] = {

    val (paths, strings) = PlusHelpers.calculateConsts(stmt)
    val pathCs = cs.filter(_.isInstanceOf[PathConstraint]).map(_.asInstanceOf[PathConstraint])
    val stringCs = cs.filter(_.isInstanceOf[StringConstraint]).map(_.asInstanceOf[StringConstraint])
    val constraintPaths = {
      pathCs.flatMap(c => Seq(c.path) ++ c.path.ancestors).toSet ++
      stringCs.flatMap(c => Seq(c.path) ++ c.path.ancestors).toSet
    }

    val basePaths = constraintPaths ++ paths
    val allStrings = stringCs.map(_.contents).toSet ++ Set("") ++ strings

    val allPaths: Set[Path] = basePaths.flatMap { path =>
        val conts = path.iterator.toList
        (1 until conts.size).flatMap{ i =>
          val lst = conts.toList.sliding(i).toList
          val mlst = lst.map(_.reduce { _ concat  _})
          println(s"for $path: $mlst")
          mlst
        } ++ basePaths
    }

    // Calculate soft constraints
    val softVals = PlusHelpers.generateSoftValueConstraints(stmt, paths)
    val softLocs = PlusHelpers.generateSoftLocationConstraints(stmt)

    val soft = softVals union softLocs
    val impl = new UpdateSynth(allPaths, allStrings)

    val trace = FSPlusEval.tracingEval(stmt)
    impl.synthesize(trace, cs, soft)
  }
}

class UpdateSynth(paths: Set[Path], strings: Set[String]) {
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
  eval(DeclareDatatypes(Seq(
    SSymbol("String") -> stringCtrs
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
    }
  }

  // Generate initial state function (state1?)
  eval(DeclareFun(SSymbol("state1?"), Seq(pathSort), stateSort))

  eval(Assert(Equals(FunctionApplication("state1?".id, Seq("NoPath".id)), "Null".id)))
  for (path <- paths) {
    eval(Assert(
      Equals(
        FunctionApplication("state1?".id, Seq(PlusHelpers.stringifyPath(path).id)),
        if (path == java.nio.file.Paths.get("/")) {
          "Dir".id
        } else {
          "Null".id
        }
      )
    ))
  }

  // Generate initial contains function (contains1?)
  eval(DeclareFun(SSymbol("contains1?"), Seq(pathSort), stringSort))
  for (path <- paths) {
    eval(Assert(
      Equals(
        FunctionApplication("contains1?".id, Seq(PlusHelpers.stringifyPath(path).id)),
        strMap.rep("").id
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
      if (typ == TPath) {
        eval(Assert(Not(Equals(hole.id, "NoPath".id))))
      }
      holes = holes + (loc -> hole.id)
      hole.id
    }
  }

  def stateN(n: Int): QualifiedIdentifier = s"state$n?".id
  def containsN(n: Int): QualifiedIdentifier = s"contains$n?".id

  def declareStateN(n: Int): Command = DeclareFun(SSymbol(s"state$n?"), Seq(pathSort), stateSort)
  def declareContainsN(n: Int): Command = DeclareFun(
    SSymbol(s"contains$n?"), Seq(pathSort), stringSort
  )

  def declareStates(trace: T.Statement): Int = declareStates(trace, 2)

  var decls: Set[Int] = Set()
  def declareStates(trace: T.Statement, n: Int): Int = trace match {
    case T.SError => n
    case T.SSkip => n
    case T.SIf(_, s1, s2) => {
      val n1 = declareStates(s1, n)
      val n2 = declareStates(s2, n)
      Math.max(n1, n2)
    }
    case T.SSeq(s1, s2) => {
      val nPrime = declareStates(s1, n)
      declareStates(s2, nPrime)
    }
    case T.SMkdir(_) | T.SCreateFile(_, _) | T.SRm(_) | T.SCp(_, _) => {
      if (!decls.contains(n)) {
        eval(declareStateN(n))
        eval(declareContainsN(n))
        decls = decls + n
      }
      n + 1
    }
  }

  def assertTrace(trace: T.Statement): Unit = assertTrace(trace, True())(1)

  def assertTrace(trace: T.Statement, cond: Term)(implicit n: Int): Int = trace match {
    case T.SError => {
      eval(Assert(Implies(
        cond,
        Equals("error".id, True())
      )))
      n
    }
    case T.SSkip => n
    case T.SIf(p, s1, s2) => {
      val pred = freshName("pred")
      eval(DeclareConst(pred, BoolSort()))
      eval(Assert(Equals(pred.id, convertPred(p))))
      val n1 = assertTrace(s1, cond && pred.id)
      val n2 = assertTrace(s2, cond && Not(pred.id))
      if (n1 > n2) {
        for (nPrime <- n2 until n1) {
          // Carry over all paths between simulated states.
          eval(Assert(Implies(
            cond && Not(pred.id),
            Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
              And(
                Equals(
                  FunctionApplication(stateN(nPrime + 1), Seq("p".id)),
                  FunctionApplication(stateN(nPrime), Seq("p".id))
                ),
                Equals(
                  FunctionApplication(containsN(nPrime + 1), Seq("p".id)),
                  FunctionApplication(containsN(nPrime), Seq("p".id))
                )
              )
            )
          )))
        }
        n1
      } else if (n2 > n1) {
        for (nPrime <- n1 until n2) {
          // Carry over all paths between simulated states.
          eval(Assert(Implies(
            cond && pred.id,
            Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
              And(
                Equals(
                    FunctionApplication(stateN(nPrime + 1), Seq("p".id)),
                    FunctionApplication(stateN(nPrime), Seq("p".id))
                ),
                Equals(
                  FunctionApplication(containsN(nPrime + 1), Seq("p".id)),
                  FunctionApplication(containsN(nPrime), Seq("p".id))
                )
              )
            )
          )))
        }
        n2
      } else {
        n1
      }
    }
    case T.SSeq(s1, s2) => {
      val nPrime = assertTrace(s1, cond)
      assertTrace(s2, cond)(nPrime)
    }
    case T.SMkdir(path) => {
      val pathTerm = convertExpr(path)

      // Carry over untouched paths between states.
      eval(Assert(Implies(
        cond,
        Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
          And(
            Implies(
              Not(Equals("p".id, pathTerm)),
              Equals(
                FunctionApplication(stateN(n + 1), Seq("p".id)),
                FunctionApplication(stateN(n), Seq("p".id))
              )
            ),
            Equals(
              FunctionApplication(containsN(n + 1), Seq("p".id)),
              FunctionApplication(containsN(n), Seq("p".id))
            )
          )
        )
      )))

      // Semantics of mkdir.
      eval(Assert(Implies(
        cond,
        ITE(
          And(
            Equals(FunctionApplication(stateN(n), Seq(pathTerm)), "Null".id),
            Equals(FunctionApplication(stateN(n), Seq(
              FunctionApplication("parent".id, Seq(pathTerm))
            )), "Dir".id)
          ),
          Equals(FunctionApplication(stateN(n + 1), Seq(pathTerm)), "Dir".id),
          Equals("error".id, True())
        )
      )))

      n + 1
    }
    case T.SCreateFile(path, contents) => {
      val pathTerm = convertExpr(path)
      val contentsTerm = convertExpr(contents)

      // Carry over untouched paths between states.
      eval(Assert(Implies(
        cond,
        Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
          Implies(
            Not(Equals("p".id, pathTerm)),
            And(
              Equals(
                FunctionApplication(stateN(n + 1), Seq("p".id)),
                FunctionApplication(stateN(n), Seq("p".id))
              ),
              Equals(
                FunctionApplication(containsN(n + 1), Seq("p".id)),
                FunctionApplication(containsN(n), Seq("p".id))
              )
            )
          )
        )
      )))

      // Semantics of createfile.
      eval(Assert(Implies(
        cond,
        ITE(
          And(
            Equals(FunctionApplication(stateN(n), Seq(pathTerm)), "Null".id),
            Equals(FunctionApplication(stateN(n), Seq(
              FunctionApplication("parent".id, Seq(pathTerm))
            )), "Dir".id)
          ),
          And(
            Equals(FunctionApplication(stateN(n + 1), Seq(pathTerm)), "File".id),
            Equals(FunctionApplication(containsN(n + 1), Seq(pathTerm)), contentsTerm)
          ),
          Equals("error".id, True())
        )
      )))

      n + 1
    }
    case T.SRm(path) => {
      val pathTerm = convertExpr(path)

      // Carry over untouched paths between states.
      eval(Assert(Implies(
        cond,
        Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
          And(
            Implies(
              Not(Equals("p".id, pathTerm)),
              Equals(
                FunctionApplication(stateN(n + 1), Seq("p".id)),
                FunctionApplication(stateN(n), Seq("p".id))
              )
            ),
            Equals(
              FunctionApplication(containsN(n + 1), Seq("p".id)),
              FunctionApplication(containsN(n), Seq("p".id))
            )
          )
        )
      )))

      // Semantics of rm.
      eval(Assert(Implies(
        cond,
        ITE(
          Or(
            Equals(FunctionApplication(stateN(n), Seq(pathTerm)), "File".id),
            And(
              Equals(FunctionApplication(stateN(n), Seq(pathTerm)), "Dir".id),
              Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
                Implies(
                  Equals(FunctionApplication("parent".id, Seq("p".id)), pathTerm),
                  Equals(FunctionApplication(stateN(n), Seq("p".id)), "Null".id)
                )
              )
            )
          ),
          Equals(FunctionApplication(stateN(n + 1), Seq(pathTerm)), "Null".id),
          Equals("error".id, True())
        )
      )))

      n + 1
    }
    case T.SCp(src, dst) => {
      val srcTerm = convertExpr(src)
      val dstTerm = convertExpr(dst)

      // Carry over untouched paths between states.
      eval(Assert(Implies(
        cond,
        Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
          Implies(
            Not(Equals("p".id, dstTerm)),
            And(
              Equals(
                FunctionApplication(stateN(n + 1), Seq("p".id)),
                FunctionApplication(stateN(n), Seq("p".id))
              ),
              Equals(
                FunctionApplication(containsN(n + 1), Seq("p".id)),
                FunctionApplication(containsN(n), Seq("p".id))
              )
            )
          )
        )
      )))

      // Semantics of cp.
      eval(Assert(Implies(
        cond,
        ITE(
          And(
            Equals(FunctionApplication(stateN(n), Seq(srcTerm)), "File".id),
            Equals(FunctionApplication(stateN(n), Seq(dstTerm)), "Null".id)
          ),
          And(
            Equals(FunctionApplication(stateN(n + 1), Seq(dstTerm)), "File".id),
            Equals(
              FunctionApplication(containsN(n + 1), Seq(dstTerm)),
              FunctionApplication(containsN(n), Seq(srcTerm))
            )
          ),
          Equals("error".id, True())
        )
      )))

      n + 1
    }
  }

  def convertPred(pred: T.Pred)(implicit n: Int): Term = pred match {
    case T.PTrue => True()
    case T.PFalse => False()
    case T.PAnd(lhs, rhs) => convertPred(lhs) && convertPred(rhs)
    case T.POr(lhs, rhs) => convertPred(lhs) || convertPred(rhs)
    case T.PNot(p) => Not(convertPred(p))
    case T.PTestFileState(path, state) => Equals(
      FunctionApplication(stateN(n), Seq(convertExpr(path))),
      convertFileState(state)
    )
    case T.PTestFileContains(path, cts) => Equals(
      FunctionApplication(containsN(n), Seq(convertExpr(path))),
      convertExpr(cts)
    )
  }

  def convertFileState(st: FileState): Term = st match {
    case IsFile(_) => "File".id
    case IsDir => "Dir".id
    case DoesNotExist => "Null".id
  }

  def convertExpr(expr: T.Expr)(implicit n: Int): Term = {

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
    // Initialization...
    val lastN = declareStates(trace) - 1
    assertTrace(trace)

    // Hard constraints...
    cs.foreach {
      // Hard path constraints
      case PathConstraint(path, st) => {
        val pathTerm = PlusHelpers.stringifyPath(path).id
        eval(Assert(Equals(
          FunctionApplication(stateN(lastN), Seq(pathTerm)),
          convertFileState(st)
        )))
      }

      // Hard string constraints
      case StringConstraint(path, str) => {
        val pathTerm = PlusHelpers.stringifyPath(path).id
        val strTerm = strMap.rep(str).id
        eval(Assert(Equals(
          FunctionApplication(containsN(lastN), Seq(pathTerm)),
          strTerm
        )))
      }
    }

    // Soft constraints...
    val counts = soft.map {
      // Soft path constraints
      case PathConstraint(path, st) => {
        val pathTerm = PlusHelpers.stringifyPath(path).id
        val count = freshName("count")
        eval(DeclareConst(count, IntSort()))
        eval(Assert(ITE(
          Equals(
            FunctionApplication(stateN(lastN), Seq(pathTerm)),
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
          Equals(
            FunctionApplication(containsN(lastN), Seq(pathTerm)),
            strTerm
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
