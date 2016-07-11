package rehearsal

import edu.umass.cs.smtlib._
import smtlib.parser.Commands._
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._
import rehearsal.FSPlusSyntax.{FileState, IsFile, IsDir, DoesNotExist}
import rehearsal.FSPlusSyntax.{Constraint, Substitution}
import rehearsal.{FSPlusSyntax => FSP}
import rehearsal.{FSPlusTrace => T}

object UpdateSynth {
  import FSP._

  def applySubst(stmt: Statement)(implicit subst: Substitution): Statement = stmt match {
    case SError | SSkip => stmt
    case SLet(id, e, body) => SLet(id, applySubstExpr(e), applySubst(body))
    case SIf(p, s1, s2) => SIf(applySubstPred(p), applySubst(s1), applySubst(s2))
    case SSeq(s1, s2) => SSeq(applySubst(s1), applySubst(s2))
    case SMkdir(path) => SMkdir(applySubstExpr(path))
    case SCreateFile(path, contents) => SCreateFile(applySubstExpr(path), applySubstExpr(contents))
    case SRm(path) => SRm(applySubstExpr(path))
    case SCp(src, dst) => SCp(applySubstExpr(src), applySubstExpr(dst))
  }

  def applySubstExpr(expr: Expr)(implicit subst: Substitution): Expr = expr match {
    case EId(_) => expr
    case EPath(path) => EPath(applySubstConst(path))
    case EString(str) => EString(applySubstConst(str))
    case EParent(e) => EParent(applySubstExpr(e))
    case EIf(p, e1, e2) => EIf(applySubstPred(p), applySubstExpr(e1), applySubstExpr(e2))
  }

  def applySubstPred(pred: Pred)(implicit subst: Substitution): Pred = pred match {
    case PTrue | PFalse => pred
    case PAnd(lhs, rhs) => PAnd(applySubstPred(lhs), applySubstPred(rhs))
    case POr(lhs, rhs) => POr(applySubstPred(lhs), applySubstPred(rhs))
    case PNot(pred) => PNot(applySubstPred(pred))
    case PTestFileState(path, state) => PTestFileState(applySubstExpr(path), state)
  }

  def applySubstConst(const: Const)(implicit subst: Substitution): Const = const match {
    case CPath(path, loc) if subst.contains(loc) => CPath(JavaPath(subst(loc)), loc)
    case CPath(_, _) | CString(_, _) => const
  }

  def synthesize(stmt: Statement, cs: Seq[Constraint]): Option[Statement] = {
    val paths = PlusHelpers.stmtPaths(stmt) union cs.map(_._1).toSet
    val soft = PlusHelpers.generateSoftConstraints(stmt, paths)
    val impl = new UpdateSynth(paths)
    val trace = FSPlusEval.tracingEval(stmt)
    impl.synthesize(trace, cs, soft).map {
      subst => applySubst(stmt)(subst)
    }
  }
}

/*

(* The code below is an idea about how to do this. *)

(declare-datatypes () ((Path NoPath Root Foo Bar)))
(declare-datatypes () ((State Dir File Null)))

(declare-const error Bool)
(assert (not error))

(declare-fun parent (Path) Path)
(assert (= (parent Root) NoPath))
(assert (= (parent Foo) Root))
(assert (= (parent Bar) Root))

(declare-fun state1? (Path) State)
(assert (= (state1? NoPath) Null))
(assert (= (state1? Root) Dir))
(assert (= (state1? Foo) Null))
(assert (= (state1? Bar) Null))


(declare-fun state2? (Path) State)


(declare-const loc-1 Path)
(assert (not (= loc-1 NoPath)))
(assert (forall ((p Path))
  (=>
    (not (= p loc-1))
    (= (state2? p) (state1? p))
  )
))
(assert (ite
  (and
    (= (state1? loc-1) Null)
    (= (state1? (parent loc-1)) Dir)
  )
  (= (state2? loc-1) Dir)
  (= error true)
))

(assert (= (state2? Foo) Dir))

(check-sat)
(get-model)

*/
class UpdateSynth(paths: Set[Path]) {
  import SMT._
  import SMT.Implicits._

  val smt = SMT()
  import smt.eval

  // Generate Path datatype.
  val pathSort = Sort(SimpleIdentifier(SSymbol("Path")))
  val pathCtrs = paths.toSeq.map(path => Constructor(SSymbol(PlusHelpers.stringifyPath(path)), Seq()))
  val noPathCtr = Constructor(SSymbol("NoPath"), Seq())
  eval(DeclareDatatypes(Seq(
    SSymbol("Path") -> (noPathCtr +: pathCtrs)
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

  // Generate initial state function (state1?)
  eval(DeclareFun(SSymbol("state1?"), Seq(pathSort), stateSort))
  eval(Assert(Equals(FunctionApplication("state1?".id, Seq("NoPath".id)), "Null".id)))
  for (path <- paths) {
    eval(Assert(
      Equals(
        FunctionApplication("state1?".id, Seq(PlusHelpers.stringifyPath(path).id)),
        Option(path.getParent) match {
          case Some(_) => "Null".id
          case None => "Dir".id
        }
      )
    ))
  }

  // Hole-tracking utilities
  var holes: Map[Int, Term] = Map()
  def mkHole(loc: Int): Term = holes.get(loc) match {
    case Some(term) => term
    case None => {
      val hole = freshName(s"loc-$loc@")
      eval(DeclareConst(hole, pathSort))
      eval(Assert(Not(Equals(hole.id, "NoPath".id))))
      holes = holes + (loc -> hole.id)
      hole.id
    }
  }

  def stateN(n: Int): QualifiedIdentifier = s"state$n?".id

  def declareStateN(n: Int): Command = DeclareFun(SSymbol(s"state$n?"), Seq(pathSort), stateSort)

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
        decls = decls + n
      }
      n + 1
    }
  }

  def assertTrace(trace: T.Statement): Unit = assertTrace(trace, True(), 1)

  def assertTrace(trace: T.Statement, cond: Term, n: Int): Int = trace match {
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
      eval(Assert(Equals(pred.id, convertPred(p, n))))
      val n1 = assertTrace(s1, cond && pred.id, n)
      val n2 = assertTrace(s2, cond && Not(pred.id), n)
      Math.max(n1, n2)
    }
    case T.SSeq(s1, s2) => {
      val nPrime = assertTrace(s1, cond, n)
      assertTrace(s2, cond, nPrime)
    }
    case T.SMkdir(path) => {
      val pathTerm = convertExpr(path, n)

      // Carry over untouched paths between states.
      eval(Assert(Implies(
        cond,
        Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
          Implies(
            Not(Equals("p".id, pathTerm)),
            Equals(
              FunctionApplication(stateN(n + 1), Seq("p".id)),
              FunctionApplication(stateN(n), Seq("p".id))
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
    case T.SCreateFile(path, _) => {
      val pathTerm = convertExpr(path, n)

      // Carry over untouched paths between states.
      eval(Assert(Implies(
        cond,
        Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
          Implies(
            Not(Equals("p".id, pathTerm)),
            Equals(
              FunctionApplication(stateN(n + 1), Seq("p".id)),
              FunctionApplication(stateN(n), Seq("p".id))
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
          Equals(FunctionApplication(stateN(n + 1), Seq(pathTerm)), "File".id),
          Equals("error".id, True())
        )
      )))

      n + 1
    }
    case T.SRm(path) => {
      val pathTerm = convertExpr(path, n)

      // Carry over untouched paths between states.
      eval(Assert(Implies(
        cond,
        Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
          Implies(
            Not(Equals("p".id, pathTerm)),
            Equals(
              FunctionApplication(stateN(n + 1), Seq("p".id)),
              FunctionApplication(stateN(n), Seq("p".id))
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
      // FIXME is this really the right semantics for copy? Or does it also do dirs?
      val srcTerm = convertExpr(src, n)
      val dstTerm = convertExpr(dst, n)

      // Carry over untouched paths between states.
      eval(Assert(Implies(
        cond,
        Forall(SortedVar(SSymbol("p"), pathSort), Seq(),
          Implies(
            Not(And(
              Equals("p".id, srcTerm),
              Equals("p".id, dstTerm)
            )),
            Equals(
              FunctionApplication(stateN(n + 1), Seq("p".id)),
              FunctionApplication(stateN(n), Seq("p".id))
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
          Equals(FunctionApplication(stateN(n + 1), Seq(dstTerm)), "File".id),
          Equals("error".id, True())
        )
      )))

      n + 1
    }
  }

  def convertPred(pred: T.Pred, n: Int): Term = pred match {
    case T.PTrue => True()
    case T.PFalse => False()
    case T.PAnd(lhs, rhs) => convertPred(lhs, n) && convertPred(rhs, n)
    case T.POr(lhs, rhs) => convertPred(lhs, n) || convertPred(rhs, n)
    case T.PNot(p) => Not(convertPred(p, n))
    case T.PTestFileState(path, state) => Equals(
      FunctionApplication(stateN(n), Seq(convertExpr(path, n))),
      convertFileState(state)
    )
  }

  def convertFileState(st: FileState): Term = st match {
    case IsFile => "File".id
    case IsDir => "Dir".id
    case DoesNotExist => "Null".id
  }

  def convertExpr(expr: T.Expr, n: Int): Term = expr match {
    case T.EHole(loc) => mkHole(loc)
    case T.EParent(e) => FunctionApplication("parent".id, Seq(convertExpr(e, n)))
    case T.EIf(p, e1, e2) => ITE(convertPred(p, n), convertExpr(e1, n), convertExpr(e2, n))
  }

  def parseModel(exprs: List[SExpr]): Substitution = {
    exprs.map {
      case DefineFun(FunDef(SSymbol(str), Seq(), pathSort, body)) if str.startsWith("loc") => {
        val key = Integer.parseInt(
          str.substring(str.indexOf('-') + 1, str.indexOf('@'))
        )
        body match {
          case QualifiedIdentifier(Identifier(SSymbol(id), _), _) => Some(key -> PlusHelpers.destringifyPath(id))
          case _ => None
        }
      }
      case _ => None
    }.flatten.toMap
  }

  def synthesize(trace: T.Statement, cs: Seq[Constraint], soft: Seq[Constraint]) = {
    // Initialization...
    val lastN = declareStates(trace) - 1
    assertTrace(trace)

    // Hard constraints...
    for ((path, st) <- cs) {
      val pathTerm = PlusHelpers.stringifyPath(path).id
      eval(Assert(Equals(
        FunctionApplication(stateN(lastN), Seq(pathTerm)),
        convertFileState(st)
      )))
    }

    // Soft constraints...
    val counts = for ((path, st) <- soft) yield {
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
    val sumTerm = counts.foldRight[Term](SNumeral(0)) {
      case (count, acc) => Add(count, acc)
    }
    val sum = freshName("sum")
    eval(DeclareConst(sum, IntSort()))
    eval(Assert(Equals(sum.id, sumTerm)))

    // Optimization...
    var lo = 0
    var hi = counts.length
    var res: Option[List[SExpr]] = None

    while (lo < hi) {
      smt.pushPop {
        eval(Assert(
          GreaterEquals(
            sum.id,
            SNumeral((lo + hi) / 2)
          )
        ))
        if (smt.checkSat()) {
          res = Some(smt.getModel())
          lo = (lo + hi) / 2 + 1
        } else {
          hi = (lo + hi) / 2 - 1
        }
      }
    }

    res.map(model => parseModel(model))
  }
}
