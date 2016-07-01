package rehearsal

import edu.umass.cs.extras.Implicits._
import edu.umass.cs.smtlib._
import smtlib._
import parser._
import Commands._
import Terms._
import theories.Core.{And => _, Or => _, _}
import theories.Ints.{IntSort, Add}
import rehearsal.{FSPlusSyntax => FSP}
import rehearsal.{FSPlusTrace => T}

object TraceSymbolicEvaluator {
  type Constraint = (FSP.Const, FSP.FileState)
  type Substitution = Map[Int, FSP.Const]

  def synthUpdate(stmt: FSP.Statement, cs: Seq[Constraint], softCs: Seq[Constraint]): Option[Substitution] = {
    val impl = new TraceSymbolicEvaluator(PlusHelpers.stmtPaths(stmt).toList, Set(), Set())
    val trace = FSPlusEval.tracingEval(stmt)
    impl.synthUpdate(cs, trace, softCs)
  }
}

class TraceSymbolicEvaluator(
  allPaths: List[Path], hashes: Set[String], readOnlyPaths: Set[Path]
) extends com.typesafe.scalalogging.LazyLogging {

  case class ST(isErr: Term, paths: Map[Path, Term])

  import TraceSymbolicEvaluator.{Constraint, Substitution}
  import SMT._
  import SMT.Implicits._

  val writablePaths = allPaths.filterNot(p => readOnlyPaths.contains(p))

  val smt = SMT()
  import smt.eval

  eval(DeclareSort(SSymbol("hash"), 0))

  val hashSort = Sort(SimpleIdentifier(SSymbol("hash")))

  eval(DeclareDatatypes(Seq((SSymbol("stat"),
  Seq(Constructor(SSymbol("IsDir"), Seq()),
    Constructor(SSymbol("DoesNotExist"), Seq()),
    Constructor(SSymbol("IsFile"), Seq((SSymbol("hash"), hashSort))))))))

  val statSort = Sort(SimpleIdentifier(SSymbol("stat")))

  val readOnlyMap = {
    val ids = readOnlyPaths.map(p => {
      val z = freshName("path")
      ((p, QualifiedIdentifier(Identifier(z))), DeclareConst(z, statSort))
    })
    val (paths, cmds) = ids.unzip
    for (c <- cmds) { eval(c) }
    paths.toMap
  }

  val holes: scala.collection.mutable.Map[Int, Term] = scala.collection.mutable.Map()

  def mkHole(loc: Int): Term = holes.get(loc) match {
    case Some(term) => term
    case None => {
      val hole = freshName(s"hole-$loc")
      eval(DeclareConst(hole, statSort))
      holes + (loc -> hole)
      hole.id
    }
  }

  def assertLocIs(st: ST, loc: Int, path: Path): Unit =
    eval(Assert(Equals(mkHole(loc), st.paths(path))))

  def pathIs(st: ST, path: Path, state: FSP.FileState): Term = state match {
    case FSP.IsFile => FunctionApplication("is-IsFile".id, Seq(st.paths(path)))
    case FSP.IsDir => Equals(st.paths(path), "IsDir".id)
    case FSP.DoesNotExist => Equals(st.paths(path), "DoesNotExist".id)
  }

  def assertPathIs(st: ST, path: Path, state: FSP.FileState): Unit =
    eval(Assert(pathIs(st, path, state)))

  def freshST(): ST = {
    val ids = writablePaths.map(p => {
      val z = freshName("path")
      ((p, QualifiedIdentifier(Identifier(z))), DeclareConst(z, statSort))
    })
    val (paths, cmds) = ids.unzip
    val isErr = freshName("isErr")
    val commands = DeclareConst(isErr, BoolSort()) +: cmds
    commands.map(eval(_))
    ST(QualifiedIdentifier(Identifier(isErr)), paths.toMap ++ readOnlyMap)
  }

  def evalPred(st: ST, pred: T.Pred): Term = pred match {
    case T.PTrue => True()
    case T.PFalse => False()
    case T.PAnd(lhs, rhs) => evalPred(st, lhs) && evalPred(st, rhs)
    case T.POr(lhs, rhs) => evalPred(st, lhs) || evalPred(st, rhs)
    case T.PNot(pred) => Not(evalPred(st, pred))
    case T.PTestFileState(p, FSP.IsDir) => Equals(evalExpr(st, p), "IsDir".id)
    case T.PTestFileState(p, FSP.DoesNotExist) => Equals(evalExpr(st, p), "DoesNotExist".id)
    case T.PTestFileState(p, FSP.IsFile) => FunctionApplication("is-IsFile".id, Seq(evalExpr(st, p)))
  }

  def evalExpr(st: ST, expr: T.Expr): Term = expr match {
    case T.EHole(loc) => mkHole(loc)
    case T.EParent(e) => parentOf(evalExpr(st, e))
    case T.EIf(p, e1, e2) => ITE(evalPred(st, p), evalExpr(st, e1), evalExpr(st, e2))
  }

  def parentOf(term: Term): Term = FunctionApplication("parent".id, Seq(term))

  def evalStmt(st: ST, stmt: T.Statement): ST = stmt match {
    case T.SError => ST(True(), st.paths)
    case T.SSkip => st
    case T.SIf(p, s1, s2) => {
      val st1 = evalStmt(st, s1)
      val st2 = evalStmt(st, s2)
      val b = freshName("b")
      eval(DeclareConst(b, BoolSort()))
      ST(ite(b.id, st1.isErr, st2.isErr), writablePaths.map({
        p => (p, ite(b.id, st1.paths(p), st2.paths(p)))
      }).toMap ++ readOnlyMap)
    }
    case T.SSeq(s1, s2) => {
      val stInter = evalStmt(st, s1)
      val stInterPrime = freshST()
      eval(Assert(Equals(stInter.isErr, stInterPrime.isErr)))
      for (p <- writablePaths) {
        eval(Assert(Equals(stInter.paths(p), stInterPrime.paths(p))))
      }
      evalStmt(stInterPrime, s2)
    }
    case T.SMkdir(path) => {
      val term = evalExpr(st, path)
      var vars: Map[Term, Term] = Map()
      val paths = st.paths.map {
        case (p, t) => {
          val condPart = vars.values.foldRight(False()) {
            case (x, acc) => x || acc
          }
          val b = freshName("b")
          vars = vars + (t -> b.id)
          eval(DeclareConst(b, BoolSort()))
          val cond = Not(condPart) && b.id
          p -> ite(cond, "IsDir".id, t)
        }
      }
      val preTerm = vars.foldRight[Term]("DoesNotExist".id) {
        case ((p, t), acc) => ite(t, p, acc)
      }
      val pre = Equals(preTerm, "DoesNotExist".id) && Equals(parentOf(preTerm), "IsDir".id)
      ST(st.isErr || pre, paths)
    }
    case T.SCreateFile(path, contents) => {
      val term = evalExpr(st, path)
      var vars: Map[Term, Term] = Map()
      val paths = st.paths.map {
        case (p, t) => {
          val condPart = vars.values.foldRight(False()) {
            case (x, acc) => x || acc
          }
          val b = freshName("b")
          vars = vars + (t -> b.id)
          eval(DeclareConst(b, BoolSort()))
          val cond = Not(condPart) && b.id
          p -> ite(cond, FunctionApplication("IsFile".id, Seq(evalExpr(st, contents))), t)
        }
      }
      val preTerm = vars.foldRight(False()) {
        case ((p, t), acc) => ite(t, p, acc)
      }
      val pre = Equals(preTerm, "DoesNotExist".id) && Equals(parentOf(preTerm), "IsDir".id)
      ST(st.isErr || pre, paths)
    }

    case T.SRm(path) => {
      val term = evalExpr(st, path)
      var vars: Map[Term, Term] = Map()
      val paths = st.paths.map {
        case (p, t) => {
          val condPart = vars.values.foldRight(False()) {
            case (x, acc) => x || acc
          }
          val b = freshName("b")
          vars = vars + (t -> b.id)
          eval(DeclareConst(b, BoolSort()))
          val cond = Not(condPart) && b.id
          p -> ite(cond, "DoesNotExist".id, t)
        }
      }
      val preTerm = vars.foldRight(False()) {
        case ((p, t), acc) => ite(t, p, acc)
      }
      val pre = Not(Equals(preTerm, "DoesNotExist".id)) && Not(Equals(preTerm, False()))
      ST(st.isErr || pre, paths)
    }
    case T.SCp(src, dst) => ???
  }

  def ite(cond: Term, tru: Term, fls: Term): Term = {
    if (tru == fls) {
      tru
    }
    else {
      ITE(cond, tru, fls)
    }
  }

  def synthUpdate(cs: Seq[Constraint], trace: T.Statement, softCs: Seq[Constraint]): Option[Substitution] = smt.pushPop {
    val st = freshST()
    val term = evalStmt(st, trace)
    // Assert hard constraints...
    for ((c, fs) <- cs) {
      c match {
        case FSP.CPath(p, _) => assertPathIs(st, p.path, fs)
        case FSP.CString(_, _) => () // TODO string constants
      }
    }
    // Maximize number of unchanged paths...
    val counts = for ((c, fs) <- softCs) yield {
      c match {
        case FSP.CPath(p, _) => {
          val path = p.path
          val term = pathIs(st, path, fs)
          val held = freshName(s"held-$path")
          eval(Assert(Equals(held.id, term)))
          val count = freshName(s"count-$path")
          eval(Assert(Implies(held.id, Equals(count.id, SNumeral(1)))))
          eval(Assert(Implies(Not(held.id), Equals(count.id, SNumeral(0)))))
          count.id
        }
        case FSP.CString(_, _) => ??? // TODO string constants
      }
    }
    val sumTerm = counts.foldRight[Term](SNumeral(0)) {
      case (count, acc) => Add(count, acc)
    }
    val sum = freshName(s"sum")
    eval(DeclareConst(sum, IntSort()))
    eval(Assert(Equals(sum.id, sumTerm)))
    // TODO how do we actually use maximize in smtlib?
    if (smt.checkSat()) {
      eval(GetModel())
      ??? // TODO make substitution
    } else {
      None
    }
  }
}
