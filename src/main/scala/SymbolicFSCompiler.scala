package pup

import edu.umass.cs.smtlib.SMT.Implicits._
import smtlib.parser.Commands._
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.theories.experimental.Strings._
import pup.{CommonSyntax => C}
import pup.{FSSyntax => F}
import SymbolicFS._

object SymbolicFSCompiler {
  def compileFileState(state: FileState): Term = state match {
    case File => file
    case Dir => dir
    case Nil => nil
  }

  def compileConst(const: C.Const): Term = const match {
    case C.CStr(str) => StringLit(str)
    case C.CNum(n) => NumeralLit(n)
    case C.CBool(b) => BoolConst(b)
  }

  def compileUnOp(op: F.UnOp, operand: Term)(implicit st: State): Term = op match {
    case F.UNot => Not(operand)
    case F.UNeg => Neg(operand)
    case F.UFile => Equals(stateHuh(operand), file)
    case F.UDir => Equals(stateHuh(operand), file)
    case F.UDefined => Equals(operand, undef)
  }

  def compileBinOp(op: F.BinOp, lhs: Term, rhs: Term)(implicit st: State): Term = op match {
    case F.BAnd => And(lhs, rhs)
    case F.BOr => Or(lhs, rhs)
    case F.BEq => Equals(lhs, rhs)
    case F.BLt => LessThan(lhs, rhs)
    case F.BGt => GreaterThan(lhs, rhs)
    case F.BConcat => Concat(lhs, rhs)
  }

  def compileExpr(expr: F.Expr)(implicit st: State): Term = expr match {
    case F.EUndef => undef
    case F.EVar(id) => id.id
    case F.EConst(c) => compileConst(c)
    case F.EUnOp(op, operand) => compileUnOp(op, compileExpr(operand))
    case F.EBinOp(op, lhs, rhs) => compileBinOp(op, compileExpr(lhs), compileExpr(rhs))
  }

  def compileStatement(
    prog: F.Statement, cond: Term, prev: Funs, curr: Funs
  ): (Commands, Funs) = prog match {
    // skip
    case F.SSkip => (Seq(), prev)

    // mkdir(p)
    case F.SMkdir(path) => {
      val pathTerm = compileExpr(path)(prev._1.num)
      val (lastStateHuh, lastContainsHuh, lastModeHuh) = prev
      val (currStateHuh, currContainsHuh, currModeHuh) = curr

      val stateHuhDef =
        DefineFun(FunDef(currStateHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stateSort,
          ITE(
            Equals("p".id, pathTerm),
            dir,
            lastStateHuh("p".id)
          )
        ))

      val containsHuhDef =
        DefineFun(FunDef(currContainsHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            StringLit(""),
            lastContainsHuh("p".id)
          )
        ))

      val modeHuhDef =
        DefineFun(FunDef(currModeHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            defaultMode,
            lastModeHuh("p".id)
          )
        ))

      (Seq(stateHuhDef, containsHuhDef, modeHuhDef), curr)
    }

    // create(p, c)
    case F.SCreate(path, contents) => {
      val pathTerm = compileExpr(path)(prev._1.num)
      val contentsTerm = compileExpr(contents)(prev._1.num)
      val (lastStateHuh, lastContainsHuh, lastModeHuh) = prev
      val (currStateHuh, currContainsHuh, currModeHuh) = curr

      val stateHuhDef =
        DefineFun(FunDef(currStateHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stateSort,
          ITE(
            Equals("p".id, pathTerm),
            file,
            lastStateHuh("p".id)
          )
        ))

      val containsHuhDef =
        DefineFun(FunDef(currContainsHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            contentsTerm,
            lastContainsHuh("p".id)
          )
        ))

      val modeHuhDef =
        DefineFun(FunDef(currModeHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            defaultMode,
            lastModeHuh("p".id)
          )
        ))

      (Seq(stateHuhDef, containsHuhDef, modeHuhDef), curr)
    }

    // rm(p)
    case F.SRm(path) => {
      val pathTerm = compileExpr(path)(prev._1.num)
      val (lastStateHuh, lastContainsHuh, lastModeHuh) = prev
      val (currStateHuh, currContainsHuh, currModeHuh) = curr

      val stateHuhDef =
        DefineFun(FunDef(currStateHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stateSort,
          ITE(
            Equals("p".id, pathTerm),
            nil,
            lastStateHuh("p".id)
          )
        ))

      val containsHuhDef =
        DefineFun(FunDef(currContainsHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            StringLit(""),
            lastContainsHuh("p".id)
          )
        ))

      val modeHuhDef =
        DefineFun(FunDef(currModeHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            defaultMode,
            lastModeHuh("p".id)
          )
        ))

      (Seq(stateHuhDef, containsHuhDef, modeHuhDef), curr)
    }

    // cp(p)
    case F.SCp(src, dst) => {
      val srcTerm = compileExpr(src)(prev._1.num)
      val dstTerm = compileExpr(dst)(prev._1.num)
      val (lastStateHuh, lastContainsHuh, lastModeHuh) = prev
      val (currStateHuh, currContainsHuh, currModeHuh) = curr

      val stateHuhDef =
        DefineFun(FunDef(currStateHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stateSort,
          ITE(
            Equals("p".id, dstTerm),
            lastStateHuh(srcTerm),
            lastStateHuh("p".id)
          )
        ))

      val containsHuhDef =
        DefineFun(FunDef(currContainsHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            Equals("p".id, dstTerm),
            lastContainsHuh(srcTerm),
            lastContainsHuh("p".id)
          )
        ))

      val modeHuhDef =
        DefineFun(FunDef(currModeHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            Equals("p".id, dstTerm),
            lastModeHuh(srcTerm),
            lastModeHuh("p".id)
          )
        ))

      (Seq(stateHuhDef, containsHuhDef, modeHuhDef), curr)
    }

    // chmod(p)
    case F.SChmod(path, mode) => {
      val pathTerm = compileExpr(path)(prev._1.num)
      val modeTerm = compileExpr(mode)(prev._1.num)
      val (lastStateHuh, lastContainsHuh, lastModeHuh) = prev
      val (currStateHuh, currContainsHuh, currModeHuh) = curr

      val stateHuhDef =
        DefineFun(FunDef(currStateHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stateSort,
          lastStateHuh("p".id)
        ))

      val containsHuhDef =
        DefineFun(FunDef(currContainsHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          lastContainsHuh("p".id)
        ))

      val modeHuhDef =
        DefineFun(FunDef(currModeHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            Equals("p".id, pathTerm),
            modeTerm,
            lastModeHuh("p".id)
          )
        ))

      (Seq(stateHuhDef, containsHuhDef, modeHuhDef), curr)
    }

    // lhs; rhs
    case F.SSeq(lhs, rhs) => {
      val (leftCmds, funPrimes) = compileStatement(lhs, cond, prev, curr)
      val (rightCmds, finalFuns) = compileStatement(rhs, cond, funPrimes, funPrimes.next)
      (leftCmds ++ rightCmds, finalFuns)
    }

    // let id = expr or label in body
    case F.SLet(id, expr, Some(label), body) => {
      val (hole, holeDefs) = makeHole(label)
      val exprTerm = compileExpr(expr)(prev._1.num)
      val variable = SSymbol(id)

      val count = SSymbol(s"count-$label")
      val countDef = DeclareConst(count, intSort)

      // There's a couple ways to do this, and I'm not 100% certain this one is the correct one.
      val assert =
        Assert(Or(
          And(Equals(count.id, NumeralLit(1)), Equals(variable.id, exprTerm)),
          And(Equals(count.id, NumeralLit(0)), Equals(variable.id, hole))
        ))

      val (bodyCmds, bodyFuns) = compileStatement(body, cond, prev, curr)

      (holeDefs ++ Seq(countDef, assert) ++ bodyCmds, bodyFuns)
    }

    // let id = expr in body
    case F.SLet(id, expr, None, body) => {
      val exprTerm = compileExpr(expr)(prev._1.num)
      val variable = SSymbol(id)

      val assert =
        Assert(Equals(variable.id, exprTerm))

      val (bodyCmds, bodyFuns) = compileStatement(body, cond, prev, curr)

      (assert +: bodyCmds, bodyFuns)
    }

    // if pred then cons else alt
    case F.SIf(pred, cons, alt) => {
      val predTerm = compileExpr(pred)(prev._1.num)
      val (currStateHuh, currContainsHuh, currModeHuh) = curr

      val consFuns = (currStateHuh.cons, currContainsHuh.cons, currModeHuh.cons)
      val altFuns = (currStateHuh.alt, currContainsHuh.alt, currModeHuh.alt)

      val (consCmds, (consStateHuh, consContainsHuh, consModeHuh)) =
        compileStatement(cons, And(cond, predTerm), prev, consFuns)

      val (altCmds, (altStateHuh, altContainsHuh, altModeHuh)) =
        compileStatement(alt, And(cond, Not(predTerm)), prev, altFuns)

      val stateHuhDef =
        DefineFun(FunDef(currStateHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stateSort,
          ITE(
            predTerm,
            consStateHuh("p".id),
            altStateHuh("p".id)
          )
        ))

      val containsHuhDef =
        DefineFun(FunDef(currContainsHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            predTerm,
            consContainsHuh("p".id),
            altContainsHuh("p".id)
          )
        ))

      val modeHuhDef =
        DefineFun(FunDef(currModeHuh.sym, Seq(SortedVar(SSymbol("p"), stringSort)), stringSort,
          ITE(
            predTerm,
            consModeHuh("p".id),
            altModeHuh("p".id)
          )
        ))

      (consCmds ++ altCmds ++ Seq(stateHuhDef, containsHuhDef, modeHuhDef), curr)
    }
  }
}
