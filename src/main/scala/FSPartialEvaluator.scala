package pup

import CommonSyntax._
import FSSyntax._

/**
  * Partially evaluates programs written in FS++.
  *
  * NOTE: this is a somewhat unusual notion of partial evaluation in that it intentionally does not
  * propagate bindings that could be replaced during synthesis. This is to maintain program
  * equivalence under synthesis.
  */
object FSPartialEvaluator {
  type Env = Map[String, Const]

  def evalExpr(expr: Expr)(implicit env: Env): Expr = expr match {
    case EVar(id) if env.contains(id) => EConst(env(id))
    case EVar(_) | EUndef | EConst(_) => expr
    case EUnOp(op, operand) => (op, evalExpr(operand)) match {
      case (UNot, EConst(CBool(bool))) => EConst(CBool(!bool))
      case (UNeg, EConst(CNum(n))) => EConst(CNum(-n))
      case (UDefined, EConst(_)) => EConst(CBool(true))
      case (UDefined, EUndef) => EConst(CBool(false))

      // Cases with type errors.
      case (UNot, EConst(CNum(_))) => throw TypeError("num", "bool")
      case (UNot, EConst(CStr(_))) => throw TypeError("string", "bool")
      case (UNeg, EConst(CBool(_))) => throw TypeError("bool", "num")
      case (UNeg, EConst(CStr(_))) => throw TypeError("string", "num")

      // Cases that cannot be evaluated further.
      case (_, _) => expr
    }
    case EBinOp(op, lhs, rhs) => (op, evalExpr(lhs), evalExpr(rhs)) match {
      // Cases for boolean and.
      case (BAnd, EConst(CBool(lhs)), EConst(CBool(rhs))) => EConst(CBool(lhs && rhs))
      case (BAnd, EConst(CBool(true)), right) => right
      case (BAnd, left, EConst(CBool(true))) => left
      case (BAnd, EConst(CBool(false)), right) => EConst(CBool(false))
      case (BAnd, left, EConst(CBool(false))) => EConst(CBool(false))

      // Cases for boolean and with type errors.
      case (BAnd, EConst(CNum(_)), _) => throw TypeError("num", "bool")
      case (BAnd, _, EConst(CNum(_))) => throw TypeError("num", "bool")
      case (BAnd, EConst(CStr(_)), _) => throw TypeError("string", "bool")
      case (BAnd, _, EConst(CStr(_))) => throw TypeError("string", "bool")

      // Cases for boolean or.
      case (BOr, EConst(CBool(lhs)), EConst(CBool(rhs))) => EConst(CBool(lhs || rhs))
      case (BOr, EConst(CBool(true)), right) => EConst(CBool(true))
      case (BOr, left, EConst(CBool(true))) => EConst(CBool(true))
      case (BOr, EConst(CBool(false)), right) => right
      case (BOr, left, EConst(CBool(false))) => left

      // Cases for boolean or with type errors.
      case (BOr, EConst(CNum(_)), _) => throw TypeError("num", "bool")
      case (BOr, _, EConst(CNum(_))) => throw TypeError("num", "bool")
      case (BOr, EConst(CStr(_)), _) => throw TypeError("string", "bool")
      case (BOr, _, EConst(CStr(_))) => throw TypeError("string", "bool")

      // Cases for generic equality.
      case (BEq, EConst(CBool(lhs)), EConst(CBool(rhs))) => EConst(CBool(lhs == rhs))
      case (BEq, EConst(CNum(lhs)), EConst(CNum(rhs))) => EConst(CBool(lhs == rhs))
      case (BEq, EConst(CStr(lhs)), EConst(CStr(rhs))) => EConst(CBool(lhs == rhs))

      // Cases for equality with type errors.
      case (BEq, EConst(CBool(_)), EConst(CNum(_))) => throw TypeError("num", "bool")
      case (BEq, EConst(CBool(_)), EConst(CStr(_))) => throw TypeError("string", "bool")
      case (BEq, EConst(CNum(_)), EConst(CBool(_))) => throw TypeError("bool", "num")
      case (BEq, EConst(CNum(_)), EConst(CStr(_))) => throw TypeError("string", "num")
      case (BEq, EConst(CStr(_)), EConst(CBool(_))) => throw TypeError("bool", "string")
      case (BEq, EConst(CStr(_)), EConst(CNum(_))) => throw TypeError("num", "string")

      // Cases for less than.
      case (BLt, EConst(CNum(lhs)), EConst(CNum(rhs))) => EConst(CBool(lhs < rhs))

      // Cases for less than with type errors.
      case (BLt, EConst(CBool(_)), _) => throw TypeError("bool", "num")
      case (BLt, _, EConst(CBool(_))) => throw TypeError("bool", "num")
      case (BLt, EConst(CStr(_)), _) => throw TypeError("string", "num")
      case (BLt, _, EConst(CStr(_))) => throw TypeError("string", "num")

      // Cases for greater than.
      case (BGt, EConst(CNum(lhs)), EConst(CNum(rhs))) => EConst(CBool(lhs > rhs))

      // Cases for greater than with type errors.
      case (BGt, EConst(CBool(_)), _) => throw TypeError("bool", "num")
      case (BGt, _, EConst(CBool(_))) => throw TypeError("bool", "num")
      case (BGt, EConst(CStr(_)), _) => throw TypeError("string", "num")
      case (BGt, _, EConst(CStr(_))) => throw TypeError("string", "num")

      // Cases for string concatenation.
      case (BConcat, EConst(CStr(lhs)), EConst(CStr(rhs))) => EConst(CStr(lhs + rhs))

      case (BConcat, EConst(CStr(lhs)), EBinOp(BConcat, lhsExpr, rhs)) => evalExpr(lhsExpr) match {
        case EConst(CStr(lhs2)) => EConst(CStr(lhs + lhs2))
        case EConst(CBool(_)) => throw TypeError("bool", "string")
        case EConst(CNum(_)) => throw TypeError("num", "string")
        case lhsPrime => EBinOp(BConcat, EConst(CStr(lhs)), EBinOp(BConcat, lhsPrime, evalExpr(rhs)))
      }

      case (BConcat, EBinOp(BConcat, lhs, rhsExpr), EConst(CStr(rhs))) => evalExpr(rhsExpr) match {
        case EConst(CStr(rhs2)) => EConst(CStr(rhs + rhs2))
        case EConst(CBool(_)) => throw TypeError("bool", "string")
        case EConst(CNum(_)) => throw TypeError("num", "string")
        case rhsPrime => EBinOp(BConcat, EBinOp(BConcat, evalExpr(lhs), rhsPrime), EConst(CStr(rhs)))
      }

      // Cases for string concatenation with type errors.
      case (BConcat, EConst(CBool(_)), _) => throw TypeError("bool", "string")
      case (BConcat, _, EConst(CBool(_))) => throw TypeError("bool", "string")
      case (BConcat, EConst(CNum(_)), _) => throw TypeError("num", "string")
      case (BConcat, _, EConst(CNum(_))) => throw TypeError("num", "string")

      // Cases that cannot be evaluated further.
      case (_, _, _) => expr
    }
  }

  def evalStatement(stmt: Statement)(implicit env: Env): Statement = stmt match {
    case SSkip => SSkip
    case SMkdir(path) => SMkdir(evalExpr(path))
    case SCreate(path, contents) => SCreate(evalExpr(path), evalExpr(contents))
    case SRm(path) => SRm(evalExpr(path))
    case SCp(src, dst) => SCp(evalExpr(src), evalExpr(dst))
    case SChmod(path, mode) => SChmod(evalExpr(path), evalExpr(mode))
    case SChown(path, owner) => SChown(evalExpr(path), evalExpr(owner))
    case SSeq(lhs, rhs) => (evalStatement(lhs), evalStatement(rhs)) match {
      case (SSkip, rhs) => rhs
      case (lhs, SSkip) => lhs
      case (lhs, rhs) => SSeq(lhs, rhs)
    }
    case SLet(id, expr, index@Some(_), body) => SLet(id, evalExpr(expr), index, evalStatement(body))
    case SLet(id, expr, None, body) => evalExpr(expr) match {
      case EConst(const) => evalStatement(body)(env + (id -> const))
      case expr => SLet(id, expr, None, evalStatement(body))
    }
    case SIf(pred, cons, alt) => evalExpr(pred) match {
      case EConst(CBool(true)) => evalStatement(cons)
      case EConst(CBool(false)) => evalStatement(alt)
      case EConst(CNum(_)) => throw TypeError("num", "bool")
      case EConst(CStr(_)) => throw TypeError("string", "bool")
      case pred => SIf(pred, evalStatement(cons), evalStatement(alt))
    }
  }

  def eval(stmt: Statement): Statement = evalStatement(stmt)(Map())
}
