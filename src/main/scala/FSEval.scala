package pup

import CommonSyntax._
import FSSyntax._
import Infrastructure._

object FSEvalTypes {
  sealed trait Value {
    lazy val typ: String = this match {
      case VUndef => "undef"
      case VConst(CBool(_)) => "bool"
      case VConst(CNum(_)) => "num"
      case VConst(CStr(_)) => "string"
    }
  }

  case object VUndef extends Value
  case class VConst(const: Const) extends Value

  type Env = Map[String, Value]
}

object FSEval {
  import FSEvalTypes._

  def evalUnOp(op: UnOp)(implicit fs: FileSystem): Value => Value = op match {
    case UNot => operand => operand match {
      case VConst(CBool(bool)) => VConst(CBool(!bool))
      case _ => throw TypeError(operand.typ, "bool")
    }
    case UNeg => operand => operand match {
      case VConst(CNum(num)) => VConst(CNum(-num))
      case _ => throw TypeError(operand.typ, "num")
    }
    case UFile => operand => operand match {
      case VConst(CStr(path)) => VConst(CBool(fs.contains(path) && fs(path).isFile))
      case _ => throw TypeError(operand.typ, "string")
    }
    case UDir => operand => operand match {
      case VConst(CStr(path)) => VConst(CBool(fs.contains(path) && fs(path).isFile))
      case _ => throw TypeError(operand.typ, "string")
    }
    case UDefined => operand => operand match {
      case VUndef => VConst(CBool(false))
      case _ => VConst(CBool(true))
    }
  }

  def evalBinOp(op: BinOp)(implicit fs: FileSystem): Value => Value => Value = op match {
    case BAnd => lhs => rhs => (lhs, rhs) match {
      case (VConst(CBool(lhs)), VConst(CBool(rhs))) => VConst(CBool(lhs && rhs))
      case (VConst(CBool(_)), rhs) => throw TypeError(rhs.typ, "bool")
      case (lhs, rhs) => throw TypeError(lhs.typ, "bool")
    }
    case BOr => lhs => rhs => (lhs, rhs) match {
      case (VConst(CBool(lhs)), VConst(CBool(rhs))) => VConst(CBool(lhs || rhs))
      case (VConst(CBool(_)), rhs) => throw TypeError(rhs.typ, "bool")
      case (lhs, rhs) => throw TypeError(lhs.typ, "bool")
    }
    case BEq => lhs => rhs => (lhs, rhs) match {
      case (VConst(CNum(lhs)), VConst(CNum(rhs))) => VConst(CBool(lhs == rhs))
      case (VConst(CBool(lhs)), VConst(CBool(rhs))) => VConst(CBool(lhs == rhs))
      case (VConst(CStr(lhs)), VConst(CStr(rhs))) => VConst(CBool(lhs == rhs))
      case (VUndef, VUndef) => VConst(CBool(true))
      case (lhs, rhs) => throw TypeError(rhs.typ, lhs.typ)
    }
    case BLt => lhs => rhs => (lhs, rhs) match {
      case (VConst(CNum(lhs)), VConst(CNum(rhs))) => VConst(CBool(lhs < rhs))
      case (VConst(CNum(_)), rhs) => throw TypeError(rhs.typ, "num")
      case (lhs, rhs) => throw TypeError(lhs.typ, "num")
    }
    case BGt => lhs => rhs => (lhs, rhs) match {
      case (VConst(CNum(lhs)), VConst(CNum(rhs))) => VConst(CBool(lhs > rhs))
      case (VConst(CNum(_)), rhs) => throw TypeError(rhs.typ, "num")
      case (lhs, rhs) => throw TypeError(lhs.typ, "num")
    }
    case BConcat => lhs => rhs => (lhs, rhs) match {
      case (VConst(CStr(lhs)), VConst(CStr(rhs))) => VConst(CStr(lhs + rhs))
      case (VConst(CStr(_)), rhs) => throw TypeError(rhs.typ, "string")
      case (lhs, rhs) => throw TypeError(lhs.typ, "string")
    }
  }

  def evalExpr(expr: Expr, env: Env)(implicit fs: FileSystem): Value = expr match {
    case EUndef => VUndef
    case EVar(id) => env(id)
    case EConst(const) => VConst(const)
    case EUnOp(op, operand) => evalUnOp(op)(fs)(evalExpr(operand, env))
    case EBinOp(op, lhs, rhs) => evalBinOp(op)(fs)(evalExpr(lhs, env))(evalExpr(rhs, env))
  }

  def evalStatement(stmt: Statement, env: Env)(implicit fs: FileSystem): FileSystem = stmt match {
    case SSkip => fs
    case SMkdir(path) => evalExpr(path, env) match {
      case VConst(CStr(path)) => fs + (path -> Dir(None))
      case value => throw TypeError(value.typ, "string")
    }
    case SCreate(path, content) => (evalExpr(path, env), evalExpr(content, env)) match {
      case (VConst(CStr(path)), VConst(CStr(content))) => fs + (path -> File(Some(content), None))
      case (VConst(CStr(path)), VUndef) => fs + (path -> File(None, None))
      case (VConst(CStr(_)), value) => throw TypeError(value.typ, "string")
      case (value, _) => throw TypeError(value.typ, "string")
    }
    case SRm(path) => evalExpr(path, env) match {
      case VConst(CStr(path)) => fs + (path -> Nil)
      case value => throw TypeError(value.typ, "string")
    }
    case SCp(src, dst) => (evalExpr(src, env), evalExpr(dst, env)) match {
      case (VConst(CStr(src)), VConst(CStr(dst))) => fs + (dst -> fs(src))
      case (VConst(CStr(_)), value) => throw TypeError(value.typ, "string")
      case (value, _) => throw TypeError(value.typ, "string")
    }
    case SChmod(path, mode) => (evalExpr(path, env), evalExpr(mode, env)) match {
      case (VConst(CStr(path)), VConst(CStr(mode))) => fs(path) match {
        case File(content, _) => fs + (path -> File(content, Some(mode)))
        case Dir(_) => fs + (path -> Dir(Some(mode)))
        case Nil => throw EvalError(s"Cannot set the mode of undefined path $path to $mode.")
      }
      case (VConst(CStr(_)), value) => throw TypeError(value.typ, "string")
      case (value, _) => throw TypeError(value.typ, "string")
    }
    case SSeq(lhs, rhs) => evalStatement(rhs, env)(evalStatement(lhs, env))
    case SLet(id, expr, _, body) => evalStatement(body, env + (id -> evalExpr(expr, env)))
    case SIf(pred, cons, alt) => evalExpr(pred, env) match {
      case VConst(CBool(true)) => evalStatement(cons, env)
      case VConst(CBool(false)) => evalStatement(alt, env)
      case value => throw TypeError(value.typ, "bool")
    }
  }

  def eval(stmt: Statement): FileSystem = evalStatement(stmt, Map())(emptyFS)
}
