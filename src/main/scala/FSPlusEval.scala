package rehearsal

import FSPlusSyntax._
import rehearsal.{FSPlusTrace => T}

case class FSPlusEvalError(msg: String) extends RuntimeException(msg)
case object MalformedFSPlusException extends RuntimeException("Tried to evaluate a malformed FS+ program.")

object FSPlusEval {
  sealed trait FState {
    def toFileState(): FileState = this match {
      case FDir => IsDir
      case FFile(_) => IsFile
      case FEmpty => DoesNotExist
    }
  }

  case object FDir extends FState
  case class FFile(hash: String) extends FState
  case object FEmpty extends FState

  type State = Map[Path, FState]
  def emptyState: State = Map()
  def defaultState: State = Map(java.nio.file.Paths.get("/") -> FDir)

  type Env = Map[String, Const]
  def emptyEnv: Env = Map()

  type TraceEnv = Map[String, T.Expr]
  def emptyTraceEnv: TraceEnv = Map()

  def lookup(id: String, env: Env): Const = env.get(id) match {
    case Some(res) => res
    case None => throw FSPlusEvalError(s"Unbound identifier $id.")
  }

  def lookup(id: String, env: TraceEnv): T.Expr = env.get(id) match {
    case Some(res) => res
    case None => throw FSPlusEvalError(s"Unbound identifier $id.")
  }

  def lookup(p: LangPath, st: State): Option[FState] = st.get(p.path)

  def eval(stmt: Statement): Option[State] = eval(stmt, defaultState, emptyEnv)

  def eval(stmt: Statement, state: State): Option[State] = eval(stmt, state, emptyEnv)

  def eval(stmt: Statement, state: State, env: Env): Option[State] = stmt match {
    case SError => None
    case SSkip => Some(state)
    case SLet(id, e, body) => eval(body, state, env + (id -> evalExpr(e, state, env)))
    case SIf(p, s1, s2) => if (evalPred(p, state, env)) {
      eval(s1, state, env)
    } else {
      eval(s2, state, env)
    }
    case SSeq(s1, s2) => eval(s1, state, env) match {
      case Some(st) => eval(s2, st, env)
      case None => None
    }
    case SMkdir(e) => evalExpr(e, state, env) match {
      case CPath(p, loc) => lookup(p, state) match {
        case Some(FEmpty) => Some(state + (p.path -> FDir))
        case None => Some(state + (p.path -> FDir))
        case _ => None
      }
      case _ => None
    }
    case SCreateFile(e1, e2) => (evalExpr(e1, state, env), evalExpr(e2, state, env)) match {
      case (CPath(p, l1), CString(c, l2)) => lookup(p, state) match {
        case Some(FEmpty) => Some(state + (p.path -> FFile(c)))
        case None => Some(state + (p.path -> FFile(c)))
        case Some(_) => None
      }
      case _ => None
    }
    case SRm(e) => evalExpr(e, state, env) match {
      case CPath(p, loc) => lookup(p, state) match {
        case Some(FEmpty) => None
        case Some(_) => Some(state + (p.path -> FEmpty))
        case None => None
      }
      case _ => None
    }
    case SCp(e1, e2) => (evalExpr(e1, state, env), evalExpr(e2, state, env)) match {
      case (CPath(src, l1), CPath(dst, l2)) => (lookup(src, state), lookup(dst, state)) match {
        case (Some(FEmpty), _) => None
        case (None, _) => None
        case (Some(st), Some(_)) => Some(state + (dst.path -> st))
        case (Some(st), None) => Some(state + (dst.path -> st))
      }
      case _ => None
    }
  }

  def evalPred(pred: Pred, state: State, env: Env): Boolean = pred match {
    case PTrue => true
    case PFalse => false
    case PAnd(lhs, rhs) => evalPred(lhs, state, env) && evalPred(rhs, state, env)
    case POr(lhs, rhs) => evalPred(lhs, state, env) || evalPred(rhs, state, env)
    case PNot(p) => !evalPred(p, state, env)
    case PTestFileState(p, st) => evalExpr(p, state, env) match {
      case CPath(p, _) => state.get(p.path) == Some(st)
      case _ => throw MalformedFSPlusException
    }
  }

  def evalExpr(expr: Expr, state: State, env: Env): Const = expr match {
    case EId(id) => lookup(id, env)
    case EPath(path) => path
    case EString(str) => str
    case EParent(e) => evalExpr(e, state, env) match {
      case CPath(p, loc) => CPath(Parent(p), loc)
      case _ => throw MalformedFSPlusException
    }
    case EIf(p, e1, e2) => if (evalPred(p, state, env)) {
      evalExpr(e1, state, env)
    } else {
      evalExpr(e2, state, env)
    }
  }

  def tracingEval(stmt: Statement): T.Statement = tracingEval(stmt, Map())

  def tracingEval(stmt: Statement, env: TraceEnv): T.Statement = stmt match {
    case SError => T.SError
    case SSkip => T.SSkip
    case SLet(id, e, body) => tracingEval(body, env + (id -> tracingEvalExpr(e, env)))
    case SIf(p, s1, s2) => tracingEvalPred(p, env) match {
      case T.PTrue => tracingEval(s1, env)
      case T.PFalse => tracingEval(s2, env)
      case pred => T.SIf(pred, tracingEval(s1, env), tracingEval(s2, env))
    }
    case SSeq(s1, s2) => (tracingEval(s1, env), tracingEval(s2, env)) match {
      case (T.SSkip, T.SSkip) => T.SSkip
      case (T.SSkip, s) => s
      case (s, T.SSkip) => s
      case (T.SError, _) => T.SError
      case (_, T.SError) => T.SError
      case (s1, s2) => T.SSeq(s1, s2)
    }
    case SMkdir(p) => T.SMkdir(tracingEvalExpr(p, env))
    case SCreateFile(p, c) => T.SCreateFile(tracingEvalExpr(p, env), tracingEvalExpr(c, env))
    case SRm(p) => T.SRm(tracingEvalExpr(p, env))
    case SCp(src, dst) => T.SCp(tracingEvalExpr(src, env), tracingEvalExpr(dst, env))
  }

  def tracingEvalPred(pred: Pred, env: TraceEnv): T.Pred = pred match {
    case PTrue => T.PTrue
    case PFalse => T.PFalse
    case PAnd(lhs, rhs) => (tracingEvalPred(lhs, env), tracingEvalPred(rhs, env)) match {
      case (T.PFalse, _) => T.PFalse
      case (_, T.PFalse) => T.PFalse
      case (T.PTrue, p) => p
      case (p, T.PTrue) => p
      case (p, q) => T.PAnd(p, q)
    }
    case POr(lhs, rhs) => (tracingEvalPred(lhs, env), tracingEvalPred(rhs, env)) match {
      case (T.PTrue, _) => T.PTrue
      case (_, T.PTrue) => T.PTrue
      case (T.PFalse, p) => p
      case (p, T.PFalse) => p
      case (p, q) => T.POr(p, q)
    }
    case PNot(pred) => tracingEvalPred(pred, env) match {
      case T.PTrue => T.PFalse
      case T.PFalse => T.PTrue
      case p => T.PNot(p)
    }
    case PTestFileState(p, st) => T.PTestFileState(tracingEvalExpr(p, env), st)
  }

  def tracingEvalExpr(expr: Expr, env: TraceEnv): T.Expr = expr match {
    case EId(id) => lookup(id, env)
    case EPath(CPath(_, loc)) => T.EHole(loc)
    case EString(CString(_, loc)) => T.EHole(loc)
    case EPath(_) | EString(_) => throw MalformedFSPlusException
    case EParent(e) => T.EParent(tracingEvalExpr(e, env))
    case EIf(p, e1, e2) => tracingEvalPred(p, env) match {
      case T.PTrue => tracingEvalExpr(e1, env)
      case T.PFalse => tracingEvalExpr(e2, env)
      case pred => T.EIf(pred, tracingEvalExpr(e1, env), tracingEvalExpr(e2, env))
    }
  }
}
