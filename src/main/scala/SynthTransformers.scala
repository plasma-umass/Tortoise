package tortoise

import CommonSyntax._
import FSSyntax._

/**
  * This module will contain a number of AST transformations for FS++ programs that will change the
  * kinds of updates that the synthesizer will produce. Not all of these are guaranteed to be
  * useful, but this is a site of experimentation for the synthesis procedure.
  */
object SynthTransformers {
  type Transformer = Statement => Statement

  def identity(prog: Statement): Statement = prog

  def doNotEditAbstractions(prog: Statement): Statement = prog match {
    case SMkdir(_) | SCreate(_, _) | SRm(_) | SCp(_, _) | SChmod(_, _) | SChown(_, _) => prog
    case SSkip => SSkip
    case SSeq(lhs, rhs) => SSeq(doNotEditAbstractions(lhs), doNotEditAbstractions(rhs))
    case SLet(id, vari@EVar(_), _, body) => SLet(id, vari, None, doNotEditAbstractions(body))
    case SLet(id, expr, index, body) => SLet(id, expr, index, doNotEditAbstractions(body))
    case SIf(pred, cons, alt) => SIf(pred, doNotEditAbstractions(cons), doNotEditAbstractions(alt))
  }

  def onlyEditAbstractions(prog: Statement): Statement = prog match {
    case SMkdir(_) | SCreate(_, _) | SRm(_) | SCp(_, _) | SChmod(_, _) | SChown(_, _) => prog
    case SSkip => SSkip
    case SSeq(lhs, rhs) => SSeq(onlyEditAbstractions(lhs), onlyEditAbstractions(rhs))
    case SLet(id, vari@EVar(_), index, body) => SLet(id, vari, index, onlyEditAbstractions(body))
    case SLet(id, expr, _, body) => SLet(id, expr, None, onlyEditAbstractions(body))
    case SIf(pred, cons, alt) => SIf(pred, onlyEditAbstractions(cons), onlyEditAbstractions(alt))
  }

  def onlyEditPaths(paths: Set[String])(prog: Statement): Statement = prog match {
    case SMkdir(_) | SCreate(_, _) | SRm(_) | SCp(_, _) | SChmod(_, _) | SChown(_, _) => prog
    case SSkip => SSkip
    case SSeq(lhs, rhs) => SSeq(onlyEditPaths(paths)(lhs), onlyEditPaths(paths)(rhs))
    // All concatenations remain edit sites. It may make sense to change this.
    case SLet(id, e@EBinOp(BConcat, _, _), index, body) => {
      SLet(id, e, index, onlyEditPaths(paths)(body))
    }
    case SLet(id, EConst(CStr(path)), index, body) if paths.contains(path) => {
      SLet(id, EConst(CStr(path)), index, onlyEditPaths(paths)(body))
    }
    case SLet(id, expr, _, body) => SLet(id, expr, None, onlyEditPaths(paths)(body))
    case SIf(pred, cons, alt) => SIf(pred, onlyEditPaths(paths)(cons), onlyEditPaths(paths)(alt))
  }

  def doNotEditPaths(paths: Set[String])(prog: Statement): Statement = prog match {
    case SMkdir(_) | SCreate(_, _) | SRm(_) | SCp(_, _) | SChmod(_, _) | SChown(_, _) => prog
    case SSkip => SSkip
    case SSeq(lhs, rhs) => SSeq(doNotEditPaths(paths)(lhs), doNotEditPaths(paths)(rhs))
    case SLet(id, vari@EVar(_), _, body) => SLet(id, vari, None, doNotEditPaths(paths)(body))
    case SLet(id, EConst(CStr(path)), _, body) if paths.contains(path) => {
      SLet(id, EConst(CStr(path)), None, doNotEditPaths(paths)(body))
    }
    case SLet(id, expr, index, body) => SLet(id, expr, index, doNotEditPaths(paths)(body))
    case SIf(pred, cons, alt) => SIf(pred, doNotEditPaths(paths)(cons), doNotEditPaths(paths)(alt))
  }
}
