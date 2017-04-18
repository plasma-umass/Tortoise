package pup

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
    case SSkip | SMkdir(_) | SCreate(_, _) | SRm(_) | SCp(_, _) | SChmod(_, _) => prog
    case SSeq(lhs, rhs) => SSeq(doNotEditAbstractions(lhs), doNotEditAbstractions(rhs))
    case SLet(id, vari@EVar(_), _, body) => SLet(id, vari, None, doNotEditAbstractions(body))
    case SLet(id, expr, index, body) => SLet(id, expr, index, doNotEditAbstractions(body))
    case SIf(pred, cons, alt) => SIf(pred, doNotEditAbstractions(cons), doNotEditAbstractions(alt))
  }
}
