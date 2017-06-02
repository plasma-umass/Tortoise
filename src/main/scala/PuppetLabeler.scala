package tortoise

import PuppetSyntax._

object PuppetLabeler {
  var loc = 0
  def freshLoc(): Int = {
    loc += 1
    loc
  }

  def nLocs(n: Int): Seq[Int] = 0.to(n).map(_ => freshLoc())

  def label(mani: Manifest): Manifest = mani match {
    case MEmpty | MResource(_, _, _) => mani
    case MAssign(id, expr, body) => MAssign(id, expr, label(body)).setLabel(freshLoc())
    case MDefine(typ, args, body) => MDefine(typ, args, label(body)).setLabels(nLocs(args.length))
    case MSeq(lhs, rhs) => MSeq(label(lhs), label(rhs))
    case MIf(pred, cons, alt) => MIf(pred, label(cons), label(alt))
  }
}
