package pup

import PuppetSyntax._

object PuppetVisitors {
  def size(manifest: Manifest): Int = manifest match {
    case MEmpty => 0
    case MAssign(_, _, body) => size(body)
    case MResource(_, _, _) => 1
    case MDefine(_, _, body) => size(body)
    case MSeq(lhs, rhs) => size(lhs) + size(rhs)
    case MIf(_, cons, alt) => size(cons) + size(alt)
  }
}
