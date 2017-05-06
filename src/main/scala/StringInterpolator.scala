package pup

import CommonSyntax._
import PuppetSyntax._

object StringInterpolator {
  val validId = {
    Set('-', '_', ':') union ('a' to 'z').toSet union ('A' to 'Z').toSet union ('0' to '9').toSet
  }

  def interpolateString(str: String): Expr = {
    def nonEmpty(expr: Expr): Boolean = expr match {
      case EConst(CStr(s)) => s.length > 0
      case EVar(id) => id.length > 0
      case _ => true
    }

    // Split strings into individual components, separating out variables in order.
    val terms = str.toSeq.foldLeft[Seq[Expr]](Seq(EConst(CStr("")))) {
      case (acc, char) if char == '$' => EVar("") +: acc
      case (EConst(CStr(str)) +: acc, char) => EConst(CStr(str + char)) +: acc
      case (EVar(id) +: acc, char) if validId.contains(char) => EVar(id + char) +: acc
      case (EVar(id) +: acc, char) if Set('{', '}').contains(char) => EVar(id) +: acc
      case (acc@(EVar(_) +: _), char) => EConst(CStr(char.toString)) +: acc
    }.filter(nonEmpty).reverse

    // Return a simple EStr or EVar if interpolation is not actually taking place here.
    terms match {
      case Seq(term) => term
      case Seq() => EConst(CStr(""))
      case _ => EStrInterp(terms)
    }
  }
}
