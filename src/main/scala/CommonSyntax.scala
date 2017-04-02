package pup

object CommonSyntax {
  sealed trait Const
  case class CStr(str: String) extends Const
  case class CNum(n: Int) extends Const
  case class CBool(bool: Boolean) extends Const
}
