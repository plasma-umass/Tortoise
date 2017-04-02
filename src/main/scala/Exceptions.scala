package pup

case class ParseError(msg: String) extends RuntimeException(msg)
