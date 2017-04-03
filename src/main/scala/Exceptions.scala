package pup

case class ParseError(msg: String) extends RuntimeException(msg)

case class MisusedEDSL[A](example: A) extends RuntimeException(
  s"The FS embedded DSL was misused! Here's what happened: $example"
)
