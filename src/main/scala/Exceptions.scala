package pup

case class ParseError(msg: String) extends RuntimeException(msg)

case class MisusedEDSL[A](example: A) extends RuntimeException(
  s"The FS embedded DSL was misused! Here's what happened: $example"
)

case class ConfigError(msg: String) extends RuntimeException(msg)

case class TypeError(found: String, expected: String) extends RuntimeException(
  s"A type error occurred during evaluation. Found: $found, expected: $expected."
)

case class UpdateError(expr: PuppetSyntax.Expr) extends RuntimeException(
  s"Failed to apply substitution because the expression `$expr` could not be updated."
)
