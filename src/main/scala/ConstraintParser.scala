package tortoise

import scala.util.parsing.combinator._

import SymbolicFS._

private class ConstraintParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  lazy val number: P[String] = "" ~> "[0-9]+".r
  lazy val doubleQuotedString: P[String] = "\"" ~> "[^\"]*".r <~ "\""
  lazy val singleQuotedString: P[String] = "\'" ~> "[^\']*".r <~ "\'"

  lazy val path: P[String] =
    doubleQuotedString | singleQuotedString

  lazy val state: P[FileState] =
    "file" ^^ { _ => File } |
    "dir" ^^  { _ => Dir  } |
    "nil" ^^  { _ => Nil  }

  lazy val contents: P[String] =
    doubleQuotedString | singleQuotedString

  lazy val mode: P[String] =
    number

  lazy val owner: P[String] =
    doubleQuotedString | singleQuotedString

  lazy val stateConstraint: P[StateConstraint] =
    path ~ ("->" ~> state) ^^ { case path ~ state => StateConstraint(path, state) }

  lazy val contentsConstraint: P[ContentsConstraint] =
    path ~ ("=>" ~> contents) ^^ { case path ~ contents => ContentsConstraint(path, contents) }

  lazy val modeConstraint: P[ModeConstraint] =
    path ~ ("~>" ~> mode) ^^ { case path ~ mode => ModeConstraint(path, mode) }

  lazy val ownerConstraint: P[OwnerConstraint] =
    path ~ ("~>" ~> owner) ^^ { case path ~ owner => OwnerConstraint(path, owner) }

  lazy val constraint: P[Constraint] =
    stateConstraint | contentsConstraint | modeConstraint | ownerConstraint

  lazy val constraints: P[Seq[Constraint]] =
    repsep(constraint, ",")
}

object ConstraintParser {
  private val parser = new ConstraintParser()
  import parser._

  def parse(str: String): Seq[Constraint] = parseAll(constraints, str) match {
    case Success(res, _) => res
    case res => throw new ParseError(s"$res")
  }
}
