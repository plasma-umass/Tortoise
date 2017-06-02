package tortoise

import scala.util.parsing.combinator._

import STrace._

class STraceParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  // General parser combinators
  def parens[T](parser: P[T]): P[T] = "(" ~> parser <~ ")"

  // General syntax components
  lazy val term: P[String] = "" ~> """[^,\)]+""".r

  lazy val word: P[String] = "" ~> "[a-zA-Z][a-zA-Z_]*".r

  lazy val path: P[String] = "\"" ~> "[^\"]*".r <~ "\""

  lazy val mode: P[String] = "" ~> "[0-9]+".r

  lazy val number: P[Int] = "(-)?[0-9]+".r ^^ { n => Integer.parseInt(n) }

  lazy val openFlag: P[OpenFlag] =
    "O_RDONLY" ^^ { _ => OpenReadOnly }       |
    "O_WRONLY" ^^ { _ => OpenWriteOnly }      |
    "O_RDWR" ^^ { _ => OpenReadWrite }        |
    "O_CREAT" ^^ { _ => OpenCreate }          |
    "O_DIRECTORY" ^^ { _ => OpenDirectory }   |
    "O_EXCL" ^^ { _ => OpenFailIfFileExists } |
    "O_TRUNC" ^^ { _ => OpenTruncate }

  lazy val openFlags: P[Set[OpenFlag]] = rep1sep(openFlag, "|") ^^ { _.toSet }

  // Trace Statements
  lazy val open: P[SOpen] =
    "open" ~> parens(path ~ ("," ~> openFlags) ~ opt("," ~> mode)) ~ ("=" ~> number) ^^ {
      case path ~ flags ~ mode ~ exitCode => SOpen(path, flags, mode, exitCode)
    }

  lazy val stat: P[SStat] =
    "stat" ~> parens(path ~ ("," ~> rep1sep(term, ","))) ~ ("=" ~> number) ^^ {
      case path ~ args ~ exitCode => SStat(path, args, exitCode)
    }

  lazy val rename: P[SRename] =
    "rename" ~> parens(path ~ ("," ~> path)) ~ ("=" ~> number) ^^ {
      case src ~ dst ~ exitCode => SRename(src, dst, exitCode)
    }

  lazy val unknown: P[SUnknown] =
    word ~ parens(path ~ ("," ~> rep1sep(term, ","))) ~ ("=" ~> number) ^^ {
      case cmd ~ (path ~ args) ~ exitCode => SUnknown(cmd, path, args, exitCode)
    }

  lazy val statements: P[Statement] =
    open | stat | rename | unknown

  // Dummy parsers to deal with strace oddities we don't care about.
  lazy val exitMessage: P[String] =
    """\w+""".r ~ """\([a-zA-Z ]+\)""".r ^^ {_ => "message"} | success("message")

  lazy val pid: P[String] =
    "[" ~ "pid" ~ """\d+""".r ~ "]" ^^ {_ => "pid"} | success("pid")

  // Entry point
  lazy val statement: P[Statement] =
    pid ~> statements <~ exitMessage
}

object STraceParser {
  private val parser = new STraceParser()
  import parser._

  def parse(str: String): Statement = parseAll(statement, str) match {
    case Success(res, _) => res
    case res => throw new ParseError(s"$res")
  }
}
