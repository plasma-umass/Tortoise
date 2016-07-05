package rehearsal

import scala.util.parsing.combinator._
import java.nio.file.Paths
import FSPlusSyntax._

private class FSPlusParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  lazy val id: P[String] = "" ~> "[a-z_][a-zA-Z0-9_]*".r
  lazy val quotedText: P[String] = "\"" ~> "[^\"]*".r <~ "\""
  lazy val angleQuotedText: P[String] = "<" ~> "[^>]*".r <~ ">"
  lazy val num: P[Int] = """-?\d+""".r ^^ { _.toInt }

  lazy val predAtom: P[Pred] =
    "true" ^^ { _ => PTrue } |
    "false" ^^ { _ => PFalse } |
    "file?(" ~> expr <~ ")" ^^ { case path => PTestFileState(path, IsFile) } |
    "dir?(" ~> expr <~ ")" ^^ { case path => PTestFileState(path, IsDir) } |
    "dne?(" ~> expr <~ ")" ^^ { case path => PTestFileState(path, DoesNotExist) } |
    "(" ~> pred <~ ")"

  lazy val not: P[Pred] =
    "!" ~> not ^^ { PNot(_) } |
    predAtom

  lazy val and: P[Pred] =
    (not <~ "&&") ~ and ^^ { case lhs ~ rhs => PAnd(lhs, rhs) } |
    not

  lazy val or: P[Pred] =
    (and <~ "||") ~ or ^^ { case lhs ~ rhs => POr(lhs, rhs) } |
    and

  lazy val pred: P[Pred] = or

  var loc = 0
  def freshLoc(): Int = {
    loc += 1
    loc
  }

  lazy val location: P[Int] = "[" ~> num <~ "]"

  lazy val path: P[Const] =
    angleQuotedText ~ opt(location) ^^ {
      case path ~ Some(loc) => CPath(JavaPath(Paths.get(path)), loc)
      case path ~ None => CPath(JavaPath(Paths.get(path)), freshLoc())
    }

  lazy val str: P[Const] =
    quotedText ~ opt(location) ^^ {
      case str ~ Some(loc) => CString(str, loc)
      case str ~ None => CString(str, freshLoc())
    }

  lazy val expr: P[Expr] =
    path ^^ { EPath(_) } |
    str ^^ { EString(_) } |
    id ^^ { EId(_) } |
    "parent(" ~> expr <~ ")" ^^ { EParent(_) } |
    ("if" ~> pred <~ "then") ~ expr ~ ("else" ~> expr) ^^ { case p ~ e1 ~ e2 => EIf(p, e1, e2) }

  lazy val stmtAtom: P[Statement] =
    "error" ^^ { _ => SError } |
    "skip" ^^ { _ => SSkip } |
    "mkdir(" ~> expr <~ ")" ^^ { SMkdir(_) } |
    ("mkfile(" ~> expr <~ ",") ~ (expr <~ ")") ^^ { case p ~ c => SCreateFile(p, c) } |
    "rm(" ~> expr <~ ")" ^^ { SRm(_) } |
    ("cp(" ~> expr <~ ",") ~ (expr <~ ")") ^^ { case src ~ dst => SCp(src, dst) }


  lazy val stmt: P[Statement] =
    ("if" ~> pred <~ "then") ~ stmt ~ ("else" ~> stmt) ^^ { case p ~ s1 ~ s2 => SIf(p, s1, s2) } |
    ("let" ~> id <~ "=") ~ expr ~ ("in" ~> stmt) ^^ { case id ~ e ~ body => SLet(id, e, body) } |
    stmtAtom ~ (";" ~> stmt) ^^ { case s1 ~ s2 => SSeq(s1, s2) } |
    stmtAtom
}

object FSPlusParser {
  private val parser = new FSPlusParser()
  import parser._

  def parse(str: String): Statement = parseAll(stmt, str) match {
    case Success(r, _) => r
    case m => throw new ParseError(s"$m")
  }

  def parseFile(fileName: String): Statement = {
    import java.nio.file.Files
    parse(new String(Files.readAllBytes(Paths.get(fileName))))
  }
}
