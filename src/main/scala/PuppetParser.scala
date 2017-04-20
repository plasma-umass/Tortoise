package pup

import scala.util.parsing.combinator._

import CommonSyntax._
import PuppetSyntax._
import StringInterpolator._

private class PuppetParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  // Override whitespace filtering to include comments.
  override protected val whiteSpace = """(\s|#.*|(/\*((\*[^/])|[^*])*\*/))+""".r

  // General parser combinators
  def parens[T](parser: P[T]): P[T] = "(" ~> parser <~ ")"
  def braces[T](parser: P[T]): P[T] = "{" ~> parser <~ "}"

  // General syntax components
  lazy val word: P[String] = "" ~> "((::)?[a-zA-Z][a-zA-Z_]*)+".r
  lazy val capitalizedWord: P[String] = "" ~> "((::)?[A-Z][a-zA-Z_]*)+".r
  lazy val identifier: P[String] = "" ~> "[a-z_][a-zA-Z0-9_]*".r
  lazy val attributeName: P[String] = "" ~> "[a-z]+".r
  lazy val dataType: P[String] = "" ~> "[A-Z][a-zA-Z]+".r
  lazy val variableName: P[String] =  "$" ~> "[a-z_(::)][a-zA-Z0-9_(::)]*[a-zA-Z0-9_]+|[a-z_(::)]".r

  // Base constants
  lazy val doubleQuotedString: P[String] = "\"" ~> "[^\"]*".r <~ "\""
  lazy val singleQuotedString: P[String] = "\'" ~> "[^\']*".r <~ "\'"
  lazy val number: P[Int] = "(-)?[0-9]+".r ^^ { n => Integer.parseInt(n) }
  lazy val boolean: P[Boolean] =
    "true" ^^ { _ => true } | "false" ^^ { _ => false }

  // Puppet constants
  lazy val const: P[Const] =
    singleQuotedString ^^ { str => CStr(str) } |
    number ^^ { num => CNum(num) } |
    boolean ^^ { bool => CBool(bool) }

  // Puppet expressions
  lazy val variable: P[EVar] =
    variableName ^^ { id => EVar(id) }

  lazy val constant: P[EConst] =
    const ^^ { c => EConst(c) }

  lazy val strInterp: P[Expr] =
    doubleQuotedString ^^ { str => interpolateString(str) }

  lazy val atomicExpr: P[Expr] =
    variable  |
    strInterp |
    constant  |
    parens(expr)

  lazy val unaryOp: P[Expr] =
    "!" ~> unaryOp ^^ { case expr => EUnOp(UNot, expr) } |
    "-" ~> unaryOp ^^ { case expr => EUnOp(UNeg, expr) } |
    atomicExpr

  lazy val binaryAnd: P[Expr] =
    unaryOp ~ ("and" ~> binaryAnd) ^^ { case lhs ~ rhs => EBinOp(BAnd, lhs, rhs) } |
    unaryOp

  lazy val binaryOr: P[Expr] =
    binaryAnd ~ ("or" ~> binaryOr) ^^ { case lhs ~ rhs => EBinOp(BOr, lhs, rhs) } |
    binaryAnd

  lazy val binaryOp: P[Expr] =
    binaryOr ~ ("==" ~> binaryOp) ^^ { case lhs ~ rhs => EBinOp(BEq, lhs, rhs) } |
    binaryOr ~ ("!=" ~> binaryOp) ^^ { case lhs ~ rhs => EUnOp(UNot, EBinOp(BEq, lhs, rhs)) } |
    binaryOr ~ ("<"  ~> binaryOp) ^^ { case lhs ~ rhs => EBinOp(BLt, lhs, rhs) } |
    binaryOr ~ (">"  ~> binaryOp) ^^ { case lhs ~ rhs => EBinOp(BGt, lhs, rhs) } |
    binaryOr ~ ("<=" ~> binaryOp) ^^ {
      case lhs ~ rhs => EBinOp(BOr, EBinOp(BEq, lhs, rhs), EBinOp(BLt, lhs, rhs))
    } |
    binaryOr ~ (">=" ~> binaryOp) ^^ {
      case lhs ~ rhs => EBinOp(BOr, EBinOp(BEq, lhs, rhs), EBinOp(BGt, lhs, rhs))
    } |
    binaryOr

  lazy val expr: P[Expr] = binaryOp

  // Puppet arguments
  lazy val argument: P[Argument] =
    // NOTE: we may wish to include types in the AST later.
    opt(dataType) ~ variable ~ opt("=" ~> expr) ^^ {
      case _ ~ vari ~ default => Argument(vari, default)
    }

  lazy val arguments: P[Arguments] =
    repsep(argument, ",") <~ opt(",") |
    success(()) ^^ { case _ => Seq() }

  // Puppet attributes
  lazy val keyword: P[Expr] =
    identifier ^^ { str => EConst(CStr(str)) }

  lazy val attribute: P[Attribute] =
    identifier ~ ("=>" ~> (keyword | expr)) ^^ {
      case name ~ expr => Attribute(name, expr)
    }

  lazy val attributes: P[Attributes] =
    repsep(attribute, ",") <~ opt(",")

  // Puppet manifests
  lazy val resourceTitle: P[Expr] = expr <~ ":"

  lazy val resourceInstantiation: P[MResource] =
    // NOTE: we may wish to parse multiresource declarations.
    word ~ braces(opt(resourceTitle) ~ attributes) ^^ {
      case typ ~ (Some(title) ~ attrs) => MResource(typ, title, attrs)
      case typ ~ (None ~ attrs) => MResource(typ, EUndef, attrs)
    }

  lazy val defineType: P[MDefine] =
    "define" ~> word ~ parens(arguments) ~ braces(manifest) ^^ {
      case typ ~ args ~ body => MDefine(typ, args, body)
    }

  lazy val ifManifest: P[MIf] =
    "if" ~> parens(expr) ~ braces(manifest) ~ opt("else" ~> braces(manifest)) ^^ {
      case pred ~ cons ~ Some(alt) => MIf(pred, cons, alt)
      case pred ~ cons ~ None => MIf(pred, cons, MEmpty)
    }

  lazy val atomicManifest: P[Manifest] =
    resourceInstantiation | defineType | ifManifest

  lazy val assign: P[Manifest] =
    variableName ~ ("=" ~> expr) ~ assign ^^ { case id ~ expr ~ body => MAssign(id, expr, body) } |
    atomicManifest

  lazy val sequence: P[Manifest] =
    assign ~ sequence ^^ { case lhs ~ rhs => MSeq(lhs, rhs) } |
    assign

  lazy val manifest: P[Manifest] = sequence
}

object PuppetParser {
  private val parser = new PuppetParser()
  import parser._

  def parse(str: String): Manifest = parseAll(manifest, str) match {
    case Success(res, _) => res
    case res => throw new ParseError(s"$res")
  }

  def parseFile(path: String): Manifest = {
    import java.nio.file.{Files, Paths}
    parse(new String(Files.readAllBytes(Paths.get(path))))
  }
}
