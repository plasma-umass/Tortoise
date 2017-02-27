package pup

import scala.util.parsing.combinator._
import PuppetSyntax._
import Implicits._

private class PuppetParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  override protected val whiteSpace = """(\s|#.*|(/\*((\*[^/])|[^*])*\*/))+""".r

  var loc = 0
  def freshLoc(): Int = {
    loc += 1
    loc
  }

  def interpolateString(str: String): Expr = {
    // Split strings into individual components, separating out variables in order.
    val regex = """(\$\{[^}]*\})|([^$]*)""".r
    val terms = regex.findAllMatchIn(str).map(_.matched).filter(_.nonEmpty).map {
      case s if s.startsWith("$") => EVar(s.substring(2, s.length - 1))
      case s => EStr(s).setLoc(freshLoc())
    }.toSeq

    // Return a simple EStr or EVar if interpolation is not actually taking place here.
    terms match {
      case Seq(term) => term
      case _ => EStrInterp(terms)
    }
  }

  var holes: Map[(String, String), Int] = Map()
  def locForRes(typ: String, title: Expr): Option[Int] = title match {
    case EStr(title) => holes.get(typ -> title) match {
      case Some(n) => Some(n)
      case None => {
        val loc = freshLoc()
        holes = holes + ((typ, title) -> loc)
        Some(loc)
      }
    }
    case _ => None
  }

  // TODO(arjun): escape sequences? interpolation?
  lazy val stringVal: P[String] =
    "\"" ~> "[^\"]*".r <~ "\"" |
    "'" ~> "[^']*".r <~ "'"
  lazy val word: P[String] = "((::)?[a-zA-Z][a-zA-Z_]*)+".r ^^ {case x => x}
  lazy val capWord: P[String] = "((::)?[A-Z][a-zA-Z_]*)+".r ^^ {case x => x}
  lazy val id: P[String] = "" ~> "[a-z_][a-zA-Z0-9_]*".r
  lazy val attributeName: P[String] = "" ~> "[a-z]+".r
  lazy val dataType: P[String] = "" ~> "[A-Z][a-zA-Z]+".r
  lazy val varName: P[String] =  "$" ~> "[a-z_(::)][a-zA-Z0-9_(::)]*[a-zA-Z0-9_]+|[a-z_(::)]".r

  lazy val className: P[Expr] = expr | word ^^ (EStr(_))

  //Manifest
  lazy val manifest: P[Manifest] = positioned {
    varName ~ ("=" ~> expr) ^^
      { case x ~ e => MSet(x, e) } |
    "define" ~> word ~ params ~ body ^^
      { case x ~ xs ~ m => MDefine(x, xs, m) } |
    "class" ~> word ~ params ~ body ^^
      { case x ~ xs ~ m => MClass(x, xs, None, m) } |
    "class" ~> word ~ params ~ ("inherits" ~> word) ~ body ^^
      { case x ~ xs ~ y ~ m => MClass(x, xs, Some(y), m) } |
    "case" ~> expr ~ ("{" ~> cases <~ "}") ^^
      { case e ~ lst => MCase(e, lst) } |
    "include" ~> repsep(className, ",") ^^
      { case xs => MInclude(xs) } |
    "require" ~> className ^^
      { case x => MRequire(x) } |
    "if" ~> expr ~ body ~ elses ^^
      { case e ~ m1 ~ m2 => MIte(e, m1, m2) } |
    rep1sep(resource, "->") ^^
      { case lst => MResources(lst) } |
    word ~ ("(" ~> repsep(expr, ",") <~ ")") ^^
      { case f ~ xs => MApp(f, xs) } |
    capWord ~ ("{" ~> attributes <~ "}") ^^
      { case typ ~ attrs => MResourceDefault(typ, attrs) }
  }

  lazy val elses: P[Manifest] = positioned {
    "else" ~> body ^^
      { case m => m } |
    "elsif" ~> parenExpr ~ body ~ elses ^^
      { case e ~ m1 ~ m2 => MIte(e, m1, m2) } |
    success(()) ^^
      { case () => MEmpty }
    }

  lazy val body: P[Manifest] = "{" ~> prog <~ "}"

  lazy val prog: P[Manifest] = rep(manifest) ^^ { case exprs => blockExprs(exprs) }

  def blockExprs(exprs: Seq[Manifest]): Manifest = {
    exprs.foldRight[Manifest](MEmpty) { case (m1, m2) => m1 >> m2 }
  }

  lazy val cases: P[Seq[Case]] =
    ("default" ~ ":") ~> body ^^ { case m => Seq(CaseDefault(m)) } |
    expr ~ (":" ~> body) ~ cases ^^ { case e ~ m ~ rest => CaseExpr(e, m) +: rest } |
    success(()) ^^ { case _ => Seq[Case]() }

  lazy val parameter: P[Argument] = opt(dataType) ~ varName ~ opt("=" ~> expr) ^^ {
    case typ ~ id ~ Some(default) => Argument(id, Some(default))
    case typ ~ id ~ None => Argument(id, None)
  }

  lazy val params: P[Seq[Argument]] =
    ("(" ~> repsep(parameter, ",") <~ opt(",")) <~ ")" |
    success(()) ^^ { case _ => Seq[Argument]() }

  lazy val edges: P[Seq[Resource]] =
    resource ~ ("->" ~> edges) ^^
      { case x ~ xs => x +: xs } |
    resource ^^
      { case x => Seq(x) }

  lazy val resource: P[Resource] =
    word ~ ("{" ~> rep1sep(resourcePair, ";")) <~ (opt(";") ~ "}") ^^
      { case typ ~ lst => ResourceDecl(typ, lst) } |
    capWord ~ ("[" ~> expr <~ "]") ~ ("{" ~> attributes <~ "}") ^^
      { case typ ~ title ~ attrs => ResourceRef(typ, title.setLoc(locForRes(typ, title)), attrs) } |
    capWord ~ ("[" ~> expr <~ "]") ^^
      { case typ ~ title => ResourceRef(typ, title.setLoc(locForRes(typ, title)), Seq()) } |
    capWord ~ ("<|" ~> rexpr <~ "|>") ^^
      { case typ ~ e => RCollector(typ, e) }

  lazy val resourcePair: P[(Expr, Seq[Attribute])] = (expr <~ ":") ~ attributes ^^ {
    case id ~ attr => (id, attr)
  }

  //Attribute
  lazy val attrId: P[Expr] = id ^^ (interpolateString(_).setKeyword())
  lazy val attribute: P[Attribute] =
    (attrId | vari) ~ ("=>" ~> (expr | attrId)) ^^ { case name ~ value => Attribute(name, value) }

  lazy val attributes: P[Seq[Attribute]] = repsep(attribute, ",") <~ opt(",")

  lazy val vari: P[Expr] = varName ^^ (EVar(_))

  //
  // Expressions. Use "expr" to parse an expression. Do not use any of the other
  // parsers (e.g., atom, bop, etc.) outside this block.
  //

  lazy val parenExpr: P[Expr] = "(" ~> expr <~ ")"

  lazy val atom: P[Expr] = positioned {
    "undef" ^^ { _ => EUndef } |
    "true" ^^ { _ => EBool(true) } |
    "false" ^^ { _ => EBool(false) } |
    vari |
    stringVal ^^ (interpolateString(_)) |
    """\d+""".r ^^
      { n => ENum(n.toInt) } |
    "[" ~> repsep(expr, ",") <~ (opt(",") ~ "]") ^^ { case es => EArray(es) } |
    word ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case f ~ xs => EApp(f, xs) } |
    ("/" ~> "[^/]*".r <~ "/") ^^ { case expr => ERegex(expr) } |
    word ~ ("[" ~> expr <~ "]") ^^
      { case typ ~ title => EResourceRef(typ, title) } |
    parenExpr
  }

  lazy val not: P[Expr] = positioned {
    "!" ~> not ^^ { ENot(_) } |
    atom
  }

  lazy val and: P[Expr] = positioned {
    not ~ ("and" ~> and) ^^ { case lhs ~ rhs => EAnd(lhs, rhs) } |
    not
  }

  lazy val or: P[Expr] = positioned {
    and ~ ("or" ~> or) ^^ { case lhs ~ rhs => EOr(lhs, rhs) } |
    and
  }

  lazy val bop: P[Expr] = positioned {
    or ~ ("==" ~> bop) ^^ { case lhs ~ rhs => EEq(lhs, rhs) } |
    or ~ ("!=" ~> bop) ^^ { case lhs ~ rhs => ENot(EEq(lhs, rhs)) } |
    or ~ ("<" ~> bop) ^^ { case lhs ~ rhs => ELT(lhs, rhs) } |
    or ~ (">" ~> bop) ^^ { case lhs ~ rhs => ELT(rhs, lhs) } |
    or ~ ("<=" ~> bop) ^^ { case lhs ~ rhs => EOr(ELT(lhs, rhs), EEq(lhs, rhs)) } |
    or ~ (">=" ~> bop) ^^ { case lhs ~ rhs => EOr(ELT(rhs, lhs), EEq(lhs, rhs)) } |
    or ~ ("=~" ~> bop) ^^ { case lhs ~ rhs => EMatch(lhs, rhs) } |
    or ~ ("!~" ~> bop) ^^ { case lhs ~ rhs => ENot(EMatch(lhs, rhs)) } |
    or ~ ("in" ~> bop) ^^ { case lhs ~ rhs => EIn(lhs, rhs) } |
    or
  }

  lazy val cond: P[Expr] = positioned {
    bop ~ ("?" ~> bop <~ ":") ~ cond ^^
      { case e1 ~ e2 ~ e3 => ECond(e1, e2, e3) } |
    ("if" ~> bop <~ "{") ~ expr ~ (("}" ~ "else" ~ "{") ~> expr <~ "}") ^^
      { case e1 ~ e2 ~ e3 => ECond(e1, e2, e3) } |
    bop
  }

  lazy val expr: P[Expr] = cond

  lazy val rexprRhs: P[Expr] = positioned {
    "undef" ^^ { _ => EUndef } |
    "true" ^^ { _ => EBool(true) } |
    "false" ^^ { _ => EBool(false) } |
    stringVal ^^ (interpolateString(_)) |
    """\d+""".r ^^  { n => ENum(n.toInt) } |
    word ~ ("[" ~> expr <~ "]") ^^ { case typ ~ title => EResourceRef(typ, title.setLoc(locForRes(typ, title))) }
  }

  lazy val rexprAtom: P[RExpr] = positioned {
    "(" ~> rexpr <~ ")" |
    attributeName ~ ("=" ~> rexprRhs) ^^ { case lhs ~ rhs => REAttrEqual(lhs, rhs) } |
    attributeName ~ ("!=" ~> rexprRhs) ^^ { case lhs ~ rhs => RENot(REAttrEqual(lhs, rhs)) }
  }

  lazy val rexprAnd: P[RExpr] = positioned {
    rexprAtom ~ ("and" ~> rexprAnd) ^^ { case lhs ~ rhs => REAnd(lhs, rhs) } |
    rexprAtom
  }

  lazy val rexprOr: P[RExpr] = positioned {
    rexprAnd ~ ("or" ~> rexprOr)  ^^ { case lhs ~ rhs => REOr(lhs, rhs) } |
    rexprAnd
  }

  lazy val rexpr: P[RExpr] = rexprOr

}

object PuppetParser {
  private val parser = new PuppetParser()
  import parser._

  def parse(str: String): Manifest = parseAll(prog, str) match {
    case Success(r, _) => r
    case m => throw new ParseError(s"$m")
  }

  def parseFile(filename: String): Manifest = {
    import java.nio.file._
    parse(new String(Files.readAllBytes(Paths.get(filename))))
  }

  def parseExpr(str: String): Option[Expr] = parseAll(expr, str) match {
    case Success(e, _) => Some(e)
    case msg => println(msg); None
  }
}
