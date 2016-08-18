package rehearsal

import org.bitbucket.inkytonik.kiama.output.PrettyPrinter
import PuppetSyntax._

object PuppetPretty {
  def pretty(m: Manifest): String = PrettyPuppet.prettyPuppet(m)
}

private object PrettyPuppet extends PrettyPrinter {

  import scala.language.implicitConversions

  override val defaultIndent = 2

  // Are you serious, Scala?
  implicit def seq2seq[A](seq: Seq[A]): scala.collection.immutable.Seq[A] = scala.collection.immutable.Seq(seq: _*)

  def optional[A](opt: Option[A])(pp: A => Doc): Doc = opt match {
    case Some(v) => pp(v)
    case None => emptyDoc
  }


  def prettyPuppet(m: Manifest): String = {
    layout(prettyManifest(m))
  }

  def scope(doc: Doc): Doc = braces(nest(line <> doc) <> line)

  def commasep(docs: Seq[Doc]): Doc = fillsep(docs, comma <> space)

  def prettyManifest(manifest: Manifest): Doc = {
    implicit val cnxt = ENotContext
    manifest match {
      case MEmpty => emptyDoc
      case MSeq(m1, m2) => prettyManifest(m1) <@> prettyManifest(m2)
      case MResources(res) => vsep(res.map(prettyResource(_)))
      case MDefine(n, params, b) => {
        "define" <+> n <+> parens(commasep(params.map(prettyArg(_)))) <+>
        scope(prettyManifest(b))
      }
      case MClass(n, ps, inhs, b) => {
        "define" <+> n <+> parens(commasep(ps.map(prettyArg(_)))) <+>
        optional(inhs)(str => "inherits" <+> str <> space) <>
        scope(prettyManifest(b))
      }
      case MSet(vn, exp) => vn <+> equal <+> prettyExpr(exp)
      case MCase(e, cases) => {
        "case" <+> prettyExpr(e) <+> scope(vsep(cases.map(prettyCase(_))))
      }
      case MIte(p, m1, m2) => {
        "if" <+> prettyExpr(p) <+> scope(prettyManifest(m1)) <+>
        "else" <+> scope(prettyManifest(m2))
      }
      case MInclude(es) => "include" <+> commasep(es.map(prettyExpr(_)))
      case MRequire(e) => "require" <+> prettyExpr(e)
      case MApp(n, args) => n <+> parens(commasep(args.map(prettyExpr(_))))
      case MResourceDefault(t, attrs) => {
        t <+> scope(vsep(attrs.map(prettyAttr(_))))
      }
    }
  }

  sealed trait EContext
  case object ENotContext extends EContext
  case object EAndContext extends EContext
  case object EOrContext extends EContext

  def prettyExpr(exp: Expr)(implicit cnxt: EContext): Doc = exp match {
    case EUndef => "undef"
    case EStr(s) => if (exp.isKeyword()) {
      s
    } else {
      dquotes(s)
    }
    case EStrInterp(terms) => dquotes(fillcat(terms.map {
      case EVar(id) => dollar <> braces(id)
      case EStr(s) => text(s)
      case term => text(s"Malformed interpolation term: $term")
    }))
    case ENum(n) => n.toString
    case EBool(b) => b.toString
    case EVar(v) => dollar <> v
    case ENot(e) => exclamation <+> prettyExpr(e)(ENotContext)
    case EAnd(e1, e2) => {
      val pp = prettyExpr(e1)(EAndContext) <+> "&&" <+> prettyExpr(e2)(EAndContext)
      cnxt match {
        case ENotContext => parens(pp)
        case _ => pp
      }
    }
    case EOr(e1, e2) => {
      val pp = prettyExpr(e1)(EOrContext) <+> "||" <+> prettyExpr(e2)(EOrContext)
      cnxt match {
        case ENotContext | EAndContext => parens(pp)
        case _ => pp
      }
    }
    case EEq(e1, e2) => prettyExpr(e1) <+> "==" <+> prettyExpr(e2)
    case ELT(e1, e2) => prettyExpr(e1) <+> ">" <+> prettyExpr(e2)
    case EMatch(e1, e2) => prettyExpr(e1) <+> "=~" <+> prettyExpr(e2)
    case EIn(e1, e2) => prettyExpr(e1) <+> "in" <+> prettyExpr(e2)
    case EArray(es) => brackets(commasep(es.map(prettyExpr(_))))
    case EApp(n, args) => n <+> parens(vsep(args.map(prettyExpr(_))))
    case ERegex(reg) => reg
    case ECond(test, t, f) => {
      "if" <+> prettyExpr(test) <+> scope(prettyExpr(t)) <+>
      "else" <+> scope(prettyExpr(f))
    }
    case EResourceRef(typ, title) => typ <> brackets(prettyExpr(title))
  }

  def prettyRExpr(re: RExpr)(implicit cnxt: EContext): Doc = re match {
    case REAttrEqual(attr, v) => attr <+> equal <+> prettyExpr(v)
    case RENot(e) => exclamation <+> prettyRExpr(e)(ENotContext)
    case REAnd(e1, e2) => {
      val pp = prettyRExpr(e1)(EAndContext) <+> "&&" <+>
               prettyRExpr(e2)(EAndContext)
      cnxt match {
        case ENotContext => parens(pp)
        case _ => pp
      }
    }
    case REOr(e1, e2) => {
      val pp = prettyRExpr(e1)(EOrContext) <+> "||" <+>
               prettyRExpr(e2)(EOrContext)
      cnxt match {
        case ENotContext | EAndContext => parens(pp)
        case _ => pp
      }
    }
  }

  def prettyAttr(attr: Attribute): Doc = {
    implicit val cnxt = ENotContext
    prettyExpr(attr.name) <+> "=>" <+> prettyExpr(attr.value)
  }

  def prettyArg(arg: Argument): Doc = {
    implicit val cnxt = ENotContext
    dollar <> arg.id <> optional(arg.default)(e => equal <+> prettyExpr(e))
  }

  def prettyResource(res: Resource): Doc = {
    implicit val cnxt = ENotContext

    def prettyResPair(pair: (Expr, Seq[Attribute])): Doc =
      prettyExpr(pair._1) <> colon <@> vsep(pair._2.map(prettyAttr(_)), comma)

    res match {
      case ResourceDecl(typ, resources) => {
        typ <+> scope(vsep(resources.map(res => nest(prettyResPair(res)))))
      }
      case ResourceRef(typ, title, Seq()) => typ <> brackets(prettyExpr(title))
      case ResourceRef(typ, title, attrs) => {
        typ <> brackets(prettyExpr(title)) <+>
        scope(vsep(attrs.map(prettyAttr(_))))
      }
      case RCollector(typ, e) => typ <> angles(surround(prettyRExpr(e), verticalbar))
    }
  }

  def prettyCase(c: Case): Doc = {
    implicit val cnxt = ENotContext
    c match {
      case CaseDefault(m) => "default" <> colon <@> nest(prettyManifest(m))
      case CaseExpr(e, m) => prettyExpr(e) <> colon <@> nest(prettyManifest(m))
    }
  }
}
