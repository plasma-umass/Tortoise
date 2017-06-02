package tortoise

import PuppetSyntax._
import PuppetUpdater._
import SymbolicFS._

object UpdateRanker {
  def rank(substs: Seq[Substitution])(implicit manifest: Manifest): Seq[Substitution] = {
    substs.sortBy {
      subst => (subst.size, affectedTerms(manifest, subst).foldRight(0) {
        case (TManifest(_, cxt), acc) => acc + cxt.env.size
        case (TAttribute(_, cxt), acc) => acc + cxt.env.size
      })
    }
  }

  def promptRankedChoice(substs: Seq[Substitution])(implicit manifest: Manifest): Manifest = {
    val ranked = rank(substs)

    Stream.from(1).zip(ranked).take(5).foreach {
      case (n, subst) => {
        println(s"Update [$n]:")
        summarizeUpdate(manifest, subst).foreach(println)
        println()
      }
    }

    print("Which update would you like to apply? ")
    io.StdIn.readInt() match {
      case n if n >= 1 && n <= 5 => PuppetUpdater.update(manifest, ranked(n - 1))
      case _ => manifest
    }
  }

  def summarizeUpdate(manifest: Manifest, subst: Substitution): Seq[String] = {
    val originalTerms = affectedTerms(manifest, subst)
    val updatedTerms = originalTerms.map(_.updated)
    val affected = originalTerms.zip(updatedTerms).map { case (x, y) => (x.pretty, y.pretty) }

    affected.map {
      case (before, after) => s"$before BECOMES $after"
    }
  }

  sealed trait Term {
    lazy val pretty = this match {
      case TManifest(mani, _) => mani.line
      case TAttribute(attr, _) => attr.pretty
    }

    def updated: Term = this match {
      case TManifest(mani, cxt) => TManifest(PuppetUpdater.updateManifest(mani)(cxt), cxt)
      case TAttribute(attr, cxt) => TAttribute(PuppetUpdater.updateAttribute(attr)(cxt), cxt)
    }
  }

  case class TManifest(mani: Manifest, cxt: UpdateContext) extends Term
  case class TAttribute(attr: Attribute, cxt: UpdateContext) extends Term

  def affectedExpr(expr: Expr)(implicit cxt: UpdateContext): Boolean = expr match {
    case EUndef | EConst(_) => false
    case EVar(id) => cxt.contains(id)
    case EStrInterp(exprs) => exprs.map(affectedExpr).foldRight(false)(_ || _)
    case EUnOp(_, operand) => affectedExpr(operand)
    case EBinOp(_, lhs, rhs) => affectedExpr(lhs) || affectedExpr(rhs)
  }

  def affectedTerms(attrs: Attributes)(implicit cxt: UpdateContext): Seq[Term] = {
    attrs.foldRight[Seq[Term]](Seq()) {
      case (attr, acc) if affectedExpr(attr.value) => TAttribute(attr, cxt) +: acc
      case (_, acc) => acc
    }
  }

  def affectedTerms(mani: Manifest)(implicit cxt: UpdateContext): Seq[Term] = mani match {
    case MEmpty => Seq()
    case MAssign(_, _, body) if cxt.subst.contains(mani.label) => {
      TManifest(mani, cxt) +: affectedTerms(body)
    }
    case MAssign(_, _, body) => affectedTerms(body)
    case MResource(_, title, attrs) if affectedExpr(title) => {
      TManifest(mani, cxt) +: affectedTerms(attrs)
    }
    case MResource(_, _, attrs) => affectedTerms(attrs)
    case MDefine(_, args, body) => affectedTerms(body)(cxt ++ args.map(_.vari.id).zip(mani.labels))
    case MSeq(lhs, rhs) => affectedTerms(lhs) ++ affectedTerms(rhs)
    case MIf(pred, cons, alt) if affectedExpr(pred) => {
      Seq(TManifest(mani, cxt)) ++ affectedTerms(cons) ++ affectedTerms(alt)
    }
    case MIf(pred, cons, alt) => affectedTerms(cons) ++ affectedTerms(alt)
  }

  def affectedTerms(mani: Manifest, subst: Substitution): Seq[Term] = {
    affectedTerms(mani)(UpdateContext(Map(), subst))
  }
}
