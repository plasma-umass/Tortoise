package pup

import CommonSyntax._

object PuppetSyntax {
  // Attributes for resource instantiations.
  case class Attribute(name: String, value: Expr)
  type Attributes = Seq[Attribute]
  // Arguments to define types.
  // NOTE: we may wish to include types later.
  case class Argument(vari: EVar, default: Option[Expr])
  type Arguments = Seq[Argument]

  sealed trait UnOp
  case object UNot extends UnOp
  case object UNeg extends UnOp

  sealed trait BinOp
  case object BAnd extends BinOp
  case object BOr extends BinOp
  case object BEq extends BinOp
  case object BLt extends BinOp
  case object BGt extends BinOp

  sealed trait Expr {
    lazy val pretty: String = PrettyPuppet.prettyExpr(this)
  }

  type Exprs = Seq[Expr]
  case object EUndef extends Expr
  case class EVar(id: String) extends Expr
  case class EConst(c: Const) extends Expr
  case class EStrInterp(terms: Exprs) extends Expr
  case class EUnOp(op: UnOp, operand: Expr) extends Expr
  case class EBinOp(op: BinOp, lhs: Expr, rhs: Expr) extends Expr

  sealed trait Manifest {
    lazy val pretty: String = PrettyPuppet.prettyManifest(this)
    lazy val labeled: Manifest = PuppetLabeler.label(this)
    lazy val compile: FSSyntax.Statement = PuppetCompiler.compile(this)

    private var internalLabels: Seq[Int] = Seq()

    def label(): Int = internalLabels(0)
    def labels(): Seq[Int] = internalLabels
    def setLabel(label: Int): Manifest = setLabels(Seq(label))
    def setLabels(labels: Seq[Int]): Manifest = {
      this.internalLabels = labels
      this
    }
  }
  case object MEmpty extends Manifest
  case class MAssign(id: String, expr: Expr, body: Manifest) extends Manifest
  case class MResource(typ: String, title: Expr, attrs: Attributes) extends Manifest
  case class MDefine(typ: String, args: Arguments, body: Manifest) extends Manifest
  case class MSeq(lhs: Manifest, rhs: Manifest) extends Manifest
  case class MIf(pred: Expr, cons: Manifest, alt: Manifest) extends Manifest
}
