package pup.benchmarks

import pup._
import PuppetEmbeddedDSL._
import PuppetSyntax._
import SymbolicFS._

object UpdateScaling {
  def file(vari: Expr): Manifest = resource("file")(s("/", vari),
    ensure ~> directory
  )

  def scale(n: Int): Manifest = 0.to(n).foldRight[Manifest](empty) {
    case (n, mani) => MAssign(s"var$n", s"foo$n", file($(s"var$n"))) >> mani
  }

  def constraints(n: Int): Seq[Constraint] = 0.to(n).foldRight[Seq[Constraint]](Seq()) {
    case (n, cs) => Seq(StateConstraint(s"/foo$n", Nil), StateConstraint(s"/bar$n", Dir)) ++ cs
  }

  type Result = Map[Int, Seq[Long]]
  def benchmark(trials: Int, max: Int, optimized: Boolean = false): Result = {
    0.to(max).map {
      n => n -> 1.to(trials).map { _ =>
        val manifest = scale(n)
        val cs = constraints(n)
        Benchmark.synthTimed(manifest, cs, optimized)
      }
    }.toMap
  }
}
