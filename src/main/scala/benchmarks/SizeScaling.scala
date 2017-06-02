package tortoise.benchmarks

import tortoise._
import PuppetEmbeddedDSL._
import PuppetSyntax._
import SymbolicFS._

object SizeScaling {
  lazy val ignoredDef = define ("ignored")("name") {
    resource("file")(s("/ignored/", $("name")),
      ensure ~> present
    )
  }

  def ignored(name: String): Manifest = {
    resource("ignored")($(name),
      "name" ~> name
    )
  }

  def scale(mani: Manifest, n: Int): Manifest = {
    ignoredDef >> 0.to(n).foldRight(mani) {
      case (n, mani) => ignored(s"file$n") >> mani
    }
  }

  // Mapping from size to timing (in nanoseconds) for each trial.
  type Result = Map[Int, Seq[Long]]
  def benchmark(
    mani: Manifest, constraints: Seq[Constraint], trials: Int, max: Int, optimized: Boolean = true
  ): Result = {
    0.to(max).map {
      n => n -> Benchmark.trials(trials) {
        val manifest = scale(mani, n)
        Benchmark.synthTimed(manifest, constraints, optimized)
      }
    }.toMap
  }
}
