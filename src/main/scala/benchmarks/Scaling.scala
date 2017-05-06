package pup.benchmarks

import pup._
import PuppetEmbeddedDSL._
import PuppetSyntax._
import SymbolicFS._

object Scaling {
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

  // NOTE: https://blogs.oracle.com/dholmes/entry/inside_the_hotspot_vm_clocks
  // Mapping from size to timing (in nanoseconds) for each trial.
  type Result = Map[Int, Seq[Long]]
  def benchmark(mani: Manifest, constraints: Seq[Constraint], trials: Int, max: Int): Result = {
    import Implicits._

    0.to(max).map {
      n => n -> 1.to(trials).map { _ =>
        val manifest = scale(mani, n)
        val labeledManifest = manifest.labeled
        val prog = labeledManifest.compile
        val startTime = System.nanoTime()

        val progPaths = FSVisitors.collectPaths(manifest.labeled.compile).flatMap(_.ancestors)
        val constraintPaths = constraints.flatMap(_.paths.flatMap(_.ancestors)).toSet
        val paths = progPaths -- constraintPaths
        Synthesizer.synthesize(prog, constraints, SynthTransformers.doNotEditPaths(paths))

        val endTime = System.nanoTime()
        endTime - startTime
      }
    }.toMap
  }
}
