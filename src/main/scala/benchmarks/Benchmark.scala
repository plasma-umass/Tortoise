package pup

import pup._
import PuppetEmbeddedDSL._
import PuppetSyntax._
import SymbolicFS._

object Benchmark {
  import Implicits._

  // NOTE: https://blogs.oracle.com/dholmes/entry/inside_the_hotspot_vm_clocks
  def synthTimed(mani: Manifest, constraints: Seq[Constraint], optimized: Boolean = true): Long = {
    val labeledManifest = mani.labeled
    val prog = labeledManifest.compile
    val startTime = System.nanoTime()

    val transformer = if (optimized) {
      val progPaths = FSVisitors.collectPaths(prog).flatMap(_.ancestors)
      val constraintPaths = constraints.flatMap(_.paths.flatMap(_.ancestors)).toSet
      val paths = progPaths -- constraintPaths
      SynthTransformers.doNotEditPaths(paths)(_)
    } else {
      SynthTransformers.identity(_)
    }

    Synthesizer.synthesize(prog, constraints, transformer)

    val endTime = System.nanoTime()
    endTime - startTime
  }
}
