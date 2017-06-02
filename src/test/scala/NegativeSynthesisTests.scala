import tortoise._

import Implicits._
import PuppetSyntax._
import SymbolicFS._
import SynthTransformers._

class NegativeSynthesisTests extends org.scalatest.FunSuite {
  def synthesisAssert(
    manifest: Manifest, constraints: Seq[Constraint], transformer: Transformer = identity
  ) {
    val labeledManifest = manifest.labeled
    val prog = labeledManifest.compile

    Synthesizer.synthesize(prog, constraints, transformer).map {
      subst => PuppetUpdater.update(labeledManifest, subst)
    } match {
      case Some(res) => assert(false,
        s"""Expected synthesis to fail. Succeeded with:
            ${res.pretty}

            Constraints were:
            $constraints
         """
      )
      case None => assert(true)
    }
  }

  test(
    "Attempt update of contents and title of a single resource using onlyEditPaths(constraintPaths)"
  ) {
    val manifest = PuppetParser.parse("""
      define ignored($path) {
        file {$path:
          ensure => present
        }
      }

      $a = "/foo"
      ignored { path => $a }
      $b = "/bar"
      ignored { path => $b }
      $c = "/baz"
      ignored { path => $c }
      $d = "/bop"
      ignored { path => $d }
      $e = "/fom"
      ignored { path => $e }
      $f = "/fro"
      ignored { path => $f }
      $g = "/bap"
      ignored { path => $g }

      $x = "/awe"
      $y = "I like dogs."
      file {$x:
        ensure => present,
        content => $y
      }
    """)

    // rm /awe
    // echo "I like cats." > /rachit
    val constraints = ConstraintParser.parse("""
      "/awe" -> nil, "/rachit" => "I like cats."
    """)

    val constraintPaths = constraints.flatMap(_.paths.flatMap(_.ancestors)).toSet

    synthesisAssert(manifest, constraints, onlyEditPaths(constraintPaths))
  }

  test("Attempt to update the body of a define type with over-constraints.") {
    val manifest = PuppetParser.parse("""
      define g($y) {
        file {$y:
          ensure => directory
        }
      }

      define f($x) {
        g { y => $x }
      }

      $w = "/foo"
      f { x => $w }
      $v = "/baz"
      f { x => $v }
    """)

    // mv /foo /bar
    // mkdir /baz
    val constraints = ConstraintParser.parse("""
      "/foo" -> nil, "/bar" -> dir, "/baz" -> dir
    """)

    synthesisAssert(manifest, constraints, onlyEditAbstractions)
  }
}
