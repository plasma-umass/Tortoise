import pup._

import PuppetSyntax._
import SymbolicFS._

class SynthesisTests extends org.scalatest.FunSuite {
  def synthesisAssert(manifest: Manifest, constraints: Seq[Constraint], expected: Manifest) {
    val labeledManifest = manifest.labeled
    val prog = labeledManifest.compile

    Synthesizer.synthesize(prog, constraints).map {
      subst => PuppetUpdater.update(labeledManifest, subst)
    } match {
      case Some(res) => assert(res == expected, s"\nThe constraints were:\n$constraints")
      case None => assert(false, s"Synthesis failed. The constraints were:\n$constraints")
    }
  }

  test("Update a single file resource") {
    val manifest = PuppetParser.parse("""
      $x = "/foo"
      file {$x:
        ensure => directory
      }
    """)

    val constraints = ConstraintParser.parse("""
      "/foo" -> nil, "/bar" -> dir
    """)

    val expected = PuppetParser.parse("""
      $x = "/bar"
      file {$x:
        ensure => directory
      }
    """)

    synthesisAssert(manifest, constraints, expected)
  }

  test("Update a single file resource in a define type.") {
    val manifest = PuppetParser.parse("""
      define f($x) {
        file {$x:
          ensure => directory
        }
      }

      $y = "/foo"
      f { x => $y }
    """)

    val constraints = ConstraintParser.parse("""
      "/foo" -> nil, "/bar" -> dir
    """)

    val expected = PuppetParser.parse("""
      define f($x) {
        file {$x:
          ensure => directory
        }
      }

      $y = "/bar"
      f { x => $y }
    """)

    synthesisAssert(manifest, constraints, expected)
  }

  test("Update a single file resource with two define type instantiations.") {
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

    val constraints = ConstraintParser.parse("""
      "/foo" -> nil, "/bar" -> dir
    """)

    val expected = PuppetParser.parse("""
      define g($y) {
        file {$y:
          ensure => directory
        }
      }

      define f($x) {
        g { y => $x }
      }

      $w = "/bar"
      f { x => $w }
      $v = "/baz"
      f { x => $v }
    """)

    synthesisAssert(manifest, constraints, expected)
  }
}
