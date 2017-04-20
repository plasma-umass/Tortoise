import pup._

import Implicits._
import PuppetSyntax._
import SymbolicFS._
import SynthTransformers._

class SynthesisTests extends org.scalatest.FunSuite {
  def synthesisAssert(
    manifest: Manifest, constraints: Seq[Constraint], expected: Manifest,
    transformer: Transformer = identity
  ) {
    val labeledManifest = manifest.labeled
    val prog = labeledManifest.compile

    Synthesizer.synthesize(prog, constraints, transformer).map {
      subst => PuppetUpdater.update(labeledManifest, subst)
    } match {
      case Some(res) => assert(res == expected, s"\nThe constraints were:\n$constraints")
      case None => assert(false, s"Synthesis failed. The constraints were:\n$constraints")
    }
  }

  test("Update a single file resource.") {
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

  test("Update the contents of a single file resource.") {
    val manifest = PuppetParser.parse("""
      $x = "/awe"
      $y = "I like cats."
      file {$x:
        ensure => present,
        content => $y
      }
    """)

    val constraints = ConstraintParser.parse("""
      "/awe" => "I like dogs."
    """)

    val expected = PuppetParser.parse("""
      $x = "/awe"
      $y = "I like dogs."
      file {$x:
        ensure => present,
        content => $y
      }
    """)

    synthesisAssert(manifest, constraints, expected)
  }

  test("Update the contents and title of a single file resource.") {
    val manifest = PuppetParser.parse("""
      $x = "/awe"
      $y = "I like dogs."
      file {$x:
        ensure => present,
        content => $y
      }
    """)

    val constraints = ConstraintParser.parse("""
      "/awe" -> nil, "/rachit" => "I like cats."
    """)

    val expected = PuppetParser.parse("""
      $x = "/rachit"
      $y = "I like cats."
      file {$x:
        ensure => present,
        content => $y
      }
    """)

    synthesisAssert(manifest, constraints, expected)
  }

  test("Update a single instantiation of a complex resource.") {
    val manifest = PuppetParser.parse("""
      file {"/bin/vim":
        ensure => present
      }

      define vim($user) {
        file {"/etc/users/$user":
          ensure => present
        }

        file {"/home/$user":
          ensure => directory
        }

        file {"/home/$user/.vimrc":
          ensure => present,
          content => "set syntax=on"
        }
      }

      $user1 = "arjun"
      vim { user => $user1 }
      $user2 = "awe"
      vim { user => $user2 }
    """)

    val constraints = ConstraintParser.parse("""
      "/etc/users/arjun" -> nil, "/home/arjun" -> nil, "/home/arjun/.vimrc" -> nil,
      "/etc/users/rachit" -> file, "/home/rachit" -> dir, "/home/rachit/.vimrc" -> file
    """)

    val expected = PuppetParser.parse("""
      file {"/bin/vim":
        ensure => present
      }

      define vim($user) {
        file {"/etc/users/$user":
          ensure => present
        }

        file {"/home/$user":
          ensure => directory
        }

        file {"/home/$user/.vimrc":
          ensure => present,
          content => "set syntax=on"
        }
      }

      $user1 = "rachit"
      vim { user => $user1 }
      $user2 = "awe"
      vim { user => $user2 }
    """)

    synthesisAssert(manifest, constraints, expected, doNotEditAbstractions)
  }

  test("Update the body of a define type.") {
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
        g { y => "/bar" }
      }

      $w = "/foo"
      f { x => $w }
      $v = "/baz"
      f { x => $v }
    """)

    synthesisAssert(manifest, constraints, expected, onlyEditAbstractions)
  }

  test("Update the contents and title of a single file resource with many additional resources.") {
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

    val constraints = ConstraintParser.parse("""
      "/awe" -> nil, "/rachit" => "I like cats."
    """)

    val expected = PuppetParser.parse("""
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

      $x = "/rachit"
      $y = "I like cats."
      file {$x:
        ensure => present,
        content => $y
      }
    """)

    val progPaths = FSVisitors.collectPaths(manifest.labeled.compile).flatMap(_.ancestors)
    val constraintPaths = constraints.flatMap(_.paths.flatMap(_.ancestors)).toSet
    val paths = progPaths -- constraintPaths

    synthesisAssert(manifest, constraints, expected, doNotEditPaths(paths))
  }

  test("Update the contents and title of a single file resource in an if statement") {
    val manifest = PuppetParser.parse("""
      $x = "/awe"
      $y = "I like dogs."
      if (true) {
        file {$x:
          ensure => present,
          content => $y
        }
      } else {
        file {$x:
          ensure => directory
        }
      }
    """)

    val constraints = ConstraintParser.parse("""
      "/awe" -> nil, "/rachit" => "I like cats."
    """)

    val expected = PuppetParser.parse("""
      $x = "/rachit"
      $y = "I like cats."
      if (true) {
        file {$x:
          ensure => present,
          content => $y
        }
      } else {
        file {$x:
          ensure => directory
        }
      }
    """)

    synthesisAssert(manifest, constraints, expected)
  }

  test("Update the a single file resource in a complex if statement") {
    val manifest = PuppetParser.parse("""
      $flag = "file"
      $x = "/awe"
      $y = "I like dogs."
      if ($flag == "file") {
        file {$x:
          ensure => present,
          content => $y
        }
      } else {
        if ($flag == "dir") {
          file {$x:
            ensure => directory
          }
        }
      }
    """)

    val constraints = ConstraintParser.parse("""
      "/awe" -> nil, "/rachit" -> dir
    """)

    val expected = PuppetParser.parse("""
      $flag = "dir"
      $x = "/rachit"
      $y = "I like dogs."
      if ($flag == "file") {
        file {$x:
          ensure => present,
          content => $y
        }
      } else {
        if ($flag == "dir") {
          file {$x:
            ensure => directory
          }
        }
      }
    """)

    synthesisAssert(manifest, constraints, expected)
  }
}
