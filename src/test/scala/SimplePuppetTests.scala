import rehearsal._
import Implicits._
import PuppetSyntax._
import FSPlusSyntax.{ValueConstraint}
import PuppetParser.{parse}
import FSPlusParser.{parseConstraints}
import SubstitutionPuppet._
import PuppetPretty._

class SimplePuppetTests extends org.scalatest.FunSuite {
	def makeAssertion(man: Manifest, constraints: Seq[ValueConstraint], expect: Manifest) {
		PuppetEval.reset()

		val prog = man.eval.resourceGraph.fsGraph("ubuntu-trusty").statement

		val subst = UpdateSynth.synthesize(prog,constraints)

		if(subst.isDefined) {
			val res = SubstitutionPuppet.applySubst(man)(convertSubst(subst.get))
			assert(res == expect, s"\nThe constraints were $constraints")
		} else {
			assert(false, s"Synthesis failed.The constraints were:\n$constraints")
		}
	}

	test("Update file resource title") {
		val prog = parse("""
			file { '/rachit':
			  ensure => file,
			  content => "I am a scrublord."
			}
		""")

		val constraints = parseConstraints("""
			</awe> -> File
		""")

		val expect = parse("""
			file { '/awe':
			  ensure => file,
			  content => "I am a scrublord."
			}
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update file resource content") {
		val prog = parse("""
			file { '/rachit':
			  ensure => file,
			  content => "I am a scrublord."
			}
		""")

		val constraints = parseConstraints("""
			</rachit> => "I am 1337 h4x0r"
		""")

		val expect = parse("""
			file { '/rachit':
			  ensure => file,
			  content => "I am 1337 h4x0r"
			}
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update both resource title and content") {
		val prog = parse("""
			file { '/rachit':
			  ensure => file,
			  content => "I am a scrublord."
			}
		""")

		val constraints = parseConstraints("""
			</awe> => "I am 1337 h4x0r"
		""")

		val expect = parse("""
			file { '/awe':
			  ensure => file,
			  content => "I am 1337 h4x0r"
			}
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update title with three resources") {
		val prog = parse("""
			file { '/rachit':
			  ensure => file,
			  content => "I am a scrublord."
			}
			
			file { '/awe':
			  ensure => file,
			  content => "I am a scrublord."
			}

			file { '/arjun':
			  ensure => file,
			  content => "I am a scrublord."
			}
		""")

		val constraints = parseConstraints("""
			</rachit> => "I am 1337 h4x0r"
		""")

		val expect = parse("""
			file { '/rachit':
			  ensure => file,
			  content => "I am 1337 h4xor"
			}
			
			file { '/awe':
			  ensure => file,
			  content => "I am a scrublord."
			}

			file { '/arjun':
			  ensure => file,
			  content => "I am a scrublord."
			}
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update contents with three resources") {
		val prog = parse("""
			file { '/rachit':
			  ensure => file,
			  content => "I am a scrublord."
			}
			
			file { '/awe':
			  ensure => file,
			  content => "I am a scrublord."
			}

			file { '/arjun':
			  ensure => file,
			  content => "I am a scrublord."
			}
		""")

		val constraints = parseConstraints("""
			</rachit> -> null, </cat> => "c4tx0r"
		""")

		val expect = parse("""
			file { '/cat':
			  ensure => file,
			  content => "c4tx0r"
			}
			
			file { '/arjun':
			  ensure => file,
			  content => "I am a scrublord."
			}

			file { '/awe':
			  ensure => file,
			  content => "I am a scrublord."
			}
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update multiple resources to maintain well formedness") {
		val prog = parse("""
			file { '/foo':
			  ensure => directory
			}
			
			file { '/bar':
			  ensure => directory
			}

			file { '/baz':
			  ensure => directory
			}
		""")

		val constraints = parseConstraints("""
			</arjun/awe/rachit> -> Dir
		""")

		val expect = parse("""
			file { '/arjun':
			  ensure => directory
			}
			
			file { '/arjun/awe':
			  ensure => directory
			}

			file { '/arjun/awe/rachit':
			  ensure => directory
			}
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update class instantiation (fails due to path concat generating the wrong string)") {
		val prog = parse("""
			class vim($user) {
			  package { 
			    'vim':
			      ensure => present
			  }

			  user {
			    $user:
			      ensure => present,
			      managehome => true
			  }

			  file {
			    '/home/${user}/.vimrc':
			      ensure => present,
			      content => "set syntax=on",
			      require => Package['vim']
			  }
			}

			vim {
			  'awe':
			    user => 'awe'
			}
		""")

		val constraints = parseConstraints("""
			</home/rachit> -> dir, </etc/users/rachit> -> dir, </etc/groups/rachit> -> dir, 
			</home/rachit/.vimrc> -> file
		""")

		val expect = parse("""
			class vim($user) {
			  package { 
			    'vim':
			      ensure => present
			  }

			  user {
			    $user:
			      ensure => present,
			      managehome => true
			  }

			  file {
			    '/home/${user}/.vimrc':
			      ensure => present,
			      content => "set syntax=on",
			      require => Package['vim']
			  }
			}

			vim {
			  'awe':
			    user => 'rachit'
			}
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update class definition(fails due to path concat generating the wrong string)") {
		val prog = parse("""
			class vim($user) {
			  package { 
			    'vim':
			      ensure => present
			  }

			  user {
			    $user:
			      ensure => present,
			      managehome => true
			  }

			  file {
			    '/home/${user}/.vimrc':
			      ensure => present,
			      content => "set syntax=on",
			      require => Package['vim']
			  }
			}

			vim {
			  'awe':
			    user => 'awe'
			}
		""")

		val constraints = parseConstraints("""
			</home/awe/.vimrc> -> file
		""")

		val expect = parse("""
			class vim($user) {
			  package { 
			    'vim':
			      ensure => present
			  }

			  user {
			    $user:
			      ensure => present,
			      managehome => true
			  }

			  file {
			    '/home/${user}/.vimrc':
			      ensure => present,
			      content => "set syntax=on",
			      require => Package['vim']
			  }
			}

			vim {
			  'awe':
			    user => 'awe'
			}
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update class instantiation with minimum update (useradd + userdel) 1") {
		val prog = parse("""
			package { 
			  'vim':
			    ensure => present
			}

			class vim($user = $title) {

			  user {
			    $user:
			      ensure => present,
			      managehome => true
			  }

			  file {
			    '/home/${user}/.vimrc':
			      ensure => present,
			      content => "set syntax=on",
			      require => Package['vim']
			  }
			}

			vim { 'arjun': }
			vim { 'awe': }
		""")

		val constraints = parseConstraints("""
			</home/rachit> -> dir, </etc/users/rachit> -> dir, </etc/groups/rachit> -> dir, 
			</home/rachit/.vimrc> -> file, </home/arjun> -> null, </etc/users/arjun> -> null, 
			</etc/groups/arjun> -> null, </home/arjun/.vimrc> -> null
		""")

		val expect = parse("""
			package { 
			  'vim':
			    ensure => present
			}

			class vim($user = $title) {

			  user {
			    $user:
			      ensure => present,
			      managehome => true
			  }

			  file {
			    '/home/${user}/.vimrc':
			      ensure => present,
			      content => "set syntax=on",
			      require => Package['vim']
			  }
			}

			vim { 'rachit': }
			vim { 'awe': }
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update class instantiation with minimum update (useradd + userdel) 2") {
		val prog = parse("""
			package { 
			  'vim':
			    ensure => present
			}

			class vim($user = $title) {

			  user {
			    $user:
			      ensure => present,
			      managehome => true
			  }

			  file {
			    '/home/${user}/.vimrc':
			      ensure => present,
			      content => "set syntax=on",
			      require => Package['vim']
			  }
			}

			vim { 'arjun': }
			vim { 'awe': }
		""")

		val constraints = parseConstraints("""
			</home/rachit> -> dir, </etc/users/rachit> -> dir, </etc/groups/rachit> -> dir, 
			</home/rachit/.vimrc> -> file, </home/awe> -> null, </etc/users/awe> -> null, 
			</etc/groups/awe> -> null, </home/awe/.vimrc> -> null
		""")

		val expect = parse("""
			package { 
			  'vim':
			    ensure => present
			}

			class vim($user = $title) {

			  user {
			    $user:
			      ensure => present,
			      managehome => true
			  }

			  file {
			    '/home/${user}/.vimrc':
			      ensure => present,
			      content => "set syntax=on",
			      require => Package['vim']
			  }
			}

			vim { 'arjun': }
			vim { 'rachit': }
		""")

		makeAssertion(prog, constraints, expect)
	}

}
