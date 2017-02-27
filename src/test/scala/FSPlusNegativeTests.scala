import pup._
import FSPlusParser._
import FSPlusSyntax.{Statement, ValueConstraint}

class FSPlusNegativeTests extends org.scalatest.FunSuite {

	def makeNegativeAssertion(prog: Statement, constraints: Seq[ValueConstraint]) {
		val subst = UpdateSynth.synthesize(prog,constraints)


		if(subst.isDefined) {
			val res = SubstitutionPlus.applySubst(prog)(subst.get)
			assert(false, s"Synthesis should have failed, instead, it produced:\n$res\nConstraints:$constraints ")
		}
	}

	test("Update dir path, unsat constraints") {
		val prog = parse("""
			mkdir(</rachit>)
		""")

		val constraints = parseConstraints("""
			</awe> -> Dir, </rachit> -> Dir
		""")

		makeNegativeAssertion(prog, constraints)
	}

	test("Update dir path, parent dir missing") {
		val prog = parse("""
			mkdir(</rachit>)
		""")

		val constraints = parseConstraints("""
			</haxor/rachit> -> Dir
		""")

		makeNegativeAssertion(prog, constraints)
	}

	test("Assert mkdir path is null. Synthesis should not introduce random paths to sat.") {
		val prog = parse("""
			mkdir(</rachit>)
		""")

		val constraints = parseConstraints("""
			</rachit> -> Null
		""")

		makeNegativeAssertion(prog, constraints)
	}

	test("Synthesis shoud not remove random paths to satisfy constraints.") {
		val prog = parse("""
			mkdir(</rachit>);
			rm(</rachit>)
		""")

		val constraints = parseConstraints("""
			</rachit> -> Dir
		""")

		makeNegativeAssertion(prog, constraints)
	}

	test("Synthesis should not generate scripts that lead to incorrect file systems.") {
		val prog = parse("""
			mkdir(</foo>);
			mkdir(</bar>);
			mkdir(</foo/rachit>);
			rm(</bar>)
		""")

		val constraints = parseConstraints("""
			</foo> -> Null
		""")

		makeNegativeAssertion(prog, constraints)
	}

}
