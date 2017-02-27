import pup._
import FSPlusParser._
import FSPlusSyntax.{Statement, ValueConstraint}

class FSPlusTests extends org.scalatest.FunSuite {

	def makeAssertion(prog: Statement, constraints: Seq[ValueConstraint], expect: Statement) {
		val subst = UpdateSynth.synthesize(prog,constraints)

		if(subst.isDefined) {
			val res = SubstitutionPlus.applySubst(prog)(subst.get)
			assert(res == expect)
		} else {
			assert(false, "Synthesis failed")
		}
	}

	test("Update dir path") {
		val prog = parse("""
			mkdir(</rachit>)
		""")

    val constraints = parseConstraints("""
			</awe> -> Dir
		""")

		val expect = parse("""
			mkdir(</awe>)
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update with frozen holes") {

		val prog = parse("""
			mkdir(!</rachit>);
			mkdir(</awe>)
		""")

    val constraints = parseConstraints("""
			</arjun> -> Dir
		""")

		val expect = parse("""
			mkdir(</rachit>);
			mkdir(</arjun>)
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update string in a file resource") {
		val prog = parse("""
			mkfile(</rachit>, "n00b")
		""")

    val constraints = parseConstraints("""
			</rachit> => "1337"
		""")

		val expect = parse("""
			mkfile(</rachit>, "1337")
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update multiple paths to preserve well-formedness") {
		val prog = parse("""
			mkdir(</foo>);
			mkdir(</bar>);
			mkdir(</baz>)
		""")

    val constraints = parseConstraints("""
			</rachit/arjun/awe> -> Dir
		""")

		val expect = parse("""
			mkdir(</rachit>);
			mkdir(</rachit/arjun>);
			mkdir(</rachit/arjun/awe>)
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Make minimum edit by specifying the path to be deleted") {
		val prog = parse("""
			mkdir(</foo>);
			mkdir(</bar>);
			mkdir(</baz>)
		""")

    val constraints = parseConstraints("""
			</rachit> -> Dir, </baz> -> Null
		""")

		val expect = parse("""
			mkdir(</foo>);
			mkdir(</bar>);
			mkdir(</rachit>)
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update in branch") {
		val prog = parse("""
			mkdir(</foo>);
			if dir?(</foo>)
			then mkdir(</bar>)
			else mkdir(</baz>)
		""")

    val constraints = parseConstraints("""
			</awe> -> Dir, </foo> -> Dir
		""")

		val expect = parse("""
			mkdir(</foo>);
			if dir?(</foo>)
			then mkdir(</awe>)
			else mkdir(</baz>)
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update with let bindings") {
		val prog = parse("""
			let path = </init>
			in let main = </vote>
			in mkdir(path);
			   if dir?(path)
			   then mkdir(main);
			        mkfile(main + </awe>, "yea")
			   else mkdir(main)
		""")

		val constraints = parseConstraints("""
			</note/awe> -> File, </init> -> Dir
		""")

		val expect = parse("""
			let path = </init>
			in let main = </note>
			in mkdir(path);
			   if dir?(path)
			   then mkdir(main);
			        mkfile(main + </awe>, "yea")
			   else mkdir(main)
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update simple path concatenation") {
		val prog = parse("""
			mkdir(</home> + <rachit>)
		""")

    val constraints = parseConstraints("""
			</home/awe> -> Dir
		""")

		val expect = parse("""
			mkdir(</home> + <awe>)
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update path concatenation with let bindings") {
		val prog = parse("""
			let home = </home> in
			mkdir(home);
			mkdir(home + <rachit>);
			mkdir(home + <awe>);
			mkdir(home + <arjun>)
		""")

		val constraints = parseConstraints("""
			</root/awe> -> Dir,
			</root/rachit> -> Dir,
			</root/arjun> -> Dir
		""")

		val expect = parse("""
			let home = </root> in
			mkdir(home);
			mkdir(home + <rachit>);
			mkdir(home + <awe>);
			mkdir(home + <arjun>)
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update simple string concatenation") {
		val prog = parse("""
			mkfile(</rachit>, !"more cats " + "more dogs")
		""")

		val constraints = parseConstraints("""
			</rachit> => "more cats less dogs"
		""")

		val expect = parse("""
			mkfile(</rachit>, !"more cats " + "less dogs")
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update string concatenation with let bindings") {
		val prog = parse("""
			let str = "user is " in
			mkfile(</rachit>, str + "active");
			mkfile(</arjun>, str + "active");
			mkfile(</awe>, str + "active")
		""")

		val constraints = parseConstraints("""
			</rachit> => "user is inactive",
			</arjun> => "user is super-user",
			</awe> => "user is a cat"
		""")

		val expect = parse("""
			let str = "user is" in
			mkfile(</rachit>, str + "inactive");
			mkfile(</arjun>, str + "super-user");
			mkfile(</awe>, str + "a cat")
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update let binding in string concatenation 2") {
		val prog = parse("""
			let str = "user is " in
			mkfile(</rachit>, str + "active");
			mkfile(</arjun>, str + "active");
			mkfile(</awe>, str + "active")
		""")

		val constraints = parseConstraints("""
			</rachit> => "cat is active",
			</arjun> => "cat is active",
			</awe> => "cat is active"
		""")

		val expect = parse("""
			let str = "cat is " in
			mkfile(</rachit>, str + "active");
			mkfile(</arjun>, str + "active");
			mkfile(</awe>, str + "active")
		""")

		makeAssertion(prog, constraints, expect)
	}

	test("Update remove") {
		val prog = parse("""
			mkdir(</foo>);
			mkdir(</bar>);
			mkdir(</baz>);
			rm(</foo>)
		""")

		val constraints = parseConstraints("""
			</baz> -> Null
		""")

		val expect = parse("""
			mkdir(</foo>);
			mkdir(</bar>);
			mkdir(</baz>);
			rm(</baz>)
		""")

		makeAssertion(prog, constraints, expect)
	}

}
