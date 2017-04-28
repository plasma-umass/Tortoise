package pup

import scala.util.{Try, Success, Failure}

object Main extends App {

  case class Config(
    command: Config => Unit,
    string: Map[String, String],
    bool: Map[String, Boolean],
    int: Map[String, Int]
  )

  def usage(config: Config): Unit = {
    println(parser.usage)
    System.exit(1)
  }

  def ranktest(config: Config): Unit = {
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

    // userdel awe
    // useradd -m arjun
    val constraints = ConstraintParser.parse("""
      "/foo" -> nil, "/bar" -> dir, "/baz" -> dir
    """)

    val labeledManifest = manifest.labeled
    val prog = labeledManifest.compile

    val programs = Synthesizer.synthesizeAll(prog, constraints).map {
      subst => PuppetUpdater.update(labeledManifest, subst)
    }

    programs.foreach {
      program => {
        println(program.pretty)
        println()
      }
    }
  }

  def watch(config: Config): Unit = {
    import java.nio.file.{Files, Paths, StandardOpenOption}
    import java.nio.charset._

    val fileName = config.string("filename")
    val shell = config.string("shell")

    val strace = STrace(fileName, shell).start()
  }

  def shell(config: Config): Unit = {
    val fileName = config.string("filename")
    PupShell(fileName).start()
  }

  def synthesize(config: Config): Unit = {
    val fileName = config.string("filename")
    val constraintString = config.string("constraints")

    Try({
      val manifest = PuppetParser.parseFile(fileName)
      println(s"Original manifest:\n\n${manifest.pretty}\n")
      val labeledManifest = manifest.labeled
      val constraints = ConstraintParser.parse(constraintString)
      val prog = labeledManifest.compile
      Synthesizer.synthesize(prog, constraints).map {
        subst => PuppetUpdater.update(labeledManifest, subst)
      }
    }) match {
      case Success(Some(result)) => {
        println("Update synthesis was successful!\nThe updated manifest is:")
        println()
        println(result.pretty)
      }
      case Success(None) => {
        println("Failed to synthesize an update to the specified manifest given those constraints.")
      }
      case Failure(exn) => throw exn
    }
  }

  val parser = new scopt.OptionParser[Config]("pup") {

    def string(name: String) = opt[String](name).required.action {
      (x, c)  => c.copy(string = c.string + (name -> x))
    }

    def bool(name: String) =  opt[Boolean](name).required.action {
      (x, c)  => c.copy(bool = c.bool + (name -> x))
    }

    def int(name: String) = opt[Int](name).required.action {
      (x, c)  => c.copy(int = c.int + (name -> x))
    }

    head("pup", "0.1")

    cmd("synth")
      .action((_, c) => c.copy(command = synthesize))
      .text("Synthesize an update to the specified Puppet manifest.")
      .children(string("filename"), string("constraints"))

    cmd("shell")
      .action((_, c) => c.copy(command = shell))
      .text("Starts a simulated shell for updating the specified Puppet manifest.")
      .children(string("filename"))

    cmd("watch")
      .action((_, c) => c.copy(command = watch))
      .text("Instruments the specified shell for updating the specified Puppet manifest.")
      .children(string("filename"), string("shell"))

    cmd("ranktest")
      .action((_, c) => c.copy(command = ranktest))
      .text("Runs a test of the update ranking.")
  }

  parser.parse(args, Config(usage, Map(), Map(), Map())) match {
    case None => {
      println(parser.usage)
      System.exit(1)
    }
    case Some(config) => {
      config.command(config)
    }
  }
}
