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

  def watch(config: Config): Unit = {
    import java.nio.file.{Files, Paths, StandardOpenOption}
    import java.nio.charset._

    val fileName = config.string("filename")
    val shell = config.string("shell")

    val strace = STrace(shell)
    while (true) {
      if (strace.shouldUpdate) {
        Try({
          val manifest = PuppetParser.parseFile(fileName)
          val labeledManifest = manifest.labeled
          val prog = labeledManifest.compile

          val fs = FSEval.eval(prog)
          val constraints = strace.constraints(fs)

          Synthesizer.synthesize(prog, constraints).map {
            subst => PuppetUpdater.update(labeledManifest, subst)
          }
        }) match {
          case Success(Some(res)) => {
            val javaPath = Paths.get(fileName)
            val content = (res.pretty + "\n").getBytes(StandardCharsets.UTF_8)
            Files.write(javaPath, content, StandardOpenOption.TRUNCATE_EXISTING)
          }
          case Success(None) => {
            println("Failed to synthesize an update to the specified manifest given those constraints.")
          }
          case Failure(exn) => throw exn
        }

        strace.updated()
      }
    }
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
