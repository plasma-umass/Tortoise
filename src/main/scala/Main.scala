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

  def synthesize(config: Config): Unit = {
    val fileName = config.string("filename")
    val constraintString = config.string("constraints")

    Try({
      val manifest = PuppetParser.parseFile(fileName)
      val constraints = ConstraintParser.parse(constraintString)
      val prog = manifest.compile
      println(prog.partialed.pretty)
      println()
      Synthesizer.synthesize(prog, constraints).get
    }) match {
      case Success(subst) => {
        println(subst)
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
