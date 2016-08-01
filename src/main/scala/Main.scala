package rehearsal

object Main extends App {

  import edu.umass.cs.extras.Implicits._
  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global
  import java.nio.file.Files
  import Implicits._
  import scala.util.{Try, Success, Failure}

  case class Config(
    command: Config => Unit,
    string: Map[String, String],
    bool: Map[String, Boolean],
    int: Map[String, Int])

  def usage(config: Config): Unit = {
    println(parser.usage)
    System.exit(1)
  }

  def puppetSynthesizer(config: Config): Unit = {
    val fileName = config.string("filename")
    val constraints = FSPlusParser.parseConstraints(config.string("constraints"))
    
    Try({
      val manifest = PuppetParser.parseFile(fileName)
      val prog = manifest.eval.resourceGraph.fsGraph("ubuntu").statement
      UpdateSynth.synthesize(prog, constraints)
    }) match {
       case Success(Some(stmt)) => {
        println("Successfully synthesized an updated program:")
        println(stmt)
      }
      case Success(None) => {
        println("An update could not be synthesized given those constraints.")
      }
      case Failure(ParseError(msg)) => {
        println("Failed to parse original program:")
        println(msg)
      }
      case Failure(MalformedFSPlusException) => {
        println("Failed to evaluate program:")
        println("Tried to evaluate a malformed FS+ program.")
      }
      case Failure(FSPlusEvalError(msg)) => {
        println("Failed to evaluate program:")
        println(msg)
      }
      case Failure(exn) => throw exn
    } 
  }

  def synthesizer(config: Config): Unit = {
    val fileName = config.string("filename")
    val constraints = FSPlusParser.parseConstraints(config.string("constraints"))

    Try(UpdateSynth.synthesize(FSPlusParser.parseFile(fileName), constraints)) match {
      case Success(Some(stmt)) => {
        println("Successfully synthesized an updated program:")
        println(stmt)
      }
      case Success(None) => {
        println("An update could not be synthesized given those constraints.")
      }
      case Failure(ParseError(msg)) => {
        println("Failed to parse original program:")
        println(msg)
      }
      case Failure(MalformedFSPlusException) => {
        println("Failed to evaluate program:")
        println("Tried to evaluate a malformed FS+ program.")
      }
      case Failure(FSPlusEvalError(msg)) => {
        println("Failed to evaluate program:")
        println(msg)
      }
      case Failure(exn) => throw exn
    }
  }

  val parser = new scopt.OptionParser[Config]("rehearsal") {

    def string(name: String) = {
      opt[String](name).required
        .action((x, c)  => c.copy(string = c.string + (name -> x)))
    }

    def bool(name: String) = {
      opt[Boolean](name).required
        .action((x, c)  => c.copy(bool = c.bool + (name -> x)))
    }

    def int(name: String) = {
      opt[Int](name).required
        .action((x, c)  => c.copy(int = c.int + (name -> x)))
    }

    head("rehearsal", "0.1")

    cmd("synth")
        .action((_, c) => c.copy(command = synthesizer))
        .text("Synthesize an update to the specified Puppet manifest.")
        .children(string("filename"), string("constraints"))

    cmd("synth-manifest")
        .action((_, c) => c.copy(command = puppetSynthesizer))
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
