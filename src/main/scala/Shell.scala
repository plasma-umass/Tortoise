package rehearsal

import java.nio.file._
import java.nio.charset._
import scala.collection.JavaConverters._
import scala.io.StdIn
import scala.util.{Try, Success, Failure}
import FSPlusSyntax._
import Implicits._

case class InvalidCommand(
  cmd: String, args: Seq[String]
) extends RuntimeException(s"Failed to parse command: $cmd, with arguments: ${args.mkString(" ")}")

case class Shell(path: String) {
  val prompt = "\u03bb "

  def loop(): Unit = {
    import scala.collection.mutable.Set

    var constraints: Set[ValueConstraint] = Set()

    while (true) {
      print(prompt)
      val line = StdIn.readLine()
      val outProg = Try({
        import SubstitutionPuppet._

        val cmd = parseCommand(line)
        constraints ++= calculateConstraints(cmd)
        val manifest = PuppetParser.parseFile(path)
        PuppetEval.reset()
        val prog = manifest.eval.resourceGraph.fsGraph("ubuntu-trusty").statement
        val optSubst = UpdateSynth.synthesize(prog, constraints.toSeq)
        optSubst.map(subst => applySubst(manifest)(convertSubst(subst)))
      }) match {
        case Success(Some(m)) => {
          println("Successfully synthesized an updated program.")
          val javaPath = Paths.get(path)
          val content = PrettyPuppet.pretty(m).getBytes(StandardCharsets.UTF_8)
          Files.write(javaPath, content, StandardOpenOption.TRUNCATE_EXISTING)
          constraints = Set()
        }
        case Success(None) => {
          println("An update could not be synthesized given those constraints.")
          println(s"Constraints: $constraints")
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
        case Failure(e@InvalidCommand(_, _)) => {
          println(e.getMessage())
        }
        case Failure(exn) => throw exn
      }
    }
  }

  sealed trait Command
  case class CMv(src: String, dst: String) extends Command
  case class CMkdir(path: String) extends Command
  case class CTouch(path: String) extends Command
  // Assumes that create home directory flag is true.
  case class CUserAdd(name: String) extends Command
  case class CUserDel(name: String) extends Command

  def parseCommand(str: String): Command = str.split(" ").toSeq match {
    case Seq("mv", src, dst) => CMv(src, dst)
    case Seq("mkdir", path) => CMkdir(path)
    case Seq("touch", path) => CTouch(path)
    case Seq("useradd", name) => CUserAdd(name)
    case Seq("userdel", name) => CUserDel(name)
    case cmd +: args => throw InvalidCommand(cmd, args)
    case _ => throw Unreachable
  }

  def getCurrentState(pathStr: String): FileState = {
    val path = Paths.get(pathStr)
    if (Files.isDirectory(path)) {
      IsDir
    } else if (Files.notExists(path)) {
      DoesNotExist
    } else {
      IsFile(Files.readAllLines(path, StandardCharsets.UTF_8).asScala.mkString("\n"))
    }
  }

  def calculateConstraints(cmd: Command): Seq[ValueConstraint] = cmd match {
    case CMv(src, dst) => Seq(
      Paths.get(src) -> DoesNotExist,
      Paths.get(dst) -> getCurrentState(src)
    )

    case CMkdir(path) => Seq(
      Paths.get(path) -> IsDir
    )

    case CTouch(path) => Seq(
      Paths.get(path) -> IsFile("")
    )

    case CUserAdd(name) => Seq(
      Paths.get(s"/etc/users/$name") -> IsDir,
      Paths.get(s"/etc/groups/$name") -> IsDir,
      Paths.get(s"/home/$name") -> IsDir
    )

    case CUserDel(name) => Seq(
      Paths.get(s"/etc/users/$name") -> DoesNotExist,
      Paths.get(s"/etc/groups/$name") -> DoesNotExist,
      Paths.get(s"/home/$name") -> DoesNotExist
    )
  }
}
