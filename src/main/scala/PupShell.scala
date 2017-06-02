package tortoise

import java.nio.file.{Files, Paths, StandardOpenOption}
import java.nio.charset._
import scala.io.StdIn

import Implicits._
import Infrastructure._
import ShellCommands._

case class PupShell(path: String) {
  import PupShell._
  val prompt = "\u03bb "

  def start(): Unit = {
    val fs = FSEval.eval(PuppetParser.parseFile(path).labeled.compile)
    loop(fs)
  }

  def loop(fileSystem: FileSystem): Unit = {
    print(prompt)
    val line = StdIn.readLine()

    parseCommand(line) match {
      // Perform synthesis!
      case CSynth => {
        val manifest = PuppetParser.parseFile(path)
        val labeledManifest = manifest.labeled
        val prog = labeledManifest.compile
        val constraints = fileSystem.toConstraints

        Synthesizer.synthesizeAll(prog, constraints) match {
          case substs@(_ +: _) => {
            val res = UpdateRanker.promptRankedChoice(substs)(labeledManifest)
            val javaPath = Paths.get(path)
            val content = (res.pretty + "\n").getBytes(StandardCharsets.UTF_8)
            Files.write(javaPath, content, StandardOpenOption.TRUNCATE_EXISTING)
            println("Update applied!")

            loop(FSEval.eval(res.labeled.compile))
          }
          case Seq() => {
            println("Failed to synthesize an update to the manifest given those constraints.")
            println(constraints)

            loop(fileSystem)
          }
        }
      }

      case cmd => loop(updateFileSystem(cmd, fileSystem))
    }
  }
}

object PupShell {
  def updateFileSystem(cmd: Command, fs: FileSystem): FileSystem = cmd match {
    case CSynth => throw Unreachable
    case CAptInstall(pkg) => fs update (s"dpkg://$pkg" -> File(None, None, None))
    case CAptRemove(pkg) => fs update (s"dpkg://$pkg" -> Nil)
    case CDnfInstall(pkg) => fs update (s"rpm://$pkg" -> File(None, None, None))
    case CDnfRemove(pkg) => fs update (s"rpm://$pkg" -> Nil)
    case CChmod(mode, path) => fs.getOrElse(path, Nil) match {
      case Nil => fs
      case Dir(_, owner) => fs update (path -> Dir(Some(mode), owner))
      case File(content, _, owner) => fs update (path -> File(content, Some(mode), owner))
    }
    case CChown(owner, path) => fs.getOrElse(path, Nil) match {
      case Nil => fs
      case Dir(mode, _) => fs update (path -> Dir(mode, Some(owner)))
      case File(content, mode, _) => fs update (path -> File(content, mode, Some(owner)))
    }
    case CRm(path) => fs update (path -> Nil)
    case CMv(src, dst) => fs update (dst -> fs(src)) update (src -> Nil)
    case CMkdir(path, false) => fs update (path -> Dir(None, None))
    case CMkdir(path, true) => fs ++ path.ancestors.map(_ -> Dir(None, None))
    case CUserAdd(name, false) => {
      fs update (s"/etc/users/$name" -> Dir(None, None)) update
      (s"/etc/groups/$name" -> Dir(None, None))
    }
    case CUserAdd(name, true) => {
      fs update (s"/etc/users/$name" -> Dir(None, None)) update
      (s"/etc/groups/$name" -> Dir(None, None)) update (s"/home/$name" -> Dir(None, None))
    }
    case CUserDel(name) => {
      fs update (s"/etc/users/$name" -> Nil) update (s"/etc/groups/$name" -> Nil)
    }
    case CPut(path, content) => fs.getOrElse(path, Nil) match {
      case Nil => fs update (path -> File(Some(content), None, None))
      case Dir(mode, owner) => fs update (path -> File(Some(content), mode, owner))
      case File(_, mode, owner) => fs update (path -> File(Some(content), mode, owner))
    }
  }
}
