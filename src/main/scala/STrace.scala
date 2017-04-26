package pup

import scala.sys.process._
import scala.util.{Try, Success, Failure}

import Infrastructure._
import STrace._
import SymbolicFS._

object STrace {
  sealed trait OpenFlag
  case object OpenReadOnly extends OpenFlag
  case object OpenWriteOnly extends OpenFlag
  case object OpenReadWrite extends OpenFlag
  case object OpenCreate extends OpenFlag
  case object OpenDirectory extends OpenFlag
  case object OpenFailIfFileExists extends OpenFlag
  case object OpenTruncate extends OpenFlag

  sealed trait Statement

  case class SOpen(
    path: String, flags: Set[OpenFlag], mode: Option[String], exitCode: Int
  ) extends Statement

  case class SStat(
    path: String, flags: Seq[String], exitCode: Int
  ) extends Statement

  case class SRename(
    src: String, dst: String, exitCode: Int
  ) extends Statement

  case class SUnknown(
    cmd: String, path: String, args: Seq[String], exitCode: Int
  ) extends Statement
}

case class STrace(path: String, pid: String) {
  val cmd = Seq("sudo", "strace", "-e", "trace=file,write", "-f", "-p", pid)

  private var shouldSynth = false
  private var traces: Seq[Statement] = Seq()
  val straceLogger = ProcessLogger(
    line => println(line),
    line => Try(STraceParser.parse(line)) match {
      case Success(SStat(path, _, _)) if path == "/bin/synth" => update()
      case Success(SUnknown(_, path, _, _)) if path == "/bin/synth" => update()
      case Success(stmt) => traces = traces :+ stmt
      case Failure(_) => ()
    }
  )

  def start(): Unit = {
    cmd.run(straceLogger)
    println(s"Watching $pid for changes.")
    println("Run `synth` in the shell to update the manifest.")
  }

  def update(): Unit = {
    import java.nio.file.{Files, Paths, StandardOpenOption}
    import java.nio.charset._

    Try({
      val manifest = PuppetParser.parseFile(path)
      val labeledManifest = manifest.labeled
      val prog = labeledManifest.compile

      val fs = FSEval.eval(prog)
      val constraints = this.constraints(fs)

      Synthesizer.synthesize(prog, constraints).map {
        subst => PuppetUpdater.update(labeledManifest, subst)
      }
    }) match {
      case Success(Some(res)) => {
        val javaPath = Paths.get(path)
        val content = (res.pretty + "\n").getBytes(StandardCharsets.UTF_8)
        Files.write(javaPath, content, StandardOpenOption.TRUNCATE_EXISTING)
        println(s"Successfully updated $path")
      }
      case Success(None) => {
        println("Failed to synthesize an update to the specified manifest given those constraints.")
      }
      case Failure(exn) => throw exn
    }
  }

  def affectedPaths(): Set[String] = traces.flatMap {
    case SOpen(path, flags, _, 0) if flags.contains(OpenWriteOnly) => Seq(path)
    case SOpen(path, flags, _, 0) if flags.contains(OpenReadWrite) => Seq(path)
    case SOpen(_, _, _, _) => Seq()
    case SStat(_, _, _) => Seq()
    case SRename(src, dst, 0) => Seq(src, dst)
    case SRename(_, _, _) => Seq()
    case SUnknown(_, _, _, _) => Seq()
  }.toSet

  def constraints(fs: FileSystem): Seq[Constraint] = affectedPaths.map {
    path => path -> getCurrentState(path)
  }.foldLeft(fs) {
    case (acc, pair) => acc update pair
  }.toConstraints
}
