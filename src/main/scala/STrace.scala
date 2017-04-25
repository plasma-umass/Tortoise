package pup

import scala.sys.process._

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

  case class SUnknown(
    cmd: String, path: String, args: Seq[String], exitCode: Int
  ) extends Statement
}

case class STrace(shell: String) {
  val cmd = Seq("strace", "-e", "trace=file,write", "-f", shell)

  var traces: Seq[Statement] = Seq()
  val straceLogger = ProcessLogger(
    line => {
      traces = traces :+ STraceParser.parse(line) 
    },
    _ => ()
  )

  cmd ! straceLogger

  def affectedPaths(): Set[String] = traces.flatMap {
    case SOpen(path, flags, _, _) if flags.contains(OpenWriteOnly) => Some(path)
    case SOpen(path, flags, _, _) if flags.contains(OpenReadWrite) => Some(path)
    case SOpen(_, _, _, _) => None
    case SUnknown(_, _, _, _) => None
  }.toSet

  def constraints(fs: FileSystem): Seq[Constraint] = affectedPaths.map {
    path => path -> getCurrentState(path)
  }.foldLeft(fs) {
    case (acc, pair) => acc update pair
  }.toConstraints
}
