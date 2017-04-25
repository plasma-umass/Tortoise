package pup

import scala.collection.JavaConverters._
import scala.util.{Try, Success, Failure}

import SymbolicFS.{Constraint, StateConstraint, ContentsConstraint, ModeConstraint}
import pup.{SymbolicFS => S}

/**
  * This module defines general infrastructure for building interactive setups using Pup. It defines
  * a process for converting properties of a file system into synthesis constraints. This will be
  * used both for the PupShell (with a fake file system) and with instrumentation for real shells.
  */
object Infrastructure {
  sealed trait State {
    lazy val isFile = this.isInstanceOf[File]
    lazy val isDir = this.isInstanceOf[Dir]
  }

  case object Nil extends State
  case class Dir(mode: Option[String]) extends State
  case class File(content: Option[String], mode: Option[String]) extends State

  type FileSystem = Map[String, State]
  def emptyFS(): FileSystem = Map()

  implicit class RichFileSystem(fs: FileSystem) {
    def update(pair: (String, State)): FileSystem = fs + pair
    def update(path: String, state: State): FileSystem = fs + (path -> state)

    def toConstraints(): Seq[Constraint] = fs.toSeq.flatMap {
      case (path, Nil) => Seq(StateConstraint(path, S.Nil))
      case (path, Dir(mode)) => {
        StateConstraint(path, S.Dir) +: mode.map(ModeConstraint(path, _)).toSeq
      }
      case (path, File(content, mode)) => {
        Seq(StateConstraint(path, S.File)) ++ content.map(ContentsConstraint(path, _)).toSeq ++
          mode.map(ModeConstraint(path, _)).toSeq
      }
    }
  }

  def getCurrentState(path: String): State = {
    import java.nio.file._
    import java.nio.file.attribute._

    val javaPath = Paths.get(path)

    if (Files.exists(javaPath)) {
      Nil
    } else {
      val mode = PosixFilePermissions.toString(Files.getPosixFilePermissions(javaPath))
      if (Files.isDirectory(javaPath)) {
        Dir(Some(mode))
      } else {
        val content = Files.readAllLines(javaPath).asScala.mkString("\n")
        File(Some(content), Some(mode))
      }
    }
  }
}
