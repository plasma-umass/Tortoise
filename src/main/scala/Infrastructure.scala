package pup

import scala.collection.JavaConverters._

import SymbolicFS.{Constraint, StateConstraint, ContentsConstraint, ModeConstraint, OwnerConstraint}
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
  case class Dir(mode: Option[String], owner: Option[String]) extends State
  case class File(
    content: Option[String], mode: Option[String], owner: Option[String]
  ) extends State

  type FileSystem = Map[String, State]
  def emptyFS(): FileSystem = Map()

  implicit class RichFileSystem(fs: FileSystem) {
    def update(pair: (String, State)): FileSystem = fs + pair
    def update(path: String, state: State): FileSystem = fs + (path -> state)

    def toConstraints(): Seq[Constraint] = fs.toSeq.flatMap {
      case (path, Nil) => Seq(StateConstraint(path, S.Nil))
      case (path, Dir(mode, owner)) => {
        Seq(StateConstraint(path, S.Dir)) ++ mode.map(ModeConstraint(path, _)).toSeq ++
          owner.map(OwnerConstraint(path, _)).toSeq
      }
      case (path, File(content, mode, owner)) => {
        Seq(StateConstraint(path, S.File)) ++ content.map(ContentsConstraint(path, _)).toSeq ++
          mode.map(ModeConstraint(path, _)).toSeq ++ owner.map(OwnerConstraint(path, _)).toSeq
      }
    }
  }

  def getCurrentState(path: String): State = {
    import java.nio.file._
    import java.nio.file.attribute._

    def convertToString(perms: Set[PosixFilePermission]): String = {
      import PosixFilePermission._

      val owner =
        perms.find(_ == OWNER_READ).map(_ => "1").getOrElse("0") +
        perms.find(_ == OWNER_WRITE).map(_ => "1").getOrElse("0") +
        perms.find(_ == OWNER_EXECUTE).map(_ => "1").getOrElse("0")

      val group =
        perms.find(_ == GROUP_READ).map(_ => "1").getOrElse("0") +
        perms.find(_ == GROUP_WRITE).map(_ => "1").getOrElse("0") +
        perms.find(_ == GROUP_EXECUTE).map(_ => "1").getOrElse("0")

      val others =
        perms.find(_ == OTHERS_READ).map(_ => "1").getOrElse("0") +
        perms.find(_ == OTHERS_WRITE).map(_ => "1").getOrElse("0") +
        perms.find(_ == OTHERS_EXECUTE).map(_ => "1").getOrElse("0")

      Integer.parseInt(owner, 2).toString +
      Integer.parseInt(group, 2).toString +
      Integer.parseInt(others, 2).toString
    }

    val javaPath = Paths.get(path)

    if (Files.notExists(javaPath)) {
      Nil
    } else {
      val mode = convertToString(Files.getPosixFilePermissions(javaPath).asScala.toSet)
      if (Files.isDirectory(javaPath)) {
        Dir(Some(mode), None)
      } else {
        val content = Files.readAllLines(javaPath).asScala.mkString("\n")
        File(Some(content), Some(mode), None)
      }
    }
  }
}
