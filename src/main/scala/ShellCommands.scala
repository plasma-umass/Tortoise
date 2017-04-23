package pup

object ShellCommands {
  sealed trait Command
  case object CSynth extends Command
  case class CMv(src: String, dst: String) extends Command
  case class CMkdir(path: String, createSubDirs: Boolean) extends Command
  case class CUserAdd(name: String, createHome: Boolean) extends Command
  case class CUserDel(name: String) extends Command

  def parseCommand(str: String): Command = str.split(" ").toSeq match {
    case Seq("synth") => CSynth
    case Seq("mv", src, dst) => CMv(src, dst)
    case Seq("mkdir", path) => CMkdir(path, false)
    case Seq("mkdir", "-p", path) => CMkdir(path, true)
    case Seq("useradd", name) => CUserAdd(name, false)
    case Seq("useradd", "-m", name) => CUserAdd(name, true)
    case Seq("userdel", name) => CUserDel(name)
  }
}
