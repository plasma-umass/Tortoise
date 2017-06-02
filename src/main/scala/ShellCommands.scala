package tortoise

object ShellCommands {
  sealed trait Command
  case object CSynth extends Command
  case class CAptInstall(pkg: String) extends Command
  case class CAptRemove(pkg: String) extends Command
  case class CDnfInstall(pkg: String) extends Command
  case class CDnfRemove(pkg: String) extends Command
  case class CChmod(mode: String, path: String) extends Command
  case class CChown(owner: String, path: String) extends Command
  case class CRm(path: String) extends Command
  case class CMv(src: String, dst: String) extends Command
  case class CMkdir(path: String, createSubDirs: Boolean) extends Command
  case class CUserAdd(name: String, createHome: Boolean) extends Command
  case class CUserDel(name: String) extends Command
  case class CPut(path: String, content: String) extends Command

  def parseCommand(str: String): Command = str.split(" ").toSeq match {
    case Seq("synth") => CSynth
    case Seq("apt", "install", pkg) => CAptInstall(pkg)
    case Seq("apt", "remove", pkg) => CAptRemove(pkg)
    case Seq("dnf", "install", pkg) => CDnfInstall(pkg)
    case Seq("dnf", "remove", pkg) => CDnfRemove(pkg)
    case Seq("chmod", mode, path) => CChmod(mode, path)
    case Seq("chown", owner, path) => CChown(owner, path)
    case Seq("rm", path) => CRm(path)
    case Seq("mv", src, dst) => CMv(src, dst)
    case Seq("mkdir", path) => CMkdir(path, false)
    case Seq("mkdir", "-p", path) => CMkdir(path, true)
    case Seq("useradd", name) => CUserAdd(name, false)
    case Seq("useradd", "-m", name) => CUserAdd(name, true)
    case Seq("userdel", name) => CUserDel(name)
    case "put" +: path +: content => CPut(path, content.mkString(" "))
  }
}
