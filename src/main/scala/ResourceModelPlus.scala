package rehearsal

import java.nio.file.Paths
import scalaj.http.Http
import rehearsal.FSPlusSyntax._
import rehearsal.Implicits._

object ResourceModelPlus {
  sealed trait Content
  case class CInline(data: String) extends Content
  case class CFile(src: String) extends Content

  sealed trait Res {
    def compile(distro: String): Statement = ResourceModelPlus.compile(this, distro)
  }

  case class File(path: Path, content: Content, force: Boolean) extends Res
  case class EnsureFile(path: Path, content: Content) extends Res
  case class AbsentPath(path: Path, force: Boolean) extends Res
  case class Directory(path: Path) extends Res
  case class Package(name: String, present: Boolean) extends Res
  case class Group(name: String, present: Boolean) extends Res
  case class User(name: String, present: Boolean, manageHome: Boolean) extends Res
  case class Service(name: String) extends Res {
    val path = s"/etc/init.d/$name"
  }
  case class Cron(present: Boolean, command: String, user: String, hour: String,
                  minute: String, month: String, monthday: String) extends Res
  case class Host(ensure: Boolean, name: String, ip: String, target: String) extends Res

  case class SshAuthorizedKey(user: String, present: Boolean, name: String, key: String) extends Res {
    val keyPath = s"/home/$user/.ssh/$name"
  }

  case object Notify extends Res

  def queryPackage(distro: String, pkg: String): Option[Set[Path]] = {
    val resp = logTime(s"Fetching package listing for $pkg ($distro)") {
      Http(s"http://${Settings.packageHost}/query/$distro/$pkg").timeout(2 * 1000, 60 * 1000).asString
    }
    if (resp.isError) {
      None
    }
    else {
      Some(resp.body.lines.map(s => Paths.get(s)).toSet)
    }
  }

  def compile(res: Res, distro: String): Statement = res match {

    case EnsureFile(p, CInline(c)) => {
      SLet("path", EPath(CPath(JavaPath(p), ???)),
        ite(PTestFileState(EId("path"), IsFile("")), rm(EId("path")), SSkip) >> mkfile(EId("path"), EString(CString(c, ???)))
      )
    }

    case EnsureFile(p, CFile(s)) => {
      SLet("path", EPath(CPath(JavaPath(p), ???)),
        SLet("srcPath", EPath(CPath(JavaPath(Paths.get(s)), ???)),
          ite(PTestFileState(EId("path"), IsFile("")), rm(EId("path")), SSkip) >> cp(EId("srcPath"), EId("path"))
        )
      )
    }

    case File(p, CInline(c), false) => {
      SLet("path", EPath(CPath(JavaPath(p), ???)),
        SLet("content", EString(CString(c, ???)),
          ite(PTestFileState(EId("path"), IsFile("")),
            rm(EId("path")) >> mkfile(EId("path"), EId("content")),
            ite(PTestFileState(EId("path"), DoesNotExist),
              mkfile(EId("path"), EId("content")),
              SError
            )
          )
        )
      )
    }

    case File(p, CInline(c), true) => {
      // Uses a loop in generation.
      ???
    }

    case File(p, CFile(s), false) => {
      SLet("srcPath", EPath(CPath(JavaPath(Paths.get(s)), ???)),
        SLet("dstPath", EPath(CPath(JavaPath(p), ???)),
          ite(PTestFileState(EId("dstPath"), IsFile("")),
            rm(EId("dstPath")) >> cp(EId("srcPath"), EId("dstPath")),
            ite(PTestFileState(EId("dstPath"), DoesNotExist),
              cp(EId("srcPath"), EId("dstPath")),
              SError
            )
          )
        )
      )
    }

    case File(p, CFile(s), true) => {
      SLet("srcPath", EPath(CPath(JavaPath(Paths.get(s)), ???)),
        SLet("dstPath", EPath(CPath(JavaPath(p), ???)),
          ite(PTestFileState(EId("dstPath"), IsDir) || PTestFileState(EId("dstPath"), IsFile("")),
            rm(EId("dstPath")),
            SSkip
          ) >> cp(EId("srcPath"), EId("dstPath"))
        )
      )
    }

    case AbsentPath(p, false) => {
      SLet("path", EPath(CPath(JavaPath(p), ???)),
        ite(PTestFileState(EId("path"), IsFile("")),
          rm(EId("path")),
          SSkip
        )
      )
    }

    case AbsentPath(p, true) => {
      SLet("path", EPath(CPath(JavaPath(p), ???)),
        ite(PTestFileState(EId("path"), DoesNotExist),
          SSkip,
          rm(EId("path"))
        )
      )
    }

    case Directory(p) => {
      SLet("path", EPath(CPath(JavaPath(p), ???)),
        ite(PTestFileState(EId("path"), IsDir),
          SSkip,
          ite(PTestFileState(EId("path"), IsFile("")),
            rm(EId("path")),
            SSkip
          ) >> mkdir(EId("path"))
        )
      )
    }

    case User(name, present, manageHome) => {
      val (uRoot, gRoot, hRoot, sub) = ("/etc/users", "/etc/groups", "/home", name)
      val (u, g, h) = (EId("userPath"), EId("groupPath"), EId("homePath"))

      SLet("userPath", EConcat(EPath(CPath(JavaPath(uRoot), ???)), EString(CString(sub, ???))),
        SLet("groupPath", EConcat(EPath(CPath(JavaPath(gRoot), ???)), EString(CString(sub, ???))),
          SLet("homePath", EConcat(EPath(CPath(JavaPath(hRoot), ???)), EString(CString(sub, ???))),
            if (present) {
              val homeCmd = if (manageHome) {
                ite(PTestFileState(h, DoesNotExist), mkdir(h), SSkip)
              } else {
                SSkip
              }
              ite(PTestFileState(u, DoesNotExist), mkdir(u), SSkip) >>
              ite(PTestFileState(g, DoesNotExist), mkdir(g), SSkip) >>
              homeCmd
            } else {
              val homeCmd = if (manageHome) {
                ite(PTestFileState(h, DoesNotExist), SSkip, rm(h))
              } else {
                SSkip
              }
              ite(PTestFileState(u, DoesNotExist), SSkip, rm(u)) >>
              ite(PTestFileState(g, DoesNotExist), SSkip, rm(g)) >>
              homeCmd
            }
          )
        )
      )
    }

    case Group(name, present) => {
      val (root, sub) = ("/etc/groups", name)

      SLet("path", EConcat(EPath(CPath(JavaPath(root), ???)), EString(CString(sub, ???))),
        if (present) {
          ite(PTestFileState(EId("path"), DoesNotExist),
            mkdir(EId("path")),
            SSkip
          )
        } else {
          ite(PTestFileState(EId("path"), DoesNotExist),
            SSkip,
            rm(EId("path"))
          )
        }
      )
    }

    case Package(name, true) => {
      val paths = queryPackage(distro, name).getOrElse(throw PackageNotFound(distro, name))
      val dirs = paths.map(_.ancestors).reduce(_ union _) - root -- Settings.assumedDirs.toSet
      val files = paths -- dirs

      val mkdirs = dirs.toSeq.sortBy(_.getNameCount).map { p =>
        SLet("path", EPath(CPath(JavaPath(p), ???)),
          ite(PTestFileState(EId("path"), IsDir),
            SSkip,
            mkdir(EId("path"))
          )
        )
      }

      val content = "arbitrary content"
      val mkfiles = files.toSeq.map { p =>
        mkfile(EPath(CPath(JavaPath(p), ???)), EString(CString(content, ???)))
      }

      val stmts = mkdirs ++ mkfiles
      val (main, sub) = ("/packages", name)

      // apt does not remove pre-existing conflicting files.
      SLet("path", EConcat(EPath(CPath(JavaPath(main), ???)), EString(CString(sub, ???))),
        ite(PTestFileState(EId("path"), IsFile("")),
          SSkip,
          mkfile(EId("path"), EString(CString(content, ???))) >> seq(stmts: _*)
        )
      )
    }

    case Package(name, false) => {
      val files = queryPackage(distro, name).getOrElse(throw PackageNotFound(distro, name)).toSeq
      val stmts: Seq[Statement] = files.map { p =>
        SLet("path", EPath(CPath(JavaPath(p), ???)),
          ite(PTestFileState(EId("path"), DoesNotExist),
            SSkip,
            rm(EId("path"))
          )
        )
      }

      val (root, sub) = ("/packages", name)

      SLet("path", EConcat(EPath(CPath(JavaPath(root), ???)), EString(CString(sub, ???))),
        ite(PTestFileState(EId("path"), DoesNotExist),
          SSkip,
          rm(EId("path")) >> seq(stmts: _*)
        )
      )
    }

    case self@SshAuthorizedKey(user, present, _, key) => {
      SLet("path", EPath(CPath(JavaPath(self.keyPath), ???)),
        if (present) {
          ite(PTestFileState(EId("path"), IsFile("")),
            rm(EId("path")),
            SSkip
          ) >> mkfile(EId("path"), EString(CString(key, ???)))
        } else {
          ite(PTestFileState(EId("path"), IsFile("")),
            rm(EId("path")),
            SSkip
          )
        }
      )
    }

    case self@Service(name) => {
      ite(PTestFileState(EPath(CPath(JavaPath(self.path), ???)), IsFile("")),
        SSkip,
        SError
      )
    }

    case Cron(present, cmd, user, hour, minute, month, monthday) => {
      val name = cmd.hashCode.toString + "-" + cmd.toLowerCase.filter(c => c >= 'a' && c <= 'z')
      val (root, sub) = (Settings.modelRoot, s"crontab-$name")
      val content = "arbitrary content"

      SLet("path", EConcat(EPath(CPath(JavaPath(root), ???)), EString(CString(sub, ???))),
        if (present) {
          ite(PTestFileState(EId("path"), DoesNotExist),
            mkfile(EId("path"), EString(CString(content, ???))),
            SSkip
          )
        } else {
          ite(PTestFileState(EId("path"), DoesNotExist),
            SSkip,
            rm(EId("path"))
          )
        }
      )
    }

    case Host(ensure, name, ip, target) => {
      val (root, sub) = (Settings.modelRoot, s"host-$name")
      val content = "Managed by Rehearsal."

      val s1 = {
        ite(PTestFileState(EId("target"), DoesNotExist),
          SSkip,
          rm(EId("target"))
        ) >> mkfile(EId("target"), EString(CString(content, ???)))
      }

      val s2 = if (ensure) {
        ite(PTestFileState(EId("path"), DoesNotExist),
          mkfile(EId("path"), EString(CString(ip, ???))),
          SSkip
        )
      } else {
        ite(PTestFileState(EId("path"), DoesNotExist),
          SSkip,
          rm(EId("path"))
        )
      }

      SLet("path", EConcat(EPath(CPath(JavaPath(root), ???)), EString(CString(sub, ???))),
        SLet("target", EPath(CPath(JavaPath(target), ???)),
          s1 >> s2
        )
      )
    }

    case Notify => SSkip

    case _ => ???

  }
}
