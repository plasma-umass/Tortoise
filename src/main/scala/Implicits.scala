package pup

import java.nio.file.{Path, Paths}

object Implicits {
  import scala.language.implicitConversions

  implicit def stringToPath(str: String): Path = Paths.get(str)

  implicit class RichString(path: String) {
    def ancestors(): Set[Path] = Paths.get(path).ancestors
  }

  implicit class RichPath(path: Path) {
    def ancestors(): Set[Path] = {
      def loop(p: Path, set: Set[Path]): Set[Path] = {
        if (p == null) {
          set
        } else {
          loop(p.getParent(), set + p)
        }
      }

      loop(path.getParent(), Set())
    }
  }
}
