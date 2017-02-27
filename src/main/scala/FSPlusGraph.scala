package pup

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import com.typesafe.scalalogging.LazyLogging
import Implicits._

case class ExecTreePlus(commutingGroup: List[(FSPlusGraph.Key, FSPlusSyntax.Statement)],
                    branches: List[ExecTreePlus],
                    graph: FSPlusGraph) {

  import FSPlusSyntax._

  val fringeSize: Int = if (branches.isEmpty) 1 else branches.map(_.fringeSize).sum

  def statements(): Statement = {
    seq(commutingGroup.map(_._2): _*) >> seq(branches.map(_.statements()): _*)
  }

  def size(): Int = 1 + branches.map(_.size).sum

}

object FSPlusGraph {

  trait Key
  case class Unlabelled private[FSPlusGraph](index: Int) extends Key

  private var i = 0

  def key(): Key = {
    val n = i
    i = i + 1
    Unlabelled(n)
  }

}

// A potential issue with graphs of FS programs is that several resources may
// compile to the same FS expression. Slicing makes this problem more likely.
// To avoid this problem, we keep a map from unique keys to expressions and
// build a graph of the keys. The actual values of the keys don't matter, so
// long as they're unique. PuppetSyntax.Node is unique for every resource, so
// we use that when we load a Puppet file. For testing, the keys can be
// anything.
case class FSPlusGraph(
  stmts: Map[FSPlusGraph.Key, FSPlusSyntax.Statement],  deps: Graph[FSPlusGraph.Key, DiEdge]
) extends LazyLogging {

  import FSPlusGraph._

  lazy val size: Int = {
    deps.nodes.map(n => stmts(n).size).reduce(_ + _) + deps.edges.size
  }

  def statement(): FSPlusSyntax.Statement = {
    FSPlusSyntax.seq(deps.topologicalSort().map(k => stmts(k)): _*)
  }

  def toExecTreePlus: ExecTreePlus = {
    def loop(g: Graph[Key, DiEdge]): List[ExecTreePlus] = {
      if (g.isEmpty) {
        Nil
      }
      else {
        val fringe = g.nodes.filter(_.inDegree == 0).toList.map(_.value)
        fringe.map(key => ExecTreePlus(List((key, stmts(key))), loop(g - key), this))
      }
    }

    logTime("building execution tree") {
      loop(deps) match {
        case List(node) => node
        case alist => ExecTreePlus(Nil, alist, this)
      }
    }
  }

  /** All paths used by the nodes of this graph. */
  lazy val allPaths = this.deps.nodes.map(n => this.stmts(n).paths)
    .foldLeft(Set.empty[Path])(_ union _)

}
