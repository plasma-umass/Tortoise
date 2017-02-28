package pup

import spray.json._
import scala.io.Source
import scalax.collection.mutable.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import PuppetSyntax._

object Catalog {
  case class CRes(value: ResourceVal, edges: Seq[CEdge])
  case class CEdge(source: EResourceRef, target: EResourceRef)
  case class Catalog(resources: Seq[CRes], edges: Seq[CEdge])

  trait WithoutWriter[T] {
    def write(x: T): JsValue = throw new SerializationException(
      "Cannot serialize values of type " + x.getClass.toString
    )
  }

  object CatalogProtocol extends DefaultJsonProtocol {
    implicit object exprFormat extends JsonFormat[Expr] with WithoutWriter[Expr] {
      val re = """^(.*)\[(.*)\]$""".r

      def read(value: JsValue): Expr = value match {
        case JsString(str) => re.findFirstMatchIn(str) match {
          case Some(res) => EResourceRef(res.group(2).toLowerCase, EStr(res.group(1).toLowerCase))
          case None => EStr(str)
        }
        case JsBoolean(b) => EBool(b)
        case JsArray(arr) => EArray(arr.map(read(_)))
        case _ => throw new DeserializationException(s"Unexpected value: $value")
      }
    }

    implicit object edgeFormat extends JsonFormat[CEdge] with WithoutWriter[CEdge] {
      def read(value: JsValue): CEdge = value.asJsObject.getFields("source", "target") match {
        case Seq(source, target) => CEdge(
          source.convertTo[Expr].asInstanceOf[EResourceRef],
          target.convertTo[Expr].asInstanceOf[EResourceRef]
        )
        case res => throw new DeserializationException(s"Failed to deserialize edge from $res")
      }
    }

    implicit object resFormat extends JsonFormat[CRes] with WithoutWriter[CRes] {
      def read(value: JsValue): CRes = value.asJsObject.getFields(
        "type", "title", "parameters"
      ) match {
        case Seq(JsString(typ), JsString(title), JsObject(params)) => {
          def edgify(require: Boolean)(expr: Expr): CEdge = expr match {
            case ref@EResourceRef(_, _) if require => CEdge(ref, EResourceRef(typ, EStr(title)))
            case ref@EResourceRef(_, _) => CEdge(EResourceRef(typ, EStr(title)), ref)
            case _ => throw new DeserializationException(s"Expected resource ref, but found $expr")
          }

          val require = params.get("require").map(_.convertTo[Expr]).map(edgify(true))
          val before = params.get("before").map(_.convertTo[Expr]).map(edgify(false))
          val notify = params.get("notify").map(_.convertTo[Expr]).map(edgify(false))
          val edges = Seq(require, before, notify).flatten
          val attrs = (params - "require" - "before" - "notify" - "type").mapValues(x =>
            x.convertTo[Expr]
          )
          CRes(ResourceVal(typ.toLowerCase, title.toLowerCase, attrs), edges)
        }
        case Seq(JsString(typ), JsString(title)) => {
          CRes(ResourceVal(typ.toLowerCase, title.toLowerCase, Map()), Seq())
        }
        case res => throw new DeserializationException(s"Failed to deserialize resource from $res")
      }
    }

    implicit val catalogFormat = jsonFormat2(Catalog.apply)
  }

  def makeGraph(resources: Seq[CRes]): Graph[FSPlusGraph.Key, DiEdge] = {
    val graph = Graph[FSPlusGraph.Key, DiEdge]()
    for (res <- resources) {
      graph.add(res.value.node)
    }
    for (res <- resources) {
      addEdges(graph, res.edges)
    }
    graph
  }

  def addEdges(graph: Graph[FSPlusGraph.Key, DiEdge], edges: Seq[CEdge]): Unit = {
    for (
      CEdge(
        EResourceRef(srcTyp, EStr(srcTitle)), EResourceRef(targetTyp, EStr(targetTitle))
      ) <- edges
    ) {
      val edge: DiEdge[FSPlusGraph.Key] = DiEdge(
        Node(srcTyp, srcTitle), Node(targetTyp, targetTitle)
      )
      graph.add(edge)
    }
  }

  val toElim = Set("stage", "class")

  def elimCompoundResources(graph: Graph[FSPlusGraph.Key, DiEdge]): Unit = {
    graph.foreach(param => param match {
      case OuterNode(node) if toElim.contains(node.asInstanceOf[Node].typ) => {
        for (pre <- graph.find(node).get.inNeighbors; post <- graph.find(node).get.outNeighbors) {
          graph.add(DiEdge(from = pre.value, to = post.value))
        }
      }
      case _ => ()
    })
    graph.foreach(param => param match {
      case OuterNode(node) if toElim.contains(node.asInstanceOf[Node].typ) => {
        assert(graph.remove(node))
      }
      case _ => ()
    })
  }

  def evaluate(catalog: Catalog): EvaluatedManifest = {
    val deps = makeGraph(catalog.resources)
    addEdges(deps, catalog.edges)
    elimCompoundResources(deps)

    val resources = catalog.resources.map({
      case CRes(value, _) => (value.node, value)
    }).toMap

    EvaluatedManifest(resources, deps, Map(), Map())
  }

  def parseFile(path: String): EvaluatedManifest = {
    import CatalogProtocol._

    val catalog = Source.fromFile(path).getLines.mkString.parseJson.convertTo[Catalog]
    evaluate(catalog)
  }
}
