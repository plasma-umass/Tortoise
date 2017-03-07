package pup

import spray.json._
import scala.io.Source
import scalax.collection.mutable.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import PuppetSyntax._

object Catalog {
  case class CRes(typ: String, title: Expr, attrs: Map[String, Expr] , edges: Seq[CEdge]) {
    lazy val value: ResourceVal = {
      implicit val env: Map[String, Expr] = Map()

      ResourceVal(typ, evalToString(title), attrs)
    }
  }

  case class CEdge(source: EResourceRef, target: EResourceRef)
  case class Catalog(resources: Seq[CRes], edges: Seq[CEdge])

  trait WithoutWriter[T] {
    def write(x: T): JsValue = throw new SerializationException(
      "Cannot serialize values of type " + x.getClass.toString
    )
  }

  object CatalogProtocol extends DefaultJsonProtocol {
    var lastLoc = -1
    def freshLoc(): Int = {
      lastLoc += 1
      lastLoc
    }

    val validId = {
      Set('-', '_') union ('a' to 'z').toSet union ('A' to 'Z').toSet union ('0' to '9').toSet
    }

    def interpolateString(str: String): Expr = {
      def nonEmpty(expr: Expr): Boolean = expr match {
        case EStr(s) => s.length > 0
        case EVar(id) => id.length > 0
        case _ => true
      }

      // Split strings into individual components, separating out variables in order.
      val terms = str.toSeq.foldLeft[Seq[Expr]](Seq(EStr(""))) {
        case (acc, char) if char == '$' => EVar("") +: acc
        case (EStr(str) +: acc, char) => EStr(str + char) +: acc
        case (EVar(id) +: acc, char) if validId.contains(char) => EVar(id + char) +: acc
        case (EVar(id) +: acc, char) if Set('{', '}').contains(char) => EVar(id) +: acc
        case (acc@(EVar(_) +: _), char) => EStr(char.toString) +: acc
      }.filter(nonEmpty).reverse.map({
        case str@EStr(_) => str.setLoc(freshLoc())
        case expr => expr
      })

      // Return a simple EStr or EVar if interpolation is not actually taking place here.
      terms match {
        case Seq(term) => term
        case _ => EStrInterp(terms)
      }
    }

    implicit object exprFormat extends JsonFormat[Expr] with WithoutWriter[Expr] {
      val re = """^(.*)\[(.*)\]$""".r

      def read(value: JsValue): Expr = value match {
        case JsString(str) => re.findFirstMatchIn(str) match {
          case Some(res) => EResourceRef(
            res.group(1).toLowerCase, interpolateString(res.group(2).toLowerCase)
          )
          case None => interpolateString(str)
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
            case ref@EResourceRef(_, _) if require => CEdge(
              ref, EResourceRef(typ, interpolateString(title.toLowerCase))
            )
            case ref@EResourceRef(_, _) => CEdge(
              EResourceRef(typ, interpolateString(title.toLowerCase)), ref
            )
            case _ => throw new DeserializationException(s"Expected resource ref, but found $expr")
          }

          val require = params.get("require").map(_.convertTo[Expr]).map(edgify(true))
          val before = params.get("before").map(_.convertTo[Expr]).map(edgify(false))
          val notify = params.get("notify").map(_.convertTo[Expr]).map(edgify(false))
          val edges = Seq(require, before, notify).flatten
          val attrs = (params - "require" - "before" - "notify" - "type").mapValues(x =>
            x.convertTo[Expr]
          )
          CRes(typ.toLowerCase, JsString(title.toLowerCase).convertTo[Expr], attrs, edges)
        }
        case Seq(JsString(typ), title) => {
          CRes(typ.toLowerCase, title.convertTo[Expr], Map(), Seq())
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

  def updateCatalog(catalog: Catalog): (Catalog, Map[String, Int], Map[String, Seq[String]]) = {
    // Build a mapping from class names to environments.
    val envMap: Map[String, Map[String, Expr]] = catalog.resources.filter({
      res => res.typ == "class"
    }).map({
      classRes => (classRes.title.asInstanceOf[EStr].s, classRes.attrs)
    }).toMap

    // Calculate the updated catalog.
    val resCatalog = catalog.edges.filter(edge => edge.source.typ == "class").foldLeft(catalog) {
      case (currentCatalog, edge@CEdge(source, target)) => {
        implicit val env = envMap(source.title.asInstanceOf[EStr].s)

        val resources = currentCatalog.resources.map({
          case res@CRes(typ, title, attrs, edges) => {
            if (typ == target.typ && evalToString(title) == evalToString(target.title)) {
              CRes(typ, evalExpr(title), attrs.mapValues(evalExpr), edges)
            } else {
              res
            }
          }
        })

        val edges = currentCatalog.edges.map({
          case edge@CEdge(source, EResourceRef(targetTyp, targetTitle)) => {
            if (source.typ == "class") {
              implicit val env = envMap(source.title.asInstanceOf[EStr].s)

              CEdge(source, EResourceRef(targetTyp, evalExpr(targetTitle)))
            } else {
              edge
            }
          }
        })

        Catalog(resources, edges)
      }
    }

    // Calculate the location map.
    val locMap = envMap.values.map(_.values).flatten.filter(_.isInstanceOf[EStr]).map({
      case expr@EStr(str) => (str, expr.loc)
      case _ => throw Unexpected("This path should be unreachable.")
    }).toMap

    // Calculate the interpolation map.
    val interpMap = envMap.values.flatMap({
      implicit env => env.values.filter(_.isInstanceOf[EStrInterp]).map({
        case expr@EStrInterp(exprs) => (evalToString(expr), exprs.map(evalToString))
        case _ => throw Unexpected("This path should be unreachable.")
      })
    }).toMap

    (resCatalog, locMap, interpMap)
  }

  def evalToString(expr: Expr)(implicit env: Map[String, Expr]): String = expr match {
    case EVar(id) => evalToString(env(id))
    case EStr(str) => str
    case EStrInterp(exprs) => exprs.map(evalToString).mkString
    case _ => throw Unexpected("This path should be unreachable.")
  }

  def evalExpr(expr: Expr)(implicit env: Map[String, Expr]): Expr = expr match {
    case EVar(id) => evalExpr(env(id))
    case EBool(_) | EStr(_) => expr
    case EStrInterp(exprs) => EStrInterp(exprs.map(evalExpr))
    case EArray(es) => EArray(es.map(evalExpr))
    case EResourceRef(typ, title) => EResourceRef(typ, evalExpr(title))
    case _ => throw Unexpected("This path should be unreachable.")
  }

  def evaluate(initalCatalog: Catalog): EvaluatedManifest = {
    val (catalog, locMap, interpMap) = updateCatalog(initalCatalog)
    val deps = makeGraph(catalog.resources)
    addEdges(deps, catalog.edges)
    elimCompoundResources(deps)

    val resources = catalog.resources.map({
      case res@CRes(_, _, _, _) => (res.value.node, res.value)
    }).toMap

    EvaluatedManifest(resources, deps, locMap, interpMap)
  }

  def parseFile(path: String): EvaluatedManifest = {
    import CatalogProtocol._

    val catalog = Source.fromFile(path).getLines.mkString.parseJson.convertTo[Catalog]
    evaluate(catalog)
  }
}
