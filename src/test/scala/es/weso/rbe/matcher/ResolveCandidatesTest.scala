package es.weso.rbe.matcher

import org.scalatest._
import es.weso.collection._
import util._
import es.weso.rbe._
import StringGraph._
import es.weso.typing._
import cats._, data._, implicits._

class ResolveCandidatesTest extends FunSpec with Matchers with EitherValues {
  
  type Node = String
  type Label = String
  type Edge = String
  type Evidence = String
  type Shape_ = Shape[DirectedEdge[Edge], Node, Label, RbeError, Evidence]
  type NodeShape_ = NodeShape[Node, Label, RbeError, Evidence]
  type SingleShape_ = SingleShape[DirectedEdge[Edge], Node, Label, RbeError, Evidence]
  type Schema_ = Schema[Edge, Node, Label, RbeError, Evidence]
  type Graph_ = Graph[Edge, Node]
  type Candidate_ = Candidate[Edge, Node, Label, RbeError, Evidence]
  type Candidates_ = Seq[Candidate_]
  type Neigh_ = Neigh[Edge, Node]
  type Neighs_ = Seq[Neigh_]
  type Table_ = Table[Edge, Node, Label, RbeError, Evidence]
  type Typing_ = Typing[Node, Label, RbeError, Evidence]
  type MatcherLog_ = MatcherLog[Edge,Node,Label,Evidence]
  type Triples_ = Set[(Node,Edge,Node)]

  def any: NodeShape_ = NodeShape.any

  def ref(n: Int) = ConstraintRef(value = n)
  val matcher = new IterativeMatcher[Edge, Node, Label] {}
  

  describe("Resolve candidates of :a int") {

    val shape: SingleShape_ =
      Shape.singleShape(Symbol(((DirectEdge("a"), integer)), 1, 1))

    // S { :a int }
    val schema: Schema_ =
      Schema(m = Map("S" -> shape), ignored = Seq())

    val graph: Graph_ = GraphMap(Map("x" -> Seq(("a", "50"))))

    val sorbe = Symbol(ref(1), 1, 1)

    val table: Table_ = Table(
      constraints = Map(ref(1) -> integer),
      edges = Map(DirectEdge("a") -> Set(ref(1))),
      elems = 1)


    it("Compares table with expected table") {
      compareResults(Table.mkTable(shape), (table, sorbe))
    }

    it("Matches (x, a, 50) with S") {
      val result = matcher.run(schema, graph, matcher.matchNodeLabel("x", "S"))
      assertResultHasType(result, "x", "S")
    }

  }

  describe("Resolve candidates of S {:a T}, T {:b Int} ") {

      val shapeS : SingleShape_ =
        Shape.singleShape(Symbol(((DirectEdge("a"), Ref("T"))), 1, 1))

      val shapeT : SingleShape_ =
        Shape.singleShape(Symbol(((DirectEdge("b"), integer)), 1, 1))
        
      val schema: Schema_ =
        Schema(m = Map("S" -> shapeS,"T" -> shapeT),
               ignored = Seq())

      val graph: Graph_ =
        GraphMap(Map(
            "x" -> Seq(("a","y")),
            "y" -> Seq(("b","51"))
            ))

      val sorbe = Symbol(ref(1), 1, 1)

      val table: Table_ = Table(
        constraints = Map(ref(1) -> Ref("T")),
        edges = Map(DirectEdge("a") -> Set(ref(1))),
        elems = 1)

      it("Compares table with expected table") {
        compareResults(Table.mkTable(shapeS), (table, sorbe))
      }

      it("Matches (x,a,50) with S") {
        val result = matcher.run(schema, graph, matcher.matchNodeLabel("x", "S"))
        assertResultHasType(result, "x", "S")
        assertResultHasType(result, "y", "T")
      } 

    }

   describe("Resolve candidates of S {:a T1, :a T2}, T1 {:b Int}, T2 {:b Int} ") {

      val shapeA1 : SingleShape_ =
        Shape.singleShape(Symbol(((DirectEdge("a"), Ref("T1"))), 1, 1))
      val shapeA2 : SingleShape_ =
        Shape.singleShape(Symbol(((DirectEdge("a"), Ref("T2"))), 1, 1))
/*      val shapeS : SingleShape_ =
        Shape.singleShape(And(shapeA1,shapeA2))
        
      val singleShapeS : SingleShape_ =
        Shape.empty.copy(rbe = And(Symbol(((DirectEdge("a"), Ref("T1"))),1,1),Symbol(((DirectEdge("a"), Ref("T2"))),1,1)))

      // S { :a int, (:b any + | :a any) }
      val schema: Schema_ =
        Schema(Map(
            "S" -> singleShapeS,
            "T1" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("b"), integer)), 1, 1)),
            "T2" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("b"), integer)), 1, 1))
            ), Seq())


      val schemaClosed: Schema_ =
        Schema(Map(
            "S" -> Shape.empty.copy(
                rbe = And(Symbol(((DirectEdge("a"), Ref("T1"))),1,1),Symbol(((DirectEdge("a"), Ref("T2"))),1,1)),
                closed = true),
            "T1" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("b"), integer)), 1, 1)),
            "T2" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("b"), integer)), 1, 1))
            ),Seq())


      val g1: Graph_ =
        GraphMap(Map(
            "x" -> Seq(("a","y"), ("a","z")),
            "y" -> Seq(("b","51")),
            "z" -> Seq(("b","52"))
            ))

      val g2: Graph[String,String] =
        GraphMap(Map(
            "x" -> Seq(("a","y"), ("x","x"),("a","z")),
            "y" -> Seq(("b","51")),
            "z" -> Seq(("b","52"))
            ))


      val sorbe = And(Symbol(ref(1), 1, 1),Symbol(ref(2), 1, 1))

      val table: Table[String, String, String] = Table(
        constraints = Map(ref(1) -> Ref("T1"), ref(2) -> Ref("T2")),
        edges = Map(DirectEdge("a") -> Set(ref(1),ref(2))),
        elems = 2)

//      val matcher1 = IterativeMatcher(schema,g1)
//      val matcher2 = IterativeMatcher(schema,g2)
//      val matcherClosed1 = IterativeMatcher(schemaClosed,g1)
//      val matcherClosed2 = IterativeMatcher(schemaClosed,g2)

      it("Compares table with expected table") {
        compareResults(matcher1.mkTable(singleShapeS), Success((table, sorbe)))
      }

  *//*    it("Matches (x,a,y)(x,a,z)(y,b,40)(y,b,51) with S") {
        val typeS: TypeRow[String] = TypeRow(pos = Set("S"),Set())
        val typeT1: TypeRow[String] = TypeRow(pos = Set("T1"),Set())
        val typeT2: TypeRow[String] = TypeRow(pos = Set("T2"),Set())
        val expectedType1 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT1, "z" -> typeT2))
        val expectedType2 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT2, "z" -> typeT1))
        assertResult(Success(Seq(
            (expectedType1,Set(("x","a","y"),("y","b","51"))),
            (expectedType2,Set(("x","a","y"),("y","b","51")))
            ))) {
          matcher1.matchNode("x", "S")
        }
      }
      it("Matches (x,a,y)(x,x,x)(x,a,z)(y,b,40)(y,b,51) with S") {
        val typeS: TypeRow[String] = TypeRow(pos = Set("S"),Set())
        val typeT1: TypeRow[String] = TypeRow(pos = Set("T1"),Set())
        val typeT2: TypeRow[String] = TypeRow(pos = Set("T2"),Set())
        val expectedType1 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT1, "z" -> typeT2))
        val expectedType2 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT2, "z" -> typeT1))
        assertResult(Success(Seq(
            (expectedType1,Set(("x","a","y"),("y","b","51"))),
            (expectedType2,Set(("x","a","y"),("y","b","51")))
            ))) {
          matcher2.matchNode("x", "S")
        }
      }
      it("Doesn't match (x,a,y)(x,x,x)(x,a,z)(y,b,40)(y,b,51) with S if schemaClosed") {
        val typeS: TypeRow[String] = TypeRow(pos = Set("S"),Set())
        val typeT1: TypeRow[String] = TypeRow(pos = Set("T1"),Set())
        val typeT2: TypeRow[String] = TypeRow(pos = Set("T2"),Set())
        val expectedType1 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT1, "z" -> typeT2))
        val expectedType2 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT2, "z" -> typeT1))
        assertResult(Success(Seq())) {
          matcherClosed2.matchNode("x", "S")
        }
      }
*/
    }


  def compareResults[A](s1: A, s2: A) = {
    if (s1 !== s2) {
      fail(s"Values are different\n$s1\n$s2")
    }
  }
  
  implicit def ShowMatcherLog_ = new Show[MatcherLog_] {
    def show(ml: MatcherLog_): String = 
      ml.messages.map{ case (a,msg) => s"$a: $msg" }.mkString("\n")
  }

  def assertResultHasType(result: (MatcherLog_, (Either[RbeError, Typing_], Triples_)),
                          node: Node, label: Label): Unit = {
    val r = result._2._1
    info(s"Log: ${result._1.show}\n-----------------------------")
    if (r.isRight) 
      r.right.value.getOkValues(node) should contain(label)
    else
      fail(s"Result has errors: $r")
  }

}
