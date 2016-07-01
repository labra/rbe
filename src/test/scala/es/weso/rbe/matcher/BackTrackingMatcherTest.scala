package es.weso.rbe.matcher
import es.weso.rbe._

import org.scalatest._

class BacktrackingMatcherTest extends FunSpec with Matchers {

  describe("Backtracking matcher") {
    
    describe("Matching label") {
    val s : Schema[String,String,String,Throwable] =
      Schema(Map("t0" ->
               Shape.empty.copy(rbe = Symbol(((DirectEdge("a"), Ref("t0"))), 1, 1))),
             Seq())
    val g = GraphMap(Map("n0" -> List(("a", "n0"))))
    noMatchNodeLabel("n0", "t1",g,s)
    }
  }
  
    def noMatchNodeLabel[Edge, Node, Label,Err](
      n: Node,
      l: Label,
      g: Graph[Edge, Node],
      s: Schema[Edge, Node, Label,Err]): Unit = {
      it(s"Doesn't match node $n with label $l in graph ${g} and schema ${s}") {
        val matcher = BacktrackingMatcher(s,g)
        val result = matcher.checkNodeLabel(n, l)
        info(result.value.toString)
        result.isOK should be(false)
      }
    }

}