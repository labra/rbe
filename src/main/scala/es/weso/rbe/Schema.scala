package es.weso.rbe

import es.weso.collection.Bag
import es.weso.typing.PosNegTyping
import es.weso.utils.ConsoleDebugger
import es.weso.utils.SeqUtils._
import es.weso.utils.TryUtils._
import es.weso.validating._

import scala.util._
import es.weso.rbe.matcher.Matcher

//import es.weso.rbe.SESchemaException

/**
 * Defines a Schema which is a map from Labels to Shapes
 */
case class Schema[Edge,Node,Label](
    m: Map[Label,Shape[DirectedEdge[Edge],Node,Label]],
    ignored: Seq[DirectedEdge[Edge]]  // Edges that are ignored in the definition of closed schemas
    ) {
  
  type Shape_ = Shape[DirectedEdge[Edge],Node,Label]
  
  def lookup(label: Label): Option[Shape_] = {
    m.get(label)
  }
  
  

}

object Schema {
  def empty[Edge,Node,Label](): Schema[Edge,Node,Label] = 
    Schema(Map(), Seq())
  
  def matchNode[Edge,Node,Label](
      node: Node, 
      label:Label,
      schema:Schema[Edge,Node,Label], 
      graph: Graph[Edge,Node])(implicit matcher: Matcher[Edge,Node,Label]): 
      Try[Seq[(PosNegTyping[Node,Label],Set[(Node,Edge,Node)])]] = {
    matcher.matchNode(node, label)
  }
  
}
