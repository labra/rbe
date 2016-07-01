package es.weso.rbe.matcher
import util._
import es.weso.typing._
import es.weso.validating._
import es.weso.validating.Constraint._
import es.weso.rbe.DirectedEdge
import es.weso.rbe.Graph
import es.weso.rbe.Schema
import es.weso.rbe.Shape

case class BacktrackingMatcher[Edge,Node,Label,Err](
    schema: Schema[Edge,Node,Label,Err],
    graph: Graph[Edge,Node]
  ) extends Matcher[Edge,Node,Label,Err] {
  
  def matchNode(
      node: Node, 
      label: Label): 
    Try[Seq[(PosNegTyping[Node,Label],Set[(Node,Edge,Node)])]] = {
/*    if (schema.m.constains(label) {
      
    } else 
      
  } */
  ???
  }
  
  type Shape_ = Shape[DirectedEdge[Edge],Node,Label,Err]

  type Triples = Set[(Node,Edge,Node)]
  type Typing_ = PosNegTyping[Node,Label]
  type Response = (Typing_, Triples)
  
  
  def checkNodeLabel(
      node: Node, 
      label: Label): 
    Checked[Node, ConstraintReason, ConstraintError] = {
    schema.lookup(label) match {
      case Some(shape) => checkNodeShape(node,shape)
      case None => errString(s"$label not found in schema") 
    }        
  }
  
    def checkNodeShape(
      node: Node, 
      shape: Shape_): 
    Checked[Node, ConstraintReason, ConstraintError] = ???
}