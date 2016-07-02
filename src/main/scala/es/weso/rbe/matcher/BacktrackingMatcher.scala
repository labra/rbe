package es.weso.rbe.matcher
import util._
import es.weso.typing._
import es.weso.validating._
import es.weso.validating.Constraint._
import es.weso.rbe.DirectedEdge
import es.weso.rbe._
import Shape._


case class BacktrackingMatcher[Edge,Node,Label](
    schema: Schema[Edge,Node,Label],
    graph: Graph[Edge,Node]
  ) extends Matcher[Edge,Node,Label] {

  type Shape_ = Shape[DirectedEdge[Edge],Node,Label]
  type SingleShape_ = SingleShape[DirectedEdge[Edge],Node,Label]
  type NodeShape_ = NodeShape[Label,Node]
  type Pred_ = Pred[Node]
  type Triples = Set[(Node,Edge,Node)]
  type Typing_ = PosNegTyping[Node,Label]
  type Response = (Typing_, Triples)
  type Checked_ = Checked[Node, ConstraintReason, ConstraintError[Node]]

  def matchNode(
      node: Node, 
      label: Label): 
    Try[Seq[(PosNegTyping[Node,Label],Set[(Node,Edge,Node)])]] = {
/*    if (schema.m.constains(label) {
      
    } else 
      
  } */
  ???
  }
  
  
  
  def checkNodeLabel(
      node: Node, 
      label: Label): Checked_ = {
    schema.lookup(label) match {
      case Some(shape) => checkNodeShape(node,shape)
      case None => errString(s"$label not found in schema") 
    }        
  }
  
    def checkNodeShape(node: Node,shape: Shape_): Checked_ = 
     shape match {
      case s@SingleShape(_,_,_,_) => checkNodeSingleShape(node, s)
      case AndShape(s1,s2) => 
       Constraint.all(Seq(checkNodeShape(node,s1), checkNodeShape(node,s2)))
      // case OrShape(s1,s2) => 
      case NotShape(s) => {
        if (checkNodeShape(node,s).isOK) errString(s"Not shape fails because $node matches $s")
        else Constraint.okSingle(node,s"$node matches $shape because it fails to validate $s")
      }
      case _ => errString(s"checkNodeShape: Unsupported shape $shape")
    }
    
   def checkNodeSingleShape(node: Node,shape: SingleShape_): Checked_  = {
     val checkedNode = checkNodeNodeShape(node, shape.nodeShape)
     val neighbours = graph.neighbours(node)
     // TODO: search neighbourhood of node and check with shape.rbe
     // val checkedNeighs = checkRbe(neighbours,shape.rbe) 
     checkedNode
   }

   
   def checkNodeNodeShape(node: Node, nodeShape: NodeShape_): Checked_ = {
     nodeShape match {
       case p@Pred(name) => p.pred(node)
       case _ => ???
     }
   }
   
}