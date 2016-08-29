package es.weso.rbe
import util._
import es.weso.typing._

trait Matcher[Edge,Node,Label,Err,Ev] {
  def matchNode(
      node: Node, 
      label: Label): 
    Typing[Node,Label,Err,Ev]
  
}