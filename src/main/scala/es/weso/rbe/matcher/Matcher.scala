package es.weso.rbe.matcher
import util._
import es.weso.typing._
import es.weso.rbe.Graph
import es.weso.rbe.Schema

trait Matcher[Edge,Node,Label,Err] {
  def matchNode(
      node: Node, 
      label: Label): 
    Try[Seq[(PosNegTyping[Node,Label],Set[(Node,Edge,Node)])]]
  
}