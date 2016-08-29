package es.weso.rbe

/**
 * 
 */
case class Table[Edge,Node,Label](
    constraints: Map[ConstraintRef,NodeShape[Label,Node]],
    edges: Map[DirectedEdge[Edge],Set[ConstraintRef]],
    elems: Int) {
  
  def addEdge(
      e: DirectedEdge[Edge], 
      n: ConstraintRef): Map[DirectedEdge[Edge],Set[ConstraintRef]] = {
    edges.updated(e, 
        edges.get(e).getOrElse(Set()) + n)
  }
  
  
}

object Table {
  def empty[Edge,Node,Label,Err]: Table[Edge,Node,Label] =
    Table(Map(),Map(),0)
}    
