package es.weso.rbe

abstract class Shape[Edge,Node,Label,Err]

/**
  * A shape contains a regular bag expression, a closed modifier and a list of extras
  *
  * @param rbe regular bag expression
  * @param extras list of extra edges that are allowed
  * @param closed the shape is closed
  */
case class SingleShape[Edge,Node,Label,Err](
    nodeShape: NodeShape[Label,Node,Err],
    rbe: Rbe[(Edge,NodeShape[Label,Node,Err])], 
    extras: Seq[Edge], 
    closed: Boolean
) extends Shape[Edge,Node,Label,Err]

case class AndShape[Edge,Node,Label,Err](
   s1: Shape[Edge,Node,Label,Err],
   s2: Shape[Edge,Node,Label,Err]) extends Shape[Edge,Node,Label,Err]

/*case class OrShape[Edge,Node,Label,Err](
   s1: Shape[Edge,Node,Label,Err],
   s2: Shape[Edge,Node,Label,Err]) extends Shape[Edge,Node,Label,Err] */

case class NotShape[Edge,Node,Label,Err](
   s: Shape[Edge,Node,Label,Err]) extends Shape[Edge,Node,Label,Err]

object Shape {

  def empty = SingleShape(
    nodeShape = NodeShape.any,
    rbe = Empty,
    extras = Seq(),
    closed = false)

  def singleShape[Edge,Node,Label,Err](
      rbe: Rbe[(Edge,NodeShape[Label,Node,Err])],
      extras: Seq[Edge] = Seq(),
      closed: Boolean = false): SingleShape[Edge,Node,Label,Err] =
    SingleShape(
     nodeShape = NodeShape.any,
     rbe = rbe,
     extras = extras,
     closed = closed)

}
