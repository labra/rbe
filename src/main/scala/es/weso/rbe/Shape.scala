package es.weso.rbe

import es.weso.validating.ConstraintError

abstract class Shape[Edge,Node,Label]

/**
  * A shape contains a regular bag expression, a closed modifier and a list of extras
  *
  * @param rbe regular bag expression
  * @param extras list of extra edges that are allowed
  * @param closed the shape is closed
  */
case class SingleShape[Edge,Node,Label](
    nodeShape: NodeShape[Label,Node],
    rbe: Rbe[(Edge,NodeShape[Label,Node])], 
    extras: Seq[Edge], 
    closed: Boolean
) extends Shape[Edge,Node,Label]

case class AndShape[Edge,Node,Label](
   s1: Shape[Edge,Node,Label],
   s2: Shape[Edge,Node,Label]) extends Shape[Edge,Node,Label]

/*case class OrShape[Edge,Node,Label,Err](
   s1: Shape[Edge,Node,Label,Err],
   s2: Shape[Edge,Node,Label,Err]) extends Shape[Edge,Node,Label,Err] */

case class NotShape[Edge,Node,Label](
   s: Shape[Edge,Node,Label]) extends Shape[Edge,Node,Label]

object Shape {

  def empty = SingleShape(
    nodeShape = NodeShape.any,
    rbe = Empty,
    extras = Seq(),
    closed = false)

  def singleShape[Edge,Node,Label](
      rbe: Rbe[(Edge,NodeShape[Label,Node])],
      extras: Seq[Edge] = Seq(),
      closed: Boolean = false): SingleShape[Edge,Node,Label] =
    SingleShape(
     nodeShape = NodeShape.any,
     rbe = rbe,
     extras = extras,
     closed = closed)

}
