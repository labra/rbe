package es.weso.rbe.matcher

import es.weso.validating.ConstraintError
import es.weso.rbe.DirectedEdge
import es.weso.rbe.NodeShape

/**
 * A candidate to match
 */
trait Candidate[Edge,+Node,+Label] {
  def sign : Int
  def value: ConstraintRef
  def isPending: Boolean
  def edge: DirectedEdge[Edge]
}

/**
 * A positive candidate
 */
case class Pos[Edge,Node](
    ref : ConstraintRef, 
    arc: (Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Nothing] {
  def sign = 1
  def value = ref
  def isPending = false
  
  override def toString: String = {
    s"Pos[$ref, $arc, $edge]"
  }
}

/**
 * A candidate that is pending 
 */
case class Pending[Edge,Node,Label](
    n : ConstraintRef, 
    node: Node, 
    ref: Label,
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Label] {
  def sign = 1
  def value = n
  def isPending = true

  override def toString: String = {
    s"Pending[$n, $node, $ref, $arc, $edge]"
  }
}

case class PendingNot[Edge,Node,Label](
    n : ConstraintRef, 
    node: Node, 
    ref: Label,
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Label] {
  def sign = 1
  def value = n
  def isPending = true

  override def toString: String = {
    s"PendingNot[$n, $node, $ref, $arc, $edge]"
  }
}

case class PendingSeq[Edge,Node,Label](
    n : ConstraintRef, 
    node: Node, 
    ref: Seq[Label],
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Label] {
  def sign = 1
  def value = n
  def isPending = true

  override def toString: String = {
    s"PendingSeq[$n, $node, $ref, $arc, $edge]"
  }
}

case class PendingAlt[Edge,Node,Label](
    n : ConstraintRef, 
    node: Node, 
    ref: Seq[Label],
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Label] {
  def sign = 1
  def value = n
  def isPending = true

  override def toString: String = {
    s"PendingAlt[$n, $node, $ref, $arc, $edge]"
  }
}

case class PendingOr[Edge,Node,Label](
    n : ConstraintRef, 
    node: Node, 
    es: Seq[NodeShape[Label,Node]],
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Label] {
  def sign = 1
  def value = n
  def isPending = true

  override def toString: String = {
    s"PendingOr[$n, $node, $es, $arc, $edge]"
  }
}

/**
 * A negative candidate
 */
case class Neg[Edge,Node](
    n : ConstraintRef,
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge],
    errors: Seq[ConstraintError[Node]]) extends Candidate[Edge,Node,Nothing] {
  def sign = -1
  def value = n
  def isPending = false

  override def toString: String = {
    s"PendingNeg[$n, $arc, $edge, $errors]"
  }
}

/**
 * A missing candidate
 */
case class Missing[Edge,Node](
   n: ConstraintRef, 
   arc: (Node,Edge,Node),
   edge: DirectedEdge[Edge]   
   ) extends Candidate[Edge,Node,Nothing] {
  def sign = -1
  def value = n
  def isPending = false
}
