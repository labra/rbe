package es.weso.rbe
// import es.weso.utils.Checker
import es.weso.validating._
import es.weso.validating.ConstraintError
import ConstraintReason._
import Checked._

/**
 * A node shape
 */
sealed trait NodeShape[+Label,+Node] {
}

/**
 *  Reference to another label
 */
case class Ref[Label](label:Label) extends NodeShape[Label,Nothing] {
}

/**
 * Negation of a expression
 */
case class RefNot[Label](label:Label) extends NodeShape[Label,Nothing]

/**
 *  Reference to a sequence of labels
 */
case class ConjRef[Label](labels:Seq[Label]) extends NodeShape[Label,Nothing]

/**
 *  Reference to an sequence of labels which are disjunctive
 */
case class DisjRef[Label](labels:Seq[Label]) extends NodeShape[Label,Nothing]

case class OrShape[+Label,+Node,+Err](ns:Seq[NodeShape[Label,Node]]) extends NodeShape[Label,Node]

/**
 *  Constraint on nodes (it has a name and a predicate).
 *  
 *  Note: pred is defined in the 2nd parameter section to avoid equality and hashing of functions
 *  
 */
case class Pred[Node](name: String)
  ( val pred: Node => Checked[Node,ConstraintReason,ConstraintError[Node]]) extends NodeShape[Nothing,Node] {
  
}

/**
 * Some common node shapes
 */
object NodeShape {
  
  /**
   * any = any value matches, so no constraint at all
   */
  def any[Node]: Pred[Node] = 
      Pred("any")((node) => ok(singleReason(node, s"$node satisfies constraint any")))
}
