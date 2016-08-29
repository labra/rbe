package es.weso.rbe
import es.weso.checking._
import cats._, data._
import implicits._
import es.weso.utils.Read
import CheckVal._

/**
 * A node shape
 */
sealed trait NodeShape[+Label,+Node,Err,Evidence] {
}

/**
 *  Reference to another label
 */
case class Ref[Label,Err,Evidence](label:Label) extends NodeShape[Label,Nothing,Err,Evidence] {
}

/**
 * Negation of a expression
 */
case class RefNot[Label,Err,Evidence](label:Label) extends NodeShape[Label,Nothing,Err,Evidence]

/**
 *  Reference to a sequence of labels
 */
case class ConjRef[Label,Err,Evidence](labels:Seq[Label]) extends NodeShape[Label,Nothing,Err,Evidence]

/**
 *  Reference to an sequence of labels which are disjunctive
 */
case class DisjRef[Label,Err,Evidence](labels:Seq[Label]) extends NodeShape[Label,Nothing,Err,Evidence]

case class OrShape[+Label,+Node,Err,Evidence](ns:Seq[NodeShape[Label,Node]]) extends NodeShape[Label,Node,Err,Evidence]

/**
 * Constraint on nodes (it has a name and a predicate).
 *
 * Note: pred is defined in the 2nd parameter section to avoid equality and
 * hashing of functions
 *  
 */
// TODO: Previous type of pred: Node => CheckedVal[Node,List[Node]]
case class Pred[Node,Err,Evidence](name: String)
   ( val pred: Node => CheckedVal[Node,Err,Evidence]) extends NodeShape[Nothing,Node,Err,Evidence] {

}

/**
 * Some common node shapes
 */
object NodeShape {

  /**
   * any = any value matches, so no constraint at all
   */
  def any[Node,Err,Evidence:Read]: Pred[Node,Err,Evidence] =
      Pred("any")((node) => ok(node, s"$node satisfies constraint any"))
}
