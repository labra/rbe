package es.weso.rbe
import es.weso.checking._
import cats._, data._
import implicits._
import es.weso.utils.Read
import CheckVal._

/**
 * A node shape
 */
sealed trait NodeShape[+Node,+Label,Err,Evidence] {
}

/**
 *  Reference to another label
 */
case class Ref[Label,Err,Evidence](label:Label) extends NodeShape[Nothing, Label,Err,Evidence] {
}

/**
 * Negation of a expression
 */
case class RefNot[Label,Err,Evidence](label:Label) extends NodeShape[Nothing,Label,Err,Evidence]

/**
 *  Reference to a sequence of labels
 */
case class ConjRef[Label,Err,Evidence](labels:Seq[Label]) extends NodeShape[Nothing,Label,Err,Evidence]

/**
 *  Reference to an sequence of labels which are disjunctive
 */
case class DisjRef[Label,Err,Evidence](labels:Seq[Label]) extends NodeShape[Nothing,Label,Err,Evidence]

case class OrShape[+Node, +Label,Err,Evidence](
    ns:Seq[NodeShape[Node,Label,Err,Evidence]]
 ) extends NodeShape[Node,Label,Err,Evidence]

/**
 * Constraint on nodes (it has a name and a predicate).
 *
 * Note: pred is defined in the 2nd parameter section to avoid equality and
 * hashing of functions
 *  
 */
// TODO: Previous type of pred: Node => CheckedVal[Node,List[Node]]
case class Pred[Node,Err,Evidence](name: String)
   ( val pred: Node => CheckVal[Node,Err,Evidence]) extends NodeShape[Node,Nothing,Err,Evidence] {

}

/**
 * Some common node shapes
 */
object NodeShape {

  /**
   * any = any value matches, so no constraint at all
   */
  def any[Node,Label,Err,Evidence:Read]: Pred[Node,Err,Evidence] =
      Pred("any")((node) => ok(node, s"$node satisfies constraint any"))
}
