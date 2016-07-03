package es.weso.rbe
import es.weso.collection._
import es.weso.validating._

trait BagChecker[A] {
  
  def rbe: Rbe[A]

  def check(bag:Bag[A], open: Boolean): Checked[Bag[A],ConstraintReason,ConstraintError[Bag[A]]]

}