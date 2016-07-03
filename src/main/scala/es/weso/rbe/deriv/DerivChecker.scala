package es.weso.rbe.deriv
import es.weso.rbe._
import es.weso.validating._
import es.weso.collection._
import Constraint._

case class DerivChecker[A](rbe: Rbe[A]) extends BagChecker[A] {
  
  def check(bag:Bag[A], open: Boolean): Checked[Bag[A],ConstraintReason,ConstraintError[Bag[A]]] = {
    val d = rbe.derivBag(bag, open, rbe.symbols)
    if (d.nullable) okSingle(bag, "Bag matches regular bag expression")
    else {
      d match {
        case Fail(msg) => errString(msg)
        case _ => errString(s"Non nullable expression: $d, bag: $bag, rbe: $rbe, open: $open")
      }
    }
  }
  
}