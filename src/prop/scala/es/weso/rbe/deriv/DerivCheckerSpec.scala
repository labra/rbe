package es.weso.rbe.interval

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import es.weso.collection._
import es.weso.rbe._
import IntOrUnbounded.int2LimitInt
import es.weso.rbe.deriv._

class DerivCheckerSpec 
  extends IntervalSpec 
  with PropSpec 
  with Matchers 
  with PropertyChecks {
  
    
  property("derivChecker...") { 
    forAll(rbe,bag,open)((rbe,bag,open: Boolean) => {
      val derivChecker = DerivChecker(rbe)
      val checked = derivChecker.check(bag,open)
      checked.isOK should (equal(true) or equal(false))
    })
  }

}