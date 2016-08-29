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

class IntervalCheckerSpec 
 extends IntervalSpec 
 with PropSpec 
 with Matchers 
 with PropertyChecks {
  
  property("intervalChecker...") { 
    forAll(rbe,bag,open)((rbe,bag,open: Boolean) => {
      val intervalChecker = IntervalChecker(rbe)
      val checked = intervalChecker.check(bag,open)
      checked.isOK should (equal(true) or equal(false))
    })
  }
  property("derivMatcher == intervalMatcher") { 
    forAll(rbe,bag,open)((rbe,bag,open: Boolean) => {
      val intervalChecker = IntervalChecker(rbe)
      val derivChecker = DerivChecker(rbe)
      val v1 = intervalChecker.check(bag,open)
      val v2 = derivChecker.check(bag,open)
      if (v1.isOK != v2.isOK) {
        fail(s"Failed: interval = $v1, deriv = $v2")
      }
    })
    
  }
  

}