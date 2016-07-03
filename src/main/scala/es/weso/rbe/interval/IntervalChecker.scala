package es.weso.rbe.interval
import es.weso.rbe._
import es.weso.validating._
import Constraint._
import es.weso.collection._
import Interval._
import IntOrUnbounded._
import es.weso.rbe.deriv._

case class IntervalChecker[A](rbe: Rbe[A]) extends BagChecker[A] {
  
  lazy val derivChecker = DerivChecker(rbe)
  
  def check(bag:Bag[A], open: Boolean): Checked[Bag[A],ConstraintReason,ConstraintError[Bag[A]]] = {
    if (rbe.containsRepeats) {
      derivChecker.check(bag,open)
    } else {
       if (!open && extraSymbols(bag).isEmpty == false) 
         errString(s"Closed expression $rbe will not match with bag $bag because it contains extra symbols ${extraSymbols(bag)}") 
       else 
         if (IntervalChecker.interval(rbe,bag).contains(1)) okSingle(bag,s"$rbe ~ $bag")
         else derivChecker.check(bag,open)
    }
  }
  
  private def extraSymbols(bag: Bag[A]): Seq[A] = {
    bag.elems.map(_._1).filter(!rbe.symbols.contains(_)).toSeq
  }

}

object IntervalChecker {
  
  def interval[A](rbe: Rbe[A], bag: Bag[A]): Interval = {
    rbe match {
      case Fail(_)       => Interval(1,0)
      case Empty         => Interval(0,Unbounded)
      case Symbol(a,n,m) => {
        val wa = bag.multiplicity(a)
        Interval(divIntLimitUp(wa, m),divIntLimitDown(wa,n))
      }
      case And(v1,v2)     => interval(v1,bag) & interval(v2,bag)
      case Or(v1,v2)      => interval(v1,bag) + interval(v2,bag)
      case Star(v)        => {
        if (rbe.noSymbolsInBag(bag)) Interval(0,Unbounded)
        else {
         val ie = interval(v,bag) 
         if (ie.isEmpty) ie
         else Interval(1,Unbounded)
        }
      }
      case Plus(v)        => {
        if (rbe.noSymbolsInBag(bag)) Interval(0,0)
        else {
         val ie = interval(v,bag) 
         if (ie.isEmpty) ie
          else Interval(1,ie.m) 
        }
      } 
      
      // Adding Repetitions on expressions breaks the single-occurrence bag expression
      // This case is handled by detecting repetitions and invoking the derivatives algorithm
      case Repeat(v,n,m) =>
         throw RbeException("Intervals algorithm doesn't work with repetitions. RBE expr: " + this)  
      
    }
    
  }
}