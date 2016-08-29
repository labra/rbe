package es.weso.collection

import org.scalatest._
//import org.scalatest.matchers._
import org.scalatest.prop._
import org.scalacheck.Gen


class BagSpec 
  extends PropSpec 
  with Matchers 
  with GeneratorDrivenPropertyChecks
  with GenBag
  {

  val bg : Gen[Boolean] = Gen.oneOf(true,false)

  val evenInts = for (n <- Gen.choose(-1000, 1000)) yield 2 * n

  forAll (evenInts) { (n) => n % 2 should equal (0) }

  property("Size of bag increases after inserting a value"){
    forAll (bagOfCharFromContainer)((bag: Bag[Char]) => {
      (bag.insert('a').size) should equal (bag.size + 1)
    })
  } 

}
