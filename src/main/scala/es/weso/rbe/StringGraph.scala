package es.weso.rbe

import es.weso.utils._
import es.weso.validating._
import Checked._
import ConstraintReason._
/**
 *  Simple graphs whose nodes and edges are strings
 *  They are used for testing purposes only
 */
trait StringGraph extends Graph[String,String] {
}

case class Err(str: String) 

object StringGraph {
  
  implicit def mkErr = Err
  
/*  def cond(x: String, p: String => Boolean,msg: String): Checker[String,Err] = {
    if (p(x)) ok(x)
    else err(Err(msg))
  } */
  /**
   * Checks a predicate on a value
   * @param x the value to check
   * @param p the predicate
   * @param name name of the condition to check
   * @param ferr a function that converts a String into an Error
   * @return if the value satisfies the predicate, a Checker with an ok value, otherwise the error that results of applying ferr to the name of the condition
   */
  def cond[A](x: A, p: A => Boolean, name: String)(implicit ferr: String => Err): Checked[A,ConstraintReason,Err] = {
    if (p(x)) ok(singleReason(x,"OK"))
    else checkError(Err(s"Failed condition $name on $x"))
  }

  lazy val isA: Pred[String,Err] = 
      Pred("isA")(x => 
        cond(x, (x: String) => x == "a","eqA"))

  lazy val integer: Pred[String,Err] = 
      Pred("int")(x => 
        cond(x, (x : String) => x.matches("""\d+"""), "integer"))
        
  lazy val one: Pred[String,Err] = 
      Pred("one")(x => 
        cond(x, (x : String) => x=="1", "== 1"))
      
  lazy val two: Pred[String,Err] = 
      Pred("two")(x => 
        cond(x, (x : String) => x=="2", "== 2"))
          
}