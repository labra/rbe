package es.weso.rbe
import es.weso.utils._
import es.weso.checking._

import CheckVal._

/**
 *  Simple graphs whose nodes and edges are strings
 *  They are used for testing purposes mainly
 */
trait StringGraph extends Graph[String,String] {
}

case class Err(str: String)


object StringGraph {

implicit val readErr = new Read[Err] {
  def read(str: String) = Err(str)
}

implicit val readString = new Read[String] {
  def read(str: String) = str
}

  implicit def mkErr = Err

  type Pred_ = Pred[String,Err,String]
  /**
   * Checks a predicate on a value
   * @param x the value to check
   * @param p the predicate
   * @param name name of the condition to check
   * @param ferr a function that converts a String into an Error
   * @return if the value satisfies the predicate, a Checker with an ok value, otherwise the error that results of applying ferr to the name of the condition
   */
  def cond[A](x: A,
      p: A => Boolean,
      name: String)
      (implicit ferr: String => Err): CheckVal[A,Err,String] = {
    if (p(x)) ok(x,"OK")
    else errString(s"Failed condition $name on $x")
  }

  lazy val isA: Pred_ =
      Pred("isA")(x =>
        cond(x, (x: String) => x == "a","eqA"))

  lazy val integer: Pred_ =
      Pred("int")(x =>
        cond(x, (x : String) => x.matches("""\d+"""), "integer"))

  lazy val letter: Pred_ =
      Pred("letter")(x =>
        cond(x, (x : String) => x.matches("""[a-zA-Z]+"""), "letter"))

  lazy val size2: Pred_ =
       Pred("size2")(x =>
        cond(x, (x : String) => x.length == 2, "size2"))

  lazy val one: Pred_ =
      Pred("one")(x =>
        cond(x, (x : String) => x=="1", "== 1"))

  lazy val two: Pred_ =
      Pred("two")(x =>
        cond(x, (x : String) => x=="2", "== 2"))

}
