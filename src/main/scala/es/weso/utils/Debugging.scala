package es.weso.utils

import org.slf4j._
import org.apache.log4j._
import scala.collection.JavaConversions._

/**
  * Trait for things that can log
  */
trait Debugging {

 val usingLogger: Boolean = false
 val usingPrintln: Boolean = false
 val log = LoggerFactory.getLogger("Debugging")

 def debug(msg: String) {
    if (usingLogger) {
      log.debug(msg)
    }
    if (usingPrintln) {
      println(msg)
    }
  }

}

