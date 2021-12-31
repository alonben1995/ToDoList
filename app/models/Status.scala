package models

import slick.jdbc.H2Profile.api._
import scala.Enumeration
  
object status extends Enumeration {
    type status = Value
    val active = Value("active")
    val done = Value("done")

    def isLegalStatus(status: String) = values.exists(_.toString == status)
}