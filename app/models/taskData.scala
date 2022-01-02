package models

import slick.jdbc.H2Profile.api._
import models.status
import java.time.LocalDate

case class TaskData(title:String, details:String, dueDate: LocalDate, status: String)