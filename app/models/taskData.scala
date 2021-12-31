package models

import slick.jdbc.H2Profile.api._
import models.status

case class TaskData(title:String, details:String, dueDate: String, status: String)