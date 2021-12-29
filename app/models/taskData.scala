package models

import slick.jdbc.H2Profile.api._

case class TaskData(title:String, details:String, dueDate: String, status: String)