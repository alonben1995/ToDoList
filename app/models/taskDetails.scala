package models

import slick.jdbc.H2Profile.api._
import models.PersonDetails
import models.PersonTable
import models.status


case class TaskDetails(title:String, details:String, dueDate: String, 
                        status: String, ownerID: String,id:String)

class TaskTable(tag: Tag) extends Table[TaskDetails](tag, None, "task") { 
    override def * = (title, details, dueDate, status,ownerID,id) <> (TaskDetails.tupled, TaskDetails.unapply) 
    val title: Rep[String] = column[String]("title") 
    val details: Rep[String] = column[String]("details") 
    val dueDate: Rep[String] = column[String]("dueDate")  //maybe change to specific date format
    val status: Rep[String] = column[String]("status") //maybe change to enum
    val ownerID : Rep[String] = column[String]("ownerID") 
    val id : Rep[String] = column[String]("id") 
    def owner = foreignKey("TASK_FK", ownerID,TableQuery[PersonTable])(_.id, ForeignKeyAction.Restrict,ForeignKeyAction.Cascade)
}