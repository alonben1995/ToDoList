package models

import slick.jdbc.H2Profile.api._
import models.PersonTable

case class TaskDetails(name:String, email:String, favoriteProgrammingLanguage: String, 
                        activeTaskCount: Int, id: String)

class TaskTable(tag: Tag) extends Table[TaskDetails](tag, None, "task") { 
    override def * = (title, details, dueDate, status,ownerID) <> (TaskDetails.tupled, TaskDetails.unapply) 
    val title: Rep[String] = column[String]("title") 
    val details: Rep[String] = column[String]("details") 
    val dueDate: Rep[String] = column[String]("dueDate")  //maybe change to specific date format
    val status: Rep[Int] = column[Int]("activeTaskCount") //maybe change to enum
    val ownerID : Rep[String] = column[String]("ownerID") 
    def owner = foreignKey("TASK_FK", ownerID, PersonTable)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)
}