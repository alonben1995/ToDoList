package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scala.collection.mutable
import models._
import slick.jdbc.H2Profile.api._
import slick.jdbc.H2Profile
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.atomic._
import java.time.LocalDate

@Singleton
class TodoListController @Inject()(val controllerComponents: ControllerComponents)
extends BaseController {
    // definition of Json formatters for easy json parsing into each our case classes -   
    implicit val personDataJson = Json.format[PersonData]
    implicit val personDetailsJson = Json.format[PersonDetails]
    implicit val taskDetailsJson = Json.format[TaskDetails]
    implicit val taskDataJson = Json.format[TaskData]
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    // counters for new people / tasks, incremented when added.
    var personId = 0;
    var taskId = 0;

    // connection to the H2 database - 
    val db = Database.forConfig("h2mem")

    // definition and creation of the relevant tables for our database - one of PersonDetails and one of TaskDetails.
    val personTable = TableQuery[PersonTable]
    val taskTable = TableQuery[TaskTable]
    val schemas= personTable.schema ++ taskTable.schema
    val setupTables = db.run(schemas.create)
    
    // this function queries a person by an id and returns a PersonDetails Sequence (which might be empty if no such person exists)
    def queryPersonById(id: String) = 
    {
      val personByIdQuery = personTable.filter(_.id === id)
      val personFuture: Future[Seq[PersonDetails]] = db.run[Seq[PersonDetails]](personByIdQuery.result)
      val personSeq: Seq[PersonDetails] = Await.result(personFuture, 5.seconds)
      personSeq
    }

    // this function queries a task by an id and returns a TaskDetails Sequence (which might be empty if no such task exists)
    def queryTaskById(id: String) = 
    {
      val taskByIdQuery = taskTable.filter(_.id === id)
      val taskFuture: Future[Seq[TaskDetails]] = db.run[Seq[TaskDetails]](taskByIdQuery.result)
      val taskSeq: Seq[TaskDetails] = Await.result(taskFuture, 5.seconds)
      taskSeq
    }

    /* test cases -
     curl -v -d "{\"name\": \"Yossi\", \"email\": \"yos@gmail.com\", \"favoriteProgrammingLanguage\": \"Java\"}"  -H "Content-Type:application/json" -X POST localhost:9000/api/people 
     curl -v -d "{\"name\": \"Gil\", \"email\": \"gil@gmail.com\", \"favoriteProgrammingLanguage\": \"C++\"}"  -H "Content-Type:application/json" -X POST localhost:9000/api/people 
    */
    def addNewPerson() = Action 
    { implicit request =>
      val content = request.body
      val jsonObject = content.asJson
 
      val newPerson: Option[PersonData] = jsonObject.flatMap(Json.fromJson[PersonData](_).asOpt)
      newPerson match 
      {
        case Some(newPerson) =>
          personId.synchronized {personId = personId + 1}
          val toBeAdded = PersonDetails(newPerson.name, newPerson.email,newPerson.favoriteProgrammingLanguage, 0, personId.toString)
          val doesEmailExistQuery = personTable.filter(_.email === newPerson.email)
          val sameEmailPeopleFuture: Future[Seq[PersonDetails]] = db.run[Seq[PersonDetails]](doesEmailExistQuery.result)
          val sameEmailPeople: Seq[PersonDetails] = Await.result(sameEmailPeopleFuture, 5.seconds)
          if (sameEmailPeople.length > 0)
            BadRequest("A person with this email already exists\n")
          else 
          {
            val insertPlayerQuery = personTable += toBeAdded
            val insertResult:Future[Int] = db.run(insertPlayerQuery)
            Created(Json.toJson(toBeAdded))
          }
        case None =>
          BadRequest("invalid data\n")
      }
    }

    // curl localhost:9000/api/people 
    def getPeople() = Action
    {
      val peopleFuture: Future[Seq[PersonDetails]] = db.run(personTable.result)
      val people = Await.result(peopleFuture, 5.seconds)
      val jsonPeople = Json.toJson(people)
      Ok(jsonPeople)
    }

    /* test cases - 
      curl localhost:9000/api/people/1
    */
    def getPerson(id: String) = Action
    {
      val personSeq: Seq[PersonDetails] = queryPersonById(id)
      if (personSeq.length > 0)
        {
          val person = personSeq.head
          val personJson = Json.toJson(person)
          Ok(personJson)
        }
      else NotFound("No person with this id, please try again\n")
    }
    /* test cases - 
      curl -v -d "{\"name\": \"YOS\", \"email\": \"YOS@gmail.com\", \"favoriteProgrammingLanguage\": \"Python\"}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/people/1 
      curl -v -d "{\"favoriteProgrammingLanguage\": \"C\"}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/people/1 
    */
    def updatePerson(id: String) = Action 
    {
      // store the recieved data for updating as Json, and then extract the optional fields.
      implicit request =>
      val content: AnyContent = request.body
      val jsonObject: Option[JsValue] = content.asJson
      val extractedJson: JsValue = jsonObject.get
      val nameOption: Option[String] = (extractedJson \ "name").asOpt[String]
      val emailOption = (extractedJson \ "email").asOpt[String]
      val languageOption = (extractedJson \ "favoriteProgrammingLanguage").asOpt[String]

      val personSeq: Seq[PersonDetails] = queryPersonById(id)
      if (personSeq.length > 0)
      {
        // update optional fields which we received data for - 
        if (nameOption.isDefined)
        {
          val name = nameOption.get
          val updatePersonName = personTable.filter(_.id === id).map(_.name).update(name)
          val updateName = db.run(updatePersonName)
        }
        if (emailOption.isDefined)
        {
          val email = emailOption.get
          val updatePersonEmail = personTable.filter(_.id === id).map(_.email).update(email)
          val updateEmail = db.run(updatePersonEmail)
        }
        if (languageOption.isDefined)
        {
          val language = languageOption.get
          val updatePersonLanguage = personTable.filter(_.id === id).map(_.favoriteProgrammingLanguage).update(language)
          val updateLanguage = db.run(updatePersonLanguage)
        }
        // return the opdated personDetails with code 200 -
        val personSeq: Seq[PersonDetails] = queryPersonById(id)
        val person = personSeq.head
        val personJson = Json.toJson(person)
        Ok(personJson)
      }
      else NotFound("No person with this id, please try again\n")
      
    }

    // curl -X DELETE localhost:9000/api/people/1 
    def deletePerson(id: String) = Action
    {
      val personSeq = queryPersonById(id)
      if (personSeq.length > 0){    //if person exists
        val deleteAction = personTable.filter(_.id === id).delete
        val runDel= db.run(deleteAction)
        Ok("Person removed successfully\n")
      }
      else  NotFound("No person with this id, please try again\n")  
    }

    /* test cases - 
       curl -v -d "{\"title\": \"Homework\", \"details\": \"finish the assignments\", \"dueDate\": \"2021-12-30\", \"status\": \"active\"}"  -H "Content-Type:application/json" -X POST localhost:9000/api/people/1/tasks 
       curl -v -d "{\"title\": \"Gardening\", \"details\": \"water the plants\", \"dueDate\": \"2022-01-02\", \"status\": \"active\"}"  -H "Content-Type:application/json" -X POST localhost:9000/api/people/1/tasks 
       curl -v -d "{\"title\": \"Homework\", \"details\": \"bad status\", \"dueDate\": \"2021-12-30\", \"status\": \"unknown\"}"  -H "Content-Type:application/json" -X POST localhost:9000/api/people/1/tasks 
    */
    def addNewTask(id:String) = Action{
      //parse request
      implicit request =>
      val content = request.body
      val jsonObject: Option[JsValue] = content.asJson
      val extractedJson: JsValue = jsonObject.get
      val titleOption: Option[String] = (extractedJson \ "title").asOpt[String]
      val detailsOption = (extractedJson \ "details").asOpt[String]
      val dueDateOption = (extractedJson \ "dueDate").asOpt[String]
      val statusOption = (extractedJson \ "status").asOpt[String]
      var newStatus = "active";

      if (titleOption.isEmpty || detailsOption.isEmpty || dueDateOption.isEmpty || ( statusOption.isDefined && !(status.isLegalStatus(statusOption.get)) ))
        BadRequest("invalid data\n")
      else 
      {
        // query person by ID
        val personSeq: Seq[PersonDetails] = queryPersonById(id)
        if (personSeq.length > 0) //if person exists
        {
          if (statusOption.isDefined)
            newStatus =statusOption.get 
          taskId.synchronized { taskId=taskId +1 }
          val toBeAdded =TaskDetails(titleOption.get,detailsOption.get, LocalDate.parse(dueDateOption.get),newStatus,id,taskId.toString)
          val insertTaskQuery = taskTable += toBeAdded
          val person : PersonDetails = personSeq.head
          var taskCount=person.activeTaskCount
          if(newStatus == "active")
            taskCount=taskCount+1
          val updateQuery = personTable.filter(_.id === id).map(_.activeTaskCount).update(taskCount)
          val combinedAction = DBIO.seq(insertTaskQuery, updateQuery)
          val transactionStatus:Future[Unit] = db.run(combinedAction.transactionally)
          Created(Json.toJson(toBeAdded))    
        }
        else  NotFound("No person with this id, please try again\n")
      }
      

    }

    /* test cases -
     curl localhost:9000/api/people/1/tasks?status=done
     curl localhost:9000/api/people/1/tasks?status=active
     curl localhost:9000/api/people/1/tasks
    */
    def getTasksofPerson(id: String, status: Option[String]=None)= Action
    {
      val personSeq: Seq[PersonDetails]  = queryPersonById(id)

      if (personSeq.length > 0){  //if person exists
        status match{
          case None => 
              val tasksFuture: Future[Seq[TaskDetails]] = db.run(taskTable.filter(_.ownerID === id).result)
              val tasks = Await.result(tasksFuture, 5.seconds)
              val jsontasks = Json.toJson(tasks)
              Ok(jsontasks)
          
          case Some(value) =>
               val tasksFuture: Future[Seq[TaskDetails]] = db.run(taskTable.filter(_.ownerID === id).filter(_.status === value).result)
               val tasks = Await.result(tasksFuture, 5.seconds)
               val jsontasks = Json.toJson(tasks)
               Ok(jsontasks)
       
        }
      }
      else  NotFound("No person with this id, please try again\n")
    }

    // curl localhost:9000/api/tasks/1
    def getTask(id: String) =Action
    {
      val taskSeq: Seq[TaskDetails] = queryTaskById(id)
      if (taskSeq.length > 0)
        {
          val task = taskSeq.head
          val taskJson = Json.toJson(task)
          Ok(taskJson)
        }
      else NotFound("No task with this id, please try again\n")
    }
    
    /* test cases -
     curl -v -d "{\"title\": \"paying bills\", \"details\": \"water and electricity\", \"dueDate\": \"2021-11-04\"}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/tasks/1 
     curl -v -d "{\"details\": \"details updated\"}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/tasks/1 
     curl -v -d "{\"details\": \"details updated\", \"status\": \"active\"}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/tasks/1 
     curl -v -d "{\"details\": \"details updated\", \"status\": \"done\"}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/tasks/1 
     curl -v -d "{\"details\": \"invalid status\", \"status\": \"Unknown\"}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/tasks/1 
     curl -v -d "{}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/tasks/1  
    */
    def updateTask(id: String) = Action 
    {
      // store the recieved data for updating as Json, and then extract the optional fields.
      implicit request =>
      val content: AnyContent = request.body
      val jsonObject: Option[JsValue] = content.asJson
      val extractedJson: JsValue = jsonObject.get
      val titleOption: Option[String] = (extractedJson \ "title").asOpt[String]
      val detailsOption = (extractedJson \ "details").asOpt[String]
      val dueDateOption = (extractedJson \ "dueDate").asOpt[String]
      val statusOption = (extractedJson \ "status").asOpt[String]

      if ( statusOption.isDefined && !(status.isLegalStatus(statusOption.get)) )
        BadRequest("input status is illegal\n")
      else
      {
        val taskSeq: Seq[TaskDetails]  = queryTaskById(id)
        if (taskSeq.length > 0)
        {
          val oldTask = taskSeq.head
          // update optional fields which we received data for - 
          if (titleOption.isDefined)
          {
            val title = titleOption.get
            val updateTaskTitle = taskTable.filter(_.id === id).map(_.title).update(title)
            val updateTitle = db.run(updateTaskTitle)
          }
          if (detailsOption.isDefined)
          {
            val details = detailsOption.get
            val updateTaskDetails = taskTable.filter(_.id === id).map(_.details).update(details)
            val updateDetails = db.run(updateTaskDetails)
          }
          if (dueDateOption.isDefined)
          {
            val dueDate = dueDateOption.get
            val updateTaskDueDate = taskTable.filter(_.id === id).map(_.dueDate).update(LocalDate.parse(dueDate))
            val updateDueDate = db.run(updateTaskDueDate)
          }
          if (statusOption.isDefined) // if we were given a status to update we need to also increase / decrease task count accordingly
          {
            val oldStatus = oldTask.status
            val ownerId = oldTask.ownerID
            val newStatus = statusOption.get
            val personSeq: Seq[PersonDetails]  = queryPersonById(ownerId)
            var taskCount = personSeq.head.activeTaskCount
            if(newStatus == "done" &&  oldStatus == "active")
              taskCount=taskCount-1
            if(newStatus == "active" &&  oldStatus == "done")
              taskCount=taskCount+1
            val updateTaskCountQuery = personTable.filter(_.id === ownerId).map(_.activeTaskCount).update(taskCount)
            val updateTaskStatusQuery = taskTable.filter(_.id === id).map(_.status).update(newStatus)
            val combinedAction = DBIO.seq(updateTaskStatusQuery, updateTaskCountQuery)
            val transactionStatus:Future[Unit] = db.run(combinedAction.transactionally)
          }
          // return the updated taskDetails with code 200 -
          val newTaskSeq: Seq[TaskDetails] = queryTaskById(id)
          val task: TaskDetails = newTaskSeq.head
          val taskJson = Json.toJson(task)
          Ok(taskJson)
        }
        else NotFound("No task with this id, please try again\n")
      }
    
    }

     // curl -X DELETE localhost:9000/api/tasks/1 
    def deleteTask(id: String) = Action{
      val taskSeq = queryTaskById(id)

      if (taskSeq.length > 0)  //if task exists
      {    
        val task= taskSeq.head
        val taskOwner=task.ownerID
        val taskStatus=task.status
        //query owner of the task
        val personSeq: Seq[PersonDetails]  = queryPersonById(taskOwner)
        val owner =personSeq.head
        var activeTaskCount=owner.activeTaskCount

        if(taskStatus == "active"){ // decrement owner's active task count
          activeTaskCount=activeTaskCount-1
        }
        val deleteAction = taskTable.filter(_.id === id).delete
        val updateAction = personTable.filter(_.id === taskOwner).map(_.activeTaskCount).update(activeTaskCount)
        val combinedAction = DBIO.seq(deleteAction, updateAction)
        val transactionStatus:Future[Unit] = db.run(combinedAction.transactionally)    
        Ok("task removed successfully\n")
      }
      else  NotFound("No task with this id, please try again\n")
      
      
    }
    // curl localhost:9000/api/tasks/1/status
    def getTaskStatus(id: String) = Action
    {
      // query the task with the input id
      val taskSeq = queryTaskById(id)
      if (taskSeq.length > 0) //if task exists
      {    
        val task= taskSeq.head
        val taskStatus=task.status
        Ok(taskStatus)
      }
      else  NotFound("No task with this id, please try again\n")
    }
  
  /* test cases -
    curl -X PUT -H "Content-Type:application/json" -d "\"done\"" localhost:9000/api/tasks/1/status
    curl -X PUT -H "Content-Type:application/json" -d "\"active\"" localhost:9000/api/tasks/1/status 
    curl -X PUT -H "Content-Type:application/json" -d "\"unknown\"" localhost:9000/api/tasks/1/status 
  */
  def updateTaskStatus(id: String) = Action
  {
    implicit request =>
    val content: AnyContent = request.body
    val jsonObject: Option[JsValue] = content.asJson
    val extractedJson: JsValue = jsonObject.get
    val newStatus = (extractedJson).as[String]
    
    if ( !(status.isLegalStatus(newStatus)) )
      BadRequest("input status is illegal\n")
    else 
    {
      val taskSeq: Seq[TaskDetails]  = queryTaskById(id)
      if (taskSeq.length > 0)
      {
        val oldStatus = taskSeq.head.status
        val ownerId = taskSeq.head.ownerID

        val personSeq: Seq[PersonDetails]  = queryPersonById(ownerId)
        val person: PersonDetails = personSeq.head
        var taskCount=person.activeTaskCount
        if(newStatus == "done" &&  oldStatus == "active")
          taskCount=taskCount-1
        if(newStatus == "active" &&  oldStatus == "done")
          taskCount=taskCount+1
        val updateTaskCountQuery = personTable.filter(_.id === ownerId).map(_.activeTaskCount).update(taskCount)
        val updateTaskStatusQuery = taskTable.filter(_.id === id).map(_.status).update(newStatus)
        val combinedAction = DBIO.seq(updateTaskStatusQuery, updateTaskCountQuery)
        val transactionStatus:Future[Unit] = db.run(combinedAction.transactionally)
        NoContent
      }
      else NotFound("No task with this id, please try again\n")
    }  
  }

   // curl localhost:9000/api/tasks/1/owner
   def getTaskOwner(id: String) = Action
  {
    val taskSeq = queryTaskById(id)
    if (taskSeq.length > 0){    //if task exists
      val task= taskSeq.head
      val taskOwner=task.ownerID
      Ok(taskOwner)
    }
    else  NotFound("No task with this id, please try again\n")

  }

  // curl -X PUT -H "Content-Type:application/json" -d "\"2\"" localhost:9000/api/tasks/1/owner
  def updateTaskOwner(id: String) = Action
  {
    implicit request =>
    val content: AnyContent = request.body
    val jsonObject: Option[JsValue] = content.asJson
    val extractedJson: JsValue = jsonObject.get
    val newOwnerId = (extractedJson).as[String]

    val taskSeq = queryTaskById(id)
    val personSeq = queryPersonById(newOwnerId)

    if (taskSeq.length > 0)    //if task exists
    {
      val task = taskSeq.head
      val oldOwnerId = task.ownerID
      val taskStatus = task.status

      if (personSeq.length > 0)
      {
        val oldPersonSeq: Seq[PersonDetails] = queryPersonById(oldOwnerId)
        
        // if the task is active and owner changed we need to update task counts accordingly
        if (!(oldOwnerId == newOwnerId) && taskStatus == "active") 
        {
          var oldOwnerTaskCount = oldPersonSeq.head.activeTaskCount - 1
          var newOwnerTaskCount = personSeq.head.activeTaskCount + 1
          val updateOldTaskCountQuery = personTable.filter(_.id === oldOwnerId).map(_.activeTaskCount).update(oldOwnerTaskCount)
          val updateNewTaskStatusQuery = personTable.filter(_.id === newOwnerId).map(_.activeTaskCount).update(newOwnerTaskCount)
          val combinedAction = DBIO.seq(updateOldTaskCountQuery, updateNewTaskStatusQuery)
          val transactionStatus:Future[Unit] = db.run(combinedAction.transactionally)
        }
        val updateTaskOwnerId = taskTable.filter(_.id === id).map(_.ownerID).update(newOwnerId)
        val runUpdate = db.run(updateTaskOwnerId)
        NoContent
      }
      else NotFound("No person with this id, please try again\n")
    }
    else NotFound("No task with this id, please try again\n")
  }
}

