package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scala.collection.mutable
import models.TodoListItem
import models.NewTodoListItem
import models.Player
import models.PlayerTable

import models.PersonData
import models.PersonDetails
import models.PersonTable
import models.TaskData
import models.TaskDetails
import models.TaskTable

import slick.jdbc.H2Profile.api._
import slick.jdbc.H2Profile
import scala.concurrent._
import scala.concurrent.duration._

@Singleton
class TodoListController @Inject()(val controllerComponents: ControllerComponents)
extends BaseController {
    implicit val todoListJson = Json.format[TodoListItem]
    implicit val newTodoListJson = Json.format[NewTodoListItem]
    implicit val playerJson = Json.format[Player]
    implicit val personDataJson = Json.format[PersonData]
    implicit val personDetailsJson = Json.format[PersonDetails]
    implicit val taskDetailsJson = Json.format[TaskDetails]
    implicit val taskDataJson = Json.format[TaskData]
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    var personId = 0;

    // database stuff - //
    val db = Database.forConfig("h2mem")
    //val playerTable = TableQuery[PlayerTable]

    val personTable = TableQuery[PersonTable]
    val taskTable = TableQuery[TaskTable]
    val schemas= personTable.schema ++ taskTable.schema
    val setupTables = db.run(schemas.create)
    
    //val germanPlayersQuery = playerTable.filter(_.country === "Germany")
    //val germanPlayers: Future[Seq[Player]] = db.run[Seq[Player]](germanPlayersQuery.result)
    // this doesn't increment id -
    
    // this increments id - 
    // val forceInsertAction = playerTable.forceInsert(player)

    // List stuff from the example //
      private var todoList = new mutable.ListBuffer[TodoListItem]()
    todoList += TodoListItem(1, "test", true)
    todoList += TodoListItem(2, "some other value", false)

    // curl localhost:9000/todo
    def getAll(): Action[AnyContent] = Action 
    {
        if (todoList.isEmpty) 
        {
            NoContent
        }   
        else 
        {
            Ok(Json.toJson(todoList))
        }
    }   

    def getById(itemId: Long) = Action 
    {
      val foundItem = todoList.find(_.id == itemId)
      foundItem match 
      {
        case Some(item) => Ok(Json.toJson(item))
        case None => NotFound
      }
    }

    def markAsDone(itemId: Long) = Action 
    {
      val foundItem = todoList.find(_.id == itemId)
      foundItem match {
      case Some(item) =>
        val newItem = item.copy(isItDone = true)
        todoList.dropWhileInPlace(_.id == itemId)
        todoList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
      }
    }

  def deleteAllDone() = Action 
  {
    todoList.filterInPlace(_.isItDone == false)
    Accepted
  }

  // curl -v -d "{\"description\": \"some new item\"}" -H "Content-Type:application/json" -X POST localhost:9000/todo
  def addNewItem() = Action 
    { implicit request =>
        val content = request.body
        val jsonObject = content.asJson

        val todoListItem: Option[NewTodoListItem] = jsonObject.flatMap(Json.fromJson[NewTodoListItem](_).asOpt)

        todoListItem match 
        {
            case Some(newItem) =>
                val nextId = todoList.map(_.id).max + 1
                val toBeAdded = TodoListItem(nextId, newItem.description, false)
                todoList += toBeAdded
                Created(Json.toJson(toBeAdded))
            case None =>
                BadRequest
        }

    }

    // curl -v -d "{\"id\": 1, \"name\": \"Yossi\", \"country\": \"Israel\"}" -H "Content-Type:application/json" -X POST localhost:9000/todo/player  
    def addNewPlayer() = Action 
    { implicit request =>
        val content = request.body
        val jsonObject = content.asJson

        val newPlayer: Option[Player] = jsonObject.flatMap(Json.fromJson[Player](_).asOpt)

        newPlayer match 
        {
            case Some(newPlayer) =>
                // val nextId = todoList.map(_.id).max + 1
                val toBeAdded = Player(newPlayer.id, newPlayer.name,newPlayer.country)
                //val insertPlayerQuery = playerTable += toBeAdded
               // val insertResult:Future[Int] = db.run(insertPlayerQuery)
                Created(Json.toJson(toBeAdded))
            case None =>
                BadRequest
        }

    }

  // curl -v -d "{\"name\": \"Yossi\", \"email\": \"yos@gmail.com\", \"favoriteProgrammingLanguage\": \"Java\"}"  -H "Content-Type:application/json" -X POST localhost:9000/api/people 
    def addNewPerson() = Action 
    { implicit request =>
      val content = request.body
      val jsonObject = content.asJson

      val newPerson: Option[PersonData] = jsonObject.flatMap(Json.fromJson[PersonData](_).asOpt)

      newPerson match 
      {
        case Some(newPerson) =>
          personId = personId + 1
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

    // curl localhost:9000/api/people/1
    def getPerson(id: String) = Action
    {
      val personByIdQuery = personTable.filter(_.id === id)
      val personFuture: Future[Seq[PersonDetails]] = db.run[Seq[PersonDetails]](personByIdQuery.result)
      val personSeq: Seq[PersonDetails] = Await.result(personFuture, 5.seconds)
      if (personSeq.length > 0)
        {
          val person = personSeq.head
          val personJson = Json.toJson(person)
          Ok(personJson)
        }
      else NotFound("No person with this id, please try again\n")
    }


    //  curl -v -d "{\"name\": \"YOS\", \"email\": \"YOS@gmail.com\", \"favoriteProgrammingLanguage\": \"Python\"}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/people/1 
    //  curl -v -d "{\"favoriteProgrammingLanguage\": \"C\"}" -H "Content-Type:application/json" -X PATCH localhost:9000/api/people/1 
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

      // query the person with the input id
      val personByIdQuery = personTable.filter(_.id === id)
      val personFuture: Future[Seq[PersonDetails]] = db.run[Seq[PersonDetails]](personByIdQuery.result)
      val personSeq: Seq[PersonDetails]  = Await.result(personFuture, 5.seconds)
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
        val personFuture: Future[Seq[PersonDetails]] = db.run[Seq[PersonDetails]](personByIdQuery.result)
        val personSeq: Seq[PersonDetails] = Await.result(personFuture, 5.seconds)
        val person = personSeq.head
        val personJson = Json.toJson(person)
        Ok(personJson)
      }
      else NotFound("No person with this id, please try again\n")
      
    }

    //curl -X DELETE localhost:9000/api/people/1 
    def deletePerson(id: String) = Action{
     
      // query the person with the input id
      val personByIdQuery = personTable.filter(_.id === id)
      val personFuture: Future[Seq[PersonDetails]] = db.run[Seq[PersonDetails]](personByIdQuery.result)
      val personSeq = Await.result(personFuture, 5.seconds)
      if (personSeq.length > 0){    //if person exists
        val deleteAction = personTable.filter(_.id === id).delete
        val runDel= db.run(deleteAction)
        Ok("Person removed successfully\n")

      }
      else  NotFound("No person with this id, please try again\n")
      
      
    }

    def addNewTask(id:String) = Action{
      //parse request
       implicit request =>
      val content = request.body
      val jsonObject :Option[JsValue] = content.asJson
       val extractedJson: JsValue = jsonObject.get
      val titleOption: Option[String] = (extractedJson \ "title").asOpt[String]
      val detailsOption = (extractedJson \ "details").asOpt[String]
      val dueDateOption = (extractedJson \ "dueDate").asOpt[String]
      val statusOption = (extractedJson \ "status").asOpt[String]
      val newStatus = "active";
      // query person by ID
      val personByIdQuery = personTable.filter(_.id === id)
      val personFuture: Future[Seq[PersonDetails]] = db.run[Seq[PersonDetails]](personByIdQuery.result)
      val personSeq: Seq[PersonDetails]  = Await.result(personFuture, 5.seconds)

      if (personSeq.length > 0){//if person exists
          if (statusOption.isDefined)
        {
          status =statusOption.get 
        }
         val toBeAdded =TaskDetail(titleOption.get,detailsOption.get,dueDateOption.get,status,id)
         val insertTaskQuery = taskTable += toBeAdded
            val insertResult:Future[Int] = db.run(insertTaskQuery)\
            Created(Json.toJson(toBeAdded))

            //not finished, didn't check if working, need to update active task count of id, need to work as a transaction.
      }
       


    }
    

}

