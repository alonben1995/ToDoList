package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import scala.collection.mutable
import models.TodoListItem
import models.NewTodoListItem
import models.Player
import models.PlayerTable

import models.PersonData
import models.PersonDetails
import models.PersonTable

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
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    var personId = 0;

    // database stuff - //
    val db = Database.forConfig("h2mem")
    //val playerTable = TableQuery[PlayerTable]

    val personTable = TableQuery[PersonTable]

    val setupPersonTable = db.run(personTable.schema.create)
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

  // curl -v -d "{\"id\": 1, \"name\": \"Yossi\", \"country\": \"Israel\"}" -H "Content-Type:application/json" -X POST localhost:9000/todo/player  
  // curl -v -d "{\"name\": \"Yossi\", \"email\": \"yos@gmail.com\", \"favoriteProgrammingLanguage\": \"Java\"}" -H "Content-Type:application/json" -X POST localhost:9000/api/people 
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
                val insertPlayerQuery = personTable += toBeAdded
                val insertResult:Future[Int] = db.run(insertPlayerQuery)
                Created(Json.toJson(toBeAdded))
            case None =>
                BadRequest
        }

    }

    def getPeople() = Action
    {
      val peopleFuture: Future[Seq[PersonDetails]] = db.run(personTable.result)
      val people = Await.result(peopleFuture, 5.seconds)
      val jsonPeople = Json.toJson(people)
      Ok(jsonPeople)
      
    }

}

