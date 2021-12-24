package models

import slick.jdbc.H2Profile.api._

case class Player(id:Long, name:String, country:String)

class PlayerTable(tag: Tag) extends Table[Player](tag, None, "Player") { 
    override def * = (id, name, country) <> (Player.tupled, Player.unapply) 
    val id : Rep[Long] = column[Long]("PlayerId", O.AutoInc, O.PrimaryKey) 
    val name: Rep[String] = column[String]("Name") 
    val country : Rep[String] = column[String]("Country") 
}