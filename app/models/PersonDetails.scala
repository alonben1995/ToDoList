package models

import slick.jdbc.H2Profile.api._

case class PersonDetails(name:String, email:String, favoriteProgrammingLanguage: String, 
                        activeTaskCount: Int, id: String)

class PersonTable(tag: Tag) extends Table[PersonDetails](tag, None, "Person") { 
    override def * = (name, email, favoriteProgrammingLanguage, activeTaskCount, id) <> (PersonDetails.tupled, PersonDetails.unapply) 
    val name: Rep[String] = column[String]("Name") 
    val email: Rep[String] = column[String]("email") 
    val favoriteProgrammingLanguage: Rep[String] = column[String]("favoriteProgrammingLanguage") 
    val activeTaskCount: Rep[Int] = column[Int]("activeTaskCount") 
    val id : Rep[String] = column[String]("PersonId", O.AutoInc, O.PrimaryKey) 
}