package models

import slick.jdbc.H2Profile.api._

case class PersonData(name:String, email:String, favoriteProgrammingLanguage: String)