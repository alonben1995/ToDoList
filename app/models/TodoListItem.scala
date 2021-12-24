package models

case class TodoListItem(id: Long, description: String, var isItDone: Boolean)
case class NewTodoListItem(description: String)
