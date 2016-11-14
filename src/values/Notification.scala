package values

class Notification(val msg : String) extends Value{
  override def toString = msg
}

object Notification {
  val OK = new Notification("ok")
  val UNKNOWN = new Notification("UNKNOWN")
  val ERROR = new Notification("ERROR")
  val VAR_UPDATED = new Notification("VARIABLE UPDATED")
  val BINDING = new Notification("BINDING CREATED")
}