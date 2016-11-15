package values

class Notification(val msg : String) extends Value{
  override def toString = msg
}

object Notification {
  def apply(msg : String) = new Notification(msg)
  
  val OK = Notification("ok")
  val UNKNOWN = Notification("UNKNOWN")
  val ERROR = Notification("ERROR")
  val VAR_UPDATED = Notification("VARIABLE UPDATED")
  val BINDING = Notification("BINDING CREATED")
}