package values

class Notification(val msg : String) extends Value{
  override def toString = msg
}

object Notification {
  def apply(msg : String) = new Notification(msg)
  
  val OK = Notification("OK")
  val UNKNOWN = Notification("UNKNOWN")
  val ERROR = Notification("ERROR")
  val DONE = Notification("DONE")
}