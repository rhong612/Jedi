package values

class Notification(val msg : String) extends Value{
}

object Notification {
  val nOK = new Notification("OK")
  val nUNKNOWN = new Notification("UNKNOWN")
  val nERROR = new Notification("ERROR")
  val varUpdated = new Notification("VARIABLE UPDATED")
  val binding = new Notification("BINDING CREATED")
  
  def OK = nOK
  def UNKNOWN = nUNKNOWN
  def ERROR = nERROR
  def VAR_UPDATED = varUpdated
  def BIND_CREATED = binding
}