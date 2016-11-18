package values

import scala.collection.mutable.HashMap
import ui._
import expressions._

class Environment(private val nextEnv : Environment = null) extends HashMap[Identifier, Value] with Value{
  def put(names: List[Identifier], vals: List[Value]) : Unit = {
    if (names.length == vals.length) {
      for (i <- 0 until names.length) {
        put(names(i), vals(i))
      }
    }
  }
  
  def find(id : Identifier) : Value = {
    get(id) match {
      case Some(i) => i
      case None => if (nextEnv == null) Notification.UNKNOWN else nextEnv.find(id)
    }
  }
}