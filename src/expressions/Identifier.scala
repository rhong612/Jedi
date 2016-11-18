package expressions

import values._
import ui._

case class Identifier(val name : String) extends Expression with Serializable{
  def execute(env : Environment) : Value = {
    var value = env.find(this) 
    value match {
    	case Notification.UNKNOWN => throw new UndefinedException("Undefined Identifier: " + name)
    	case _ => value
    }
  }
  
  override def toString = name
}