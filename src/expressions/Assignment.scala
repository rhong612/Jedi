package expressions

import values._
import ui._

class Assignment(val varName : Identifier, val update : Expression) extends SpecialForm{
  def execute(env : Environment) = {
  	val value = varName.execute(env)
  	if (value.isInstanceOf[Variable]) {
  		value.asInstanceOf[Variable].content = update.execute(env)
  		Notification.DONE
  	}
  	else {
  		throw new TypeException("Cannot reassign value to a non-variable")
  	}
  }
}