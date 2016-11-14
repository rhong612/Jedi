package expressions

import values._
import ui._

case class Conditional(condition : Expression, body : Expression, elseBody : Expression = null) extends SpecialForm{
  def execute(env : Environment) : Value = {
    val result = condition.execute(env)
    if (!result.isInstanceOf[Boole]) {
      throw new TypeException("Conditional must be of type boole")
    }
    val cond = result.asInstanceOf[Boole]
    if (cond.value) {
      body.execute(env)
    }
    else if (elseBody != null){
      elseBody.execute(env)
    }
    else {
      Notification.OK 
    }
  }
}