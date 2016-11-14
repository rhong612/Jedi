package expressions

import values._
import ui._

case class Conditional(condition : Expression, body : Expression, elseBody : Expression = null) extends SpecialForm{
  def execute(env : Environment) : Value = {
    val result = condition.execute(env)
    var cond : Boole = null
    result match {
      case b : Boole => cond = b
      case _ => throw new TypeException("Conditional must be of type boole")
    }
    if (cond.value) {
      println("1")
      body.execute(env)
    }
    else if (elseBody != null){
      println("2")
      elseBody.execute(env)
    }
    else {
      println("3")
      Notification.OK 
    }
  }
}