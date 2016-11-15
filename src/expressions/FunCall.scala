package expressions

import values._
import ui._

case class FunCall(val operator : Expression, operands : List[Expression] = Nil) extends Expression{
  def execute(env : Environment) : Value = {
    if (operands.isEmpty) {
      operator.execute(env)
    }
    else {
      var id = operator.asInstanceOf[Identifier]
      system.execute(id, operands.map(_.execute(env))) 
    }
  }
}