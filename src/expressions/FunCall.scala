package expressions

import values._
import ui._

case class FunCall(opcode : Expression, args : List[Expression] = Nil) extends Expression{
  def execute(env : Environment) : Value = {
    if (args.isEmpty) {
      opcode.execute(env)
    }
    else {
      var id = opcode.asInstanceOf[Identifier]
      system.execute(id, args.map(_.execute(env))) 
    }
  }
}