package expressions

import values._
import ui._

case class FunCall(opcode : Identifier, args : List[Expression]) extends Expression{
  def execute(env : Environment) : Value = {
    system.execute(opcode, args.map(_.execute(env)))
  }
}