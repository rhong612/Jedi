package expressions

import values._

case class Conditional(condition : Expression, body : Expression, elseBody : Expression = null) extends SpecialForm{
  def execute(env : Environment) : Value = {
    println("Executing conditionaln...")
    Number(0) //Testing
  }
}