package expressions

import values._

case class Declaration(id : Identifier, exp : Expression) extends SpecialForm{
  def execute(env : Environment) : Value = {
    println("Executing declaration...")
    Number(0) //Testing
  }
}