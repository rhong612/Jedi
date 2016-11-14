package expressions

import values._

case class Conjunction(main : FunCall, other : List[FunCall] = Nil) extends SpecialForm{
  def execute(env : Environment) : Value = {
    println("Executing conjunction...")
    Number(0) //Testing
  }
}