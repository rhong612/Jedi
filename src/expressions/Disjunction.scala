package expressions

import values._

case class Disjunction(main : Conjunction, other : List[Conjunction] = Nil) extends SpecialForm{
  def execute(env : Environment) : Value = {
    println("Executing disjunction...")
    Number(0) //Testing
  }
}