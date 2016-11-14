package expressions

import values._
import ui._

case class Disjunction(main : Conjunction, other : List[Conjunction] = Nil) extends SpecialForm{
  def execute(env : Environment) : Value = {
    if (other.isEmpty) {
      main.execute(env)
    }
    else {
      var result = Boole(false)
      var index = 0
      //Exit loop the moment result becomes true
      while (index < other.size && !result.value) {
        var listElement = other(index).execute(env) 
        listElement match {
          case b : Boole => result = result || listElement.asInstanceOf[Boole]
          case _ => throw new TypeException("All OR values must be of type boole")
        }
        index += 1
      }
      result
    }
  }
}