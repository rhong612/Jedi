package expressions

import values._
import ui._

case class Conjunction(main : FunCall, other : List[FunCall] = Nil) extends SpecialForm{
  def execute(env : Environment) : Value = {
    if (other.isEmpty) {
      main.execute(env)
    }
    else {
      var result = Boole(true)
      var index = 0
      //Exit loop the moment result becomes false
      while(index < other.size && result.value) {
        var listElement = other(index).execute(env) 
        listElement match {
          case b : Boole => result = result && listElement.asInstanceOf[Boole]
          case _ => throw new TypeException("All AND values must be of type boole")
        }
        index += 1
      }
      result
    }
  }
}