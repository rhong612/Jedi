package values

import expressions._
import ui._

class Closure(params : List[Identifier], body : Expression, defEnv : Environment) extends Value{
  def apply(args : List[Value]) : Value = {
  	var tempEnv = new Environment(defEnv)
  	if (params.length != args.length)
  		throw new TypeException("Incorrect number of arguments")
  	tempEnv.put(params, args)
  	body.execute(tempEnv)
  }
}