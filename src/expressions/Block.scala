package expressions

import values._

case class Block (locals : List[Expression]) extends SpecialForm {
  def execute(env : Environment) = {
  	var tempEnv = new Environment(env)
  	for (i <- 0 until locals.length - 1) {
  		locals(i).execute(tempEnv)
  	}
  	locals.last.execute(tempEnv)
  }
}