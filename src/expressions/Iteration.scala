package expressions

import values._
import ui._

case class Iteration(condition : Expression, body : Expression) extends SpecialForm{
  def execute(env : Environment) = {
  	var cond = condition.execute(env)
  	if (cond.isInstanceOf[Boole]) {
  		var bCond : Boole = cond.asInstanceOf[Boole]
  		while(bCond.value) {
  			body.execute(env)
  			cond = condition.execute(env)
  			//Check to make sure that the condition still returns a boole (It might not in cases such as conditionals)
  			if (cond.isInstanceOf[Boole]) {
  				bCond = cond.asInstanceOf[Boole]
  			}
  			else {
  				throw new TypeException("The condition of a while loop must return a Boole")
  			}
  		}
  		Notification.DONE
  	}
  	else {
  		throw new TypeException("The condition of a while loop must return a Boole")
  	}
  }
}