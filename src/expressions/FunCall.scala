package expressions

import values._
import ui._

case class FunCall(val operator : Expression, operands : List[Expression] = Nil) extends Expression{
  def execute(env : Environment) : Value = {
  		val args : List[Value] = operands.map(_.execute(env))
  		
  		try {
  			val opVal = operator.execute(env) //Will throw undefined exception if operator is an undefined identifier
  			if (opVal.isInstanceOf[Closure]) {
  				opVal.asInstanceOf[Closure].apply(args)
  			}
  			else {
  				throw new UndefinedException("Not a closure")
  			}
  		}
  		catch {
  			case e : UndefinedException => {
  				system.execute(operator.asInstanceOf[Identifier], args)
  			}
  		}
  }
}