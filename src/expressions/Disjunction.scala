package expressions

import values._
import ui._

case class Disjunction(val operands: List[Conjunction] = Nil) extends SpecialForm {
	def execute(env: Environment): Value = {
		var index = 0
		var mainBoole = operands(index).execute(env)
		if (operands.size == 1) {
			mainBoole
		} 
		else {
			var result: Boole = null
			if (mainBoole.isInstanceOf[Boole]) {
				result = mainBoole.asInstanceOf[Boole]
			} 
			else {
				throw new TypeException("Disjunction values must be of type boole")
			}
			index += 1
			//Exit loop the moment result becomes true
			while (index < operands.size && !result.value) {
				var listElement = operands(index).execute(env)
				listElement match {
					case b: Boole => result = result || listElement.asInstanceOf[Boole]
					case _ => throw new TypeException("All OR values must be of type boole")
				}
				index += 1
			}
			result
		}
	}
}