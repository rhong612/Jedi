package expressions

import values._

case class Lambda (val params : List[Identifier], val body : Expression) extends SpecialForm {
  def execute(env : Environment) = {
  	new Closure(params, body, env)
  }
}