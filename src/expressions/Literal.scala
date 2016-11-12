package expressions

import values._

trait Literal extends Expression with Value {
  def execute(env : Environment) : Value = {
    this
  }
}