package expressions

import values._

case class Identifier(name : String) extends Expression with Serializable{
  def execute(env : Environment) : Value = {
    env.find(this)
  }
}