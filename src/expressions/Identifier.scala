package expressions

import values._

case class Identifier(val name : String) extends Expression with Serializable{
  def execute(env : Environment) : Value = {
    env.find(this)
  }
  
  override def toString = name
}