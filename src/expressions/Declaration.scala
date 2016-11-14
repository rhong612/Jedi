package expressions

import values._
import ui._

case class Declaration(id : Identifier, exp : Expression) extends SpecialForm{
  def execute(env : Environment) : Value = {
    env.put(id, exp.execute(env)) match {
      case None => Notification.OK
      case _ => Notification.ERROR //Identifier was already declared earlier
    }
  }
}