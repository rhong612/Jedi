package expressions

case class Conditional(cond : Expression, body : Expression, elseBody : Expression = null) extends SpecialForm{
  
}