package expressions

case class Conditional(condition : Expression, body : Expression, elseBody : Expression = null) extends SpecialForm{
  
}