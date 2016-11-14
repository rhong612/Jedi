package expressions

case class Disjunction(main : Conjunction, other : List[Conjunction]) extends SpecialForm{
  
}