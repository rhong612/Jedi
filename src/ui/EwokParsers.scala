package ui

import scala.util.parsing.combinator._
import expressions._
import values._

class EwokParsers extends RegexParsers {
   /*
   def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

   // def declaration, conditional, dusjunction, and other parsers
   def declaration: Parser[Declaration] = ("def"~identifier~"="~expression) ^^
   {
     case "def"~id~"="~exp => Declaration(id, exp)
   }
   
   def conditional : Parser[Conditional] = ("if"~"("~expression~")"~expression~opt("else"~expression)) ^^ 
   {
     case "if"~"("~cond~")"~body~None => Conditional(cond, body)
     case "if"~"("~cond~")"~body~Some("else" ~ elseBody) => Conditional(cond, body, elseBody)
   }
   /*
   def disjunction : Parser[Boolean] = (conjunction~rep("||"~conjunction)) ^^
   {
     case conjunction~Nil => conjunction
     case conjunction~listConjunctions => conjunction || listConjunctions.reduce(_||_)
   }
   */
   
   def identifier : Parser[Identifier] = "[a-zA-Z]".r~rep("[0-9a-zA-Z]".r) ^^
   {
     case first~otherChars => new Identifier(first + otherChars)
   }
   
   */
  
  
}