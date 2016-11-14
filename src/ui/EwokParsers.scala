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
   def disjunction : Parser[Boolean] = (conjunction~rep("||"~conjunction)) ^^
   {
     case conjunction~Nil => conjunction
     case conjunction~listConjunctions => conjunction || listConjunctions.reduce(_||_)
   }
   */
  
   def literal : Parser[Literal] = (boole | number)
   
   def identifier : Parser[Identifier] = "[a-zA-Z]".r~rep("[0-9a-zA-Z]".r) ^^
   {
     case first~Nil => Identifier(first)
     case first~otherChars => Identifier(first + otherChars.reduce(_+_))
   }
   
  def boole : Parser[Boole] = ("true" | "false") ^^
  {
    case tree => Boole(tree.toBoolean)
  }
  
  def number : Parser[Number] = opt("""\+|-""".r)~"""[0-9]+""".r~opt(""".[0-9]+""".r) ^^
  {
    case None ~ firstNum ~ None => Number((firstNum).toDouble)
    case None ~ firstNum ~ Some(otherNums) => Number((firstNum + otherNums).toDouble)
    case Some("+") ~ firstNum ~ None => Number((firstNum).toDouble)
    case Some("+") ~ firstNum ~ Some(otherNums) => Number((firstNum + otherNums).toDouble)
    case Some("-") ~ firstNum ~ None => Number((firstNum).toDouble * -1)
    case Some("-") ~ firstNum ~ Some(otherNums) => Number((firstNum + otherNums).toDouble * -1)     
  }
}

object EwokParsers {
  def test = {
    println("------------TESTING NUMBER PARSER----------------")
    val ewokParser = new EwokParsers
    var expression = "+10"
    var tree = ewokParser.parseAll(ewokParser.number, expression)
    println("Expected: 10")
    println("Actual: " + tree.get)
    expression = "-10"
    tree = ewokParser.parseAll(ewokParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    expression = "5.2142"
    tree = ewokParser.parseAll(ewokParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    expression = "4912"
    tree = ewokParser.parseAll(ewokParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    expression = "0"
    tree = ewokParser.parseAll(ewokParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    expression = "+9999"
    tree = ewokParser.parseAll(ewokParser.number, expression)
    println("Expected: 9999")
    println("Actual: " + tree.get)
    expression = "123"
    tree = ewokParser.parseAll(ewokParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    
    
    println("------------TESTING BOOLE PARSER----------------")
    expression = "true"
    var bTree = ewokParser.parseAll(ewokParser.boole, expression)
    println("Expected: " + expression)
    println("Actual: " + bTree.get)
    expression = "false"
    bTree = ewokParser.parseAll(ewokParser.boole, expression)
    println("Expected: " + expression)
    println("Actual: " + bTree.get)
    
    
    
    println("------------TESTING IDENTIFIER PARSER----------------")
    expression = "aVarName"
    var iTree = ewokParser.parseAll(ewokParser.identifier, expression)
    println("Expected: " + expression)
    println("Actual: " + iTree.get)
    expression = "withNumbers01231"
    iTree = ewokParser.parseAll(ewokParser.identifier, expression)
    println("Expected: " + expression)
    println("Actual: " + iTree.get)
    expression = "numbersIn1Middle2"
    iTree = ewokParser.parseAll(ewokParser.identifier, expression)
    println("Expected: " + expression)
    println("Actual: " + iTree.get)
    expression = "a"
    iTree = ewokParser.parseAll(ewokParser.identifier, expression)
    println("Expected: " + expression)
    println("Actual: " + iTree.get)
    
    
    println("------------TESTING LITERAL PARSER----------------")
    expression = "true"
    var lTree = ewokParser.parseAll(ewokParser.literal, expression)
    println("Expected: " + expression)
    println("Actual: " + lTree.get)
    expression = "4.5124"
    lTree = ewokParser.parseAll(ewokParser.literal, expression)
    println("Expected: " + expression)
    println("Actual: " + lTree.get)
    
  }
}