package ui

import scala.util.parsing.combinator._
import expressions._
import values._

class EwokParsers extends RegexParsers {
   def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

   def declaration: Parser[Declaration] = ("def"~identifier~"="~expression) ^^
   {
     case "def"~id~"="~exp => Declaration(id, exp)
   }
   
   def conditional : Parser[Conditional] = "if"~"("~expression~")"~expression~opt("else"~expression) ^^ 
   {
     case "if"~"("~cond~")"~body~Some("else" ~ elseBody) => Conditional(cond, body, elseBody)
     case "if"~"("~cond~")"~body~None => Conditional(cond, body)
   }
   
   def disjunction : Parser[SpecialForm] = (conjunction~rep("||" ~> conjunction)) ^^
   {
     case conjunction~Nil => conjunction
     case conjunction~listConjunctions => Disjunction(conjunction, listConjunctions)
   }
   def conjunction : Parser[Conjunction] = (equality~rep("&&" ~> equality)) ^^
   {
     case equality ~ Nil => Conjunction(equality)
     case equality ~ listEqualities => Conjunction(equality, listEqualities)
   }
   def equality : Parser[FunCall] = (inequality ~ rep("==" ~> inequality)) ^^
   {
     case inequality ~ Nil => inequality
     case inequality ~ listInequalities => FunCall(Identifier("equals"), inequality :: listInequalities)
   }
   def inequality : Parser[FunCall] = (sum ~ rep("<" ~> sum)) ^^
   {
     case sum ~ Nil => sum
     case sum ~ list => FunCall(Identifier("less"), sum :: list)
   }
   def sum : Parser[FunCall] = (product ~ rep(("""\+|-""".r)~>product)) ^^
   {
     case product ~ Nil => product
     case product ~ list => FunCall(Identifier("add"), product :: list)
   }
   def product : Parser[FunCall] = (funcall ~ rep(("""\*|/""".r)~>funcall)) ^^
   {
     case funcall ~ Nil => funcall
     case funcall ~ list => FunCall(Identifier("mul"), funcall :: list)
   }
   def funcall : Parser[FunCall] = (term ~ opt(operands)) ^^
   {
     case term ~ None => FunCall(term)
     case term ~ Some(opList) => FunCall(term, opList)
   }
   
   def operands : Parser[List[Expression]] = ("(" ~ opt(expression ~ rep(","~>expression)) ~")") ^^
   {
     case "(" ~ None ~ ")" => Nil
     case "(" ~ Some(exp ~ expList) ~ ")" => exp :: expList
   }
       
   def term : Parser[Expression] = literal | identifier | "("~>expression<~")"
   def literal : Parser[Literal] = (boole | number)
   
   def identifier : Parser[Identifier] = ("[a-zA-Z]".r~rep("[0-9a-zA-Z]".r))^^
   {
     case first~Nil => Identifier(first)
     case first~otherChars => Identifier(first + otherChars.reduce(_+_))
   }
   
  def boole : Parser[Boole] = ("true" | "false") ^^
  {
    case tree => Boole(tree.toBoolean)
  }
  
  def number : Parser[Number] = (opt("""\+|-""".r)~"""[0-9]+""".r~opt(""".[0-9]+""".r))  ^^
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
    
    
    println("------------TESTING OVERALL PARSER----------------")
    val globalEnv = new Environment()
    expression = "3 + 4 * 5"
    var eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println("Expected: " + 23)
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "def a2 = 3 + 4 * 5"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println("Expected: BINDING CREATED")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "5"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println("Expected: " + 5)
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "true || false"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(globalEnv))
  }
}