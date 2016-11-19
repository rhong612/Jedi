package ui

import scala.util.parsing.combinator._
import expressions._
import values._

class EwokParsers extends RegexParsers {
	def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

	def declaration: Parser[Declaration] = ("def" ~ identifier ~ "=" ~ expression) ^^
		{
			case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
		}

	def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^
		{
			case "if" ~ "(" ~ cond ~ ")" ~ body ~ Some("else" ~ elseBody) => Conditional(cond, body, elseBody)
			case "if" ~ "(" ~ cond ~ ")" ~ body ~ None => Conditional(cond, body)
		}

	def disjunction: Parser[SpecialForm] = (conjunction ~ rep("||" ~> conjunction)) ^^
		{
			case conjunction ~ Nil => conjunction
			case conjunction ~ listConjunctions => Disjunction(conjunction :: listConjunctions)
		}

	def conjunction: Parser[Conjunction] = (equality ~ rep("&&" ~> equality)) ^^
		{
			case equality ~ Nil => Conjunction(List(equality))
			case equality ~ listEqualities => Conjunction(equality :: listEqualities)
		}
	def equality: Parser[Expression] = (inequality ~ rep("==" ~> inequality)) ^^
		{
			case inequality ~ Nil => inequality
			case inequality ~ listInequalities => FunCall(Identifier("equals"), inequality :: listInequalities)
		}
	def inequality: Parser[Expression] = (sum ~ opt(("<" | ">" | "!=") ~ sum)) ^^
		{
			case sum ~ None => sum
			case sum ~ Some("<" ~ s) => FunCall(Identifier("less"), List(sum, s))
			case sum ~ Some(">" ~ s) => FunCall(Identifier("more"), List(sum, s))
			case sum ~ Some("!=" ~ s) => FunCall(Identifier("unequal"), List(sum, s))
		}
	def sum: Parser[Expression] = product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^
		{
			case product ~ Nil => product
			case product ~ list => FunCall(Identifier("add"), product :: list)
		}
	private def negate(exp: Expression): Expression = {
		val sub = Identifier("sub")
		val zero = Number(0)
		FunCall(sub, List(zero, exp))
	}
	def product: Parser[Expression] = term ~ rep(("*" | "/") ~ term ^^ { case "*" ~ s => s case "/" ~ s => inverse(s) }) ^^
		{
			case term ~ Nil => term
			case term ~ list => FunCall(Identifier("mul"), term :: list)
		}
	private def inverse(exp: Expression): Expression = {
		val div = Identifier("div")
		val one = Number(1)
		FunCall(div, List(one, exp))
	}
	def funcall: Parser[Expression] = (identifier ~ opt(operands)) ^^
		{
			case identifier ~ None => identifier
			case identifier ~ Some(Nil) => FunCall(identifier, Nil)
			case identifier ~ Some(opList) => FunCall(identifier, opList)
		}

	def operands: Parser[List[Expression]] = ("(" ~> opt(expression ~ rep("," ~> expression)) <~ ")") ^^
		{
			case None => Nil
			case Some(exp ~ Nil) => List(exp)
			case Some(exp ~ expList) => exp :: expList
			case _ => Nil
		}

	def term: Parser[Expression] = literal | funcall | identifier | "(" ~> expression <~ ")"
	def literal: Parser[Literal] = (boole | number)

	def identifier: Parser[Identifier] = ("""[a-zA-Z][0-9a-zA-Z]*""".r) ^^
		{
			case id => Identifier(id)
		}

	def boole: Parser[Boole] = ("true" | "false") ^^
		{
			case tree => Boole(tree.toBoolean)
		}

	def number: Parser[Number] = ("""[\+|-]?[0-9]+(\.[0-9]+)?""".r) ^^
		{
			case num => Number(num.toDouble)
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
    println()
    println(expression)
    println("Expected: " + 23)
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "def b2 = 3 + 4 * 5"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "5"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: " + 5)
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "true || false"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b2"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: 23")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "def b1 = 4"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b1 < b2"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b1 == b2"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: false")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "def b3 = if (b1 == b2) 3 else 4"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b3"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: 4")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "def b4 = if (b1 < b2) 7 else 10"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b4"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: 7")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    
    expression = "def a1 = 2 + 3 * 5"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: 17.0")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "def a2 = (2 + 3) * 5"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a2"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: 25.0")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 == a2"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: false")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 < a2"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 < a2 && true && a1 == a2 && a1 == a3"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: false")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 == a2 || false || a1 < a2 || a1 == a3"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 == a3"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: Undefined Identifier: a3")
    try {
      println("Actual: " + eTree.get.execute(globalEnv)) 
    }
    catch {
      case e : UndefinedException => println("Actual: " + e.msg)
    }
    expression = "def a3 = if (a1 == a2) a4 else 0"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a3"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: 0.0")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a4"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: Undefined Identifier: a4")
    try {
      println("Actual: " + eTree.get.execute(globalEnv)) 
    }
    catch {
      case e : UndefinedException => println("Actual: " + e.msg)
    }
    expression = "3 - 2 - 1"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: 0.0")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "100 / 10 / 2"
    eTree = ewokParser.parseAll(ewokParser.expression, expression)
    println()
    println(expression)
    println("Expected: 5.0")
    println("Actual: " + eTree.get.execute(globalEnv))
  }
}