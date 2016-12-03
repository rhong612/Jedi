package ui

import scala.util.parsing.combinator._
import expressions._
import values._

class SithParsers extends RegexParsers {
	def expression: Parser[Expression] = declaration | conditional | assignment | disjunction | failure("Invalid expression")

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
	def funcall: Parser[Expression] = ((identifier | ("("~>lambda<~")") | deref) ~ opt(operands)) ^^
		{
			case identifier ~ None => identifier
			case identifier ~ Some(Nil) => FunCall(identifier)
			case identifier ~ Some(opList) => FunCall(identifier, opList)
		}

	def operands: Parser[List[Expression]] = ("(" ~> opt(expression ~ rep("," ~> expression)) <~ ")") ^^
		{
			case None => Nil
			case Some(exp ~ Nil) => List(exp)
			case Some(exp ~ expList) => exp :: expList
			case _ => Nil
		}

	def term: Parser[Expression] = lambda | block | literal | funcall | deref | identifier | "(" ~> expression <~ ")"
	def literal: Parser[Literal] = (boole | number)
	
	def assignment : Parser[Expression] = identifier ~ "=" ~ expression ^^
		{
		case id ~ "=" ~ exp => Assignment(id, exp)
		}
	
	def deref : Parser[Expression] = "[" ~> expression <~ "]" ^^
		{
		case id => FunCall(Identifier("content"), List(id))
		}
	
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

	def block: Parser[Expression] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^
		{
			case exp ~ Nil => Block(List(exp))
			case exp ~ list => Block(exp :: list)
		}

	def lambda : Parser[Expression] = "lambda" ~> parameters ~ expression ^^ {
		case param ~ exp => Lambda(param, exp)
	}
	
	def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^
		{
			case None => Nil
			case Some(e ~ Nil) => List(e)
			case Some(e ~ exps) => e :: exps
			case _ => Nil
		}
}

object SithParsers {
	def test = {
    println("------------TESTING NUMBER PARSER----------------")
    val sithParser = new SithParsers
    var expression = "+10"
    var tree = sithParser.parseAll(sithParser.number, expression)
    println("Expected: 10")
    println("Actual: " + tree.get)
    expression = "-10"
    tree = sithParser.parseAll(sithParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    expression = "5.2142"
    tree = sithParser.parseAll(sithParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    expression = "4912"
    tree = sithParser.parseAll(sithParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    expression = "0"
    tree = sithParser.parseAll(sithParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    expression = "+9999"
    tree = sithParser.parseAll(sithParser.number, expression)
    println("Expected: 9999")
    println("Actual: " + tree.get)
    expression = "123"
    tree = sithParser.parseAll(sithParser.number, expression)
    println("Expected: " + expression)
    println("Actual: " + tree.get)
    
    
    println("------------TESTING BOOLE PARSER----------------")
    expression = "true"
    var bTree = sithParser.parseAll(sithParser.boole, expression)
    println("Expected: " + expression)
    println("Actual: " + bTree.get)
    expression = "false"
    bTree = sithParser.parseAll(sithParser.boole, expression)
    println("Expected: " + expression)
    println("Actual: " + bTree.get)
    
    
    
    
    println("------------TESTING IDENTIFIER PARSER----------------")
    expression = "aVarName"
    var iTree = sithParser.parseAll(sithParser.identifier, expression)
    println("Expected: " + expression)
    println("Actual: " + iTree.get)
    expression = "withNumbers01231"
    iTree = sithParser.parseAll(sithParser.identifier, expression)
    println("Expected: " + expression)
    println("Actual: " + iTree.get)
    expression = "numbersIn1Middle2"
    iTree = sithParser.parseAll(sithParser.identifier, expression)
    println("Expected: " + expression)
    println("Actual: " + iTree.get)
    expression = "a"
    iTree = sithParser.parseAll(sithParser.identifier, expression)
    println("Expected: " + expression)
    println("Actual: " + iTree.get)
    
    
    println("------------TESTING LITERAL PARSER----------------")
    expression = "true"
    var lTree = sithParser.parseAll(sithParser.literal, expression)
    println("Expected: " + expression)
    println("Actual: " + lTree.get)
    expression = "4.5124"
    lTree = sithParser.parseAll(sithParser.literal, expression)
    println("Expected: " + expression)
    println("Actual: " + lTree.get)
    
    
    println("------------TESTING OVERALL PARSER----------------")
    val globalEnv = new Environment()
    expression = "3 + 4 * 5"
    var eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: " + 23)
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "def b2 = 3 + 4 * 5"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "5"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: " + 5)
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "true || false"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b2"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 23")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "def b1 = 4"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b1 < b2"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b1 == b2"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: false")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "def b3 = if (b1 == b2) 3 else 4"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b3"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 4")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "def b4 = if (b1 < b2) 7 else 10"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    expression = "b4"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 7")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    
    expression = "def a1 = 2 + 3 * 5"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 17.0")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "def a2 = (2 + 3) * 5"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a2"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 25.0")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 == a2"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: false")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 < a2"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 < a2 && true && a1 == a2 && a1 == a3"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: false")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 == a2 || false || a1 < a2 || a1 == a3"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a1 == a3"
    eTree = sithParser.parseAll(sithParser.expression, expression)
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
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a3"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 0.0")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "a4"
    eTree = sithParser.parseAll(sithParser.expression, expression)
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
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 0.0")
    println("Actual: " + eTree.get.execute(globalEnv))
    expression = "100 / 10 / 2"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 5.0")
    println("Actual: " + eTree.get.execute(globalEnv))
    
    
    println("------------TESTING LAMBDA AND BLOCK----------------")
    val newEnv = new Environment
    expression = "def square = lambda (x) x * x"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: OK")
    println("Actual: " + eTree.get.execute(newEnv))
    
    
    expression = "square(3)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 9.0")
    println("Actual: " + eTree.get.execute(newEnv))
    
    
    expression = "def f2c = lambda (ft) {def c = 5 / 9; c * (ft - 32)}"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: OK")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "f2c(212)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 100.0")
    println("Actual: " + eTree.get.execute(newEnv))
    
    
    expression = "c"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: Undefined identifier: c")
    try {
      println("Actual: " + eTree.get.execute(newEnv)) 
    }
    catch {
      case e : UndefinedException => println("Actual: " + e.msg)
    }
    
    expression = "def addN = lambda (n) lambda (x) x + n"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(newEnv))
    
    
    expression = "def add6 = addN(6)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(newEnv))
    
    
    expression = "add6(3)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 9.0")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "def abs = lambda(x) if (x < 0) -1 * x else x"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "abs(-9)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 9.0")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "def delta = 100"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "def small = {def delta = 0.00001; lambda(x) abs(x) < delta}"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "small(-0.00000001)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: true")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "small(0.001)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: false")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "def fact = lambda (n) if (n == 0) 1 else n * fact(n - 1)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "fact(5)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 120.0")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "def fourCaller = lambda (x) x(4)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: ok")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "fourCaller(lambda (y) y + 1)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 5.0")
    println("Actual: " + eTree.get.execute(newEnv))
    
    expression = "(lambda (z) 2 * z) (3)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 6.0")
    println("Actual: " + eTree.get.execute(newEnv))
  }
}