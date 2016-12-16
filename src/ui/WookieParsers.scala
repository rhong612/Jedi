package ui

import scala.util.parsing.combinator._
import expressions._
import values._

class WookieParsers extends RegexParsers {
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
	def funcall: Parser[Expression] = ((identifier | ("("~>lambda<~")")) ~ opt(operands)) ^^
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

	def term: Parser[Expression] = lambda | block | literal | funcall  | identifier | "(" ~> expression <~ ")"
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
