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
	def inequality: Parser[Expression] = (sum ~ rep("<" ~> sum)) ^^
		{
			case sum ~ Nil => sum
			case sum ~ list => FunCall(Identifier("less"), sum :: list)
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
	def product: Parser[Expression] = funcall ~ rep(("*" | "/") ~ funcall ^^ { case "*" ~ s => s case "/" ~ s => inverse(s) }) ^^
		{
			case funcall ~ Nil => funcall
			case funcall ~ list => FunCall(Identifier("mul"), funcall :: list)
		}
	private def inverse(exp: Expression): Expression = {
		val div = Identifier("div")
		val one = Number(1)
		FunCall(div, List(one, exp))
	}
	def funcall: Parser[Expression] = (term ~ opt(operands)) ^^
		{
			case term ~ None => term
			case term ~ Some(Nil) => FunCall(term.asInstanceOf[Identifier], Nil)
			case term ~ Some(opList) => FunCall(term.asInstanceOf[Identifier], opList)
		}

	def operands: Parser[List[Expression]] = ("(" ~> opt(expression ~ rep("," ~> expression)) <~ ")") ^^
		{
			case None => Nil
			case Some(exp ~ Nil) => List(exp)
			case Some(exp ~ expList) => exp :: expList
		}

	def term: Parser[Expression] = literal | identifier | "(" ~> expression <~ ")"
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