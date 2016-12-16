package ui

import expressions._
import values._

object testDriver {
  def main(args : Array[String]) : Unit = {
    check
    check2
    parserTest
 }
  
 def parserTest() {
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
    
    
    println("------------TESTING VARIABLES, ITERATION, AND ASSIGNMENT----------------")
    val env = new Environment
    expression = "def count = var(0)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: OK")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "count"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: Variable(0.0)")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "[count]"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 0.0")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "count = [count] + 1"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: DONE")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "[count]"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 1.0")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "while([count] < 100) count = [count] + 1"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: DONE")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "[count]"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 100.0")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "def fun = var(lambda(x) x * x)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: OK")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "[fun](3)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 9.0")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "fun = lambda(x) 2 * x"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: DONE")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "[fun](3)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 6.0")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "def more = var(true)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: OK")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "more = false"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: done")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "more = 0"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: done")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "def countRef = var(count)"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: OK")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "countRef"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: Variable(Variable(100.0))")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "[countRef]"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: Variable(100.0)")
    println("Actual: " + eTree.get.execute(env))
    
    expression = "[[countRef]]"
    eTree = sithParser.parseAll(sithParser.expression, expression)
    println()
    println(expression)
    println("Expected: 100.0")
    println("Actual: " + eTree.get.execute(env))
 }
  
  
 def check2() {
    
    val globalEnv = new Environment()
    val num1 = new Number(100.0)
    val num2 = new Number(42.0)
    val bool1 = new Boole(true)
    
    // executing literals: 100, 42, true
    println("... expected = 100.0, actual = " + num1.execute(globalEnv))
    println("... expected = 42.0, actual = " + num2.execute(globalEnv))
    println("... expected = true, actual = " + bool1.execute(globalEnv))
    
    // put some stuff in global environment
    val id1 = new Identifier("x")
    val id2 = new Identifier("y")
    val id3 = new Identifier("z")
    globalEnv.put(List(id1, id2, id3), List(num1, num2, bool1))
    
    // executing identifiers: x, y, z
    println("... expected = 100.0, actual = " + id1.execute(globalEnv))
    println("... expected = 42.0, actual = " + id2.execute(globalEnv))
    println("... expected = true, actual = " + id3.execute(globalEnv))
    
    // next, create & execute some funcalls:
    
    // executing add(x, 42)
    var op = new Identifier("add")
    var args = List(id1, num2)
    var fc1 = new FunCall(op, args)
    println("... expected = 142.0, actual = " + fc1.execute(globalEnv))
    
    // executing add(x, 42, add(x, 42))
    var op2 = new Identifier("add")
    var args2 = List(id1, num2, fc1)
    var fc2 = new FunCall(op2, args2)
    println("... expected = 284.0, actual = " + fc2.execute(globalEnv))
    
    // executing sub(x, 42)
    op = new Identifier("sub")
    fc1 = new FunCall(op, args)
    println("... expected = 58.0, actual = " + fc1.execute(globalEnv))
    
    // executing mul(x, 42)
    op = new Identifier("mul")
    fc1 = new FunCall(op, args)
    println("... expected = 4200.0, actual = " + fc1.execute(globalEnv))
    
    // executing sub(x, 42)
    op = new Identifier("div")
    fc1 = new FunCall(op, args)
    println("... expected = 2.380952..., actual = " + fc1.execute(globalEnv))
    
    // executing equals(x, 42)
    op = new Identifier("equals")
    fc1 = new FunCall(op, args)
    println("... expected = false, actual = " + fc1.execute(globalEnv))
    
    // executing less(x, 42)
    op = new Identifier("less")
    fc1 = new FunCall(op, args)
    println("... expected = false, actual = " + fc1.execute(globalEnv))
    
    // executing not(less(x, 42))
    op2 = new Identifier("not")
    args2 = List(fc1)
    fc2 = new FunCall(op2, args2)
    println("... expected = true, actual = " + fc2.execute(globalEnv))
    
  }
  
  
  def check() {
    println("Checking Number class:")
    val num1 = new Number(100.0)
    val num2 = new Number(42.0)
    println("...expected = " + (100.0 + 42.0) + ", actual = " + (num1 + num2))
    println("...expected = " + (100.0 * 42.0) + ", actual = " + (num1 * num2))
    println("...expected = " + (100.0 - 42.0) + ", actual = " + (num1 - num2))
    println("...expected = " + (100.0 / 42.0) + ", actual = " + (num1 / num2))
    println("...expected = " + (100.0 < 42.0) + ", actual = " + (num1 < num2))
    println("...expected = " + (100.0 == 42.0) + ", actual = " + (num1 == num2))
    
    println("Checking Boole class:")
    val t = new Boole(true)
    val f = new Boole(false)
    println("...expected = " + (true && false) + ", actual = " + (t && f))
    println("...expected = " + (true && true) + ", actual = " + (t && t))
    println("...expected = " + (true || false) + ", actual = " + (t || f))
    println("...expected = " + (false || false) + ", actual = " + (f && f))
    
    println("Checking Environment class:")
    val n1 = new Identifier("n1")
    val n2 = new Identifier("n2")
    val n3 = new Identifier("n3")
    val n4 = new Identifier("n4")
    val b1 = new Identifier("b1")
    val b2 = new Identifier("b2")
    val b3 = new Identifier("b3")
    val b4 = new Identifier("b4")
    val x = new Identifier("x")
    val globalEnv = new Environment()
    val env1 = new Environment(globalEnv)
    val env2 = new Environment(env1)
    globalEnv.put(List(n1, n2, b1, b2), List(num1, num2, t, f))
    env1.put(List(n3, b3), List(num1 + num2, num1 < num2)) 
    env2.put(List(n2, n4, b3, b4), List(num2 - num1, num1 / num2, num2 < num1, f))
    
    println("...expected = " + num1 + " actual = " + env2.find(n1))
    println("...expected = " + (num2 - num1) + " actual = " + env2.find(n2))
    println("...expected = " + (num2 + num1) + " actual = " + env2.find(n3))
    println("...expected = " + (num1 / num2) + " actual = " + env2.find(n4))
    println("...expected = " + (t) + " actual = " + env2.find(b1))
    println("...expected = " + (f) + " actual = " + env2.find(b2))
    println("...expected = " + (num2 < num1) + " actual = " + env2.find(b3))
    println("...expected = " + (f) + " actual = " + env2.find(b4))
    println("...expected = " + Notification.UNKNOWN + " actual = " + env2.find(x))
    
    println("Checking polymorphism:")
    var exp: Expression = num1
    println("...exp = " + exp)
    exp = t
    println("...exp = " + exp)
    exp = n1
    println("...exp = " + exp)
    var value: Value = num1
    println("...value = " + value)
    value = t
    println("...value = " + value)
    value = env2
    println("...value = " + value)    
  }
}