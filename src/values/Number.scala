package values

import expressions._

case class Number(val value : Double) extends Literal{
  
  def this(sValue : String) {
    this(sValue.toDouble)
  }
  
  def +(other : Number) = {
    new Number(this.value + other.value)
  }
  
  def -(other : Number) = {
    new Number(this.value - other.value)
  }
  
  def *(other : Number) = {
    new Number(this.value * other.value)
  }
  
  def /(other : Number) = {
    new Number(this.value / other.value)
  }
  
  def ==(other : Number) = {
    new Boole(this.value == other.value)
  }
  
  def <(other : Number) = {
    new Boole(this.value < other.value)
  }
  
  override def toString = value.toString
}

object Number {
  def test = {
    var five = new Number(5)
    var seven = new Number(7)
    var zero = new Number(0)
    var twoPointFive = new Number(2.5)
    var negativeFive = new Number(-5)
    var anotherFive = new Number(5)
    
    var result = five + seven
    println("5 + 7")
    println("Expected: 12")
    println("Actual: " + result)
    println()
    
    result = five + negativeFive
    println("5 + -5")
    println("Expected: 0")
    println("Actual: " + result)
    println()
    
    result = five + zero
    println("5 + 0")
    println("Expected: 5")
    println("Actual: " + result)
    println()
    
    result = five + twoPointFive
    println("5 + 2.5")
    println("Expected: 7.5")
    println("Actual: " + result)
    println()
    
    result = zero - seven
    println("0 - 7")
    println("Expected: -7")
    println("Actual: " + result)
    println()
    
    result = seven - twoPointFive
    println("7 - 2.5")
    println("Expected: 4.5")
    println("Actual: " + result)
    println()
    
    result = seven * seven
    println("7 * 7")
    println("Expected: 49")
    println("Actual: " + result)
    println()
    
    result = zero * seven
    println("0 * 7")
    println("Expected: 0")
    println("Actual: " + result)
    println()
    
    result = five / twoPointFive
    println("5 / 2.5")
    println("Expected: 2")
    println("Actual: " + result)
    println()
    
    var bResult = five == five
    println("5 == 5")
    println("Expected: True")
    println("Actual: " + bResult)
    println()
    
    bResult = five == seven
    println("5 == 7")
    println("Expected: False")
    println("Actual: " + bResult)
    println()
    
    bResult = five < five
    println("5 < 5")
    println("Expected: False")
    println("Actual: " + bResult)
    println()
    
    bResult = twoPointFive < seven
    println("2.5 < 7")
    println("Expected: True")
    println("Actual: " + bResult)
    println()
  }
}