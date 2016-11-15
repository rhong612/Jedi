package values

import expressions._

case class Boole(val value : Boolean) extends Literal{
  
  def this(sValue : String) {
    this(sValue.toBoolean)
  }
  
  def &&(other : Boole) = {
    Boole(this.value && other.value)
  }
  
  def ||(other : Boole) = {
    Boole(this.value || other.value)
  }
  
  def unary_! = {
    Boole(!value)
  }
  
  def ==(other : Boole) = {
    Boole(this.value == other.value)
  }
  
  override def toString = value.toString
}

object Boole {
  def test = {
    var trueBoole = new Boole(true)
    var falseBoole = new Boole(false)
    var resultBoole = trueBoole && falseBoole
    println("True and False")
    println("Expected: False")
    println("Actual: " + resultBoole)
    
    println()
    resultBoole = falseBoole && trueBoole
    println("False and True")
    println("Expected: False")
    println("Actual: " + resultBoole)
    
    println()
    resultBoole = trueBoole || falseBoole
    println("True or False")
    println("Expected: True")
    println("Actual: " + resultBoole)
    
    println()
    resultBoole = trueBoole || trueBoole
    println("True or True")
    println("Expected: True")
    println("Actual: " + resultBoole)
    
    println()
    resultBoole = falseBoole || falseBoole
    println("False or False")
    println("Expected: False")
    println("Actual: " + resultBoole)
    
    println()
    resultBoole = trueBoole && trueBoole
    println("True and True")
    println("Expected: True")
    println("Actual: " + resultBoole)
    
    println()
    resultBoole = falseBoole || falseBoole
    println("False and False")
    println("Expected: False")
    println("Actual: " + resultBoole)
    
    println()
    resultBoole = !trueBoole
    println("!true")
    println("Expected: False")
    println("Actual: " + resultBoole)
    
    println()
    resultBoole = !falseBoole
    println("!false")
    println("Expected: True")
    println("Actual: " + resultBoole)
  }
}