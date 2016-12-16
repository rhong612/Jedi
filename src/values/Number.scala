package values

import expressions._

case class Number(val value : Double) extends Literal{
  
  def this(sValue : String) {
    this(sValue.toDouble)
  }
  
  def +(other : Number) = {
    Number(this.value + other.value)
  }
  
  def -(other : Number) = {
    Number(this.value - other.value)
  }
  
  def *(other : Number) = {
    Number(this.value * other.value)
  }
  
  def /(other : Number) = {
    Number(this.value / other.value)
  }
  
  def ==(other : Number) = {
    Boole(this.value == other.value)
  }
  
  def <(other : Number) = {
    Boole(this.value < other.value)
  }
  
  def >(other : Number) = {
  	Boole(this.value > other.value)
  }
  
  override def toString = value.toString
}
