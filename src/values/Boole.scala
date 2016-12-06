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