package ui

import expressions._
import values._

object system {
  
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      //Arithmetic Operations with numbers
      case "add" => add(args)
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "equals" => equals(args)
      case "less" => lessThan(args)
      case "not" => not(args)
      
      case _ => throw new UndefinedException("Unknown Operation: " + opcode.name)
    }
  }

  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_+_)
  }

  private def mul(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("multiplication expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all multiplication inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_*_)
  }
  
  private def sub(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("subtraction expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all subtraction inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_-_)
  }
  
  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("division expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all division inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_/_)
  }
  
  private def equals(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("equals expects 2 inputs")
    var numCount = 0
    var boolCount = 0
    for (i <- 0 until 2) {
      vals(i) match {
        case n : Number => numCount += 1
        case b : Boole => boolCount += 1
        case _ => throw new TypeException("Values must be both booles or both numbers")
      }
    }
    if (numCount == 2) {
      val args = vals.map(_.asInstanceOf[Number]) 
      Boole(args(0) == args(1))
    }
    else {
      val args = vals.map(_.asInstanceOf[Boole])
      Boole(args(0) == args(1))
    }
  }
  
  private def lessThan(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("less than expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all less than inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2(0) < args2(1)
  }
  
  private def not(vals: List[Value]): Value = {
    if (vals.length != 1) throw new TypeException("NOT expects 1 input")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("NOT input must be a boole")
    val args2 = vals.map(_.asInstanceOf[Boole])
    !args2(0)
  }
}