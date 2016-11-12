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
      
      //Boole operations
      case "and" => and(args)
      case "or" => or(args)
      case "not" => not(args)
      
      case _ => throw new UndefinedException(opcode.name)
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
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all equals inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2(0) == args2(1)
  }
  
  private def lessThan(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("less than expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all less than inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2(0) < args2(1)
  }
  
  private def and(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("AND expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("all AND inputs must be booles")
    val args2 = vals.map(_.asInstanceOf[Boole])
    args2.reduce(_&&_)
  }
  
  private def or(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("OR expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("all OR inputs must be booles")
    val args2 = vals.map(_.asInstanceOf[Boole])
    args2.reduce(_||_)
  }
  
  private def not(vals: List[Value]): Value = {
    if (vals.length != 1) throw new TypeException("NOT expects 1 input")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("NOT input must be a boole")
    val args2 = vals.map(_.asInstanceOf[Boole])
    !args2(0)
  }
}