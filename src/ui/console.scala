package ui

import values._
import expressions._

object console {
   val parsers = new SithParsers // for now
   val globalEnv = new Environment()

   def execute(cmmd: String): String = {
      val tree = parsers.parseAll(parsers.expression, cmmd)
      tree match {
         case t: parsers.Failure => throw new SyntaxException(t)
         case _ => "" + tree.get.execute(globalEnv)
      }
   }
   
    def repl {
      var more = true
      var input = ""
      var result = ""
      while(more) {
         try {
           input = scala.io.StdIn.readLine
           if (input.equals("quit")) {
             more = false
             println("Bye")
           }
           else {
             result = execute(input)
             println(result)
           }
         } 
         catch {
            case e: SyntaxException => {
               println(e.msg)
               println(e.result.msg)
               println("line # = " + e.result.next.pos.line)
               println("column # = " + e.result.next.pos.column)
               println("token = " + e.result.next.first)
            }
            case e: UndefinedException => {
               println(e.msg)
            }
            case e : TypeException => {
            	println(e.msg)
            }
         } finally {
            Console.flush 
         }
      }
   }
    
   def main(args: Array[String]): Unit = { repl }
}