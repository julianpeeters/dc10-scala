package dc10.scala.internal

import dc10.scala.*
// import scala.util.NotGiven

object definition


  // trait Define[A]:
  //   def statement: Statement

  // object Define:

  //   given comp[A, B]: Define[A => B] =
  //     new Define[A => B]:
  //       def statement(value: `Value.Var.Unbound.Comp`[A, B]): Statement =
  //         value.sym match
  //           case DefSym(nme) => 
  //           case `DefSym.1`(nme, arg1) =>
  //           case `DefSym.2`(nme, arg1, arg2) =>
  //           case ValSym(nme) =>
          

  //   given data[T](using NotGiven[T =!= Function[?, ?]]): Define[T] =
  //     new Define[T]:
  //       def statement(value: `Value.Var.Unbound.Comp`[A, B]): Statement =
  //         ???


  // extension [T] (value: `Value.Var.Unbound.Data`[T])
  //   def define: Statement =
  //     value.sym match
  //       case `DefSym`(nme) =>  ???
  //       case `ValSym`(nme) => ???

  //       // these aren't supposed to be reached. how to enforce?   

  //       // possibly a =!=
  //       // case `DefSym.1`(nme, arg1) => ???
  //       // case `DefSym.2`(nme, arg1, arg2) => ???
      
  // extension [A, B] (value: `Value.Var.Unbound.Comp`[A, B])
  //   def define: Statement =
  //     value.sym match
  //       case `DefSym`(nme) => ???
  //       case `DefSym.1`(nme, arg1) => ???
  //       case `DefSym.2`(nme, arg1, arg2) => ???
  //       case `ValSym`(nme) => ???
      
  // extension [T] (value: `Value.Var.Bound.Data`[T])
  //   def define: Statement =
  //     value.sym match
  //       case `DefSym`(nme) =>  ???
  //       case `DefSym.1`(nme, arg1) => ???
  //       case `DefSym.2`(nme, arg1, arg2) => ???
  //       case `ValSym`(nme) => ???
      
  // extension [A, B] (value: `Value.Var.Bound.Comp`[A, B])
  //   def define: Statement =
  //     value.sym match
  //       case `DefSym`(nme) =>  ???
  //       case `DefSym.1`(nme, arg1) => ???
  //       case `DefSym.2`(nme, arg1, arg2) => ???
  //       case `ValSym`(nme) => ???
      


  // extension [T] (value: `Value.Var`[T])
  //   def define: Statement =
  //     value match
  //       case `Value.Var.Unbound.Comp`(indent, `DefSym`(nme), tpe) => Statement.`def`.`0`(value)
  //       case `Value.Var.Unbound.Comp`(indent, `DefSym.1`(nme, arg1), tpe) => Statement.`def`.`1`(arg1, tpe.barg, None, value)
  //       case `Value.Var.Unbound.Comp`(indent, `DefSym.2`(nme, arg1, arg2), tpe) => ??? //Statement.`def`.`2`(arg1, arg2, ???, None, ???)
  //       case `Value.Var.Unbound.Comp`(indent, `ValSym`(nme), tpe) => Statement.`val`(value)
  //       case `Value.Var.Bound.Comp`(indent, sym, tpe, impl) => Statement.`val`(value)
  //       case `Value.Var.Unbound.Data`(indent, sym, tpe) => Statement.`val`(value)
  //       case `Value.Var.Bound.Data`(indent, sym, tpe, impl) => Statement.`val`(value)