/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// https://www.scala-lang.org/api/2.12.8/scala/util/matching/Regex.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/List.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/StringLike.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/Map.html

package org.mq.interpret

import scala.util.matching._


object Interpret {

  sealed abstract class LObject
  case class LSymbol(sym:String) extends LObject
  case class LNumber(num:Int) extends LObject
  case class LList(head:LObject, tail:LObject) extends LObject

  val nil = LSymbol("nil")
  val T = LSymbol("t")
  val error = LSymbol("ERROR")

  // custom map for holding user variables
  val userVar = scala.collection.mutable.Map[String,LObject]()

  // custom map for holding user function definitions
  val userFunc = scala.collection.mutable.Map[String,String]()

  // create another custom map for mapping arg to body
  // so `name` -> `arg` | and then `arg` -> `body`
  val userFuncArgBody = scala.collection.mutable.Map[String,LObject]()

  // for tokens to LObjs
  val stack = scala.collection.mutable.Stack[LObject]()

  val stack2 = List[LObject]()

  def resetEnv:Unit = 
  {
    // TO DO: reset your functions/variables environment
    userVar.clear()
    userFunc.clear()
    userFuncArgBody.clear()
    stack.clear()
  }

  //val pat = "[\\(\\)a-zA-Z]|[-?\\d]+|[+-/*]".r // for test case

  val pat = "[\\(\\)]|[a-zA-Z]+|[-?\\d]+|[+-/*]".r

  // below are used for matching strToLObj inputs
  val symbolPat = "[\\(\\)\\+\\-\\*\\/]".r

  val numPat = "[-?\\d]+".r

  val nameNumPat = "[a-zA-Z]+[a-zA-Z0-9]*".r

  // for Main.scala informal testing
  def custMatch(line:String):List[String] = nameNumPat.findAllIn(line.toLowerCase).toList 

  def matchPat(line:String):List[String] = // DO NOT CHANGE!
                              pat.findAllIn(line.toLowerCase).toList 

  // excludes brackets
  def strToLObj(s:String):LObject = {
    
    s match {
      case symbolPat(_*) => LSymbol(s) // check symbols first - otherwise negative nums won't work
      case numPat(_*) => LNumber(s.toInt) // then ints
      case nameNumPat(_*) => LSymbol(s) // and then names
      case _ => error
    }
  }


  // unused function first attempt at tokensToLObjs
  def tokensRestOfList(a:List[String]):LObject = a match {
    case Nil => nil
    case h :: t =>
      (strToLObj(h) != error) match{
        case false => eval(error) 
        case true => 
          h match {
            case "(" => tokensRestOfList(t)
            case ")" => tokensRestOfList(t)
            case _ => 
              (a.size == 1 ) match{
               case false => cons(strToLObj(h), tokensRestOfList(t))
               case true => strToLObj(h)
              }
              
          }
      }   
  }

  def tokensToLObjs(a:List[String]):Option[LObject] = {
    // TO DO: convert a list of token strings to an LObject
    // NOTE: anywhere () is seen, it should be interpreted as nil
    
    val lst: LObject = a match{
      case Nil => nil
      case h => tokenHelper(a)
    } 
    
    //Some(eval(lst))
    Some(lst)
    
  }


  // recursive function for dealing with list of strings... (tokensToLObjs)
  def tokenHelper(a: List[String]):LObject = a match{
    
    // a is empty, return the stack...
    case Nil => stack.pop()

    // t is tail item, h is rest of list
    case h :+ t => 
      (h,t) match{

        // if we come across last closing bracket it means we are done and can return
        case (Nil, "(") => stack.pop()

        // opening bracket and still items in a
        case (_,"(") => 
          (stack.length) match {
            case 0 => error
            case 1 => nil
            case _ => stack.push(LList(stack.pop(), stack.pop())); tokenHelper(h)
          }

          // come across closing bracket
           case (_,")") => stack.push(nil); tokenHelper(h)

          // all other cases we do this
           case (_,_) => 
            (stack.length) match{
              case 0 => stack.push(strToLObj(t)); tokenHelper(h)
              case _ => stack.push(LList(strToLObj(t), stack.pop)); tokenHelper(h)
            }

      }

  }



  // for testing
  def lineToLObj(line:String):LObject = tokensToLObjs(matchPat(line)) match{
    case Some(s) => s
    case None    => error
  }

  
  def setValue(varName:String, value:LObject):Unit = (varName, value) match {
    case(nameNumPat(_*), LNumber(x)) => userVar += (varName -> value)
    case(nameNumPat(_*), LList(x,y)) => userVar += (varName -> value)
  }

  def getValue(varName:String):LObject = varName match { // 
    case nameNumPat(_*) => 
      (userVar.get(varName)) match{
        case Some(s) => s
        case None => LSymbol(varName)
      }

    case _ => error
  }

  //custom class gets body of function
  def getFuncBody(varName: String):LObject = varName match{
    case nameNumPat(_*) => 
      (userFunc.get(varName)) match{
        case Some(s) => getValue(s) // CONS HERE?
        case None => nil
      }
       case _ => error
  }

  //custom class: 
  def getFuncMap(varName: String):String = varName match{
    case nameNumPat(_*) => 
      (userFunc.get(varName)) match{
        case Some(s) => s // CONS HERE?
        case None => "GETFUNCMAPERROR: " + varName
      }
      case _ => "error"
  }    

  def add(a:LObject, b:LObject):LObject = (a,b) match{
      case (LNumber(x),LNumber(y)) => LNumber(x+y)
      case _ => error
  }    

  def sub(a:LObject, b:LObject):LObject = (a,b) match{
      case (LNumber(x),LNumber(y)) => LNumber(x-y)
      case _ => error
  }   

  def mul(a:LObject, b:LObject):LObject = (a,b) match {
    case (LNumber(x),LNumber(y)) => LNumber(x*y)
      case _ => error
  }  

  def div(a:LObject, b:LObject):LObject = (a,b) match {
    case (LNumber(x),LNumber(y)) => LNumber(x/y)
    case _ => error
  }    

  def car(a:LObject):LObject = a match {
    case LNumber(x) => LNumber(x)
    case LSymbol(x) => LSymbol(x)
    case LList(x,y) =>
      x match {
        case x => x
      }
    case _ => error
  }       

  def cdr(a:LObject):LObject = a match {
    case LNumber(x) => LNumber(x)
    case LSymbol(x) => LSymbol(x)
    case LList(x,y) => y
    case _ => error
  }       

  def cons(a:LObject, b:LObject):LObject = (a,b) match {
    case (a,b) => LList(a,b)
    case _ => error
  }

  def eeqq(a:LObject, b:LObject):LObject = (a,b) match{
    case (LNumber(x), LNumber(y)) =>
      (x == y) match {
        case true => T
        case false => nil
      } 
    case (LSymbol(x), LSymbol(y)) =>
      (x == y) match {
        case true => T
        case false => nil
      }
    case _ => nil
  }   

  def setq(v:String, b:LObject):LObject = (v, b) match{
    case(nameNumPat(_*), LNumber(x)) => setValue(v,b); eval(getValue(v))
    case(nameNumPat(_*), LSymbol(a)) => setValue(v,b); eval(getValue(v))
    case(nameNumPat(_*), LList(x,y)) => setValue(v,b); eval(getValue(v))
  }
      

  def iiff(cond:LObject, ifThen:LObject, ifElse:LObject):LObject = cond match{
    case T => eval(ifThen)
    case `nil` => eval(ifElse)
    case _ => error
  } 

  // TODO: REDO defun
  def defun(name:String, arg:String, body:LObject):LObject = (name, arg, body) match {
    case(nameNumPat(_*), nameNumPat(_*), LList(a,b)) => setValue(arg,body); userFunc += (name -> arg); LSymbol(name)
    // TO DO: define a function
    // the function definition source would look like:
    //      (def name (arg) body) 
    //      e.g. def foo(y)(+ y 2) (string: name) (string arg) (LList(+ y 2)) etc.
    case _ => error
  }

  // CUSTOM function: get item in list to str
  def lSymbolToStr(inp: LObject): String = inp match {
    case LSymbol(a) => a
    case LList(LSymbol(a),nil) => a
    case _ => "nil"
  }


  // TODO: create function that iterates through the list when we call getList(name)
  // we check the function body for the existance of the "args" string 
  // if it exists we replace it with the input args...
  // then we evaluate it
  // or we can just call setq()? and then eval?

  def searchItem(strArg: String, inpArg: LObject, lst: LObject): LObject = 
    lst match {
        case LList(head,tail) => 
          head match {
            case LSymbol(a) =>
              (a == strArg) match {
                case true => cons(inpArg, searchItem(strArg, inpArg, tail))
                case false => cons(LSymbol(a), searchItem(strArg, inpArg, tail))
              }
            case LNumber(x) => cons(LNumber(x), searchItem(strArg, inpArg, tail))
            case LList(h,t) => cons(LList(h,t), searchItem(strArg, inpArg, tail))
          }
        case LNumber(x) => LNumber(-1)
        case LSymbol(a) => LSymbol("searchItemError")
  }

      
  // TODO: redo funCall
  def funCall(name:String, arg:LObject):LObject =  {
    //(name, arg) match

    val strArg: String = name match{
      case nameNumPat(_*) => getFuncMap(name)
      case _ => "FUNCALLERROR"
    }

    val lst: LObject = (name, arg) match{
      case (nameNumPat(_*), LSymbol(a)) => LSymbol(name)
      case (nameNumPat(_*), LList(a,b)) => arg
      case (nameNumPat(_*), LNumber(x)) => 
         eval(searchItem(strArg, arg, getFuncBody(name)))
      case _ => error
    }
   
   lst

  } 


  // check if a symbol is a variable or function or not...
  def isVar(a: String): LObject =  a match {

    case nameNumPat(_*) =>
        (getFuncBody(a)) match {
          case `nil` => getValue(a)
          case _ => LSymbol(a)
        }
    case _ => error

  }


  def eval(a:LObject):LObject = a match {
  
  case LSymbol("nil") => nil
  case LSymbol("t") => T
  case LNumber(x) => LNumber(x)

  case LSymbol(a) => isVar(a)

  case LList(hd, tl) =>
    (hd,tl) match{
      // list ops
      case(LSymbol("car"), LList(a,b)) => car(eval(LList(a,b)))
      case(LSymbol("cdr"), LList(a,b)) => cdr(eval(LList(a,b)))
      case(LSymbol("cons"), LList(a,b)) => cons(eval(a),eval(b))

      // eval of eval
      case(LSymbol("eval"), LList(a,b)) => eval(cdr(a))

      // conditions
      case(LSymbol("quote"), LList(a,b)) => a
      case(LSymbol("eq"), LList(a,b)) => eeqq(eval(a),eval(b))
      case(LSymbol("if"),LList(a,b)) => 
        eval(a) match{
          case T => iiff(eval(a), eval(b), eval(b)) // iiff => (condition, if true, if false)
          case nil => iiff(eval(a), eval(b), eval(cdr(b)))
        }
      
      // VARIABLES
      case(LSymbol("setq"), LList(LSymbol(a),b)) => setValue(a,eval(b)); getValue(a)
      

      // DEFUN
      // TODO
      case(LSymbol("def"), LList(LSymbol(p), b)) =>  defun(p, lSymbolToStr(car(b)), car(cdr(b)))
    
        
         
      // FUNCALLs 
      // TODO
      case(LSymbol(p),LList(LNumber(x),LSymbol("nil"))) => eval(funCall(p, LNumber(x)))

      // case (LSymbol("funCall"), LList(a,b)) => //funCall(lSymbolToStr(car(a)), car(b))
      //   (a, car(b)) match{
      //     case (LSymbol(p), LList(LNumber(q),y)) => funCall(p, LNumber(q)) 
      //     case _ => LNumber(-1)
      //   }

       // operators
      case(LNumber(a), nil) => LNumber(a)
      case(LList(p,q), nil) => eval(LList(p,q))
      case(LSymbol("+"), LList(a,b)) => add(eval(a),eval(b))
      case(LSymbol("-"), LList(a,b)) => sub(eval(a),eval(b))
      case(LSymbol("*"), LList(a,b)) => mul(eval(a),eval(b))
      case(LSymbol("/"), LList(a,b)) => div(eval(a),eval(b))    


      
      // if we get a letter evaluate it
      case(LSymbol(a), nil) => eval(hd)

      // in all other cases we return nil
      case _ => nil


    }

    // no matches return error
    case _          => error


  }

  def showLine(s:LObject):Unit = { show(s);  println() }

  def show(s:LObject):Unit = s match
  {
  case LList(h, t)  => print("(")
                       show(h)
                       showList(t)
                       print(")")
  case LSymbol(a)   => print(a)
  case LNumber(a)   => print(a)
  }

  def showList(s:LObject):Unit = s match
  {
  case LSymbol("nil") =>
  case a:LList => print(" ")
                  show(a.head)
                  a.tail match
                  {
                  case b:LList => showList(b)
                  case _ =>
                  }
  case _ => print(" . ")
            show(s)
  }

}
