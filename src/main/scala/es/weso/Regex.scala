package es.weso.regex

import util.Try

/**
 *  This code will contain a XML Schema Regex matcher
 *  
 *  @author: Jose Emilio Labra Gayo
 *  @see: See [[<http://www.w3.org/TR/xmlschema11-2/#regexs]] for more information
 *        about XML Schema Regular Expressions
 *        The code is inspired by [[http://hackage.haskell.org/package/hxt-regex-xmlschema-9.1.0/docs/src/Text-Regex-XMLSchema-String-Regex.html]]  
 *    
 */



sealed abstract class Regex 
case class Fail(msg: String) extends Regex
case class Symbol(chrs: Set[Char]) extends Regex
case class Star(r: Regex) extends Regex
case class Seq(r1: Regex, r2: Regex) extends Regex
case class Alt(r1: Regex, r2: Regex) extends Regex
case class Intl(r1: Regex, r2: Regex) extends Regex  	// Interleave r1 with r2
case class Rep(n:Int, r:Regex) extends Regex 			// n.. repetitions of r
case class Rng(m:Int, n:Int, r: Regex) extends Regex    // m..n repetitions of r
case object Empty extends Regex
case object Dot extends Regex

object Matcher {

  type Label[l] = Option[l]
  type Nullable[l] = (Boolean, List [(Label[l], String)])

  
  def nullable[l](r: Regex) : Boolean = nullableAux(r)._1
  
  def nullableAux[l](r:Regex) : Nullable[l] = {
    
    /* Auxiliary functions */
    def isectN[l](n1: Nullable[l], n2: Nullable[l]): Nullable[l] = {
    (n1,n2) match {
      case ((true,ws1), (true,ws2)) => (true,  ws1 ::: ws2) 
      case (_, _)                   => (false, List() )
     }
    }

    def unionN[l](n1: Nullable[l], n2: Nullable[l]): Nullable[l] = {
    (n1,n2) match {
      case ((false,_), (false,_)) => (false,List())
      case ((_,  ws1), (_,  ws2)) => (true, ws1 ::: ws2)
     }
    }

    r match {
      case Fail(_) => (false,List())
      case Empty   => (true, List())
      case Dot	   => (false,List()) 
      case Symbol(_) => (false, List())
      case Star(_) => (true, List())
      case Seq(r1,r2) => isectN(nullableAux(r1),nullableAux(r2))
      case Alt(r1,r2) => unionN(nullableAux(r1),nullableAux(r2))
      case Intl(r1,r2) => isectN(nullableAux(r1), nullableAux(r2)) 
      case Rep(_,r) => nullableAux(r)   
      case Rng(n,m,r) => unionN((n==0,List()), nullableAux(r))
    }

  }
  
  

  def delta(r: Regex, x: Char) : Regex = {
    
    /* Simplification to avoid extra terms */
    def mkSeq(r1:Regex, r2:Regex): Regex = {
      r1 match {
        case Fail(m) => Fail(m)
        case Empty   => r2
        case _       => r2 match {
          case Fail(m) => Fail(m)
          case _       => Seq(r1,r2)
        }
      }
    }
    
    /* Simplification to avoid extra terms */
    def mkAlt(r1:Regex, r2:Regex): Regex = {
      r1 match {
        case Fail(m) => r2
        case _       => r2 match {
          case Fail(m) => r1
          case _       => Alt(r1,r2)
        }
      }
    }
    
    def mkRep(n:Int, r:Regex): Regex = {
      if (n == 0) mkStar(r)
      else {
        r match {
          case Fail(m) => Fail(m)
          case _ => if (nullable(r)) mkStar(r)
          			else Rep(n,r)
        }
      }
    }   
    
    def rmStar(r:Regex):Regex = {
      r match {
        case Alt(r1,r2) => mkAlt(rmStar(r1),rmStar(r2))
        case Star(re)   => rmStar(re)
        case Rep(1,re)  => rmStar(re)
        case _          => r
      }
    }
    
    def mkStar(r:Regex): Regex = {
      r match {
        case Fail(m) => Empty	      // fail* = ()
        case e@Empty => e             // ()*   = ()
        case e@Star(r) => e           // (r*)* = r*
        case Rep(1,re) => mkStar(re)  // (r+)* = r*
        case e@Rep(m,re) => if (m==1 || nullable(re)) mkStar(re)  // (r{i,})* == r* where i == 1 or nullable r
                            else Star(e)
        case e@Rng(_,_,re) => if (nullable(e)) mkStar(re) // r{i,j})* == r*   when i == 0 or nullable r
                              else Star(e)
        case e@Alt(_,_) => Star(rmStar(e)) // (a*|b)* == (a|b)*
        case _ => Star(r)
      }
    }

   def mkRng(m:Int, n:Int, r:Regex): Regex = {
      (m,n,r) match {
        case (0,0,_) => Empty
        case (1,1,re) => re
        case (_,_,e@Fail(_)) => e
        case (_,_,e@Empty) => e
        case (lb,ub,e) => 
          if (lb > ub) Fail("Illegal range " + lb + ".." + ub)
          else Rng(lb,ub,e)
      }
   }
    r match {
      case Fail(m) => Fail(m)
      case Empty => Fail("Unexpected char " + x)
      case Symbol(cs) => 
        if (cs.contains(x)) Empty
        else Fail("Unexpected " + x + ", expected one of " + cs) 
      case Dot => Empty

      case Star(Dot) => Star(Dot)
      
      case Star(r) => mkSeq(delta(r,x), Star(r))
      
      case Seq(r1,r2) => {
        val dr1 = mkSeq(delta(r1,x),r2)
        if (nullable(r1)) mkAlt(dr1,delta(r2,x))
        else dr1
      }
      
      case Alt(r1,r2) => 
        mkAlt(delta(r1,x),delta(r2,x))
        
      case Rep(n,r) => 
        mkSeq(delta(r,x),mkRep(n-1,r))

      case Rng(m,n,r) => 
        mkSeq(delta(r,x),mkRng(math.max(m-1,0),n-1,r))
        
    }
  }
  
  def deltaS(re: Regex, cs: String): Regex = cs.foldLeft(re)(delta)
  
  def matchRE(re: Regex, cs: String): Boolean = nullable (deltaS(re,cs))

}