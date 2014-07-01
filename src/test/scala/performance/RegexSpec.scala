package es.weso.regex

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers

class RegexSpec 
 extends FunSpec 
 with Matchers 
 with Checkers {

 describe("Regex ") {
   
  it("Should match a single symbol") {
    val re = Symbol(Set('a'))
    val cs = "a"
    Matcher.matchRE(re,cs) should be(true)
  }

  it("Should not match a single symbol") {
    val re = Symbol(Set('a'))
    val cs = "b"
    Matcher.matchRE(re,cs) should be(false)
  }

  it("Should match a repeated symbol with an a") {
    val re = Star(Symbol(Set('a')))
    val cs = "a"
    Matcher.matchRE(re,cs) should be(true)
  }

  it("Should match a repeated symbol with empty") {
    val re = Star(Symbol(Set('a')))
    val cs = ""
    Matcher.matchRE(re,cs) should be(true)
  }

  it("Should match a repeated symbol with 4 as") {
    val re = Star(Symbol(Set('a')))
    val cs = "aaaa"
    Matcher.matchRE(re,cs) should be(true)
  }
  
  it("Should match a repeated 2,5 symbol with 4 as") {
    val re = Rng(2,5,Symbol(Set('a')))
    val cs = "aaaa"
    Matcher.matchRE(re,cs) should be(true)
  }
 
  it("Should not match a repeated 2,5 symbol with one a") {
    val re = Rng(2,5,Symbol(Set('a')))
    val cs = "a"
    Matcher.matchRE(re,cs) should be(false)
  }

  it("Should not match a repeated 2,5 symbol with six as") {
    val re = Rng(2,5,Symbol(Set('a')))
    val cs = "aaaaaa"
    Matcher.matchRE(re,cs) should be(false)
  }
  
} 

}