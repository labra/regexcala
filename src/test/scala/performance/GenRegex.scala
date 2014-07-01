package es.weso.regex

import org.scalameter.api._

object GenRegex extends PerformanceTest.Quickbenchmark {

  def genAnds(n:Int): Regex = {
    def mkSeq(re: Regex, n:Int): Regex = Seq(re,Symbol(Set('a')))

    (0 to n).toList.foldLeft(Empty: Regex)(mkSeq)
  }

  val sizes: Gen[Int] = Gen.range("size")(0, 1000, 100)
  
  val pairs: Gen[(Regex,String)] = { 
    for { size <- sizes } yield 
     (genAnds(size), List.fill(size)('a').toString) 
  }
  
  performance of "Regex and " in {
  measure method "matcher" in {
    using(pairs) in {
      p => Matcher.matchRE(p._1,p._2)
    }
  }
 }

}