package bowling

import BowlingApp._

import org.specs2.mutable._

class BowlingSpec extends Specification {
  "we should correctly calculate a lot of nines" in {
    calculateScore("9-9-9-9-9-9-9-9-9-9-") mustEqual 90
  }


  "we should correctly calculate a strike" in {
    calculateScore("XXXXXXXXXXXX") mustEqual 300
  }

  "we should correctly calculate lots of spares" in {
    calculateScore("5/5/5/5/5/5/5/5/5/5/5") mustEqual 150
  }

  "we should correctly calculate real score" in {
    calculateScore("X5-1/5-1/22----") mustEqual 56
  }


  "stringToFrame" should {
    "Return a list of frames" in {
      stringToFrames("X5-2/12") mustEqual List(Strike, NormalFrame(5,0), Spare(2), NormalFrame(1,2))
    }
  }
}
