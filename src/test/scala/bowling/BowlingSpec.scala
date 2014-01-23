package bowling

import org.specs2.mutable._

class BowlingSpec extends Specification {

  "Bowling score calculator" should {
    "54545454545454545454 is simple addition" in {
      BowlingApp.calculateScore("54545454545454545454") === 90
    }
    "41414141414141414141 is simpler addition" in {
      BowlingApp.calculateScore("41414141414141414141") === 50
    }
   "9-9-9-9-9-9-9-9-9-9- misses are not counted" in {
      BowlingApp.calculateScore("9-9-9-9-9-9-9-9-9-9-") === 90
    }
    "5/5/5/5/5/5/5/5/5/5/5 is 150" in {
      BowlingApp.calculateScore("5/5/5/5/5/5/5/5/5/5/5") === 150
    }
    "XXXXXXXXXXXX is a winner" in {
      BowlingApp.calculateScore("XXXXXXXXXXXX") === 300
    }
    "--XXXXXXXXXXX  checking 2 gutter balls" in {
      BowlingApp.calculateScore("--XXXXXXXXXXX") === 270
    }
    "-/XXXXXXXXXXX  checking 2 gutter balls" in {
      BowlingApp.calculateScore("-/XXXXXXXXXXX") === 290
    }
    "XXXXXXX--XXXX  checking 2 gutter balls after a strike" in {
      BowlingApp.calculateScore("XXXXXXX--XXXX") === 240
    }
    "XXXXXXX-4-4XXX  checking 1 gutter balls after a strike" in {
      BowlingApp.calculateScore("XXXXXXX-4-4XXX") === 222
    }
    "XXXXXXX-4XXXX  checking 1 gutter balls after a strike" in {
      BowlingApp.calculateScore("XXXXXXX-4XXXX") === 248
    }
  }
}
