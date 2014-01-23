package bowling

import org.specs2.mutable._

class BowlingSpec extends Specification {
  "A bowling session" should {
    "start score of 0" in {
      val score = BowlingScore.start

      score.frames should beEmpty
      score.total should_== 0
    }


    "have a score of 5 after one frame if 5 is scored in the first frame" in {
      val score = BowlingScore.start.addFrame(Frame(5, Some(0)))
      score.total should_== 5
    }

    "have a score of 10 after one frame and two rolls of 3 and 7" in {
      val score = BowlingScore.start.addFrame(Frame(3, Some(7)))
      score.total should_== 10
    }

    "have a score of 10 after two frames with scores of 3 and 7" in {
      val score = BowlingScore.start.addFrame(Frame(3, Some(0)))
                                    .addFrame(Frame(2, Some(5)))
      score.total should_== 10
    }

    "ensure that there was no second ball bowled after a strike" in {
      Frame(10, Some(1)) should throwAn[IllegalArgumentException]
    }

    "ensure that the total for a frame doesn't exceed 10" in {
      Frame(5, Some(6)) should throwAn[IllegalArgumentException]
    }

    "after a spare the value of the successive frame should be its double plus 10" in {
      val score = BowlingScore.start.addFrame(Frame(3, Some(7)))
                                    .addFrame(Frame(4, Some(0)))
      score.total should_== 18
    }

    "after a spare the value of the successive frame should be its double (of only the first ball) plus 10" in {
      val score = BowlingScore.start.addFrame(Frame(3, Some(7)))
                                    .addFrame(Frame(4, Some(2)))
      score.total should_== 20
    }

    "after a strike the value of the successive frame should be double it's total" in {
      val score = BowlingScore.start.addFrame(Frame(10, None))
                                    .addFrame(Frame(4, Some(2)))

      score.total should_== 22
    }

    "after 3 strikes, the total should be 50" in {
      val score = BowlingScore.start.addFrame(Frame(10, None))
                                    .addFrame(Frame(10, None))
                                    .addFrame(Frame(10, None))

      score.total should_==50
    }

  }
}
