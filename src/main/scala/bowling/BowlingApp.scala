package bowling

object BowlingApp extends App {
//  def getScore


}


case class BowlingScore(frames: List[Frame]) {
  val total = {
    ((List(Frame(0, Some(0)),
           Frame(0, Some(0)),
           Frame(0, Some(0))) ++ frames).sliding(3) map { group =>
      val thisFrame = group(2)
      val lastFrame = group(1)

      if (lastFrame.total == 10) {
        if(lastFrame.firstBall == 10) thisFrame.total * 2
        else thisFrame.firstBall * 2 + thisFrame.secondBall.getOrElse(0)
      } else {
        thisFrame.total
      }
    }).toList.sum
  }
  def addFrame(frame :Frame): BowlingScore = {
    BowlingScore(frames :+ frame)
  }
}

case class Frame(firstBall: Int, secondBall: Option[Int]){

  require(!(firstBall == 10 && secondBall.isDefined), "Strike in first roll but we've got a value in the second as well")
  require(firstBall + secondBall.getOrElse(0) <= 10, "Frame sum more than 10!!")

  val total = firstBall + secondBall.getOrElse(0)
}

object BowlingScore {
  val start = BowlingScore(List())
}

