package bowling

object BowlingApp extends App {

}

case class ScoreTracker(total: Int, frameTotal: Int, frame: Int, balls: Int)

object ScoreTracker {
  val empty = ScoreTracker(0, 0, 0, 0)

  def apply(line: String): Int =
    (ScoreTracker.empty /: line.toList.tails) {
      case (tracker, ball :: tail) if tracker.frame < 10 => tracker addBall (ball, tail)
      case (tracker, _)                                  => tracker
    }.total

  implicit class Ops(val tracker: ScoreTracker) extends AnyVal {
    import tracker._

    def addBall(ballCh: Char, rest: List[Char]) =
      ballCh match {
        case 'X' if balls == 0 => addScore(10 + processN(rest, 2)).nextFrame
        case '/' if balls == 1 => addScore(10 - frameTotal + processN(rest, 1)).nextFrame
        case _   if balls == 0 => addScore(toScore(ballCh))
        case _   if balls == 1 => addScore(toScore(ballCh)).nextFrame
      }

    def addScore(score: Int) = copy(frameTotal = frameTotal + score, balls = balls + 1)

    def nextFrame = copy(total = total + frameTotal, frameTotal = 0, frame = frame + 1, balls = 0)

    private def processN(rest: Seq[Char], n: Int): Int = (rest take n map toScore).sum

    private def toScore(ball: Char): Int = ball match {
      case 'X' | '/' => 10
      case '-' => 0
      case ch if ch.isDigit => ch.toString.toInt
    }

  }

}
