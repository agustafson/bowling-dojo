package bowling

object BowlingApp extends App {
  def process(line: String): Int = {
    val lineAndTails: List[(Char, List[Char])] = line.toList zip (line.toList.tails.toList collect { case Nil => Nil; case x => x.tail })
    val result = ((0, 0, 0, 0) /: lineAndTails){
      case ((total, frameTotal, frame, balls), (ball, tail)) =>
        if (frame < 10) {
          println((total, frame, balls, ball, tail))
          ball match {
            case 'X' if balls == 0 => (total + 10 + processStrike(tail take 2), 0, frame + 1, 0)
            case '/' if balls == 1 => (total + (10 - frameTotal) + processStrike(tail take 1), 0, frame + 1, 0)
            case '-' if balls == 0 => (total, frameTotal, frame, balls + 1)
            case '-' => (total, frameTotal, frame + 1, 0)
            case ballCh if balls == 0 => (total + ballCh.toString.toInt, frameTotal + ballCh.toString.toInt,frame, balls + 1)
            case ballCh => (total + ballCh.toString.toInt,frameTotal + ballCh.toString.toInt, frame + 1, 0)
          }
        } else {
          (total, frameTotal, frame, balls)
        }
    }
    result._1
  }

  def processStrike(rest: List[Char]): Int = (rest map toScore).sum

  def toScore(ball: Char): Int = ball match {
    case 'X' => 10
    case '-' => 0
    case ch if ch.isDigit => ch.toString.toInt
  }
}
