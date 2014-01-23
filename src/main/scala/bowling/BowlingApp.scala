package bowling

object BowlingApp extends App {
  def process(line: Seq[Char]): Int = {
    val lines = line.toList.tails.toList
    val result = ((0, 0, 0, 0) /: lines){
      case ((total, frameTotal, frame, balls), ball :: tail) if frame < 10 =>
        val ballScore = toScore(ball)
        ball match {
          case 'X'    if balls == 0 => (total + 10 + processN(tail, 2), 0, frame + 1, 0)
          case '/'    if balls == 1 => (total + (10 - frameTotal) + processN(tail, 1), 0, frame + 1, 0)
          case ballCh if balls == 0 => (total + ballScore, frameTotal + ballScore, frame, balls + 1)
          case ballCh if balls == 1 => (total + ballScore, frameTotal + ballScore, frame + 1, 0)
        }
      case (skipped, _) => skipped
    }
    result._1
  }

  def processN(rest: Seq[Char], n: Int): Int = (rest take n map toScore).sum

  def toScore(ball: Char): Int = ball match {
    case 'X' | '/' => 10
    case '-' => 0
    case ch if ch.isDigit => ch.toString.toInt
  }
}
