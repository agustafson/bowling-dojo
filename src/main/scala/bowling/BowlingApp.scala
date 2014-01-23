package bowling

object BowlingApp extends App {

  def calculateScore(rolls: String): Int = {

    calculate(frames(insertNils(rolls))) - calculate(frames(insertNils(rolls)).slice(10, 12))
  }

  def insertNils(rolls: String) : String = {
     rolls.replaceAll("X", "X-")
  }

  def frames(rolls: String) : Seq[Frame] = rolls.sliding(2, 2).toList.map { s =>
    s.toList match {
      case 'X' :: '-' :: Nil => Frame(10, None)
      case '-' :: '-' :: Nil => Frame(0, None)
      case '-' :: '/' :: Nil => Frame(0, Some(10))
      case '-' :: second :: Nil => Frame(0, Some(Integer.parseInt(second.toString)))
      case first :: '-' :: Nil => Frame(Integer.parseInt(first.toString), None)
      case first :: '/' :: Nil => Frame(Integer.parseInt(first.toString), Some(10 - Integer.parseInt(first.toString)))
      case first :: second :: Nil => Frame(Integer.parseInt(first.toString), Some(Integer.parseInt(second.toString)))
      case first :: Nil => Frame(Integer.parseInt(first.toString), None)
    }
  }

  private def calculate(frames: Seq[Frame]): Int = {
    frames match {
      case Nil => 0
      case head :: Nil => score(head, Seq())
      case head :: tail => score(head, tail.take(2)) + calculate(tail)
    }
  }

  private def score(frame: Frame, nextFrames: Seq[Frame]): Int = {
    frame match {
      case x if x.isStrike => 10 + getScoreForNextTwoRolls(nextFrames)
      case Frame(x, None) => x
      case Frame(first, Some(second)) if first + second == 10 => 10 + nextFrames.head.first
      case Frame(first, Some(second)) => first + second
    }
  }

  private def getScoreForNextTwoRolls(nextFrames: Seq[Frame]): Int = {
    val firstFrame = nextFrames.headOption
    firstFrame.map { frame =>
      if (frame.isStrike) {
        10 + nextFrames.tail.headOption.map(_.first).getOrElse(0)
      }
      else frame.first + frame.second.getOrElse(0)
    }.getOrElse(0)
  }
}

case class Frame(first: Int, second: Option[Int]) {
  def isStrike = first == 10
}

