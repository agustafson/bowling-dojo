package bowling

trait Frame {
  def getFirstScore: Int
  def getScore: Int
}

object Strike extends Frame {
  def getFirstScore = 10
  def getScore = 10
}

case class Spare(first: Int) extends Frame {
  def getFirstScore = first
  def getScore= 10
}

case class NormalFrame(first: Int, second: Int) extends Frame {
  def getFirstScore = first
  def getScore = first + second
}

object BowlingApp extends App {
  println("Hello, bowling-dojo")

  def calculateScore(rolls: String): Int = {
    val frames = stringToFrames(rolls)

    def recur(agg: Int, remainingFrames: List[Frame]): Int = {
      remainingFrames match {
        case NormalFrame(a, b) :: t => recur(agg + a + b, t)

        case Strike :: Strike :: Strike :: Nil => agg + 10 + 10 + 10
        case Strike :: Strike :: a :: Nil => agg + 10 + 10 + a.getFirstScore
        case Strike :: a :: Nil => agg + 10 + a.getScore
        case Strike :: Strike :: nnf :: t => recur(agg + 10 + 10 + nnf.getFirstScore, Strike :: nnf :: t)
        case Strike :: nf :: t => recur(agg + 10 + nf.getScore, nf :: t)
        case Strike :: Nil => agg + 10

        case Spare(_) :: nf :: Nil => agg + 10 + nf.getFirstScore
        case Spare(_) :: nf :: t => recur(agg + 10 + nf.getFirstScore, nf :: t)
        //case Spare(_) :: Nil => agg + 10

        case Nil => agg
      }

    }

    recur(0, frames.toList)
  }

  def scoreToInt(a: Char) = if (a == '-') 0 else (a.toInt - '0'.toInt)

  def stringToFrames(rolls: String): Seq[Frame]  = {
    val turns = rolls.toCharArray.toList

    def pairToFrame(a: Char, b: Char) = {
      NormalFrame(scoreToInt(a), scoreToInt(b))
    }

    def recur(r: List[Char], agg: List[Frame] = Nil): List[Frame] = {
      r match {
        case 'X' :: t => recur(t, Strike :: agg)
        case a :: '/' :: t => recur(t, Spare(scoreToInt(a)) :: agg)
        case a :: b :: t => recur(t, pairToFrame(a, b) :: agg)
        case a :: Nil => recur(Nil, pairToFrame(a, '-') :: agg)
        case Nil => agg.reverse
      }
    }

    recur(turns)
  }
}
