package bowling

object BowlingApp extends App {
  def calculate(s: String): Int = {

    def recur(l: List[Char], total: Int = 0, previous: Int = 0): Int = {
      l match {
        case h :: tail if h == '-' => recur(tail, total, h)
        case h :: h2 :: tail if h == '/' => recur(tail, (total - previous) + 10 + tail.head, h)
        case h :: tail if h == 'X' => recur(tail, total + 10, h)
        case h :: tail => recur(tail, total + h.toString.toInt, h)
        case Nil => total
      }
    }

    recur(s.toCharArray.toList)

  }

  // spare
  //  look back 1
  //  subtract value of previous, then add 10, and the bonus

  // strike
    // count value of next 2

}
