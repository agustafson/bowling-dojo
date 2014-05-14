package bowling

import org.specs2.mutable._

class CalculatorSpec extends Specification {
  val lawl = """.   .-. .  !
               ||   | | |  !
               |.   . . .  !
               ||   | | |  !
               |.   .-. .  !""".stripMargin.replace("!", "")


  val boobs = ".-. .-. .-. .-. .-.\n| | | | | | | | |  \n.-. . . . . .-. .-.\n| | | | | | | |   |\n._. .-. .-. ._. ._."

  "individual character" ! {
    Calculator format 0 must_== digits('0').lines.mkString("\n")
    Calculator format 0 must_!= digits('1').lines.mkString("\n")
    Calculator format 1 must_== List(".  ", "|  ", ".  ", "|  ", ".  ").mkString("\n")
  }

  "Multiple characters" ! {
    Calculator format 80085 must_== boobs
    Calculator format 101 must_== lawl
  }
}
