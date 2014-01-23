package bowling

import org.specs2.mutable._

class BowlingSpec extends Specification {

  val testCases = ("9-9-9-9-9-9-9-9-9-9-", 90) ::
                  ("XXXXXXXXXXXX", 300) ::
                  ("5/5/5/5/5/5/5/5/5/5/5", 150) :: Nil


  "processing a line" ! {
    forall(testCases) {
      case (testCase, expected) =>
        BowlingApp process testCase must_== expected
    }
  }

}
