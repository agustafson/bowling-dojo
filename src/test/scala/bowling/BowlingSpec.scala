package bowling

import org.specs2.mutable._

class BowlingSpec extends Specification {

  val testCases = ("9-9-9-9-9-9-9-9-9-9-", 90) ::
                  ("XXXXXXXXXXXX", 300) ::
                  ("5/5/5/5/5/5/5/5/5/5/5", 150) ::
                  ("--------------------", 0) ::
                  ("23232323232323232323", 50) ::
                  ("232323232323X232323", 60) :: Nil


  "processing a line" ! {
    forall(testCases) {
      case (testCase, expected) =>
        ScoreTracker(testCase) must_== expected
    }
  }

}
