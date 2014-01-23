package bowling

import org.specs2.mutable._
import BowlingApp.calculate

class BowlingSpec extends Specification {
  "stuff" should {
    "things" in {
      calculate("--------------------") must be_==(0)
      calculate("1-1-1-1-1-1-1-1-1-1-") must be_==(10)
      calculate("2-2-2-2-2-2-2-2-2-2-") must be_==(20)

    }
    "handle spares" in {
      calculate("------------------5/-") must be_==(10)
    }

    "handle strikes" in {
      calculate("XXXXXXXXXXXX") must be_==(300)
    }
  }
}
