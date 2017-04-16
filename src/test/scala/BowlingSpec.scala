
import org.scalatest.{FlatSpec, Matchers, WordSpecLike, _}

class BowlingSpec extends WordSpecLike with Matchers {

  "Calling BowlingScore" should {
    "Return 0 when player has not started" in {
      val emptyFrame=List[Frame]()
      Bowling.score(emptyFrame) shouldBe 0
    }

    "Return 0 when player makes scores 0 on the frame" in {
      val zeroFrame=List[Frame](Frame("0","0"))
        Bowling.score(zeroFrame) shouldBe 0
    }

    "Return 5 when on the first frame user rolls 4 and 1" in {
      val fiveFrame=List[Frame](Frame("1","4"))
      Bowling.score(fiveFrame) shouldBe 5
    }

    "Calculate a strike correctly" in {
      val strikeFrame=List[Frame](Frame("10","0"),Frame("5","2"))
      Bowling.score(strikeFrame) shouldBe 24
    }
    "Calculate 2 strikes in a row correctly" in {
      val strikeFrame=List[Frame](Frame("10","0"),Frame("10","0"),Frame("3","4"))
      Bowling.score(strikeFrame) shouldBe 47
    }

    "Calculate All strikes in a row correctly" in {
      val AllStrikesFrame=List[Frame](Frame("10","0"),Frame("10","0"),
        Frame("10","0"),Frame("10","0"),
        Frame("10","0"),Frame("10","0"),
        Frame("10","0"),Frame("10","0"),
        Frame("10","0"),Frame("10","0"),
        Frame("10","0"),Frame("10","0"))
      Bowling.score(AllStrikesFrame) shouldBe 300

    }
    "Calculate a spare correctly "in {
      val spareFrame=List[Frame](Frame("1","9"),Frame("2","6"))
      Bowling.score(spareFrame) shouldBe 20
    }

    "Calculate 2 spares in a row" in {
      val spareFrame=List[Frame](Frame("1","9"),Frame("5","5"),Frame("2","3"))
      Bowling.score(spareFrame) shouldBe 32
    }

    "Calculate all spares in a row except last" in {
      val spareFrame=List[Frame](Frame("5","5"),Frame("5","5"),
        Frame("5","5"),Frame("5","5"),
        Frame("5","5"),Frame("5","5"),
        Frame("5","5"),Frame("5","5"),
        Frame("5","5"),
        Frame("5","0"))
      Bowling.score(spareFrame) shouldBe 140
    }

    "Calculate All spares in a row" in {
      val spareFrame=List[Frame](
        Frame("5","5"),Frame("5","5"),
        Frame("5","5"),Frame("5","5"),
        Frame("5","5"),Frame("5","5"),
        Frame("5","5"),Frame("5","5"),
        Frame("5","5"),Frame("5","5"),
        Frame("5","0")
      )
        Bowling.score(spareFrame) shouldBe 150
    }

    "Return a 50 when user rolls Frames which add to 50 " in {
        val hundredFrame=List[Frame](Frame("1","4"),Frame("1","4"),Frame("1","4"),
                                    Frame("1","4"),Frame("1","4"),Frame("1","4"),
                                    Frame("1","4"),Frame("1","4"),Frame("1","4"),
                                    Frame("1","4"))
        Bowling.score(hundredFrame) shouldBe 50
    }

    "Return a 56 when user a spare and then standard Frames " in {
      val hundredFrame=List[Frame](Frame("9","1"),Frame("1","4"),Frame("1","4"),
        Frame("1","4"),Frame("1","4"),Frame("1","4"),
        Frame("1","4"),Frame("1","4"),Frame("1","4"),
        Frame("1","4"))
      Bowling.score(hundredFrame) shouldBe 56
    }

    "Return a 60 when user a has a strike and then standard Frames " in {
      val hundredFrame=List[Frame](Frame("10","0"),Frame("1","4"),Frame("1","4"),
        Frame("1","4"),Frame("1","4"),Frame("1","4"),
        Frame("1","4"),Frame("1","4"),Frame("1","4"),
        Frame("1","4"))
      Bowling.score(hundredFrame) shouldBe 60
    }

    "Return a 66 when user a has a strike and a spare and then standard Frames " in {
      val hundredFrame=List[Frame](Frame("10","0"),Frame("1","4"),Frame("1","4"),
        Frame("1","4"),Frame("5","5"),Frame("1","4"),
        Frame("1","4"),Frame("1","4"),Frame("1","4"),
        Frame("1","4"))
      Bowling.score(hundredFrame) shouldBe 66
    }

    "Return a 60 when user a has a 2 strikes and 2 spares and then standard Frames " in {
      val hundredFrame=List[Frame](Frame("10","0"),Frame("1","4"),Frame("1","4"),
        Frame("1","4"),Frame("5","5"),Frame("1","4"),
        Frame("1","4"),Frame("10","0"),Frame("9","1"),
        Frame("1","4"))
      Bowling.score(hundredFrame) shouldBe 87
    }
  }

}