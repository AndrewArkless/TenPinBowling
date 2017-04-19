
import org.scalatest.{FlatSpec, Matchers, WordSpecLike, _}

class BowlingSpec extends WordSpecLike with Matchers {

   "Calling BowlingScore"  should {
     //Basic tests
     "Return 0 when player has not started"  in {
      val emptyFrame=List[Frame]()
      Bowling.score(emptyFrame) shouldBe 0
    }

     "Return 0 when player makes scores 0 on the frame"  in {
      val zeroFrame=List[Frame](Frame( 0 , 0 ))
        Bowling.score(zeroFrame) shouldBe 0
    }

     "Return 5 when on the first frame user rolls 4 and 1"  in {
      val fiveFrame=List[Frame](Frame( 1 , 4 ))
      Bowling.score(fiveFrame) shouldBe 5
    }

     "Return a 50 when user rolls Frames which add to 50"   in {
       val testFrame=List[Frame](
         Frame( 1 , 4 ),Frame( 1 , 4 ),
         Frame( 1 , 4 ),Frame( 1 , 4 ),
         Frame( 1 , 4 ),Frame( 1 , 4 ),
         Frame( 1 , 4 ),Frame( 1 , 4 ),
         Frame( 1 , 4 ),Frame( 1 , 4 ))
       Bowling.score(testFrame) shouldBe 50
     }

     "Return a 0 when user rolls Frames which add to 0"   in {
       val testFrame=List[Frame](
         Frame( 0 , 0 ),Frame( 0 , 0 ),
         Frame( 0 , 0 ),Frame( 0 , 0 ),
         Frame( 0 , 0 ),Frame( 0 , 0 ),
         Frame( 0 , 0 ),Frame( 0 , 0 ),
         Frame( 0 , 0 ),Frame( 0 , 0 ))
       Bowling.score(testFrame) shouldBe 0
     }

     //Strikes
     "Calculate a strike correctly"  in {
      val strikeFrame=List[Frame](Frame( 10 , 0 ),Frame( 5 , 2 ))
      Bowling.score(strikeFrame) shouldBe 24
    }
     "Calculate 2 strikes in a row correctly"  in {
      val strikeFrame=List[Frame](Frame( 10 , 0 ),Frame( 10 , 0 ),Frame( 3 , 4 ))
      Bowling.score(strikeFrame) shouldBe 47
    }

     "Calculate All strikes in a row correctly"  in {
       val AllStrikesFrame=List[Frame](
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ))
       Bowling.score(AllStrikesFrame) shouldBe 300
     }

     "Calculate 9 strikes in a row correctly except last which is a open no bonus"  in {
       val AllStrikesFrame=List[Frame](
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10, 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 3,  4 )
       )
       Bowling.score(AllStrikesFrame) shouldBe 257
     }
     "Calculate 10 strikes in a row except bonus which is open correctly "  in {
       val AllStrikesFrame = List[Frame](
         Frame(10, 0), Frame(10, 0),
         Frame(10, 0), Frame(10, 0),
         Frame(10, 0), Frame(10, 0),
         Frame(10, 0), Frame(10, 0),
         Frame(10, 0), Frame(10, 0),
         Frame(2, 5))
     Bowling.score(AllStrikesFrame) shouldBe 279
   }

     "Calculate All strikes in a row except bonus which is a Spare correctly"  in {
       val AllStrikesFrame = List[Frame](
         Frame(10, 0), Frame(10, 0),
         Frame(10, 0), Frame(10, 0),
         Frame(10, 0), Frame(10, 0),
         Frame(10, 0), Frame(10, 0),
         Frame(10, 0), Frame(10, 0),
         Frame(9 ,1))
       Bowling.score(AllStrikesFrame) shouldBe 289
     }


     //Spares
     "Calculate a spare correctly"  in {
      val spareFrame=List[Frame](Frame( 1 , 9 ),Frame( 2 , 6 ))
      Bowling.score(spareFrame) shouldBe 20
    }

     "Calculate 2 spares in a row"  in {
      val spareFrame=List[Frame](
        Frame( 1 , 9 ),
        Frame( 5 , 5 ),
        Frame( 2 , 3 ))
      Bowling.score(spareFrame) shouldBe 32
    }

     "Calculate all spares in a row except last"  in {
      val spareFrame=List[Frame](
        Frame( 5 , 5 ),Frame( 5 , 5 ),
        Frame( 5 , 5 ),Frame( 5 , 5 ),
        Frame( 5 , 5 ),Frame( 5 , 5 ),
        Frame( 5 , 5 ),Frame( 5 , 5 ),
        Frame( 5 , 5 ),Frame( 5 , 0 ))
      Bowling.score(spareFrame) shouldBe 140
    }

     "Calculate All spares in a row"  in {
      val spareFrame=List[Frame](
        Frame( 5 , 5 ),Frame( 5 , 5 ),
        Frame( 5 , 5 ),Frame( 5 , 5 ),
        Frame( 5 , 5 ),Frame( 5 , 5 ),
        Frame( 5 , 5 ),Frame( 5 , 5),
        Frame( 5 , 5 ),Frame( 5 , 5),
        Frame( 9 , 0 )
      )
        Bowling.score(spareFrame) shouldBe 154
    }

     //Mixtures

     "Calculate a mixture of strikes and spares ending on a open in a row correctly"  in {
       val AllStrikesFrame=List[Frame](
         Frame( 10 , 0 ),Frame( 9 , 1 ),
         Frame( 10 , 0 ),Frame( 9 , 1 ),
         Frame( 10 , 0 ),Frame( 9, 1 ),
         Frame( 10 , 0 ),Frame( 9 , 1 ),
         Frame( 10 , 0 ),Frame( 9, 0 )
       )
       Bowling.score(AllStrikesFrame) shouldBe 188
     }

     "Calculate a random mixture of strikes and spares ending on a open in a row correctly"  in {
       val AllStrikesFrame=List[Frame](
         Frame( 10 , 0 ),Frame( 9 , 1 ),
         Frame( 10 , 0 ),Frame( 9 , 1 ),
         Frame( 10 , 0 ),Frame( 9, 1 ),
         Frame( 10 , 0 ),Frame( 9 , 1 ),
         Frame( 10 , 0 ),Frame( 10, 0 ),
         Frame( 2,  6)
       )
       Bowling.score(AllStrikesFrame) shouldBe 200
     }


     "Calculate 10 strikes in a row correctly 1 strike bonus and 1 Spare "  in {
       val AllStrikesFrame=List[Frame](
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 9 , 0 ))
       Bowling.score(AllStrikesFrame) shouldBe 299
     }

     "Calculate 8 strikes correctly an open with 1 strike bonus and 1 Spare "  in {
       val AllStrikesFrame=List[Frame](
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 1 , 1 ), Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 10 , 0 ),
         Frame( 10 , 0 ),Frame( 9 , 0 ))
       Bowling.score(AllStrikesFrame) shouldBe 244
     }

     "Return a 56 when user a spare and then standard Frames"   in {
      val testFrame=List[Frame](
        Frame( 9 , 1 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ),Frame( 1 , 4 ))
      Bowling.score(testFrame) shouldBe 56
    }

     "Return a 60 when user a has a strike and then standard Frames"   in {
      val testFrame=List[Frame](
        Frame( 10 , 0 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ), Frame( 1 , 4 ),
        Frame( 1 , 4 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ), Frame( 1 , 4 ))
      Bowling.score(testFrame) shouldBe 60
    }

     "Return a 66 when user a has a strike and a spare and then standard Frames"   in {
      val testFrame=List[Frame](
        Frame( 10 , 0 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ), Frame( 1 , 4 ),
        Frame( 5 , 5 ), Frame( 1 , 4 ),
        Frame( 1 , 4 ), Frame( 1 , 4 ),
        Frame( 1 , 4 ), Frame( 1 , 4 ))
      Bowling.score(testFrame) shouldBe 66
    }
     "Return a 60 when user a has a 2 strikes and 2 spares and then standard Frames"   in {
      val testFrame=List[Frame](
        Frame( 10 , 0 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ), Frame( 1 , 4 ),
        Frame( 5 , 5 ),Frame( 1 , 4 ),
        Frame( 1 , 4 ),Frame( 10 , 0 ),
        Frame( 9 , 1 ),Frame( 1 , 4 ))
      Bowling.score(testFrame) shouldBe 87
    }

     "Random test from original requirement calulate correctly" in {
       val testFrame=List[Frame](
         Frame( 1 , 4 ),Frame( 4 , 5 ),
         Frame( 6 , 4 ), Frame( 5 , 5 ),
         Frame( 10 , 0 ),Frame( 0 , 1 ),
         Frame( 7 , 3 ),Frame( 6 , 4 ),
         Frame( 10 , 0 ),Frame( 2 , 8),
         Frame(6,0))
       Bowling.score(testFrame) shouldBe 133
     }

     //Where spares and strikes cannot yet be evaluated

     "Where frame list is a spare which cannot be evaluated " in {
       val testFrame=List[Frame](
             Frame( 9 , 1 ))
        Bowling.score(testFrame) shouldBe 0
      }

     "Where frame list has an open frame and then a spare which cannot be evaluated " in {
         val testFrame=List[Frame](
           Frame( 1 , 1 ),
           Frame( 9 , 1 )
         )
         Bowling.score(testFrame) shouldBe 2
     }

     "Where frame list has an 10 frame and then a spare which cannot be evaluated " in {
       val testFrame = List[Frame](
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(9, 1)
       )
       Bowling.score(testFrame) shouldBe 18
     }

     "Where frame list is a strike which cannot be evaluated " in {
       val testFrame=List[Frame](
         Frame( 10 , 0 ))
       Bowling.score(testFrame) shouldBe 0
     }

     "Where frame list has an open frame and then a strike which cannot be evaluated " in {
       val testFrame=List[Frame](
         Frame( 1 , 1 ),
         Frame( 10 , 0 ),
         Frame( 10 , 0 )
       )
       Bowling.score(testFrame) shouldBe 2
     }

     "Where frame list has an 10 frame and then a strike which cannot be evaluated " in {
       val testFrame = List[Frame](
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(10, 0)
       )
       Bowling.score(testFrame) shouldBe 18
     }
     "Where frame list has an 10 frame and then a 2 strikes which cannot be evaluated " in {
       val testFrame = List[Frame](
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(1, 1),
         Frame(1, 1), Frame(10, 0),
         Frame(10,0)
       )
       Bowling.score(testFrame) shouldBe 18
     }

   }

}