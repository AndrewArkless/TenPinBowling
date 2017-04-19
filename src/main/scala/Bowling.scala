
case class Frame(firstBowl: Int, secondBowl:Int)
object Bowling {
  val Strike=10
  val TwoStrikes=20
  val Spare=10
  val IsLastNonBonusFrame=10

  def score(allFrames:List[Frame])={
    val noOfFrames=allFrames.length

    def helper(remainingFrames:List[Frame],score:Int):Option[Int]={

      val currentFrameIndex=noOfFrames-remainingFrames.length+1

      (remainingFrames,currentFrameIndex) match {
        case (Nil,_)=>Some(score)

          //test for invalid scores in Frame
        case (Frame (firstBowl,secondBowl) :: xs,_) if firstBowl  < 0 || secondBowl <0 || ((firstBowl + secondBowl)>10) =>
          {
            //Throw some error
            None
          }

        //Strikes where strike cannot be evaluated as no subsequent frames.

        case (Frame(Strike,_) :: Nil,_) => Some(score)
        case (Frame(Strike,_) :: Frame(Strike,_) ::Nil ,_)=>Some(score)

        //Strikes + BonusFrames
        case (Frame(Strike,_) :: Frame(Strike,_) :: Frame(firstBowl,_) :: xs,IsLastNonBonusFrame)=> Some(score+TwoStrikes+firstBowl)
        case (Frame(Strike,_) :: Frame(firstBowl,secondBowl) :: xs, IsLastNonBonusFrame) => Some(score+Strike+firstBowl+secondBowl)

        //Strikes
        case (Frame(Strike,_) :: Frame(Strike,_) :: xs,_) =>helper(remainingFrames.tail,score+TwoStrikes+xs.head.firstBowl)
        case (Frame(Strike,_) :: Frame(firstBowl,secondBowl) :: xs,_) =>helper(remainingFrames.tail,score+Strike+firstBowl+secondBowl)

        //Spares where no subsequent Frame
        case (Frame(firstBowl,secondBowl) :: Nil,_)if firstBowl+secondBowl==Spare =>Some(score)

        //Spares + bonusFrame
        case (Frame(firstBowl,secondBowl) :: Frame(bonus,_) :: xs,IsLastNonBonusFrame)  if firstBowl + secondBowl==Spare =>Some(score+Spare+bonus)

        //Spares
        case (Frame(firstBowl,secondBowl) :: Frame(bonus,_) :: xs,_) if firstBowl+ secondBowl==Spare =>helper(remainingFrames.tail,score+Spare+bonus)

        //Open
        case (Frame(firstBowl,secondBowl) :: xs,_) => helper(xs,score+firstBowl+secondBowl)

      }
    }

    helper(allFrames,0)
  }
}
