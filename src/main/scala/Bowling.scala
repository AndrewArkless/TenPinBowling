
case class Frame(firstBowl: Int, secondBowl:Int)
object Bowling {
  val Strike=10
  val TwoStrikes=20
  val Spare=10
  val IsLastNonBonusFrame=10

  def score(allFrames:List[Frame])={
    val noOfFrames=allFrames.length

    def helper(remainingFrames:List[Frame],score:Int):Int={

      val currentFrameIndex=noOfFrames-remainingFrames.length+1

      (remainingFrames,currentFrameIndex) match {
        case (Nil,_)=>score

        //Strikes where strike cannot be evaluated as no subsequent frames.
        case (Frame(Strike,_) :: Nil,_) => score
        case (Frame(Strike,_) :: Frame(Strike,_) ::Nil ,_)=>score

        //Strikes + BonusFrames
        case (Frame(Strike,_) :: Frame(Strike,_) :: Frame(firstBowl,_) :: xs,IsLastNonBonusFrame)=> score+TwoStrikes+firstBowl
        case (Frame(Strike,_) :: Frame(firstBowl,secondBowl) :: xs, IsLastNonBonusFrame) => score+Strike+firstBowl+secondBowl

        //Strikes
        case (Frame(Strike,_) :: Frame(Strike,_) :: xs,_) =>helper(remainingFrames.tail,score+TwoStrikes+xs.head.firstBowl)
        case (Frame(Strike,_) :: Frame(firstBowl,secondBowl) :: xs,_) =>helper(remainingFrames.tail,score+Strike+firstBowl+secondBowl)

        //Spares where no subsequent Frame
        case (Frame(firstBowl,secondBowl) :: Nil,_)if firstBowl+secondBowl==Spare =>score

        //Spares + bonusFrame
        case (Frame(firstBowl,secondBowl) :: Frame(bonus,_) :: xs,IsLastNonBonusFrame)  if firstBowl + secondBowl==Spare =>score+Spare+bonus

        //Spares
        case (Frame(firstBowl,secondBowl) :: Frame(bonus,_) :: xs,_) if firstBowl+ secondBowl==Spare =>helper(remainingFrames.tail,score+Spare+bonus)

        //Open
        case (Frame(firstBowl,secondBowl) :: xs,_) => helper(xs,score+firstBowl+secondBowl)

      }
    }

    helper(allFrames,0)
  }
}
