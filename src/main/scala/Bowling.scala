
case class Frame(firstBowl: Int, secondBowl:Int)
object Bowling {
  val Strike=10
  val Spare=10
  val IsLastNonBonusFrame=10

  def score(allFrames:List[Frame])={
    val noOfFrames=allFrames.length

    def helper(remainingFrames:List[Frame],score:Int):Int={

      val currentFrameIndex=noOfFrames-remainingFrames.length+1

      (remainingFrames,currentFrameIndex) match {
        case (Nil,_)=>score
        //Strikes + Bonus
        case (Frame(Strike,_) :: Frame(Strike,_) :: Frame(firstBowl,_) :: xs,IsLastNonBonusFrame)=> score+20+firstBowl
        case (Frame(Strike,_) :: Frame(firstBowl,secondBowl) :: xs, IsLastNonBonusFrame) => score+10+firstBowl+secondBowl

        //Strikes
        case (Frame(Strike,_) :: Frame(Strike,_) :: xs,_) =>helper(remainingFrames.tail,score+20+xs.head.firstBowl)
        case (Frame(Strike,_) :: Frame(firstBowl,secondBowl) :: xs,_) =>helper(remainingFrames.tail,score+10+firstBowl+secondBowl)

        //Spares + bonus
        case (Frame(firstBowl,secondBowl) :: Frame(bonus,_) :: xs,IsLastNonBonusFrame)  if firstBowl + secondBowl==Spare =>score+10+bonus

        //Spares
        case (Frame(firstBowl,secondBowl) :: Frame(bonus,_) :: xs,_) if firstBowl+ secondBowl==Spare =>helper(remainingFrames.tail,score+10+bonus)

        //Open
        case (Frame(firstBowl,secondBowl) :: xs,_) => helper(xs,score+firstBowl+secondBowl)

      }
    }
println()
    helper(allFrames,0)
  }
}
