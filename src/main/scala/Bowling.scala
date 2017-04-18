
case class Frame(firstBowl: Int, secondBowl:Int)
object Bowling {
  val Strike=10
  val Spare=10
  val lastNonBonusFrame=10

  def score(allFrames:List[Frame])={
    val noOfFrames=allFrames.length

    def helper(remainingFrames:List[Frame],score:Int):Int={

      val currentFrameIndex=noOfFrames-remainingFrames.length+1

      remainingFrames match {
        case Nil=>score
        //Strikes + Bonus
        case Frame(Strike,_) :: Frame(Strike,_) :: Frame(firstBowl,_) :: xs if currentFrameIndex==lastNonBonusFrame => score+20+firstBowl
        case Frame(Strike,_) :: Frame(firstBowl,secondBowl) :: xs  if currentFrameIndex==lastNonBonusFrame => score+10+firstBowl+secondBowl

        //Strikes
        case Frame(Strike,_) :: Frame(Strike,_) :: xs =>helper(remainingFrames.tail,score+20+xs.head.firstBowl)
        case Frame(Strike,_) :: Frame(firstBowl,secondBowl) :: xs =>helper(remainingFrames.tail,score+10+firstBowl+secondBowl)

        //Spares + bonus
        case Frame(firstBowl,secondBowl) :: Frame(bonus,_) :: xs  if firstBowl + secondBowl==Spare && currentFrameIndex==lastNonBonusFrame=>score+10+bonus

        //Spares
        case Frame(firstBowl,secondBowl) :: Frame(bonus,_) :: xs if firstBowl+ secondBowl==Spare =>helper(remainingFrames.tail,score+10+bonus)

        //Open
        case Frame(firstBowl,secondBowl) :: xs => helper(xs,score+firstBowl+secondBowl)

      }
    }

    helper(allFrames,0)
  }
}
