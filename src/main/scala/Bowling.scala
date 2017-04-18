
case class Frame(firstBowl: Int, secondBowl:Int)
object Bowling {
  val Strike=10
  def score(frame:List[Frame])={
    def helper(frame:List[Frame],score:Int,frameCount:Int):Int={

      frame match {
        case Nil=>score
        //Strikes + Bonus
        case Frame(Strike,_) :: Frame(Strike,_) :: xs if frameCount==10 => score+20+xs.head.firstBowl
        case Frame(Strike,_) :: xs  if frameCount==10 => score+10+xs.head.firstBowl + xs.head.secondBowl

        //Strikes
        case Frame(Strike,_) :: Frame(Strike,_) :: xs =>helper(frame.tail,score+20+xs.head.firstBowl,frameCount+1)
        case Frame(Strike,_) :: xs :: xss =>helper(frame.tail,score+10+xs.firstBowl+xs.secondBowl,frameCount+1)

        //Spares + bonus
        case x :: xs  if x.firstBowl+ x.secondBowl==10 && frameCount==10=>score+10+xs.head.firstBowl

        //Spares
        case x :: xs  if x.firstBowl+ x.secondBowl==10 =>helper(xs,score+10+xs.head.firstBowl,frameCount+1)

        //Opem
        case x :: xs => helper(xs,score+x.firstBowl+x.secondBowl, frameCount+1)

      }
    }

    helper(frame,0,1)
  }
}
