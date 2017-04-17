/**
  * Created by User on 14/04/2017.
  */
case class Frame(firstBowl: Int, secondBowl:Int)
object Bowling {


  def score(frame:List[Frame])={

//    def helper(frame:List[Frame],score:Int,previousFrame:Frame):Int={
//      println("SCORE++++++++++++++++++++++++++++" + score)
//      frame match {
//        case Nil=>score
//        case _ =>
//          val firstBowl=frame.head.firstBowl.toInt
//          val secondBowl=frame.head.secondBowl.toInt
//          val currentFrameScore=if (firstBowl+secondBowl==10) 0 else firstBowl+secondBowl
//          previousFrame match {
//
//            case Frame("10",_)=>helper(frame.tail,
//
//              score+(firstBowl+10+secondBowl)+currentFrameScore,
//                frame.head)
//
//            case Frame(f,s) if (f.toInt+s.toInt==10)=>
//                helper(frame.tail,
//                score+firstBowl+10+currentFrameScore,
//                frame.head
//              )
//            case _=>println("THERE" +score);helper(frame.tail,currentFrameScore+score,frame.head)
//          }
//      }

    val frameLength=frame.length
    var c=0
    def helper(frame:List[Frame],score:Int,previousFrame:Frame,carrySecondBowl:Boolean):Int={
      c=c+1;

      println("SCORE++++++++++++++++++++++++++++" + score + "  "+ c)
      frame match {
        case Nil=>score
        case _ =>
          val firstBowl=frame.head.firstBowl.toInt
          val secondBowl=frame.head.secondBowl.toInt
          val currentFrameScore=firstBowl+secondBowl
          (previousFrame,carrySecondBowl) match {
            case (Frame(10,_),_) if(firstBowl==10) =>{
              println("AAAAAAAAAAAAAAA")
              println("first bowl " +firstBowl)
              println("currentFrameScore "+ currentFrameScore)
              println(                score+firstBowl+currentFrameScore)
              helper(frame.tail,
                                                      score + 10 + currentFrameScore,
                                                      frame.head, true)
                                                    }
            case (Frame(10,_),true)=> {
              println("BBBBB")
              helper(frame.tail, score + (firstBowl + secondBowl) + currentFrameScore + firstBowl,
                frame.head, false)
            }
            case (Frame(10,_),false)=> {
              println("CCCCC")
              helper(frame.tail, score + (firstBowl + secondBowl) + currentFrameScore,
                                                frame.head,
                                                 false)
              }
            case (Frame(f,s),_) if (f.toInt+s.toInt==10)=>
              println("DDDDD" + score)


              helper(frame.tail,
                if (frameLength>10 && frame.length==1){
                score+firstBowl} else score+firstBowl+currentFrameScore,
                frame.head,
                false
              )
            case _=>println("EEEEE");helper(frame.tail,currentFrameScore+score,frame.head,false)
          }
      }
    }
    helper(frame,0,Frame(0,0),false)
  }
}
