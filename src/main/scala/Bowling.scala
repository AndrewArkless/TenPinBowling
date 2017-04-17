/**
  * Created by User on 14/04/2017.
  */
case class Frame(firstBowl: Int, secondBowl:Int)
object  Bowling {
  def score(frame:List[Frame])={

      def getFrame(frame:List[(Frame,Int)],index:Int)={
        frame.find(_._2==index)
      }

      def helper(frames:List[(Frame, Int)],score:Int):Int={
        if (frames==Nil) {
          score
        }
        else {
          frames.head match {
            //Strikes
            case ((frame,i)) if frame.firstBowl==10 =>
              {
                if(i<10) {
                  val nextBonusFrame = getFrame(frames, i + 1).get
                  val firstBonusRoll = nextBonusFrame._1.firstBowl
                  val secondBonusRoll = if (firstBonusRoll == 10) {
                    getFrame(frames, i + 2).get._1.firstBowl
                  } else {
                    nextBonusFrame._1.secondBowl
                  }
                  helper(frames.tail, score + frame.firstBowl + firstBonusRoll + secondBonusRoll)
                } else {
                  helper(frames.tail, score + frame.firstBowl)
                }
              }

            //Spares
            case ((frame, i)) if frame.firstBowl + frame.secondBowl == 10 => {
              if (i < 10) {
                val bonusPoints = getFrame(frames, i + 1).get
                helper(frames.tail, score + frame.firstBowl + frame.secondBowl + bonusPoints._1.firstBowl)
              } else {
                helper(frames.tail, score + frame.firstBowl + frame.secondBowl)
              }

            }
            //Open Frames
            case (frame, i) =>
              helper(frames.tail, score + frame.firstBowl + frame.secondBowl)
          }
        }
      }
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

//    val frameLength=frame.length
//    var c=0
//    def helper(frame:List[Frame],score:Int,previousFrame:Frame,carrySecondBowl:Boolean):Int={
//      c=c+1;
//
//      println("SCORE++++++++++++++++++++++++++++" + score + "  "+ c)
//      frame match {
//        case Nil=>score
//        case _ =>
//          val firstBowl=frame.head.firstBowl.toInt
//          val secondBowl=frame.head.secondBowl.toInt
//          val currentFrameScore=firstBowl+secondBowl
//          (previousFrame,carrySecondBowl) match {
//            case (Frame(10,_),_) if(firstBowl==10) => {
//              println("AAAAAAAAAAAAAAA")
//              println("first bowl " + firstBowl)
//              println("currentFrameScore " + currentFrameScore)
//              println(score + firstBowl + currentFrameScore)
//              if (frameLength>11 && frame.length==1){
//                println("HHHHHHHHHHHHHHHHHHHHHHHHh")
//                score
//              }
//              else {
//                if (carrySecondBowl == true) {
//                  helper(frame.tail,
//                    score + 20 + currentFrameScore,
//                    frame.head, true)
//                } else {
//                  helper(frame.tail,
//                    score + 10 + currentFrameScore,
//                    frame.head, true)
//                }
//              }
//            }
//            case (Frame(10,_),true)=> {
//              println("BBBBB")
//              if (frameLength>10 && frame.length==1){
//                println("GGGGGGGGGGGGGGGGGGGGGGGGGGGG")
//                helper(frame.tail, score + (firstBowl + secondBowl)  + firstBowl,
//                  frame.head, false)
//              }else {
//                helper(frame.tail, score + (firstBowl + secondBowl) + currentFrameScore + firstBowl,
//                  frame.head, false)
//              }
//            }
//            case (Frame(10,_),false)=> {
//              println("CCCCC")
//              helper(frame.tail, score + (firstBowl + secondBowl) + currentFrameScore,
//                                                frame.head,
//                                                 false)
//              }
//            case (Frame(f,s),_) if (f.toInt+s.toInt==10)=>
//              println("DDDDD" + score)
//              helper(frame.tail,
//                if (frameLength>10 && frame.length==1){
//                score+firstBowl} else score+firstBowl+currentFrameScore,
//                frame.head,
//                false
//              )
//            case _=>println("EEEEE");helper(frame.tail,currentFrameScore+score,frame.head,false)
//          }
//      }
//    }
//    helper(frame,0,Frame(0,0),false)
    val indexedFrames: List[(Frame, Int)] =frame.zip(1 to frame.length)
    helper(indexedFrames,0)
  }
}
