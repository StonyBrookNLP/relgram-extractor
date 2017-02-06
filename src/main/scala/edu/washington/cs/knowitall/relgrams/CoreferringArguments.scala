package edu.washington.cs.knowitall.relgrams

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/19/13
 * Time: 11:17 AM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import edu.washington.cs.knowitall.tool.coref.Mention
import edu.washington.cs.knowitall.collection.immutable.Interval

object CoreferringArguments {

  val XVAR = "XVAR"
  def coreferringArgs(outer: TypedTuplesRecord, outerStartOffset:Int,
                      inner: TypedTuplesRecord, innerStartOffset:Int,
                      mentionsMap: Map[Mention, List[Mention]]): Option[(String, String, String, String)] = {


    def isPartOf(interval:Interval, list:List[Mention]) = list.exists(mention => interval.subset(mention.charInterval))
    def matchingMentions(interval:Interval, text:String) = (text.toLowerCase + ":" + interval.toString()::Nil ++ mentionsMap.values
                                                                      .filter(mentionList => isPartOf(interval, mentionList))
                                                                      .flatMap(mentionList => mentionList.map(m => m.text.toLowerCase + ":" + m.charInterval.toString))
                                                                      ).toSet

    val fa1Mentions = matchingMentions(outer.arg1HeadInterval.shift(outerStartOffset), outer.arg1Head)
    val fa2Mentions = matchingMentions(outer.arg2HeadInterval.shift(outerStartOffset), outer.arg2Head)

    val sa1Mentions = matchingMentions(inner.arg1HeadInterval.shift(innerStartOffset), inner.arg1Head)
    val sa2Mentions = matchingMentions(inner.arg2HeadInterval.shift(innerStartOffset), inner.arg2Head)

    if (fa1Mentions.exists(sa1Mentions)) return Some((XVAR, outer.arg2Head, XVAR, inner.arg2Head))
    if (fa1Mentions.exists(sa2Mentions)) return Some((XVAR, outer.arg2Head, inner.arg1Head, XVAR))
    if (fa2Mentions.exists(sa1Mentions)) return Some((outer.arg1Head, XVAR, XVAR, inner.arg2Head))
    if (fa2Mentions.exists(sa2Mentions)) return Some((outer.arg1Head, XVAR, inner.arg1Head, XVAR))
    None
  }

  val YVAR="YVAR"
  def coreferringArgs_multiple(outer: TypedTuplesRecord, outerStartOffset:Int,
                      inner: TypedTuplesRecord, innerStartOffset:Int,
                      mentionsMap: Map[Mention, List[Mention]]): Option[(String, String, String, String)] = {


    def isPartOf(interval:Interval, list:List[Mention]) = list.exists(mention => interval.subset(mention.charInterval))
    def matchingMentions(interval:Interval, text:String) = (text.toLowerCase + ":" + interval.toString()::Nil ++ mentionsMap.values
      .filter(mentionList => isPartOf(interval, mentionList))
      .flatMap(mentionList => mentionList.map(m => m.text.toLowerCase + ":" + m.charInterval.toString))
      ).toSet

    val fa1Mentions = matchingMentions(outer.arg1HeadInterval.shift(outerStartOffset), outer.arg1Head)
    val fa2Mentions = matchingMentions(outer.arg2HeadInterval.shift(outerStartOffset), outer.arg2Head)

    val sa1Mentions = matchingMentions(inner.arg1HeadInterval.shift(innerStartOffset), inner.arg1Head)
    val sa2Mentions = matchingMentions(inner.arg2HeadInterval.shift(innerStartOffset), inner.arg2Head)

    val fa1sa1 = fa1Mentions.exists(sa1Mentions)
    val fa2sa2 = fa2Mentions.exists(sa2Mentions)
    val fa1sa2 = fa1Mentions.exists(sa2Mentions)
    val fa2sa1 = fa2Mentions.exists(sa1Mentions)

    (fa1sa1, fa2sa2, fa1sa2, fa2sa1) match {
      //No equality
      case (false, false, false, false) => None
      //One equality
      //1=1
      case (true, false, false, false) => Some((XVAR, outer.arg2Head, XVAR, inner.arg2Head))
      //1=2
      case (false, false, true, false) => Some((XVAR, outer.arg2Head, inner.arg1Head, XVAR))
      //2=2
      case (false, true, false, false) => Some((outer.arg1Head, XVAR, inner.arg1Head, XVAR))
      //2=1
      case (false, false, false, true) => Some((outer.arg1Head, XVAR, XVAR, inner.arg2Head))


      //Two equalities
      //1=1,2=2
      case (true, true, false, false) => Some((XVAR, YVAR, XVAR, YVAR))
      //1=2,2=1
      case (false, false, true, true) => Some((XVAR, YVAR, YVAR, XVAR))

      //Three equalities
      //1=2,2=2
      case (false, true, true, false) => Some((XVAR, XVAR, inner.arg1Head, XVAR))
      //1=1,2=1
      case (true, false, false, true) => Some((XVAR, XVAR, XVAR, inner.arg2Head))
      //2=1,2=2
      case (false, true, false, true) => Some((outer.arg1Head, XVAR, XVAR, XVAR))
      //1=1,1=2
      case (true, false, true, false) => Some((XVAR, outer.arg2Head, XVAR, XVAR))

      //four equality
      case (true, true, true, true) => Some((XVAR, XVAR, XVAR, XVAR))

      case _ => Some((XVAR, XVAR, XVAR, XVAR))
        /**
      case (true, true, true, false) => {
        println("Cannot have three trues and one false.")
        Some((XVAR, XVAR, XVAR, XVAR))
      }
      case (true, true, false, true) => {
        println("Cannot have three trues and one false.")
        Some((XVAR, XVAR, XVAR, XVAR))
      }
      case (true, false, true, false) => {
        println("Cannot have three trues and one false.")
        Some((XVAR, XVAR, XVAR, XVAR))
      }
      case (false, true, true, false) => {
        println("Cannot have three trues and one false.")
        Some((XVAR, XVAR, XVAR, XVAR))
      }  */
    }

  }

}

