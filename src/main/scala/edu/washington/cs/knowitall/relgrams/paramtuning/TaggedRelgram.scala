package edu.washington.cs.knowitall.relgrams.paramtuning

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/8/13
 * Time: 3:55 PM
 * To change this template use File | Settings | File Templates.
 */

import edu.washington.cs.knowitall.relgrams.utils.{MapUtils, StringUtils, CollectionUtils}
import edu.washington.cs.knowitall.relgrams.{AffinityMeasures, Measures, RelationTuple}
import scala.Some
import edu.washington.cs.knowitall.relgrams.solr.SolrSearchWrapper
import io.Source
import edu.washington.cs.knowitall.relgrams.utils.RegexUtils._
import scala.Some
import scala.Tuple2
import edu.washington.cs.knowitall.relgrams.paramtuning.TaggedRelgramWithCounts._
import scala.Some
import scala.Tuple2


object TaggedRelgram{

  // 0  rowNum
  // 1  First Arg1      First Arg1 Instances    First Rel        First Arg2      First Arg2 Instances
  // 6  bad T1  bad T2  correct relgram
  // 9  Second Arg1     Second Arg1 Instances   Second Rel      Second Arg2     Second Arg2 Instances
  // 14 #FS+SF  #FS     #F      #S      Combined P(S|F)

  def fromTaggedFileString(string:String) = {
    import StringUtils._
    val splits = string.split("\t")
    require(splits.size >= 12, "Expected number of tab separated fields >= %d. Actual=%d".format(12, splits.size))
    import RelationTuple._
    //val id = toIntOrElse(splits(0), 0)
    val first = fromArg1RelArg2(splits(0), splits(2), splits(3))
    val firstArg1s = splits(3)
    val firstArg2s = splits(4)
    val firstLabel = toIntOrElse(splits(5), 0)
    val secondLabel = toIntOrElse(splits(6), 0)
    val relgramLabel = toIntOrElse(splits(7), 0)

    val second = fromArg1RelArg2(splits(8), splits(10), splits(11))
    val secondArg1s = splits(9)
    val secondArg2s = splits(12)

    val biterm = splits(13).toInt
    val bigram = splits(14).toInt
    val firstUnigram = splits(15).toInt
    val secondUnigram = splits(16).toInt
    val defaultConditional = toDoubleOrElse(splits(17), 0.0)

    Some(new TaggedRelgram(first, second, firstLabel, secondLabel, relgramLabel))
                               //new Frequencies(biterm, bigram, firstUnigram, secondUnigram, defaultConditional)))
  }


  def deserialize(string:String) = {
    import StringUtils._
    val splits = string.split("\t")
    val expected = 9
    require(splits.size >= expected, "Expected number of tab separated fields >= %d. Actual=%d".format(expected, splits.size))
    var i = 0
    def nextString = {
      val out = if (i < splits.size) splits(i) else ""
      i = i + 1
      out
    }
    import RelationTuple._
    val first = fromArg1RelArg2(nextString, nextString, nextString)
    val second = fromArg1RelArg2(nextString, nextString, nextString)
    val firstLabel = toIntOrElse(nextString, 0)
    val secondLabel = toIntOrElse(nextString, 0)
    val relgramLabel = toIntOrElse(nextString, 0)
    Some(new TaggedRelgram(first, second, firstLabel, secondLabel, relgramLabel))
  }
}

object Label {

}
case class Label(value:Int){

  def ==(that:Label): Boolean = that.value == this.value

}


case class Frequencies(biterm:Int, bigram:Int, firstUnigram:Int, secondUnigram:Int, defaultMeasure:Double){

}

case class TaggedRelgram (first:RelationTuple, //firstArg1s:String, firstArg2s:String,
                          second:RelationTuple,// secondArg1s:String, secondArg2s:String,
                          isFirstBad:Int, isSecondBad:Int, relgramLabel:Int) {
  def isGoodRelgram: Boolean = relgramLabel match {
    case 1 => true
    case 0 => false
  }

  def isFirstTupleBad = isBad(isFirstBad)
  def isSecondTupleBad = isBad(isFirstBad)

  def isBad(value:Int) = value match {
    case 1 => true
    case 0 => false
  }
  def hasSomeBadTuple() = isFirstTupleBad || isSecondTupleBad

  def serialize:String = "%s\t%s\t%d\t%d\t%d".format(first.tabSeparatedTuple, second.tabSeparatedTuple, isFirstBad, isSecondBad, relgramLabel)
}

object ScoredTaggedRelgramWithCounts{

  def fromFile(file: String): List[ScoredTaggedRelgramWithCounts] = {
    var scoredRelgrams = List[ScoredTaggedRelgramWithCounts]()
    Source.fromFile(file)
      .getLines()
      .foreach(line => {
      fromString(line) match {
        case Some(strgc:ScoredTaggedRelgramWithCounts) => scoredRelgrams :+= strgc
        case None =>
      }
    })
    scoredRelgrams
  }

  def fromString(line: String) = {
    val (scoreStr, trgcStr) = firstRestSplit(line)
    val score = scoreStr.toDouble
    fromSerializedString(trgcStr) match {
      case Some(trgc: TaggedRelgramWithCounts) => {
        Some(new ScoredTaggedRelgramWithCounts(score, trgc))
      }
      case _ => {
        println("Failed to serialize from string: " + trgcStr)
        None
      }
    }
  }
}

case class ScoredTaggedRelgramWithCounts(score:Double, trgc:TaggedRelgramWithCounts){}

object TaggedRelgramWithCounts{

  val sep = "_TRGC_SEP_"
  def isValidRelgram(first:RelationTuple, second:RelationTuple, maff:(Measures, AffinityMeasures)) = TopRelgramsSampler.isValidRelgram(first, second, maff)
  def fromTaggedRelgram(solr:SolrSearchWrapper, filterReportingVerbs:Boolean)(tr:TaggedRelgram) = {
    solr.search(tr.first, tr.second, filterReportingVerbs)
      .filter(maff => isValidRelgram(tr.first, tr.second, maff))
      .headOption match {
      case Some(maff:(Measures, AffinityMeasures)) => {
        Some(new TaggedRelgramWithCounts(tr, maff._1.urgc.bitermCounts.toMap, maff._1.urgc.rgc.counts.toMap, maff._1.firstCounts, maff._1.secondCounts))
      }
      case _ => {
        None
      }
    }
  }
  def fromSerializedString(string:String) = {
    val splits = string.split(sep)
    if (splits.size == 5){
      TaggedRelgram.deserialize(splits(0)) match {
        case Some(tr:TaggedRelgram) => {
          val bitermCounts = MapUtils.IntIntMapfromCountsString(splits(1)).toMap
          val bigramCounts = if(splits(2).size > 0) MapUtils.IntIntMapfromCountsString(splits(2)).toMap else Map[Int, Int]()
          val firstCounts = splits(3).toInt
          val secondCounts = splits(4).toInt
          Some(new TaggedRelgramWithCounts(tr, bitermCounts, bigramCounts, firstCounts, secondCounts))
        }
        case _ => {
          "Failed to extract TaggedRelgram from string: " + splits(0)
          None
        }
      }
    } else {
      println("Serialized string does not have 3 splits. Actual: " + splits.size)
      println("Failed string: " + string)
      None
    }
  }
  def main(args:Array[String]) {
    val first = RelationTuple.fromArg1RelArg2("farg1", "frel", "farg2")
    val second = RelationTuple.fromArg1RelArg2("sarg1", "srel", "sarg2")
    val bitermCounts = (0 until 50).map(i => i -> i * 10).toMap
    val bigramCounts = (0 until 50).map(i => i -> i * 5).toMap
    val isFirstBad = 1
    val isSecondBad = 2
    val relgramLabel = 3
    val firstCounts = 10
    val secondCounts = 10
    val trgc = new TaggedRelgramWithCounts(new TaggedRelgram(first, second, isFirstBad,
                                                            isSecondBad, relgramLabel),
                                                            bitermCounts, bigramCounts,
                                                            firstCounts, secondCounts)
    val string = trgc.serialize
    TaggedRelgramWithCounts.fromSerializedString(string) match {
      case Some(x:TaggedRelgramWithCounts) => println("Success.")
      case None => "Failed."
    }
  }
}

case class TaggedRelgramWithCounts(taggedRelgram:TaggedRelgram, bitermCounts:Map[Int, Int], bigramCounts:Map[Int, Int], firstCounts:Int, secondCounts:Int){

  import CollectionUtils._
  def toPrettyString(): String = taggedRelgram.first.prettyString + "\t" + taggedRelgram.second.prettyString + "\t" +
                                 maxOrElse(bitermCounts.values, 0) + "\t" + maxOrElse(bigramCounts.values, 0) + "\t" +
                                 firstCounts + "\t" + secondCounts


  import TaggedRelgramWithCounts._
  def serialize:String = taggedRelgram.serialize + sep + MapUtils.toIntIntCountsString(bitermCounts) + sep + MapUtils.toIntIntCountsString(bigramCounts) + sep + firstCounts + sep + secondCounts
}
