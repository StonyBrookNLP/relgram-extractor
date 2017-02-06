package edu.washington.cs.knowitall.relgrams

import collection.{mutable, Map, Set}
import com.nicta.scoobi.core.WireFormat
import java.io.{DataInput, DataOutput}
import utils.{Pairable, MapUtils}
import util.matching.Regex
import util.matching.Regex.Match

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/18/13
 * Time: 9:10 PM
 * To change this template use File | Settings | File Templates.
 */


object RelationTupleCounts{
  val sep = "\t"
  val lastTabRe = """(.*)\t([0-9]+)""".r
  def fromSerializedString(string:String):Option[RelationTupleCounts] = {
    lastTabRe.findFirstMatchIn(string) match {
      case Some(m:Regex.Match) => {
        val tupleString = m.group(1)
        val count = m.group(2).toInt
        RelationTuple.fromSerializedString(tupleString) match {
          case Some(tuple:RelationTuple) => {
            Some(new RelationTupleCounts(tuple, count))
          }
          case None => None
        }
      }
      case None => None
    }
  }

  val dummyTupleCount = new RelationTupleCounts(RelationTuple.dummyTuple, 0)
  implicit def RelationTupleCountsFmt = new WireFormat[RelationTupleCounts]{
    def toWire(x: RelationTupleCounts, out: DataOutput) {out.writeUTF(x.toString)}
    def fromWire(in: DataInput): RelationTupleCounts = RelationTupleCounts.fromSerializedString(in.readUTF()).getOrElse(dummyTupleCount)//.getOrElse(RelgramCounts.DummyRelgramCounts)
  }
}
case class RelationTupleCounts(tuple:RelationTuple, var count:Int){
  override def toString():String = "%s\t%d".format(tuple.serialize, count)
}

object RelationTuple{
  val sep = "\t"
  val SENT_SEP="_SENT_SEP_"
  val ID_SEP = ","

  def toIntsIfPossible(string:String) = string.split(ID_SEP).map(x => try{x.toInt}catch{case e:Exception => -1}).toSet
  def fromSerializedString(string:String, allowIncomplete:Boolean = false):Option[RelationTuple] = {
    val splits = string.split("\t")
    if (allowIncomplete == true || splits.size > 5){
      val iterator = splits.iterator
      val arg1 = iterator.next
      val rel = iterator.next
      val arg2 = iterator.next
      var hashes = if(iterator.hasNext) toIntsIfPossible(iterator.next) else Set[Int]()//_//.split(ID_SEP).map(x => x.toInt).toSet else Set[Int]()
      var sentences = if(iterator.hasNext) iterator.next.split(SENT_SEP).toSet else Set[String]()
      var ids = if(iterator.hasNext) iterator.next.split(ID_SEP).toSet else Set[String]()
      var arg1HeadCounts = new mutable.HashMap[String, Int]()
      if (iterator.hasNext) arg1HeadCounts ++= MapUtils.StringIntMapfromSafeCountsString(iterator.next)
      var arg2HeadCounts = new mutable.HashMap[String, Int]()
      if (iterator.hasNext) arg2HeadCounts ++= MapUtils.StringIntMapfromSafeCountsString(iterator.next)

      Some(new RelationTuple(arg1, rel, arg2, hashes, sentences, ids, arg1HeadCounts, arg2HeadCounts))
    }else{
      println("Splits size for relation tuple: " + splits.size + " = " + splits.mkString("_RT_"))
      None
    }
  }

  val dummyTuple = new RelationTuple("NA", "NA", "NA", Set(0), Set("NA"), Set("NA"),
                                    new scala.collection.mutable.HashMap[String,Int](),
                                    new scala.collection.mutable.HashMap[String,Int]())


  def cleanRelString(rel:String): String = rel.replaceAll("""^be """, "")

  def setSubsumption(awords: Array[String], bwords: Array[String]): Boolean = {
    awords.filter(a => !a.equals("be")).toSet.subsetOf(bwords.filter(b => !b.equals("be")).toSet)
  }


  implicit def RelationTupleFmt = new WireFormat[RelationTuple]{
    def toWire(x: RelationTuple, out: DataOutput) {out.writeUTF(x.serialize)}
    def fromWire(in: DataInput): RelationTuple = RelationTuple.fromSerializedString(in.readUTF()).getOrElse(dummyTuple)
  }

  def fromArg1RelArg2(arg1:String, rel:String, arg2:String) = new RelationTuple(arg1, rel, arg2, Set[Int](), Set[String](),Set[String](),new mutable.HashMap[String, Int](), new mutable.HashMap[String, Int]())

  def from(arg1:String, rel:String, arg2:String, from:RelationTuple) = new RelationTuple(arg1, rel, arg2, from.hashes, from.sentences, from.ids, from.arg1HeadCounts, from.arg2HeadCounts)

}

case class RelationTuple(arg1:String, var rel:String, arg2:String,
                         var hashes:Set[Int],
                         var sentences:Set[String],
                         var ids:Set[String],
                         var arg1HeadCounts:scala.collection.mutable.Map[String,Int],
                         var arg2HeadCounts:scala.collection.mutable.Map[String,Int]){
  def isIdenticalTo(that: RelationTuple) ={
    arg1.trim.toLowerCase.equals(that.arg1.trim.toLowerCase) &&
    rel.trim.toLowerCase.equals(that.rel.trim.toLowerCase) &&
    arg2.trim.toLowerCase.equals(that.arg2.trim.toLowerCase)
  }

  import RelationTuple._
  def serializeLean:String = toString
  def serialize:String = toString
  private def countsString(counts:Map[String, Int]):String = MapUtils.toSafeCountsString(counts)//, COUNTS_SEP)
  def tabSeparatedTuple:String = arg1 + "\t" + rel + "\t" + arg2
  def prettyString:String = arg1 + "\t" + rel + "\t" + arg2

  def toLeanString:String = {
    arg1 + "\t" +
      rel + "\t" +
      arg2 + "\t" +
      hashes.take(10).mkString(ID_SEP) + "\t" +
      sentences.take(1).mkString(SENT_SEP) + "\t" +
      ids.take(10).mkString(ID_SEP) + "\t" +
      countsString(arg1HeadCounts.toMap) + "\t" +
      countsString(arg2HeadCounts.toMap)

    /*"%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s".format(arg1, rel, arg2,
    hashes.mkString(ID_SEP),
    sentences.mkString(SENT_SEP),
    ids.mkString(ID_SEP),
    countsString(arg1HeadCounts.toMap),
    countsString(arg2HeadCounts.toMap))*/
  }

  override def toString:String = {
    arg1 + "\t" +
      rel + "\t" +
      arg2 + "\t" +
      hashes.mkString(ID_SEP) + "\t" +
      sentences.mkString(SENT_SEP) + "\t" +
      ids.mkString(ID_SEP) + "\t" +
      countsString(arg1HeadCounts.toMap) + "\t" +
      countsString(arg2HeadCounts.toMap)

    /*"%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s".format(arg1, rel, arg2,
    hashes.mkString(ID_SEP),
    sentences.mkString(SENT_SEP),
    ids.mkString(ID_SEP),
    countsString(arg1HeadCounts.toMap),
    countsString(arg2HeadCounts.toMap))*/
  }

  def normTupleString(): String = {
    arg1 + " " + cleanRelString(rel) + " " + arg2
  }

  def subsumesOrSubsumedBy(that:RelationTuple) = {
    val thisString = this.normTupleString()
    val thatString = that.normTupleString()

    thisString.contains(thatString) ||
    thatString.contains(thisString) ||
    setSubsumption(thisString.split(" "), thatString.split(" "))
  }
}

object Relgram {
  val sep = "_RG_SEP_"
  def fromSerializedString(string:String):Option[Relgram] = {
    val splits = string.split(sep)
    if (splits.size > 1){
      (RelationTuple.fromSerializedString(splits(0)), RelationTuple.fromSerializedString(splits(1))) match {
        case (Some(first:RelationTuple), Some(second:RelationTuple)) => Some(new Relgram(first, second))
        case _ => None
      }
    }else{
      None
    }
  }

}
case class Relgram(first:RelationTuple, second:RelationTuple){

  import Relgram._
  def serialize: String = "%s%s%s".format(first.serialize, sep, second.serialize)
  def prettyString:String = "%s\t%s\t%s\t%s".format(first.prettyString, second.prettyString, first.ids.mkString(","), second.ids.mkString(","))//, first.sentences.take(1).mkString(","), second.sentences.take(1).mkString(","))
  override def toString:String = "%s\t%s".format(first, second)
}

object ArgCounts {
  def newMap = new mutable.HashMap[String, Int]
  def newInstance = new ArgCounts(newMap, newMap, newMap, newMap)

  def fromSerializedString(string:String):Option[ArgCounts] = {
    val splits = string.split("\t")
    if (splits.size >= 3){
      val firstArg1Counts = MapUtils.StringIntMutableMapfromSafeCountsString(splits(0))
      val firstArg2Counts = MapUtils.StringIntMutableMapfromSafeCountsString(splits(1))
      val secondArg1Counts = MapUtils.StringIntMutableMapfromSafeCountsString(splits(2))
      val secondArg2Counts = MapUtils.StringIntMutableMapfromSafeCountsString(splits(3))
      Some(new ArgCounts(firstArg1Counts, firstArg2Counts, secondArg1Counts, secondArg2Counts))
    } else {
      None
    }
  }
}
case class ArgCounts(firstArg1Counts:scala.collection.mutable.Map[String,Int],
                     firstArg2Counts:scala.collection.mutable.Map[String,Int],
                     secondArg1Counts:scala.collection.mutable.Map[String,Int],
                     secondArg2Counts:scala.collection.mutable.Map[String,Int]){



  def serialize:String = toString
  override def toString:String = MapUtils.toSafeCountsString(firstArg1Counts.toMap) + "\t" +
                                 MapUtils.toSafeCountsString(firstArg2Counts.toMap) + "\t" +
                                 MapUtils.toSafeCountsString(secondArg1Counts.toMap) + "\t" +
                                 MapUtils.toSafeCountsString(secondArg2Counts.toMap)
}



object RelgramCounts{

  def isDummy(rgc: RelgramCounts):Boolean = {
    rgc.relgram.first.arg1.equals("NA") &&  rgc.relgram.second.arg1.equals("NA")
  }
  import RelationTuple._
  val DummyRelgramCounts:RelgramCounts = new RelgramCounts(new Relgram(dummyTuple, dummyTuple), new mutable.HashMap[Int, Int](), ArgCounts.newInstance)
  implicit def RelgramCountsFmt = new WireFormat[RelgramCounts]{
    override def toWire(x: RelgramCounts, out: DataOutput) {
      try{
        out.writeUTF(x.serialize)
      }catch{
        case e:Exception => {
          println("Failed to persist relgramcounts of size: "  + x.serialize.length)
          out.writeUTF(DummyRelgramCounts.serialize)
        }
      }
    }
    override def fromWire(in: DataInput): RelgramCounts = RelgramCounts.fromSerializedString(in.readUTF()).getOrElse(DummyRelgramCounts)//.getOrElse(RelgramCounts.DummyRelgramCounts)
  }
  val sep = "_RGC_SEP_"
  def fromSerializedString(serializedString:String):Option[RelgramCounts] = {
    val splits = serializedString.split(sep)
    if (splits.size > 2){
      fromSerializedStringSplits(splits)
    }else{
      println("Failed to serialize from string: " + serializedString)
      None
    }
  }

  def fromSerializedStringSplits(splits: Array[String]): Option[RelgramCounts] = {
    val relgramOption = Relgram.fromSerializedString(splits(0))
    val counts = deserializeCounts(splits(1))
    val argCountsOption = ArgCounts.fromSerializedString(splits(2))
    (relgramOption, argCountsOption) match {
      case (Some(relgram: Relgram), Some(argCounts: ArgCounts)) => Some(new RelgramCounts(relgram, counts, argCounts))
      case _ => {
        println("Relgram: " + relgramOption + " and ArgCountOption: " + argCountsOption)
        None
      }
    }
  }

  def serializeCounts(counts:Map[Int, Int]) = MapUtils.toIntIntCountsString(counts)
  def deserializeCounts(countsString:String) = try{
    if(countsString.size > 0)
      MapUtils.IntIntMutableMapfromCountsString(countsString)
    else
     new mutable.HashMap[Int, Int]()
  }catch{
    case e:Exception => {
      println("Failed to extract IntInt map from string: " + countsString)
      new mutable.HashMap[Int, Int]
    }
  }
}

case class RelgramCounts(relgram:Relgram, counts:scala.collection.mutable.Map[Int, Int], argCounts:ArgCounts){


  import RelgramCounts._

  def prettyString:String = "%s\t%s\t%s".format(relgram.prettyString,
                                                MapUtils.toIntIntCountsString(counts.toMap),
                                                argCounts.toString)

  def serialize:String = "%s%s%s%s%s".format(relgram.serialize, sep,
                                             serializeCounts(counts.toMap), sep,
                                             argCounts.serialize)

  override def toString:String = "%s\t%s\t%s".format(relgram.toString,
                                                     MapUtils.toIntIntCountsString(counts.toMap),
                                                     argCounts.toString)

}

object UndirRelgramCounts{




  val sep = "_URGC_"//RelgramCounts.sep
  def fromSerializedString(string:String) = {
    val splits = string.split(sep)
    RelgramCounts.fromSerializedString(splits(0)) match {
      case Some(rgc:RelgramCounts) => {
        val bitermCounts = RelgramCounts.deserializeCounts(splits(1))
        Some(new UndirRelgramCounts(rgc, bitermCounts))
      }
      case None => None
    }
  }

  def DummyUndirRelgramCounts = new UndirRelgramCounts(RelgramCounts.DummyRelgramCounts, Map[Int, Int]())
  def serializeCounts(counts:Map[Int, Int]) = RelgramCounts.serializeCounts(counts)
  implicit def UndirRelgramCountsFmt = new WireFormat[UndirRelgramCounts]{
    override def toWire(x: UndirRelgramCounts, out: DataOutput) {
      try{
        out.writeUTF(x.serialize)
      } catch {
        case e:Exception => {
          println("Failed to persist undir relgramcounts of size: "  + x.serialize.length)
          val bicount = if (!x.bitermCounts.isEmpty) x.bitermCounts.values.max else -1
          println("Failed on: " + x.rgc.prettyString + "\t" + bicount)
          out.writeUTF(DummyUndirRelgramCounts.serialize)
        }
      }
    }
    override def fromWire(in: DataInput): UndirRelgramCounts = UndirRelgramCounts.fromSerializedString(in.readUTF()).getOrElse(DummyUndirRelgramCounts)
  }


}
case class UndirRelgramCounts(rgc:RelgramCounts, bitermCounts:Map[Int, Int]){
  import UndirRelgramCounts._
  def serialize:String = "%s%s%s".format(rgc.serialize, sep, serializeCounts(bitermCounts.toMap))
  override def toString:String = "%s\t%s".format(rgc.toString, MapUtils.toIntIntCountsString(bitermCounts.toMap))


}


object AffinityMeasures{

  val sep = "_affsep_"
  def fromSerializedString(string:String) = {
    val splits = string.split(sep)
    if (splits.size == 4){
      (Conditionals.fromSerializedString(splits(0)),
        Conditionals.fromSerializedString(splits(1)),
        Conditionals.fromSerializedString(splits(2)),
        Conditionals.fromSerializedString(splits(3))) match {
        case (Some(fud:Conditionals), Some(fd:Conditionals), Some(sud:Conditionals), Some(sd:Conditionals)) => {
          Some(new AffinityMeasures(fud, fd, sud, sd))
        }
        case _ => None
      }
    }else{
      None
    }
  }

  def fromMeasures(measures:Measures, windowAlpha:Double, smoothingDelta:Double) = {
    val bitermCounts = measures.urgc.bitermCounts
    val bigramCounts = measures.urgc.rgc.counts
    val firstCounts = measures.firstCounts
    val secondCounts = measures.secondCounts

    import edu.washington.cs.knowitall.relgrams.measures.MeasureFunctions._
    val firstUndirConditionals = conditionals(bitermCounts, smoothingDelta, firstCounts)
    val firstUndirConditional = combine(firstUndirConditionals, windowAlpha)

    val firstDirConditionals = conditionals(bigramCounts, smoothingDelta, firstCounts)
    val firstDirConditional = combine(firstDirConditionals, windowAlpha)

    val firstDir = new Conditionals(firstDirConditionals, firstDirConditional)
    val firstUndir = new Conditionals(firstUndirConditionals, firstUndirConditional)

    val secondUndirConditionals = conditionals(bitermCounts, smoothingDelta, secondCounts)
    val secondUndirConditional = combine(secondUndirConditionals, windowAlpha)

    val secondDirConditionals = conditionals(bigramCounts, smoothingDelta, secondCounts)
    val secondDirConditional = combine(secondDirConditionals, windowAlpha)

    val secondDir = new Conditionals(secondDirConditionals, secondDirConditional)
    val secondUndir = new Conditionals(secondUndirConditionals, secondUndirConditional)

    Some(new AffinityMeasures(firstUndir, firstDir, secondUndir, secondDir))
  }



}


case class AffinityMeasures(firstUndir:Conditionals, firstDir:Conditionals,
                            secondUndir:Conditionals, secondDir:Conditionals){
  import AffinityMeasures._
  def serialize = "%s%s%s%s%s%s%s".format(firstUndir.serialize, sep, firstDir.serialize, sep, secondUndir.serialize, sep, secondDir.serialize)
  override def toString:String = "%s\t%s\t%s\t%s".format(firstUndir.toString, firstDir.toString, secondUndir.toString, secondDir.toString)
}

object WindowedMeasure{

  val countsRe = """[^0-9,]""".r
  def isValidCountsString(string:String) = countsRe.findFirstMatchIn(string) == None
  val sep = "_WSEP_"
  def fromSerializedString(string:String) = {
    val splits = string.split(sep)
    if (splits.size == 2){
      val doubles = if (isValidCountsString(splits(0))) splits(0).split(",").filter(x => x.size > 0).map(x => x.toDouble).toSeq else Seq[Double]()
      Some((doubles, splits(1).toDouble))
    } else {
      None
    }
  }



}


case class WindowedMeasure(windowValues:Seq[Double],combinedValue:Double){
  import WindowedMeasure._
  override def toString:String = "%s\t%.8f".format(windowValues.mkString("\t"), combinedValue)
  def serialize:String = "%s%s%.8f".format(windowValues.mkString(","), sep, combinedValue)
}

object Conditionals{
  def fromSerializedString(string:String) = WindowedMeasure.fromSerializedString(string) match {
    case Some((conditionals:Seq[Double], conditional:Double)) => Some(new Conditionals(conditionals, conditional))
    case None => None
  }
}

case class Conditionals(conditionals:Seq[Double], conditional:Double) extends WindowedMeasure(conditionals, conditional)

object PMIs{
  def fromSerializedString(string:String) = WindowedMeasure.fromSerializedString(string) match {
    case Some((pmis:Seq[Double], pmi:Double)) => Some(new PMIs(pmis, pmi))
    case None => None
  }
}

class PMIs(pmis:Seq[Double], pmi:Double) extends WindowedMeasure(pmis, pmi)


object Measures{


  val sep = "_MSEP_"

  def maxOrElse(values:Iterable[Int], orElse:Int) = if (!values.isEmpty) values.max else orElse

  def fromSerializedString(string:String) = {
    val splits = string.split(sep)
    UndirRelgramCounts.fromSerializedString(splits(0)) match {
      case Some(urgc:UndirRelgramCounts) => {
        val firstCounts = if (splits.size > 1) splits(1).toInt else 0
        val secondCounts = if (splits.size > 2) splits(2).toInt else 0
        Some(new Measures(urgc, firstCounts, secondCounts))
      }
      case _ => None
    }
  }
  def DummyMeasures = new Measures(UndirRelgramCounts.DummyUndirRelgramCounts, 0, 0)
  implicit def MeasuresFmt = new WireFormat[Measures]{
    override def toWire(x: Measures, out: DataOutput) {
      try {
        out.writeUTF(x.serialize)
      }catch {
        case e:Exception => {
          println("Failed to persist measures of size: "  + x.serialize.length)
          val bicount = if (!x.urgc.bitermCounts.isEmpty) x.urgc.bitermCounts.values.max else -1
          println("Failed on: " + x.urgc.rgc.prettyString + "\t" + bicount)
          out.writeUTF(DummyMeasures.serialize)
        }
      }
    }
    override def fromWire(in: DataInput): Measures = Measures.fromSerializedString(in.readUTF()).getOrElse(DummyMeasures)
  }

}

case class Measures(urgc:UndirRelgramCounts, firstCounts:Int, secondCounts:Int){
  import Measures._
  def serialize:String = "%s%s%s%s%s".format(urgc.serialize, sep, firstCounts, sep, secondCounts)
  override def toString:String = "%s\t%s\t%s".format(urgc.toString, firstCounts, secondCounts)
  def measures(windowAlpha:Double, smoothingDelta:Double) = {
    AffinityMeasures.fromMeasures(this, windowAlpha, smoothingDelta)
  }
  def frequenciesString:String = maxOrElse(urgc.bitermCounts.values, 0) + "\t" + maxOrElse(urgc.rgc.counts.values, 0) + "\t" + firstCounts + "\t" + secondCounts

}
