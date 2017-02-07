package edu.washington.cs.knowitall.relgrams

import com.nicta.scoobi.core.WireFormat
import java.io._
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.collection.immutable.Interval
import util.matching.Regex.Match
import io.Source
import scala.Some
import utils.StringUtils


/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/18/13
 * Time: 10:12 PM
 * To change this template use File | Settings | File Templates.
 */


//Holds the infomation obtained from the output of relgramtuples-app program, see that programs README for detail
case class TuplesAppData(docid:String, sentid:Int, sentence:String, arg1:String, arg1head:String, 
        arg1types:String, rel:String, relhead:String, arg2:String, arg2head:String, arg2types:String) {
    
        override def toString:String = {
            "%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s".format(docid, sentid, sentence, arg1, arg1head, arg1types, rel, relhead, arg2, arg2head, arg2types)
        }
    
    }
        

//Return a RelgramTuplesAppData from a string obtained via relgramtuples-app program
object TuplesAppDataGenerator{
    val sep = '|'
    val size = 11

    def fromString(str:String):Option[TuplesAppData] = {
        val splits = str.split(sep)
        if(splits.size >= size) {
            Some(TuplesAppData(splits(0), splits(1).toInt, splits(2), splits(3), splits(4), splits(5), splits(6), splits(7), splits(8), splits(9), splits(10)))
        }
        else {
            println("TuplesAppDataGenerator: Splits less than %s".format(size))
            None
        }
    }
}

//class to convert a TuplesAppData sentence into a TypedTuplesRecord 


/*case class TypedTuplesRecord(docid:String, sentid:Int, sentence:String, extrid:Int, hashes:Set[Int],
                             arg1:String, arg1Interval:Interval, rel:String, relInterval:Interval, arg2:String, arg2Interval:Interval,
                             arg1Head:String, arg1HeadInterval:Interval, var relHead:String, var relHeadInterval:Interval, arg2Head:String, arg2HeadInterval:Interval,
                             arg1Types:Seq[String], arg2Types:Seq[String],
                             confidence:Double){ */


object TypedTuplesRecordGenerator{
    
    //start pos indicates the charecter offset of the sentence (where in the document it starts), used for calculating the spans for the arguments
    def fromTuplesAppData(data:TuplesAppData, startpos:Int, extrid:Int = 0, conf:Double = -1.0):TypedTuplesRecord = {
        val arg1types = data.arg1types.split(',') 
        val arg2types = data.arg2types.split(',') 
        val arg1Interval = getSpan(data.sentence, data.arg1, startpos)
        val arg1HeadInterval = arg1Interval
        val arg2Interval = getSpan(data.sentence, data.arg2, startpos)
        val arg2HeadInterval = arg2Interval
        val relInterval = getSpan(data.sentence, data.rel, startpos)
        val relHeadInterval = relInterval
        val hashes = sentenceHashes(data.sentence).toSet

        TypedTuplesRecord(data.docid, data.sentid, data.sentence, extrid, hashes, data.arg1,  arg1Interval, data.rel, relInterval, data.arg2, arg2Interval,
                          data.arg1head, arg1HeadInterval, data.relhead, relHeadInterval, data.arg2head, arg2HeadInterval, arg1types, arg2types, conf)

    }

    def getSpan(sentence:String, arg:String, startpos:Int):Interval = {
        val index = sentence.indexOfSlice(arg)
        index match {
            case -1 => {
                println("Could not find string %s in sentence %s".format(arg, sentence))
                Interval.open(0,0)
            }
            case _ => {
                val start = index + startpos
                val end = start + arg.length
                Interval.open(start, end)
            }
        }

    }

    def sentenceHashes(insentence:String) = {
        val delims = "[.,!?:;]+"
        val sentence = insentence.toLowerCase()
        val sentenceHashCode = sentence.replaceAll("[^a-zA-Z0-9]", "").hashCode
        sentenceHashCode::Nil ++ sentence.split(delims).filter(split => split.split(" ").size >= 5).map(split => split.replaceAll("[^a-zA-Z0-9]", "").hashCode)
    }
}

//end new code
object TypedTuplesRecord{

  val wire = TypedTuplesRecord.TypedTuplesRecordFmt

  def main(args:Array[String]){
    val testFile = args(0)
    var success = 0
    println("Reading from file")
    val records = Source.fromFile(testFile).getLines.flatMap(line => {
      val recordOption = fromString(line)
      recordOption match {
        case Some(record:TypedTuplesRecord) => success += 1
        case None => println("Failed to read record from line: " + line)
      }
      recordOption
    })
    println("Read %s records.".format(records.size))
    println("Testing serialize/deserialize.")
    records.foreach(record => {
      testSerializeDeserialize(record)
    })
    println("Success counts: " + success)
    success = 0
    println("Testing wire.")
    testWire(records, success)
    println("Done.")
  }


  def testWire(records: Iterator[TypedTuplesRecord], _success: Int) {
    var success: Int = _success
    records.foreach(record => {
      val byteStream = new ByteArrayOutputStream()
      val out = new DataOutputStream(byteStream)
      wire.toWire(record, out)
      val bytes = byteStream.toByteArray
      val instream = new ByteArrayInputStream(bytes)

      val in = new DataInputStream(instream)
      val inrecord = wire.fromWire(in)
      if (inrecord.sentid != record.sentid) {
        println("Failed to read from wire: " + record.toString)
      } else {
        success = success + 1
      }
    })
  }

  def testSerializeDeserialize(record: TypedTuplesRecord) {
    val string = record.toString
    TypedTuplesRecord.fromString(string) match {
      case Some(x: TypedTuplesRecord) => //println("Success.")
      case None => println("Failed to deserialize from serialized string: " + string)
    }
  }

  val logger = LoggerFactory.getLogger(this.getClass)
  val span_sep = "_SPAN_"
  val textSpanRe = """(.*?)%s(.*?)%s(.*?)""".format(span_sep, "-").r
  def textSpan(text:String, interval:Interval) = "%s%s%s-%s".format(text, span_sep, interval.start, interval.end)
  def fromTextSpan(textSpan:String):Option[(String, Interval)] = {
    if (textSpanRe.findFirstMatchIn(textSpan) != None){
      val textSpanRe(x:String, s:String, e:String) = textSpan
      Some((x, Interval.open(s.toInt, e.toInt)))
    }else{
      println("Failed to extract from textspan: " + textSpan)
      None
    }
  }
  //docid sentid sentence extrid origtuple headtuple arg1types arg2types
  def fromString(string: String): Option[TypedTuplesRecord] = {
    val splits = string.split("\t")


    try{
    if (splits.size > 9){
      var i = 0
      def nextString = {
        val out = if (splits.size > i) splits(i) else ""
        i = i + 1
        out
      }
      val docid = nextString
      val sentid = nextString.toInt
      val sentence = nextString
      val extrid = nextString.toInt
      val hashes = nextString.split(",").map(x => x.toInt).toSet
      val (arg1:String, arg1Interval:Interval) = fromTextSpan(nextString).get
      val (rel:String, relInterval:Interval) = fromTextSpan(nextString).get
      val (arg2:String, arg2Interval:Interval) = fromTextSpan(nextString).get
      val (arg1Head:String, arg1HeadInterval:Interval) = fromTextSpan(nextString).get
      val (relHead:String, relHeadInterval:Interval) = fromTextSpan(nextString).get
      val (arg2Head:String, arg2HeadInterval:Interval) = fromTextSpan(nextString).get
      val arg1Types = nextString.split(",")
      val arg2Types = nextString.split(",")
      val confidence = {
        val confString = nextString
        if (confString != "") { StringUtils.toDoubleOrElse(confString, -1.0)} else -1.0
      }
      Some(new TypedTuplesRecord(docid, sentid, sentence, extrid, hashes,
        arg1, arg1Interval, rel, relInterval, arg2, arg2Interval,
        arg1Head, arg1HeadInterval, relHead, relHeadInterval, arg2Head, arg2HeadInterval,
        arg1Types, arg2Types,
        confidence))
    }else{
      println("Failed to read TypedTuplesRecord from string: " + string)
      println("String has only %d splits. Expected at least %d".format(splits.size, 10))
      logger.error("Failed to read TypedTuplesRecord from string: " + string)
      logger.error("String has only %d splits. Expected at least %d".format(splits.size, 10))
      None
    }
    }catch {
      case e:Exception => {
        logger.error("Failed to read TypedTuplesRecord from string: " + string)
        logger.error(e.getStackTraceString)
        None
      }
      case e:Error => {
        logger.error("Failed to read TypedTuplesRecord from string: " + string)
        logger.error(e.getStackTraceString)
        None
      }
    }
  }

  implicit def TypedTuplesRecordFmt = new WireFormat[TypedTuplesRecord]{
    def toWire(x: TypedTuplesRecord, out: DataOutput) {out.writeUTF(x.toString)}
    def fromWire(in: DataInput): TypedTuplesRecord = {TypedTuplesRecord.fromString(in.readUTF()).get}
  }

}

case class TypedTuplesRecord(docid:String, sentid:Int, sentence:String, extrid:Int, hashes:Set[Int],
                             arg1:String, arg1Interval:Interval, rel:String, relInterval:Interval, arg2:String, arg2Interval:Interval,
                             arg1Head:String, arg1HeadInterval:Interval, var relHead:String, var relHeadInterval:Interval, arg2Head:String, arg2HeadInterval:Interval,
                             arg1Types:Seq[String], arg2Types:Seq[String],
                             confidence:Double){



  import TypedTuplesRecord._
  override def toString:String = "%s\t%d\t%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%.6f".format(docid, sentid, sentence, extrid, hashes.mkString(","),
  textSpan(arg1, arg1Interval), textSpan(rel, relInterval), textSpan(arg2, arg2Interval),
  textSpan(arg1Head, arg1HeadInterval), textSpan(relHead, relHeadInterval), textSpan(arg2Head, arg2HeadInterval),
  arg1Types.mkString(","), arg2Types.mkString(","), confidence)


  def cleanRelString(r:String): String = r.replaceAll("""^be """, "")

  def normTupleString(): String = {
    arg1 + " " + cleanRelString(rel) + " " + arg2
  }

  def setSubsumption(awords: Array[String], bwords: Array[String]): Boolean = {
    val aset = awords.filter(a => !a.equals("be")).toSet
    val bset = bwords.filter(b => !b.equals("be")).toSet
    aset.subsetOf(bset) || bset.subsetOf(aset)
  }
  def spanSubsumes(that: TypedTuplesRecord): Boolean = {
    that.arg1Interval.subset(this.arg1Interval) && that.relInterval.subset(this.relInterval) && that.arg2Interval.subset(this.arg2Interval)
  }


  def subsumesOrSubsumedBy(that:TypedTuplesRecord):Boolean = {
    if (this.spanSubsumes(that)){
      //println("%s subsumes %s".format(this.normTupleString(), that.normTupleString()))
      return true
    }
    if (that.spanSubsumes(this)) {
      //println("%s subsumes %s".format(that.normTupleString(), this.normTupleString()))
      return true
    }
    val thisString = this.normTupleString()
    val thatString = that.normTupleString()
    thisString.contains(thatString) || thatString.contains(thisString) || setSubsumption(thisString.split(" "), thatString.split(" "))

  }
}
