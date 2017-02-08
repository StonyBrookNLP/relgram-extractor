package edu.washington.cs.knowitall.relgrams

import edu.washington.cs.knowitall.tool.coref.{TuplesCorefResolver, StanfordCoreferenceResolver, Mention}

import io.Source
import collection.mutable.{HashMap, ArrayBuffer}
import collection.mutable
import org.slf4j.LoggerFactory
import com.nicta.scoobi.core.WireFormat
import java.io.{PrintWriter, File, StringWriter, DataInput, DataOutput}
import utils.StringUtils


/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/25/13
 * Time: 3:57 PM
 * To change this template use File | Settings | File Templates.
 */

object TuplesDocumentGenerator{

  
  def trimDocument(indocument:TuplesDocument, maxLength:Int) = new TuplesDocument(indocument.docid, pruneLongSentences(indocument, maxLength))
  def pruneLongSentences(indocument: TuplesDocument, maxLength:Int=75): Seq[TypedTuplesRecord] = {
    indocument.tupleRecords.filter(x => {
      val numwords = x.sentence.count(c => c == ' ')// split(" ").size NOTE: This was expensive!!!
      val out = numwords <= maxLength
      if (!out) println("ignored\t%s\t%d\t%d".format(indocument.docid, x.sentid, numwords))
      out
    })
  }

  def sentenceIdsWithOffsets(document: TuplesDocument) = {//List[(String, Int)] = {
  var offset = 0
    var sentencesMap = new HashMap[Int, String]//new ArrayBuffer[String]()
    var sentidOffsetsMap = new mutable.HashMap[Int, Int]()
    document.tupleRecords.iterator.foreach(record => sentencesMap += record.sentid -> record.sentence)
    var sentences = new ArrayBuffer[String]
    var offsets = new ArrayBuffer[Int]()
    sentencesMap.keys.toSeq.sortBy(key => key).foreach(key => {//document.tupleRecords.iterator.foreach(record => {
    //println("key: " + key)
    val sentence = sentencesMap(key)
      sentences += sentence
      offsets += offset
      sentidOffsetsMap += key -> offset
      offset = offset + sentence.length + 1
    })
    (sentencesMap, sentidOffsetsMap)
    //(sentences.toList, offsets.toList)
  }

  def sentencesWithOffsets(document: TuplesDocument) = {//List[(String, Int)] = {
  var offset = 0
    var sentencesMap = new HashMap[Int, String]//new ArrayBuffer[String]()
    document.tupleRecords.iterator.foreach(record => sentencesMap += record.sentid -> record.sentence)
    var sentences = new ArrayBuffer[String]
    var offsets = new ArrayBuffer[Int]()
    sentencesMap.keys.toSeq.sortBy(key => key).foreach(key => {//document.tupleRecords.iterator.foreach(record => {
      val sentence = sentencesMap(key)
      sentences += sentence
      offsets += offset
      offset = offset + sentence.length + 1
    })
    (sentences.toList, offsets.toList)
  }

}

class TuplesDocumentGenerator {

  val logger = LoggerFactory.getLogger(this.getClass)
  var timeout = 0
  //var resolveWithTimeout: (Map[Mention, List[Mention]]) => Option[Map[Mention, List[Mention]]] = null//runWithTimeout(10) _
  val resolvers = new mutable.HashMap[Thread,TuplesCorefResolver] with mutable.SynchronizedMap[Thread, TuplesCorefResolver]
  def this(timeout:Int) = {
    this()
    setTimeout(timeout)
  }

  import scala.actors.Futures._
  def runWithTimeout_old(timeoutMs: Long)(f: => Option[Map[Mention, List[Mention]]]) : Option[Map[Mention, List[Mention]]] = {
    awaitAll(timeoutMs, future(f)).head.asInstanceOf[Option[Map[Mention, List[Mention]]]]
  }

  def runWithTimeout(timeoutMs: Long)(f: => Map[Mention, List[Mention]]): Option[Map[Mention, List[Mention]]] = {
    awaitAll(timeoutMs, future(f)).head match {
      case Some(x:Any) => Some(x.asInstanceOf[Map[Mention, List[Mention]]])
      case None => {
        None
      }
    }
  }

  def setTimeout(timeOut: Int){
    this.timeout = timeOut
    logger.info("Timeout seconds: %.2f".format(timeOut/1000.0))
    //resolveWithTimeout = runWithTimeout(timeOut) _
  }

  def getPrunedDocument(docid: String, records: Seq[TypedTuplesRecord]): TuplesDocument = {
    val prunedSortedRecords = pruneRecordsAndIndex(records).sortBy(x => x._2).map(x => x._1)
    new TuplesDocument(docid, prunedSortedRecords)
  }

  def getPrunedTuplesDocumentWithCorefMentions(docid: String, records: Seq[TypedTuplesRecord], maxLength:Int) = {
    val prunedSortedRecords = pruneRecordsAndIndex(records).sortBy(x => x._2).map(x => x._1)
    getTuplesDocumentWithCorefMentionsBlocks(new TuplesDocument(docid, prunedSortedRecords), maxLength)
  }


  val resolver = new TuplesCorefResolver//StanfordCoreferenceResolver()//resolvers.getOrElseUpdate(Thread.currentThread(), new StanfordCoreferenceResolver())
  def resolve(sentences:List[String]):Option[Map[Mention, List[Mention]]] =  {
    var out:Option[Map[Mention, List[Mention]]] = None
    if (!sentences.isEmpty){
      out = try{
        Some(resolver.clusters(sentences.mkString("\n")))//resolveWithTimeout(resolver.clusters(sentences.mkString("\n")))
      }catch{
        case e:Error => {
          logger.error("Skipping coref resolution for sentences.")
          e.printStackTrace()
          None
        }
        case e:Exception => {
          logger.error("Skipping coref resolution for sentences.")
          e.printStackTrace()
          None
        }
        case _ => {
          logger.error("Skipping coref resolution for sentences.")
          None
        }
      }
    }
    out
  }

  def resolveDirect(sentences:List[String]):Map[Mention, List[Mention]] =  {
    var out:Map[Mention, List[Mention]] = Map[Mention, List[Mention]]()
    if (!sentences.isEmpty){
      try{
        out = resolver.clusters(sentences.mkString("\n"))
      }catch{
        case e:Error => {
          logger.error("Skipping coref resolution for sentences.")
          e.printStackTrace()
        }
        case e:Exception => {
          logger.error("Skipping coref resolution for sentences.")
          e.printStackTrace()
        }
        case _ => {
          logger.error("Skipping coref resolution for sentences.")
        }
      }
    }
    out
  }






  def getTuplesDocumentWithCorefMentionsBlocks(indocument:TuplesDocument, maxLength:Int):Option[TuplesDocumentWithCorefMentions] = {
    import TuplesDocumentGenerator._
    try{
      val resolveWithTimeout = runWithTimeout(timeout) _
      val document = trimDocument(indocument, maxLength)
      val (sentences:List[String], offsets:List[Int]) = sentencesWithOffsets(document)
      logger.info("Processing " + document.docid + " # records: " + sentences.size)
      val mentionsOption = if(timeout > 0){
        resolveWithTimeout(resolveDirect(sentences))
      } else {
        resolve(sentences)
      }
      mentionsOption match {
        case Some(mentions:Map[Mention, List[Mention]]) => Some(new TuplesDocumentWithCorefMentions(document, offsets, mentions))
        case None => {
          logger.error("Timing out document: " + document.docid + " with " + document.tupleRecords.size + " sentences. No mentions added.")
          Some(new TuplesDocumentWithCorefMentions(document, offsets, Map[Mention, List[Mention]]()))
        }
      }

    }catch {
      case e:Exception => {

        logger.error("Caught exception assigning mentions to %s :\n%s".format(indocument.docid, e.getStackTraceString))
        e.printStackTrace()
        None
      }
      case e:Error => {
        logger.error("Caught error assigning mentions to %s :\n%s".format(indocument.docid, e.getStackTraceString))
        e.printStackTrace()
        None
      }
      case _ => {
        logger.error("Caught unknown throwable.")
        None
      }

    }
  }




  def resolveInBlocks(document:TuplesDocument, sentences:List[String], offsets:List[Int]):Option[Map[Mention, List[Mention]]] = {
    //val resolver = resolvers.getOrElseUpdate(Thread.currentThread(), new StanfordCoreferenceResolver())
    def shiftMention(mention:Mention, by:Int):Mention = new Mention(mention.text, mention.offset + by)
    val blocks = sentenceBlocksWithOffsets(document, sentences, offsets)
    val resolveWithTimeout = runWithTimeout(timeout) _
    val mentions:Map[Mention, List[Mention]] = blocks.flatMap(block => {
      val bsentences = block._1
      val boffsets = block._2
      val startOffset = boffsets.head
      resolveWithTimeout(resolver.clusters(bsentences.mkString("\n"))) match {
        case Some(mentions:Map[Mention, List[Mention]]) => mentions.map(kv => (shiftMention(kv._1, startOffset) -> kv._2.map(m => shiftMention(m, startOffset))))
        case None => {
          None
        }
      }
    }).toMap
    Some(mentions)
  }

  def sentenceBlocksWithOffsets(document:TuplesDocument, sentences:List[String], offsets:List[Int]) = {
    //val (sentences:List[String], offsets:List[Int]) = sentencesWithOffsets(document)
    val size10SentBlocks = sentences.grouped(10).toList
    val size10OffsetBlocks = offsets.grouped(10).toList
    val adjustedBlocks = size10OffsetBlocks(0)::Nil ++ (1 until size10OffsetBlocks.size).map(i => {
      val by = size10SentBlocks(i-1).mkString("\n").size
      size10OffsetBlocks(i).map(offset => offset + by)
    })
    import edu.washington.cs.knowitall.relgrams.utils.Pairable._
    size10SentBlocks pairElements adjustedBlocks
  }



  //Two relations are different as long as they are between different arguments.
  def areDifferentRelations(outer: TypedTuplesRecord, inner: TypedTuplesRecord): Boolean = {
    def sameArgs(x:TypedTuplesRecord, y:TypedTuplesRecord)     = x.arg1.equals(y.arg1) && x.arg2.equals(y.arg2)
    def switchedArgs(x:TypedTuplesRecord, y:TypedTuplesRecord) = x.arg1.equals(y.arg2) && x.arg2.equals(y.arg1)
    def subsumedArgs(x:TypedTuplesRecord, y:TypedTuplesRecord) = x.subsumesOrSubsumedBy(y)
    !sameArgs(outer, inner) && !switchedArgs(outer, inner) && !subsumedArgs(outer, inner)
  }


  //Filter typed tuples and sort them in the order of occurrence in text.
  def removeNonAlphaNumericRecords(sRecords:Seq[TypedTuplesRecord]):Seq[TypedTuplesRecord] = {
    val hasAlphabetOrDigitRe = """[a-zA-Z0-9]""".r
    def specialCharsOnly(string:String) = hasAlphabetOrDigitRe.findFirstIn(string) == None
    def isGoodExtraction(record: TypedTuplesRecord):Boolean = {
      !specialCharsOnly(record.arg1Head) &&
      !specialCharsOnly(record.relHead) &&
      !specialCharsOnly(record.arg2Head)
    }
    sRecords.filter(record => isGoodExtraction(record))
  }
  def sortValue(record: TypedTuplesRecord): Int = {
    record.sentid * 1000 + record.extrid
  }

  val beVerbs = ("be"::"is"::"was"::"are"::"were"::Nil).toSet
  def rewriteBeVerbs(text: String): String = text.split(" ").map(word => if(beVerbs.contains(word)) "be" else word).mkString(" ")

  val hasVerbs = ("has"::"have"::"had"::"having"::Nil).toSet
  def rewriteBeAndHasVerbs(text: String): String = {
    text.split(" ").map(word => {
      if(hasVerbs.contains(word))
        "had"
      else if(beVerbs.contains(word))
        "be"
      else
        word
    }).mkString(" ")
  }


  def isReportingVerbRelation(rel:String) = {
    val out = !reportingVerbs.intersect(rel.split(" ").toSet).isEmpty
    if (out) println("Rel: " + rel + " is reporting verb. Filtered")
    out
  }
  val reportingVerbs = ("say"::"said"::"saying"::"tell"::"told"::"speak"::"spoke"::Nil).toSet
  def isBeRelation(rel:String) = rel.equals("be")
  def isHadRelation(rel:String) = rel.equals("had") || rel.equals("had had")

  def removeReportingAndBeHadRelations(records: Seq[TypedTuplesRecord]): Seq[TypedTuplesRecord] = {
    records.filterNot(record => isReportingVerbRelation(record.relHead) || isBeRelation(record.relHead) || isHadRelation(record.relHead))
  }

  def removeMoreThanXWordRelations(records: Seq[TypedTuplesRecord], x:Int): Seq[TypedTuplesRecord] = {
    records.filter(record => {
      val out = record.relHead.split(" ").size <= x
      //if (!out) println("Pruning record with relHead > %d words: %s".format(x, record))
      out
    })
  }

  def findAndRemoveRedundantRecords(records: Seq[TypedTuplesRecord]):Seq[TypedTuplesRecord] = {

    val outRecords = new ArrayBuffer[TypedTuplesRecord]()
    records.iterator.copyToBuffer(outRecords)
    records.iterator.foreach(outer => {
      records.iterator.filter(inner => (inner.extrid != outer.extrid)).foreach(inner => {
        if(!areDifferentRelations(outer, inner)){
          val removeRecord = if((outer.arg1Head + outer.relHead + outer.arg2Head).size > (inner.arg1Head + inner.relHead + inner.arg2Head).size) {
            inner
          }else{
            outer
          }

          outRecords -= removeRecord
        }
      })
    })
    if(outRecords.size == records.size) return outRecords else findAndRemoveRedundantRecords(outRecords)
    return outRecords
  }

  def removeRedundantRecords(records: Seq[TypedTuplesRecord]): Seq[TypedTuplesRecord] = {
    val groupedRecords = new HashMap[Int, ArrayBuffer[TypedTuplesRecord]]()

    records.foreach(record => groupedRecords.getOrElseUpdate(record.sentid, new ArrayBuffer[TypedTuplesRecord]) += record)
    var outRecords = mutable.Seq[TypedTuplesRecord]()
    groupedRecords.keys.foreach(key => {
      val grecords = groupedRecords(key)
      if(grecords.size > 1){
        val prunedRecords = findAndRemoveRedundantRecords(grecords)
        outRecords ++= prunedRecords
      }else{
        outRecords ++= grecords
      }

    })
    return outRecords
  }

  def pruneRecordsAndIndex(sRecords:Seq[TypedTuplesRecord]):Seq[(TypedTuplesRecord, Int)] = {
    var ssRecords = removeNonAlphaNumericRecords(sRecords)
    ssRecords = removeMoreThanXWordRelations(ssRecords, 7)
    ssRecords.foreach(record => record.relHead = rewriteBeAndHasVerbs(record.relHead))
    return ssRecords.sortBy(record => sortValue(record)).zipWithIndex//removeReportingAndBeHadRelations(ssRecords).sortBy(record => sortValue(record)).zipWithIndex
  }


}


object TuplesDocumentWithCorefMentions{
  val logger = LoggerFactory.getLogger(this.getClass)
  def fromString(tdmString: String): Option[TuplesDocumentWithCorefMentions] = {
    val splits = tdmString.split(tdocsep)
    if (splits.size >= 1){
      TuplesDocument.fromString(splits(0)) match {
        case Some(tuplesDocument:TuplesDocument) => {
          val sentenceOffsets = if(splits.size > 1) splits(1).split(",").map(x => x.toInt).toList  else List[Int]()
          val mentions = if(splits.size > 2) MentionIO.fromMentionsMapString(splits(2)) else Map[Mention, List[Mention]]()
          Some(new TuplesDocumentWithCorefMentions(tuplesDocument, sentenceOffsets, mentions))
        }
        case None => {
          println("Failed to construct TuplesDocument from string: " + splits(0))
          None
        }
      }
    }else{
      println("Splits size != 3, actual = %d. String:\n%s".format(splits.size, tdmString))
      None
    }
  }

  val tdocsep = "_TDOC_SEP_"

  val numParts = 10
  implicit def TuplesDocumentWithCorefMentionsFmt = new WireFormat[TuplesDocumentWithCorefMentions]{
    def toWire(x: TuplesDocumentWithCorefMentions, out: DataOutput){
      //out.writeUTF(x.toString)
      val outstr = x.toString
      try{
        val parts = StringUtils.splitIntoNPlusOneParts(outstr, numParts)
        parts.foreach(out.writeUTF(_))
      }catch{
        case e:Exception => {
          println("Skipping document: " + x.tuplesDocument.docid + " with length: " + outstr.size)
        }
      }
    }
    def fromWire(in: DataInput): TuplesDocumentWithCorefMentions = {
      //TuplesDocumentWithCorefMentions.fromString(in.readUTF()).get
      val parts = (0 until numParts+1).map(i => in.readUTF())
      val instr = parts.mkString("").trim
      TuplesDocumentWithCorefMentions.fromString(instr).get
    }
  }



}
object MentionIO{
  val msep = "_MSEP_"
  val valsep = "_VALSEP_"
  val keyvalsep = "_KEYVAL_SEP_"
  val mfieldsep = "_MFIELD_SEP_"

  def mentionString(mention:Mention) = "%s%s%s".format(mention.text, mfieldsep, mention.offset)
  def mentionsMapString(mentions: Map[Mention, List[Mention]]):String = mentions.map(mmlist => "%s%s%s".format(mentionString(mmlist._1), valsep, mmlist._2.map(m => mentionString(m)).mkString(msep))).mkString(keyvalsep)
  def fromMentionString(string:String) = {
    val splits = string.split(mfieldsep)
    if (splits.size == 2){
      Some( new Mention(splits(0), splits(1).trim.toInt))
    }else{
      None
    }
  }
  def fromMentionsMapString(string:String) = {
    val keyvals = string.split(keyvalsep)
    keyvals.flatMap(keyval => {
      val splits = keyval.split(valsep)
      if (splits.size > 1){
        val keyMentionOption = fromMentionString(splits(0))
        keyMentionOption match {
          case Some(keyMention:Mention) => {
            val valueMentions = splits(1).split(msep).flatMap(mstring => fromMentionString(mstring)).toList
            Some(keyMention -> valueMentions)
          }
          case None => {
            None
          }
        }
      }else{
        None
      }

    }).toMap
  }
}
case class TuplesDocumentWithCorefMentions(tuplesDocument:TuplesDocument,
                                           sentenceOffsets:List[Int],
                                           mentions:Map[Mention, List[Mention]]){
  import TuplesDocumentWithCorefMentions._
  override def toString:String = {
    "%s%s%s%s%s".format(tuplesDocument.toString(), tdocsep,
                        sentenceOffsets.mkString(","), tdocsep,
                        MentionIO.mentionsMapString(mentions))
  }
}

object TuplesDocument{

  val logger = LoggerFactory.getLogger(this.getClass)
  val docidsep = "_DOCID_SEP_"
  val recsep = "_RECORD_SEP_"

  def fromString(string:String):Option[TuplesDocument] = {
    val splits = string.split(docidsep)

    if (splits.size == 2){
      val docid = splits(0)
      val recordsstring = splits(1)
      val records = recordsstring.split(recsep).flatMap(rec => TypedTuplesRecord.fromString(rec)).toSeq
      if(!records.isEmpty){
        Some(new TuplesDocument(docid, records))
      }else{
        logger.error("Failed to read document from line: " + string)
        None
      }
    }else{
      logger.error("Failed to read document with splits size != 2. Actual = %d. Line:\n%s".format(splits.size, string))
      None
    }
  }

  val numParts = 10
  implicit def TuplesDocumentFmt = new WireFormat[TuplesDocument]{
    def toWire(x: TuplesDocument, out: DataOutput) {
      val outstr = x.toString() + "\n"
      try{
        out.writeBytes(outstr)
        //val parts = StringUtils.splitIntoNPlusOneParts(outstr, numParts)
        //parts.foreach(out.writeUTF(_))
      }catch{
        case e:Exception => {
          println("Skipping document: " + x.docid + " with length: " + outstr.size)
        }
      }
    }
    def fromWire(in: DataInput): TuplesDocument = {//TuplesDocument.fromString(in.readUTF()).get
      try{
        TuplesDocument.fromString(in.readLine()).get
      //val parts = (0 until numParts+1).map(i => in.readUTF())
      //val instr = parts.mkString("").trim
      //TuplesDocument.fromString(instr).get
      }catch{
        case e:Exception => {
          println("Skipping badly formed document: ")
          new TuplesDocument("na", Seq[TypedTuplesRecord]())
        }
      }
    }
  }
}

case class TuplesDocument (docid:String, tupleRecords:Seq[TypedTuplesRecord]) {
  import TuplesDocument._
  override def toString():String = "%s%s%s".format(docid, docidsep, tupleRecords.mkString(recsep))
}

object CorefDocumentDebugger{
  val logger = LoggerFactory.getLogger(this.getClass)
  def main(args:Array[String]){
    val tgen = new TuplesDocumentGenerator(0)
    val tupleDocuments = Source.fromFile(args(0)).getLines.flatMap(line => {
      TuplesDocument.fromString(line)
    }).toSeq
    tupleDocuments.foreach(td => tgen.getTuplesDocumentWithCorefMentionsBlocks(td, maxLength = 75) match {
      case Some(tdm:TuplesDocumentWithCorefMentions) => println("%s\t%d".format(tdm.tuplesDocument.docid, tdm.mentions.size))
      case None =>
    })
    //corefs.foreach(coref => println("%s\t%d" + corecoref.mentions.keys.size))

  }
}

object CorefDocumentTester{
  val logger = LoggerFactory.getLogger(this.getClass)
  def main(args:Array[String]){

    val tgen = new TuplesDocumentGenerator(args(1).toInt)
    val tupleDocuments = Source.fromFile(args(0)).getLines()
                               .flatMap(line =>TypedTuplesRecord.fromString(line)).toSeq
                               .groupBy(record => record.docid)
                               .map(kv => tgen.getPrunedDocument(kv._1, kv._2))

    println("Size of tupleDocuments: " + tupleDocuments.size)
    tupleDocuments.foreach(td => TuplesDocument.fromString(td.toString()) match {
      case Some(m:TuplesDocument) => println("Success.")
      case None => println("failure on tdm: " + td.docid)
    })

    val tdmSeq = tupleDocuments.flatMap(td => tgen.getTuplesDocumentWithCorefMentionsBlocks(td, maxLength=75))
    tdmSeq.foreach(tdm=> TuplesDocumentWithCorefMentions.fromString(tdm.toString()) match {
      case Some(tdm:TuplesDocumentWithCorefMentions) => println("Success 2.")
      case None => println("Failure on tdm2: " + tdm.tuplesDocument.docid)
    })

  }
}


/*
    Given output of the Tuple Extractor, output the TupleDocument data 
    This output can be used as input to CoreferenceTuples in order to add coref information to the tuples
*/

object TuplesAppDataTester{
    val logger = LoggerFactory.getLogger(this.getClass)
    def main(args:Array[String]){
    
        val directory = new File(args(0))
        val outfile= args(1) //document to write to 
        val writer = new PrintWriter(outfile, "utf-8")
        var output = ""
        var curr_extrid = -1
        directory.listFiles.foreach(infile => {  

            var offset = 0
            var previd = 0 //previous sentence id
            var prevlength = -1 //length of the previous sentence

            def toTuplesAppData(line:String):TuplesAppData = {
                TuplesAppDataGenerator.fromString(line) match {   
                    case Some(tupledata:TuplesAppData) => tupledata
                    case None => {
                            logger.error("Something horrible went wrong processing the string <%s>".format(line))
                            TuplesAppData("NA", -1, "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA") 
                        }
                }
            }

            def getExtrId:Int = {
                curr_extrid += 1
                curr_extrid
            }

            //get all the TypedTupleRecords from the document
            val tuplesData = Source.fromFile(infile).getLines().map(line => toTuplesAppData(line)) //Map from string to TuplesAppData to TypedTuplesRecord
                                                               .map(tupledata => TypedTuplesRecordGenerator.fromTuplesAppData(tupledata, getExtrId))

            val tuplesList = tuplesData.toList
            val docid = if(!tuplesList.isEmpty) tuplesList(0).docid else -1
            writer.println(docid + TuplesDocument.docidsep + tuplesList.mkString(TuplesDocument.recsep))
        })
                               
        writer.close
    }
}

/*
    Take in a Tuples Document and add Coref information (TuplesDocumentWithCoreference)
    This can be used as output to RelgramsLocalApp in order to generate counts information for Relgrams
*/
object CoreferenceTuples{ //object for getting coreference from Tuples Document
    //Take in Tuples document
  val logger = LoggerFactory.getLogger(this.getClass)
  def main(args:Array[String]){
    val writer = new PrintWriter(args(1), "utf-8")
    val tgen = new TuplesDocumentGenerator(0)
    val tupleDocuments = Source.fromFile(args(0)).getLines.flatMap(line => {
      TuplesDocument.fromString(line)
    }).toSeq
//    println(tupleDocuments)

    
    tupleDocuments.foreach(td =>  {
        val doc = tgen.getTuplesDocumentWithCorefMentionsBlocks(td, maxLength = 75) 
        writer.println(doc.get.toString)

      })
      writer.close
    }
}
