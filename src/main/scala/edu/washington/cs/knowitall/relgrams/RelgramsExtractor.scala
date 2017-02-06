package edu.washington.cs.knowitall.relgrams

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/17/13
 * Time: 9:06 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._

import collection.mutable.{ArrayBuffer, HashMap}
import collection.mutable
import io.Source
import java.io.PrintWriter
import org.slf4j.LoggerFactory
import scala.Predef._
import scala.Some


import edu.washington.cs.knowitall.relgrams.utils.MapUtils._
import utils.MapUtils
import edu.washington.cs.knowitall.tool.coref.Mention
import edu.washington.cs.knowitall.tool.postag.PostaggedToken

object RelgramsExtractor{
  def relationTupleKey(arg1: String, rel: String, arg2: String): String = {
    arg1 + "\t" + rel + "\t" + arg2
  }
  def relationTupleKey(relationTuple:RelationTuple):String = relationTupleKey(relationTuple.arg1, relationTuple.rel, relationTuple.arg2)//"%s\t%s\t%s".format(relationTuple.arg1, relationTuple.rel, relationTuple.arg2)
  def relgramKey(first: RelationTuple, second: RelationTuple): String = {
    relationTupleKey(first) + "\t" + relationTupleKey(second)

  }
  def relgramKey(farg1:String, frel:String, farg2:String,
                 sarg1:String, srel:String, sarg2:String):String =  {
    relationTupleKey(farg1, frel, farg2) + "\t" + relationTupleKey(sarg1, srel, sarg2)
  }
}
class RelgramsExtractor(maxWindow:Int, equality:Boolean, noequality:Boolean, multipleEquality:Boolean, skipSentences:Boolean) {

  import RelgramsExtractor._

  val logger = LoggerFactory.getLogger(this.getClass)


  def updateArgCounts(argCounts: ArgCounts,
                      firstArg1Head: String, firstArg2Head: String,
                      secondArg1Head: String, secondArg2Head: String){
    updateCounts(argCounts.firstArg1Counts, firstArg1Head, 1)
    updateCounts(argCounts.firstArg2Counts, firstArg2Head, 1)
    updateCounts(argCounts.secondArg1Counts, secondArg1Head, 1)
    updateCounts(argCounts.secondArg2Counts, secondArg2Head, 1)
  }


  def addToSentences(relationTuple: RelationTuple, sentence: String) {
    if(relationTuple.sentences.size < 5) relationTuple.sentences += sentence

  }

  def addToIds(tuple: RelationTuple, id: String){
    if(tuple.ids.size < 5) tuple.ids += id
  }


  def rtcKey(record:TypedTuplesRecord, relKey:String) = relKey + "_:_" + record.sentid + "-" + record.extrid
  def splitRTCKey(rtcKey:String) = rtcKey.split("_:_")

  def createOrGetRelationTuple(arg1:String, rel:String, arg2:String,
                               record:TypedTuplesRecord,
                               sentences:mutable.Set[String],
                               ids:mutable.Set[String],
                               relationTuplesMap:mutable.HashMap[String, RelationTuple],
                               rtcountsMap:mutable.HashMap[String, RelationTupleCounts]) = {
    val relKey = relationTupleKey(arg1, rel, arg2)
    def newCountsMap = new mutable.HashMap[String, Int]
    val relationTuple = relationTuplesMap.get(relKey) match {
      case Some(relationTuple:RelationTuple) => relationTuple
      case None => {
        val relationTuple = new RelationTuple(arg1, rel, arg2, record.hashes, sentences, ids, newCountsMap, newCountsMap)
        relationTuplesMap += relKey -> relationTuple
        relationTuple
      }
    }


    val rtc = rtcountsMap.getOrElseUpdate(rtcKey(record, relKey), new RelationTupleCounts(relationTuple, 0))
    rtc.count = rtc.count + 1

    addToArgCounts(relationTuple.arg1HeadCounts, record.arg1Head)
    addToArgCounts(relationTuple.arg2HeadCounts, record.arg2Head)
    if(!skipSentences) addToSentences(relationTuple, record.sentence)
    addToIds(relationTuple, record.docid + "-" + record.sentid + "-" + record.extrid)
    relationTuple
  }


  def subsumes(first: TypedTuplesRecord, second: TypedTuplesRecord) = {
      def sameArgs(x:TypedTuplesRecord, y:TypedTuplesRecord)     = x.arg1.equalsIgnoreCase(y.arg1) && x.arg2.equalsIgnoreCase(y.arg2)
      def switchedArgs(x:TypedTuplesRecord, y:TypedTuplesRecord) = x.arg1.equalsIgnoreCase(y.arg2) && x.arg2.equalsIgnoreCase(y.arg1)
      def subsumedArgs(x:TypedTuplesRecord, y:TypedTuplesRecord) = x.subsumesOrSubsumedBy(y)
      val same = sameArgs(first, second)
      val switched = switchedArgs(first, second)
      val subsumed = subsumedArgs(first, second)

      same || switched || subsumed
  }


  def isTypedTuple(tuple:RelationTuple) = tuple.arg1.startsWith("Type:") || tuple.arg2.startsWith("Type:")

  def extractRelgramsFromDocument(document:TuplesDocumentWithCorefMentions, maxSentenceLength:Int): (Map[String, RelgramCounts], Map[String, RelationTupleCounts]) = {

    var relgramCountsMap = new mutable.HashMap[String, RelgramCounts]()
    var relationTuplesMap = new mutable.HashMap[String, RelationTuple]()

    def assignIndex(records:Seq[TypedTuplesRecord]) = {
      var index = 0
      var prevSentId = 0
      var prevExtrId = 0
      records.sortBy(r => (r.sentid, r.extrid))
             .map(record => {
        if (record.sentid > prevSentId || record.extrid > prevExtrId) index = index + 1
        prevSentId = record.sentid
        prevExtrId = record.extrid
        (record, index)
      })
    }
    val prepositions = Set[String] ("in", "on", "at", "over", "by", "after", "near", "of")
    def hasPreposition(rel:String) = rel.split(" ").find(x => prepositions.contains(x))
    def hasInferredPrepRelation(record:TypedTuplesRecord) = hasPreposition(record.relHead) match {
      case Some(x:String) => !record.sentence.split(" ").contains(x)
      case None => false
    }
    val prunedRecords:Seq[(TypedTuplesRecord, Int)] = assignIndex(document.tuplesDocument
                                                                          .tupleRecords
                                                                          .filter(record => !hasInferredPrepRelation(record)))

    val mentions = document.mentions
    val sentencesWithOffsets = if(equality){
    val trimdocument = TuplesDocumentGenerator.trimDocument(document.tuplesDocument, maxSentenceLength)
      TuplesDocumentGenerator.sentenceIdsWithOffsets(trimdocument)._2
    }else{
      mutable.Map[Int, Int]()
    }
    def getRecordsIterator = prunedRecords.seq

    val argRepCache = new mutable.HashMap[(Int, Int), (Set[String], Set[String])]()
    def argRepresentations(record:TypedTuplesRecord) = {
      argRepCache.getOrElseUpdate((record.sentid, record.extrid), argRepresentationsNoCache(record))
    }
    def argRepresentationsNoCache(record:TypedTuplesRecord) = {
      //def isNotQuantityPartOrGroupType(arg:String) = !arg.startsWith("type") || (!arg.contains("quantity") && !arg.contains("part") && !arg.contains("group"))
      def lowerCaseAndFilter(args:Iterable[String]) = args.map(arg => arg.toLowerCase)
                                                      .filter(arg => !arg.trim.isEmpty)



      def isTimeUnitOrPeriod(args:Iterable[String]) = args.contains("type:time_unit") || args.contains("type:time_period")
      def ifTimeUnitOrPeriodRemoveNumber(args:Iterable[String]) = if(isTimeUnitOrPeriod(args)) args.filter(arg => !arg.equals("type:number")) else args
      (ifTimeUnitOrPeriodRemoveNumber(lowerCaseAndFilter(record.arg1Head::Nil ++ record.arg1Types)).toSet,
        ifTimeUnitOrPeriodRemoveNumber(lowerCaseAndFilter(record.arg2Head::Nil ++ record.arg2Types)).toSet)
    }
    def startingOffset(record:TypedTuplesRecord) = sentencesWithOffsets.getOrElse(record.sentid, -1000000)

    def pruneMentions(mentions: Map[Mention, List[Mention]]): Map[Mention, List[Mention]] = {
      mentions.map(kv => kv._1 -> kv._2.filter(mention => mention.text.split(" ").size <= 5))
    }

    val prunedMentions = pruneMentions(mentions)
    var rtcountsMap = new mutable.HashMap[String, RelationTupleCounts]()
    val recordRelationTuples = new mutable.HashMap[(Int, Int), mutable.HashSet[RelationTuple]]()
    getRecordsIterator.foreach(index => {
      val record = index._1
      val (arg1s, arg2s) = argRepresentations(record)
      var sentences = new mutable.HashSet[String]()
      var ids = new mutable.HashSet[String]()
      val rel = record.relHead
      arg1s.foreach(arg1 => {
        arg2s.foreach(arg2 => {
          recordRelationTuples.getOrElseUpdate((record.sentid, record.extrid), new mutable.HashSet[RelationTuple]()) += createOrGetRelationTuple(arg1, rel, arg2, record, sentences, ids, relationTuplesMap, rtcountsMap)
        })
      })
    })
    def areFromDifferentExtractions(one:TypedTuplesRecord,
                                    other:TypedTuplesRecord) = ((one.sentid != other.sentid) || (one.extrid != other.extrid))
    def isValidOuterInnerPair(oi:(TypedTuplesRecord, Int), ii:(TypedTuplesRecord, Int)) = {
      val inner = ii._1
      val outer = oi._1
      val iindex = ii._2
      val oindex = oi._2
      areFromDifferentExtractions(outer, inner) &&
      (iindex > oindex) && (iindex <= oindex+maxWindow) &&
      ((outer.sentid != inner.sentid) || !subsumes(outer, inner))
    }
    getRecordsIterator.foreach(outerIndex => {

      val outer = outerIndex._1
      val oindex = outerIndex._2
      val outerStartOffset = if(equality) startingOffset(outer) else 0
      val outerRelationTuples = recordRelationTuples.get((outer.sentid, outer.extrid)).get
      val addedSeconds = new mutable.HashSet[String]()

      getRecordsIterator.filter(innerIndex => isValidOuterInnerPair(outerIndex, innerIndex))
                        .foreach(innerIndex => {
        val iindex = innerIndex._2
        val countWindow = iindex-oindex
        val inner = innerIndex._1
        val corefArgs:Option[(String, String, String, String)] = equality match {
          case true => {
            val innerStartOffset = startingOffset(inner)
            if(!multipleEquality)
              CoreferringArguments.coreferringArgs(outer, outerStartOffset, inner, innerStartOffset, prunedMentions)
            else
              CoreferringArguments.coreferringArgs_multiple(outer, outerStartOffset, inner, innerStartOffset, prunedMentions)
          }
          case false => None
        }
        val innerRelationTuples = recordRelationTuples.get((inner.sentid, inner.extrid)).get

        outerRelationTuples.foreach(first => {
          innerRelationTuples.foreach(second => {
        //import edu.washington.cs.knowitall.relgrams.utils.Crossable._
        //val firstSeconds = (outerRelationTuples x innerRelationTuples)
          //firstSeconds.foreach(firstsecond => {
          //val first = firstsecond._1
          //val second = firstsecond._2

          noequality match {
            case true => {
              val relKey = relgramKey(first, second)
              if (!addedSeconds.contains(relKey)) {
                addToRelgramCounts(outer, inner, first, second, countWindow, relgramCountsMap)
              }
              addedSeconds += relKey
            }
            case false =>
          }

          corefArgs match {
            case Some(resolvedArgs:(String, String, String, String)) => {
              if(!multipleEquality){
                extractEqualityRelgrams_single(resolvedArgs, outer, inner, first, second, relationTuplesMap, relgramCountsMap, rtcountsMap, countWindow, addedSeconds)
              }else {
                extractEqualityRelgrams_multiple(resolvedArgs, outer, inner, first, second, relationTuplesMap, relgramCountsMap, rtcountsMap, countWindow, addedSeconds)
              }
            }
            case None =>
          }

        }) //inner
        }) //outer

      })
    })
    val outputRelationCountsMap = new mutable.HashMap[String, RelationTupleCounts]()
    rtcountsMap.foreach(kv => {
      val splits = splitRTCKey(kv._1)
      outputRelationCountsMap.get(splits(0)) match {
        case Some(x:RelationTupleCounts) => {
          x.count += kv._2.count
        }
        case None => outputRelationCountsMap += splits(0) -> kv._2
      }
    })
    //println("Counts\t%s\t%d\t%d\t%d\t%d".format(document.tuplesDocument.docid, relgramCountsMap.keys.size, outputRelationCountsMap.keys.size, document.tuplesDocument.tupleRecords.size, getRecordsIterator.size))
    (relgramCountsMap.toMap, outputRelationCountsMap.toMap)

  }


  def addToRelgramCounts(outer:TypedTuplesRecord, inner:TypedTuplesRecord,
                         first:RelationTuple, second:RelationTuple,
                         countWindow:Int,
                         relgramCountsMap: HashMap[String, RelgramCounts]) {

    val rgc = relgramCountsMap.getOrElseUpdate(relgramKey(first, second),
      new RelgramCounts(new Relgram(first, second),
                        new scala.collection.mutable.HashMap[Int, Int],
                        ArgCounts.newInstance))
    updateCounts(rgc.counts, countWindow, 1)
    updateArgCounts(rgc.argCounts, outer.arg1Head, outer.arg2Head, inner.arg1Head, inner.arg2Head)
  }


  def extractEqualityRelgrams_multiple(resolvedArgs: (String, String, String, String),
                              outer: TypedTuplesRecord, inner: TypedTuplesRecord,
                              first: RelationTuple, second: RelationTuple,
                              relationTuplesMap:HashMap[String, RelationTuple],
                              relgramCountsMap: HashMap[String, RelgramCounts],
                              rtcountsMap:mutable.HashMap[String, RelationTupleCounts],
                              countWindow: Int,
                              addedSeconds:mutable.HashSet[String]){



    val infa1 = resolvedArgs._1
    val infa2 = resolvedArgs._2
    val insa1 = resolvedArgs._3
    val insa2 = resolvedArgs._4

    def isVar(string: String) = string.startsWith(CoreferringArguments.XVAR) || string.startsWith(CoreferringArguments.YVAR)

    def argRep(resolved:String, unresolved:String) = isVar(resolved) match {
      case true => resolved + ':' + unresolved
      case _ => unresolved
    }

    val fa1 = argRep(infa1, first.arg1)
    val fa2 = argRep(infa2, first.arg2)
    val sa1 = argRep(insa1, second.arg1)
    val sa2 = argRep(insa2, second.arg2)

    def isTypedVar(resolvedArg:String) = {
      val splits = resolvedArg.split(":")
      splits.size == 3 && splits(1).equalsIgnoreCase("Type")
    }
    val farg = if(isTypedVar(fa1)) fa1.split(":")(2) else if (isTypedVar(fa2)) fa2.split(":")(2) else "NA"
    val sarg = if(isTypedVar(sa1)) sa1.split(":")(2) else if (isTypedVar(sa2)) sa2.split(":")(2) else "NA"
    if (!(farg.equals("NA") || sarg.equals("NA")) && !(farg.equals(sarg))){
      return
    }

    val secondKey = relgramKey(fa1, outer.relHead, fa2, sa1, inner.relHead, sa2)
    if (addedSeconds.contains(secondKey)) {
      return
    }
    addedSeconds += secondKey

    val firstSentences = new mutable.HashSet[String]
    firstSentences ++= first.sentences

    val firstids = new mutable.HashSet[String]
    firstids ++= first.ids
    val firstCoref = createOrGetRelationTuple(fa1, first.rel, fa2, outer, firstSentences, firstids, relationTuplesMap, rtcountsMap)

    val secondSentences = new mutable.HashSet[String]
    secondSentences ++= second.sentences

    val secondids = new mutable.HashSet[String]
    secondids ++= second.ids
    val secondCoref = createOrGetRelationTuple(sa1, second.rel, sa2, inner, secondSentences, secondids, relationTuplesMap, rtcountsMap)

    val corefRgc = relgramCountsMap.getOrElseUpdate(relgramKey(firstCoref, secondCoref),
      new RelgramCounts(new Relgram(firstCoref, secondCoref),
        new scala.collection.mutable.HashMap[Int, Int],
        ArgCounts.newInstance))

    updateCounts(corefRgc.counts, countWindow, 1)
    updateArgCounts(corefRgc.argCounts, outer.arg1Head, outer.arg2Head, inner.arg1Head, inner.arg2Head)
  }


  def extractEqualityRelgrams_single(resolvedArgs: (String, String, String, String),
                              outer: TypedTuplesRecord, inner: TypedTuplesRecord,
                              first: RelationTuple, second: RelationTuple,
                              relationTuplesMap:HashMap[String, RelationTuple],
                              relgramCountsMap: HashMap[String, RelgramCounts],
                              rtcountsMap:mutable.HashMap[String, RelationTupleCounts],
                              countWindow: Int,
                              addedSeconds:mutable.HashSet[String]){



    val infa1 = resolvedArgs._1
    val infa2 = resolvedArgs._2
    val insa1 = resolvedArgs._3
    val insa2 = resolvedArgs._4

    def isType(string: String) = string.startsWith("Type:") || string.startsWith("type:")
    def isVar(string: String) = string.startsWith(CoreferringArguments.XVAR)

    def argRep(resolved:String, unresolved:String) = {
      (isVar(resolved), isType(unresolved)) match {
        case (true, true) => resolved + ':' + unresolved
        case (true, false) => resolved
        case _ => unresolved
      }
    }



    val fa1 = argRep(infa1, first.arg1)
    val fa2 = argRep(infa2, first.arg2)
    val sa1 = argRep(insa1, second.arg1)
    val sa2 = argRep(insa2, second.arg2)

    def isTypedVar(resolvedArg:String) = {
      val splits = resolvedArg.split(":")
      splits.size == 3 && splits(1).equalsIgnoreCase("Type")
    }
    val farg = if(isTypedVar(fa1)) fa1.split(":")(2) else if (isTypedVar(fa2)) fa2.split(":")(2) else "NA"
    val sarg = if(isTypedVar(sa1)) sa1.split(":")(2) else if (isTypedVar(sa2)) sa2.split(":")(2) else "NA"
    if (!(farg.equals("NA") || sarg.equals("NA")) && !(farg.equals(sarg))){
      return
    }

    val secondKey = relgramKey(fa1, outer.relHead, fa2, sa1, inner.relHead, sa2)
    if (addedSeconds.contains(secondKey)) {
      return
    }
    addedSeconds += secondKey

    val firstSentences = new mutable.HashSet[String]
    firstSentences ++= first.sentences

    val firstids = new mutable.HashSet[String]
    firstids ++= first.ids
    val firstCoref = createOrGetRelationTuple(fa1, first.rel, fa2, outer, firstSentences, firstids, relationTuplesMap, rtcountsMap)

    val secondSentences = new mutable.HashSet[String]
    secondSentences ++= second.sentences

    val secondids = new mutable.HashSet[String]
    secondids ++= second.ids
    val secondCoref = createOrGetRelationTuple(sa1, second.rel, sa2, inner, secondSentences, secondids, relationTuplesMap, rtcountsMap)

    val corefRgc = relgramCountsMap.getOrElseUpdate(relgramKey(firstCoref, secondCoref),
      new RelgramCounts(new Relgram(firstCoref, secondCoref),
                        new scala.collection.mutable.HashMap[Int, Int],
                        ArgCounts.newInstance))

    updateCounts(corefRgc.counts, countWindow, 1)
    updateArgCounts(corefRgc.argCounts, outer.arg1Head, outer.arg2Head, inner.arg1Head, inner.arg2Head)
  }

  def addToArgCounts(counts: mutable.Map[String, Int], headValue: String) {
    val count = counts.getOrElseUpdate(headValue, 0)
    counts += headValue -> (count + 1)
  }

}
object RelgramsExtractorTest{

  def main(args:Array[String]){

    var i = 0
    def nextArg:String = {
      val arg = args(i)
      i = i + 1
      arg
    }
    val typeRecordsFile = nextArg
    val window = nextArg.toInt
    val relgramsFile = nextArg
    val tuplesFile = nextArg
    val tupleDocumentsWithCorefs = Source.fromFile(typeRecordsFile)
                             .getLines()
                             .flatMap(x => TuplesDocumentWithCorefMentions.fromString(x))
    val relgramsExtractor = new RelgramsExtractor(window, equality=false, noequality=true, multipleEquality=false, skipSentences=false)
    def relgramsString (relgramCount:RelgramCounts):String = {
      "First\t" + relgramCount.relgram.first.prettyString + "\nSecond\t" + relgramCount.relgram.second.prettyString + "\nCounts\t" + MapUtils.toCountsString(relgramCount.counts, ",", ":") + "\n\n"
    }

    val relgramsWriter = new PrintWriter(relgramsFile, "utf-8")
    val tuplesWriter = new PrintWriter(tuplesFile, "utf-8")
    tupleDocumentsWithCorefs.foreach(tdm => {
      val (relgramCounts, tuplesCounts) = relgramsExtractor.extractRelgramsFromDocument(tdm, maxSentenceLength=75)
      println("RelgramCounts size: " + relgramCounts.keys.size)
      println("Tuples size: " + tuplesCounts.keys.size)
      relgramCounts.foreach(rgcKV => relgramsWriter.println(relgramsString(rgcKV._2)))//prettyString))
      tuplesCounts.foreach(tupleCount => tuplesWriter.println(tupleCount._2.tuple.prettyString + "\t" + tupleCount._2.count))
    })
    relgramsWriter.close()
    tuplesWriter.close()
  }
}
