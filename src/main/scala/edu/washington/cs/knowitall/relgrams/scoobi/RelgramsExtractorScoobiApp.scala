package edu.washington.cs.knowitall.relgrams.scoobi

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/18/13
 * Time: 7:05 PM
 * To change this template use File | Settings | File Templates.
 */


import com.nicta.scoobi.application.ScoobiApp
import com.nicta.scoobi.io.text.{TextOutput, TextInput}
import com.nicta.scoobi.core.DList

import com.nicta.scoobi.Persist._
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.relgrams._
import scopt.mutable.OptionParser
import java.io.File
import io.Source


object RelgramsExtractorScoobiApp extends ScoobiApp{

  import TypedTuplesRecord._
  import RelgramCounts._


  val logger = LoggerFactory.getLogger(this.getClass)
  var extractor:RelgramsExtractor = null
  var counter:RelgramsCounter = null

  def exportTupleDocuments(documents: DList[TuplesDocumentWithCorefMentions], outputPath: String){
    try{
      val relgramsPath = outputPath + File.separator + "tdocs"
      persist(TextOutput.toTextFile(documents.map(doc => doc.toString), relgramsPath))
    }catch{
      case e:Exception => {
        println("Failed to persist reduced relgrams.")
        e.printStackTrace
      }

    }
  }

  def exportRelgrams(relgramCounts: DList[RelgramCounts], outputPath: String) {
    try{
      val relgramsPath = outputPath + File.separator + "relgrams"
      println("Relgrams path:  " + relgramsPath)
      persist(TextOutput.toTextFile(relgramCounts.map(x => x.serialize), relgramsPath))
    }catch{
      case e:Exception => {
        println("Failed to persist reduced relgrams.")
        e.printStackTrace
      }

    }
  }

  def exportRelgramsWithKeys(relgramCountsWithKeys: DList[(String, RelgramCounts)], minDirFreq:Int, outputPath: String){
    try{
      val relgramsPath = outputPath + File.separator + "relgrams"
      persist(TextOutput.toTextFile(relgramCountsWithKeys.filter(x => x._2.counts.values.max > minDirFreq).map(x => x._2.serialize), relgramsPath))
    }catch{
      case e:Exception => {
        println("Failed to persist reduced relgrams.")
        e.printStackTrace
      }

    }
  }



  def exportTuples(tupleCounts: DList[RelationTupleCounts], outputPath: String){
    try{
      val tuplesPath = outputPath + File.separator + "tuples"
      println("tuples path: " + tuplesPath)
      persist(TextOutput.toTextFile(tupleCounts.map(x => x.toString()), tuplesPath))
    }catch{
      case e:Exception => {
        println("Failed to persist reduced tuples.")
        e.printStackTrace
      }

    }
  }

  def exportTuplesWithKeys(tupleCountsWithKeys: DList[(String, RelationTupleCounts)], outputPath: String){
    try{
      val tuplesPath = outputPath + File.separator + "tuples"
      persist(TextOutput.toTextFile(tupleCountsWithKeys.map(x => x._2.toString()), tuplesPath))
    }catch{
      case e:Exception => {
        println("Failed to persist reduced tuples.")
        e.printStackTrace
      }

    }
  }

  def combineRelgramCounts(rgcs:DList[Map[String, RelgramCounts]], maxSize:Int) = {
    val counter = new RelgramsCounter(maxSize)
    def rgcsCombine(x:RelgramCounts, y:RelgramCounts): RelgramCounts = counter.combineRelgramCounts(x, y)
    rgcs.flatten
        .groupByKey[String, RelgramCounts]
        .combine[String, RelgramCounts](rgcsCombine)
  }

  def combineTuples(tupleCounts:DList[Map[String, RelationTupleCounts]], maxSize:Int) = {
    val counter = new RelgramsCounter(maxSize)
    def tuplesCombine(x:RelationTupleCounts, y:RelationTupleCounts) = counter.combineTupleCounts(x, y)
    tupleCounts.flatten
               .map(x => (x._1, new RelationTupleCounts(x._2.tuple, x._2.count)))
               .groupByKey[String, RelationTupleCounts]
               .combine[String, RelationTupleCounts](tuplesCombine)
  }

  def extractRelgramCountsAndTuples(tuplesDocuments: DList[TuplesDocumentWithCorefMentions], maxSentenceLength:Int,
                                    maxWindow:Int,
                                    equality:Boolean,
                                    noequality:Boolean,
                                    multipleEquality:Boolean,
                                    skipSentences:Boolean): Option[DList[(Map[String, RelgramCounts], Map[String, RelationTupleCounts])]] ={

    import TuplesDocumentWithCorefMentions._
    import RelgramCounts._
    import RelationTuple._
    val relgrams: DList[(Map[String, RelgramCounts], Map[String, RelationTupleCounts])] = tuplesDocuments.map(document => {
      val extractor = new RelgramsExtractor(maxWindow, equality, noequality, multipleEquality, skipSentences)
      extractor.extractRelgramsFromDocument(document, maxSentenceLength)
    })
    Some(relgrams)
  }

  def reduceRelgramCounts(relgramCounts: DList[Map[String, RelgramCounts]],
                          maxSize:Int,
                          minFreq:Int): DList[RelgramCounts] = {
    reduceFlattenedRelgramCounts(relgramCounts.flatten, maxSize, minFreq)
    /**relgramCounts.flatten
                 .groupByKey[String, RelgramCounts]
                 .flatMap(x => counter.reduceRelgramCounts(x._2, minFreq))*/
  }
  def reduceFlattenedRelgramCounts(flattened:DList[(String, RelgramCounts)],
                                maxSize:Int,
                                minFreq:Int): DList[RelgramCounts] = {
    val counter = new RelgramsCounter(maxSize)
    flattened.groupByKey[String, RelgramCounts]
             .flatMap(x => counter.reduceRelgramCounts(x._2, minFreq))
  }

  def reduceTuplesCounts(tuplesCounts: DList[Map[String, RelationTupleCounts]], maxSize:Int = 5): DList[RelationTupleCounts] = {
    import RelationTupleCounts._
    reduceFlattenedTuplesCounts(tuplesCounts.flatten, maxSize)
    /**val counter = new RelgramsCounter(maxSize)
    tuplesCounts.flatten
      .groupByKey[String, RelationTupleCounts]
      .flatMap(x => counter.reduceTupleCounts(x._2))*/
  }

  def reduceFlattenedTuplesCounts(flattened: DList[(String, RelationTupleCounts)], maxSize:Int = 5): DList[RelationTupleCounts] = {
    import RelationTupleCounts._
    val counter = new RelgramsCounter(maxSize)
    flattened.groupByKey[String, RelationTupleCounts]
             .flatMap(x => counter.reduceTupleCounts(x._2))
  }




  override def run() {

    var inputPath, outputPath = ""
    var maxWindow = 10
    var maxSize = 5
    var equality = false
    var noequality = false
    var skipHashes, skipSentences = false
    var tuplesOnly = false
    var combine = false
    var minDirFreq = 0
    var maxSentenceLength = 75

    var multipleEquality = false

    val parser = new OptionParser() {
      arg("inputPath", "hdfs input path", {str => inputPath = str})
      arg("outputPath", "hdfs output path", { str => outputPath = str })
      opt("maxSentenceLength", "min freq.", {str => maxSentenceLength = str.toInt})
      opt("minDirFreq", "min freq.", {str => minDirFreq = str.toInt})
      opt("maxWindow", "max window size", {str => maxWindow = str.toInt})
      opt("maxSize", "max size for id's arg head values etc.", {str => maxSize = str.toInt})
      opt("equality", "Argument equality tuples.", {str => equality = str.toBoolean})
      opt("noequality", "Count tuples without equality.", {str => noequality = str.toBoolean})

      opt("multipleEquality", "Multiple equality constraints.", {str => multipleEquality = str.toBoolean})

      opt("tuplesOnly", "Tuples extraction only.", {str => tuplesOnly = str.toBoolean})
      opt("skipHashes", "Skip hashes.", {str => skipHashes = str.toBoolean})
      opt("skipSentences", "Skip sentences.", {str => skipSentences = str.toBoolean})
      opt("combine", "Use combiner?", {str => combine = str.toBoolean})

    }

    if (!parser.parse(args)) return

    assert(equality || noequality, "Both equality or noequality flags are false. One of them must be set true.")


    import TuplesDocumentWithCorefMentions._
    println("This is run once....")
    def loadTupleDocuments(inputPath: String):DList[TuplesDocumentWithCorefMentions] = {
      TextInput.fromTextFile(inputPath)
        .flatMap(x => TuplesDocumentWithCorefMentions.fromString(x))
    }
    val tupleDocuments = loadTupleDocuments(inputPath)

    println("This is run twice?")
    combine match {
      case true => extractWithCombine(tupleDocuments, maxSentenceLength, minDirFreq, maxWindow, equality, noequality, multipleEquality,
        tuplesOnly, maxSize, skipHashes, skipSentences, outputPath)
      case false => extract(tupleDocuments, maxSentenceLength, minDirFreq, maxWindow, equality, noequality, multipleEquality,
        tuplesOnly, maxSize, skipHashes, skipSentences, outputPath)
    }

  }

  def extractWithCombine(tupleDocuments: DList[TuplesDocumentWithCorefMentions],
                         maxSentenceLength:Int,
                         minDirFreq:Int, maxWindow: Int,
                         equality: Boolean,
                         noequality: Boolean,
                         multipleEquality:Boolean,
                         tuplesOnly: Boolean, maxSize: Int, skipHashes: Boolean, skipSentences: Boolean, outputPath: String) {
    println("Using combiner!!!!")
    import RelgramCounts._
    extractRelgramCountsAndTuples(tupleDocuments, maxSentenceLength, maxWindow, equality, noequality, multipleEquality, skipSentences) match {
      case Some(extracts: DList[(Map[String, RelgramCounts], Map[String, RelationTupleCounts])]) => {
        if (!tuplesOnly) {
          val reducedRelgramCountsWithKeys = combineRelgramCounts(extracts.map(x => x._1), maxSize)//, skipHashes, skipSentences)
          exportRelgramsWithKeys(reducedRelgramCountsWithKeys, minDirFreq, outputPath)
        }
        val reducedTupleCounts = combineTuples(extracts.map(x => x._2), maxSize)
        exportTuplesWithKeys(reducedTupleCounts, outputPath)

      }
      case None => "Failed relgram extraction."
    }
  }

  def extract(tupleDocuments: DList[TuplesDocumentWithCorefMentions],
              maxSentenceLength:Int,
              minDirFreq:Int,
              maxWindow: Int,
              equality: Boolean,
              noequality: Boolean,
              multipleEquality:Boolean,
              tuplesOnly: Boolean,
              maxSize: Int,
              skipHashes: Boolean, skipSentences: Boolean,
              outputPath: String) {
    import RelgramCounts._
    extractRelgramCountsAndTuples(tupleDocuments, maxSentenceLength, maxWindow, equality, noequality, multipleEquality, skipSentences) match {
      case Some(extracts: DList[(Map[String, RelgramCounts], Map[String, RelationTupleCounts])]) => {
        if (!tuplesOnly) {
          val reducedRelgramCounts = reduceRelgramCounts(extracts.map(x => x._1), maxSize, minDirFreq)//, skipHashes, skipSentences)
          exportRelgrams(reducedRelgramCounts, outputPath)
        }
        val reducedTupleCounts = reduceTuplesCounts(extracts.map(x => x._2), maxSize)
        exportTuples(reducedTupleCounts, outputPath)

      }
      case None => "Failed relgram extraction."
    }
  }
}
