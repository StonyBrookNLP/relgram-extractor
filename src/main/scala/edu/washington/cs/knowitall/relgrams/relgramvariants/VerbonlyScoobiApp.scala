package edu.washington.cs.knowitall.relgrams.relgramvariants

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 6/19/13
 * Time: 10:32 AM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import com.nicta.scoobi.application.ScoobiApp
import scopt.mutable.OptionParser
import edu.washington.cs.knowitall.relgrams._
import com.nicta.scoobi.core.DList
import com.nicta.scoobi.io.text.{TextOutput, TextInput}
import java.io.File
import edu.washington.cs.knowitall.relgrams.utils.RelationTupleUtils
import edu.washington.cs.knowitall.relgrams.scoobi.RelgramsExtractorScoobiApp
import scopt.mutable.OptionParser
import edu.washington.cs.knowitall.relgrams.scoobi.RelgramsExtractorScoobiApp._
import scopt.mutable.OptionParser
import com.nicta.scoobi.Persist._

object VerbonlyScoobiApp extends ScoobiApp {

  val logger = LoggerFactory.getLogger(this.getClass)

  def toVerb(tuple:RelationTuple) {
    RelationTupleUtils.toVerbOption(tuple.rel) match {
      case Some(string:String) => tuple.rel = string
      case None =>
    }
  }


  def verbonlyRelgrams(relgrams:DList[RelgramCounts]): DList[(String, RelgramCounts)] = {
    relgrams.map(rgc => {
      toVerb(rgc.relgram.first)
      toVerb(rgc.relgram.second)
      (RelgramsExtractor.relgramKey(rgc.relgram.first, rgc.relgram.second), rgc)
    })
  }

  def verbonlyTuples(tuples:DList[RelationTupleCounts]): DList[(String, RelationTupleCounts)] = {
    tuples.map(tc => {
      toVerb(tc.tuple)
      (RelgramsExtractor.relationTupleKey(tc.tuple), tc)
    })
  }


  def exportRelgramCounts(relgramCounts:DList[RelgramCounts], outputPath:String) {
    import RelgramCounts._
    persist(TextOutput.toTextFile(relgramCounts.map(x => x.serialize), outputPath))
  }
  def exportRelationTuples(tupleCounts:DList[RelationTupleCounts], outputPath:String) {
    import RelationTupleCounts._
    persist(TextOutput.toTextFile(tupleCounts.map(x => x.toString()), outputPath))
  }

  def run() {

    var inputPath, outputPath = ""
    var windowAlpha = 0.9
    var smoothingDelta = 1000.0
    var minFreq = 5
    var tuplesOnly = false
    var maxWindow = 10
    var maxSize = 5
    var minDirFreq = 3
    val parser = new OptionParser() {
      arg("inputPath", "hdfs input path", {str => inputPath = str})
      arg("outputPath", "hdfs output path", { str => outputPath = str})
      opt("minFreq", "minimum frequency for relgram.", {str => minFreq = str.toInt})
      opt("windowAlpha", "alpha decay for combining measures from different windows.", {str => windowAlpha = str.toDouble})
      opt("smoothingDelta", "smoothing delta.", {str => smoothingDelta = str.toDouble})
      opt("tuplesOnly", "tuplesOnly", {str => tuplesOnly = str.toBoolean})
      opt("maxWindow", "max window size", {str => maxWindow = str.toInt})
      opt("maxSize", "max size for id's arg head values etc.", {str => maxSize = str.toInt})
      opt("minDirFreq", "min freq.", {str => minDirFreq = str.toInt})
    }

    if (!parser.parse(args)) return

    logger.info("Input path: " + inputPath)
    logger.info("Output path: " + outputPath)

    val relgramsPath = inputPath + File.separator + "relgrams"
    logger.info("Relgrams path: " + relgramsPath)
    import RelgramCounts._
    val relgramCounts:DList[RelgramCounts]  = TextInput.fromTextFile(relgramsPath)
                                                       .flatMap(line => RelgramCounts.fromSerializedString(line))

    val verbRelgramCounts = verbonlyRelgrams(relgramCounts)

    val tuplesPath = inputPath + File.separator + "tuples"
    logger.info("Tuples path: " + tuplesPath)
    import RelationTupleCounts._
    val tupleCounts:DList[RelationTupleCounts] = TextInput.fromTextFile(tuplesPath).flatMap(line => RelationTupleCounts.fromSerializedString(line))
    val verbTupleCounts = verbonlyTuples(tupleCounts)

    if (!tuplesOnly) {
      val reducedRelgramCounts = reduceFlattenedRelgramCounts(verbRelgramCounts, maxSize, minDirFreq)//, skipHashes, skipSentences)
      exportRelgramCounts(reducedRelgramCounts, outputPath + File.separator + "relgrams")
    }
    val reducedTupleCounts = reduceFlattenedTuplesCounts(verbTupleCounts, maxSize)
    exportRelationTuples(reducedTupleCounts, outputPath + File.separator + "tuples")


  }
}
