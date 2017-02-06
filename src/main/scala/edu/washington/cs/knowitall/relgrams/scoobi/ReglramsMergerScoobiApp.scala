package edu.washington.cs.knowitall.relgrams.scoobi

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/4/13
 * Time: 9:38 AM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import com.nicta.scoobi.application.ScoobiApp
import scopt.mutable.OptionParser
import com.nicta.scoobi.io.text.{TextOutput, TextInput}
import edu.washington.cs.knowitall.relgrams.{RelgramsCounter, RelgramCounts}
import edu.washington.cs.knowitall.relgrams.RelationTupleCounts
import java.io.File
import com.nicta.scoobi.core.DList

import com.nicta.scoobi.Persist._


object ReglramsMergerScoobiApp extends ScoobiApp{

  val logger = LoggerFactory.getLogger(this.getClass)

  def run() {

    var inputRelgramsPath, outputPath = ""
    var maxWindow = 10
    var maxSize = 5
    var tuples = false
    var relgrams = false
    var combine = false
    var minDirFreq = 0
    val parser = new OptionParser() {
      arg("inputRelgramsPath", "hdfs input path", {str => inputRelgramsPath = str})
      arg("outputPath", "hdfs output path", { str => outputPath = str })
      opt("minDirFreq", "min freq.", {str => minDirFreq = str.toInt})
      opt("maxWindow", "max window size", {str => maxWindow = str.toInt})
      opt("maxSize", "max size for id's arg head values etc.", {str => maxSize = str.toInt})
      opt("tuples", "Tuples merge?", {str => tuples = str.toBoolean})
      opt("relgrams", "Relgrams merge?", {str => relgrams = str.toBoolean})
      opt("combine", "Use combiner?", {str => combine = str.toBoolean})
    }
    if (!parser.parse(args)) return

    if (relgrams){
      val groupedRelgrams = loadRelgramCountsGroupedByKey(inputRelgramsPath)
      val reduced = reduceGroupedRelgramCounts(maxSize, groupedRelgrams)
      exportRelgrams(outputPath, reduced)
    }
    if (tuples){
      val groupedTuples: DList[(String, Iterable[RelationTupleCounts])] = loadTupleCountsGroupedByKey(inputRelgramsPath)
      val reduced = reduceGroupedTupleCounts(maxSize, groupedTuples)
      exportTuples(outputPath, reduced)
    }
  }
  def reduceGroupedTupleCounts(maxSize:Int, groupedTuples:DList[(String, Iterable[RelationTupleCounts])]) = {
    import RelationTupleCounts._
    val counter = new RelgramsCounter(maxSize)
    groupedTuples.flatMap(kv => counter.reduceTupleCounts(kv._2))
  }

  def loadTupleCountsGroupedByKey(inputPath:String) = {
    import RelationTupleCounts._
    import edu.washington.cs.knowitall.relgrams.RelgramsExtractor._
    TextInput.fromTextFile(inputPath).flatMap(line =>
      fromSerializedString(line) match {
        case Some(tupleCount:RelationTupleCounts) => Some((relationTupleKey(tupleCount.tuple), tupleCount))
        case _ => None
      }
    ).groupByKey[String, RelationTupleCounts]
  }
  def exportTuples(outputPath: String, tuples: DList[RelationTupleCounts]) {
    try {
      val tuplesPath = outputPath + File.separator + "tuples"
      persist(TextOutput.toTextFile(tuples.map(tuple => tuple.toString()), tuplesPath))
    } catch {
      case e: Exception => logger.error(e.getStackTraceString)
    }
  }



  def reduceGroupedRelgramCounts(maxSize: Int, groupedRelgrams: DList[(String, Iterable[RelgramCounts])]) = {
    val counter = new RelgramsCounter(maxSize)
    val reduced = groupedRelgrams.map(kv => counter.reduce(kv._2))
    reduced
  }

  def loadRelgramCountsGroupedByKey(inputRelgramsPath: String) = {
    import RelgramCounts._
    import edu.washington.cs.knowitall.relgrams.RelgramsExtractor._
    TextInput.fromTextFile(inputRelgramsPath).flatMap(line => {
      fromSerializedString(line) match {
        case Some(rgc: RelgramCounts) => {
          Some((relgramKey(rgc.relgram.first, rgc.relgram.second), rgc))
        }
        case _ => None
      }
    }).groupByKey[String, RelgramCounts]
  }

  def exportRelgrams(outputPath: String, reduced: DList[RelgramCounts]) {
    try {
      val relgramsPath = outputPath + File.separator + "relgrams"
      persist(TextOutput.toTextFile(reduced.map(rgc => rgc.serialize), relgramsPath))
    } catch {
      case e: Exception => logger.error(e.getStackTraceString)
    }
  }
}
