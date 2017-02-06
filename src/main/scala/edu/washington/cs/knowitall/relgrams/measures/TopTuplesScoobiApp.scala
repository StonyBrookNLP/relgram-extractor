package edu.washington.cs.knowitall.relgrams.measures

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/3/13
 * Time: 12:09 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import com.nicta.scoobi.core.DList
import edu.washington.cs.knowitall.relgrams._
import com.nicta.scoobi.io.text.{TextOutput, TextInput}
import scopt.mutable.OptionParser
import edu.washington.cs.knowitall.relgrams.utils.{RegexUtils, Pronouns, MapUtils}

import com.nicta.scoobi.Persist._
import java.io.File
import com.nicta.scoobi.application.ScoobiApp
import scopt.mutable.OptionParser
import scala.Some

object TopTuplesScoobiApp extends ScoobiApp{

  val logger = LoggerFactory.getLogger(this.getClass)

  def maxOrElse(values:Iterable[Int], orElse:Int) = if(values.isEmpty) orElse else values.max
  def aboveThreshold(measures: Measures, minFreq:Int): Boolean = maxOrElse(measures.urgc.bitermCounts.values, 0) > minFreq
  def frequencies(measures:Measures) = "%d\t%d\t%d\t%d".format(maxOrElse(measures.urgc.bitermCounts.values, 0),maxOrElse(measures.urgc.rgc.counts.values, 0), measures.firstCounts, measures.secondCounts)
  def countsAreValid(measures: Measures) = {
    val maxBitermCount = maxOrElse(measures.urgc.bitermCounts.values, 0)
    val maxBigramCount = maxOrElse(measures.urgc.rgc.counts.values, 0)
    val out = maxBitermCount <= measures.firstCounts && maxBitermCount <= measures.secondCounts && maxBigramCount <= measures.firstCounts && maxBigramCount <= measures.secondCounts
    if(!out) {
      try {
      logger.error("Invalid counts for relgram: " + measures.urgc.rgc.relgram.prettyString + "\t" + frequencies(measures))
      } catch {
        case e:Exception => "Caught exception logging invalid counts."
      }
    }
    out
  }


  def scpAndConditionals(inputPath:String, minFreq:Int, windowAlpha:Double, smoothingDelta:Double, notypes:Boolean = false) = {
    TextInput.fromTextFile(inputPath)
      .filter(line => (!notypes || !line.contains("type:")))
      .flatMap(line => Measures.fromSerializedString(line) match {
      case Some(measures:Measures) => {
        if (aboveThreshold(measures, minFreq) && countsAreValid(measures)){
          AffinityMeasures.fromMeasures(measures, windowAlpha, smoothingDelta) match {
            case Some(affinities:AffinityMeasures) => {
              val s_given_f = affinities.firstUndir.conditional
              val f_given_s = affinities.secondUndir.conditional
              if(s_given_f < 0) println("s_given_f == " + s_given_f + " for: " + measures.urgc.toString)
              val scp = s_given_f * f_given_s
              Some((measures, scp, s_given_f, f_given_s))
            }
            case _ => None
          }
        }else None
      }
      case _ => None
    })
  }

  def isValidTuple(tuple:RelationTuple) = {
    def isPronoun(string:String) = Pronouns.isPronoun(string)
    !isPronoun(tuple.arg1) &&
      !isPronoun(tuple.arg2) &&
      !RegexUtils.hasSpecialCharsExcept(":")(tuple.rel)
  }
  def topkTotals(scpMeasures:DList[(Measures, Double, Double, Double)], topk:Int, minRelgrams:Int): DList[(Int, Int, Double, Double, Double, String)] = {
    import Measures._
    val groupedByFirst: DList[(String, Iterable[(Measures, Double, Double, Double)])] = scpMeasures.map(mtuple => (mtuple._1.urgc.rgc.relgram.first.tabSeparatedTuple, mtuple))
                                    .groupByKey[String, (Measures, Double, Double, Double)]
    groupedByFirst.flatMap(g => {
      val gseq = g._2.toSeq
      val filteredGSeq = gseq.filter(y => isValidTuple(y._1.urgc.rgc.relgram.second))
      if (gseq.size >= minRelgrams) {
        val scpsum = gseq.map(x => x._2).sortBy(scp => -scp).take(topk).sum
        val psfsum = gseq.map(x => x._3).sortBy(psf => -psf).take(topk).sum
        val pfssum = gseq.map(x => x._4).sortBy(pfs => -pfs).take(topk).sum
        Some((gseq.size, filteredGSeq.size, scpsum, psfsum, pfssum, g._1))
      }else{
        None
      }
    })
  }
  def exportScoredTuples(scoredTuples:DList[(Int, Int, Double, Double, Double, String)], outputPath:String) {
    try{
      persist(TextOutput.toTextFile(scoredTuples.map(st => st._1 + "\t" + st._2 + "\t" + st._3 + "\t" + st._4 + "\t" + st._5 + "\t" + st._6), outputPath))
    }catch{
      case e:Exception => logger.error("Failed to persist tuples: " + e.getStackTraceString)
    }
  }

  def exportSCPRelgrams(scplist:DList[(Measures, Double, Double, Double)], outputPath:String) {
    try{
      def relgramString(measures:Measures) = measures.urgc.rgc.relgram.first.tabSeparatedTuple + "\t" + measures.urgc.rgc.relgram.second.tabSeparatedTuple + frequencies(measures)
      persist(TextOutput.toTextFile(scplist.map(s => s._2 + "\t" + s._3 + "\t" + s._4 + "\t" + relgramString(s._1)), outputPath))
    }catch{
      case e:Exception => logger.error("Failed to persist tuples: " + e.getStackTraceString)
    }
  }



  override def run() {

    var inputPath, outputPath = ""
    var windowAlpha = 0.9
    var smoothingDelta = 1000.0
    var minFreq = 0
    var topk = 0
    var minRelgrams = 0
    var notypes = false
    val parser = new OptionParser() {
      arg("inputPath", "hdfs input path", {str => inputPath = str})
      arg("outputPath", "hdfs output path", { str => outputPath = str})
      opt("minFreq", "minimum frequency for relgram.", {str => minFreq = str.toInt})
      opt("windowAlpha", "alpha decay for combining measures from different windows.", {str => windowAlpha = str.toDouble})
      opt("smoothingDelta", "smoothing delta.", {str => smoothingDelta = str.toDouble})
      opt("topk", "Top K relgrams to use in computing sums for scoring tuples.", {str => topk = str.toInt})
      opt("minRelgrams", "Retain only those tuples that have at least this number of rel-grams with minFreq.", {str => minRelgrams = str.toInt})

      opt("notypes", "Ignore measures with types.", {str => notypes = str.toBoolean})

    }

    if (!parser.parse(args)) return

    val scplist = scpAndConditionals(inputPath, minFreq, windowAlpha, smoothingDelta, notypes)
    //exportSCPRelgrams(scplist, outputPath + File.separator + "withscp")

    println("Topk relgrams to use for computing scores: " + topk)
    println("Filtering tuples that don't contain %d relgrams.".format(minRelgrams))
    val scoredTuples = topkTotals(scplist, topk, minRelgrams)

    exportScoredTuples(scoredTuples, outputPath + File.separator + "toptuples")

  }


  def measuresGroupedByConditional(inputPath: String, minFreq: Int, windowAlpha: Double, smoothingDelta: Double): DList[(Double, Iterable[Measures])] = {
    val groupedMeasures = TextInput.fromTextFile(inputPath)
      .flatMap(line => Measures.fromSerializedString(line) match {
      case Some(measures: Measures) => {
        if (aboveThreshold(measures, minFreq) && countsAreValid(measures)){
          AffinityMeasures.fromMeasures(measures, windowAlpha, smoothingDelta) match {
            case Some(affinities: AffinityMeasures) => {
              Some((affinities.firstUndir.conditional, measures))
            }
            case _ => None
          }
        } else None
      }
      case _ => None
    }).groupByKey[Double, Measures]
    groupedMeasures
  }

  //Dummy comment.
  def export(groupedMeasures: DList[(Double, Iterable[Measures])], outputPath: String){
    import Measures._
    def flatten(gm:(Double, Iterable[Measures])) = {
      def relgramString(relgram:Relgram) = relgram.first.tabSeparatedTuple + "\t"  + relgram.second.tabSeparatedTuple
      def maxOrElse(values:Iterable[Int], orElse:Int) = if (!values.isEmpty) values.max else orElse
      def frequencies(measure:Measures) = maxOrElse(measure.urgc.bitermCounts.values, 0) + "\t" +
                                          maxOrElse(measure.urgc.rgc.counts.values, 0) + "\t" +
                                          measure.firstCounts + "\t" +
                                          measure.secondCounts
      gm._2.map(measure => gm._1 + "\t" + relgramString(measure.urgc.rgc.relgram) + "\t" + frequencies(measure))
    }
    try{
      persist(TextOutput.toTextFile(groupedMeasures.flatMap(gm => flatten(gm)), outputPath))
    }catch{
      case e:Exception => {
        println("Failed to persist reduced tuples.")
        e.printStackTrace
      }
    }
  }

}
