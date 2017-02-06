package edu.washington.cs.knowitall.relgrams

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 10/17/13
 * Time: 10:15 AM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import com.nicta.scoobi.application.ScoobiApp
import com.nicta.scoobi.io.text.{TextOutput, TextInput}
import com.nicta.scoobi.core.DList
import com.nicta.scoobi.Persist._
import scopt.mutable.OptionParser
import edu.washington.cs.knowitall.relgrams.utils.{CollectionUtils, RegexUtils}
import scala.collection

object RelgramsExport extends ScoobiApp{

  val logger = LoggerFactory.getLogger(this.getClass)


  def loadRelgrams(relgramsPath:String,
                   filter:(UndirRelgramCounts => Boolean)) = {
    import UndirRelgramCounts._
    TextInput.fromTextFile(relgramsPath)
      .flatMap(fromSerializedString)
      .filter(filter)
  }

  def export(outputPath: String, urgcs: DList[UndirRelgramCounts]) {

    def exportHeadCounts(map:scala.collection.mutable.Map[String, Int]) = map.toSeq.sortBy(c => -c._2).take(5).map(c => c._1 + ":" + c._2).mkString(",")


    def exportTuple(tuple:RelationTuple) = {
      "%s\t%s\t%s\t%s\t%s".format(tuple.arg1, exportHeadCounts(tuple.arg1HeadCounts),
        tuple.rel,
        tuple.arg2, exportHeadCounts(tuple.arg2HeadCounts)
        )
    }
    def exportRelgram(rg:Relgram) = {
      "%s\t%s".format(exportTuple(rg.first), exportTuple(rg.second))
    }

    val pos = 1::5::10::20::30::40::50::Nil
    def exportCounts(map: collection.Map[Int, Int]): String = {
      pos.map(p => map.getOrElse(p, 0)).mkString(",")
    }

    def export(urgc:UndirRelgramCounts) = {
      "%s\t%s\t%s".format(exportRelgram(urgc.rgc.relgram), exportCounts(urgc.bitermCounts), exportCounts(urgc.rgc.counts))
    }

    try{
      persist(TextOutput.toTextFile(urgcs.map(urgc => export(urgc)), outputPath))
    }catch{
      case e:Exception => logger.error("Failed to persist tuples: " + e.getStackTraceString)
    }

  }

  def run() {

    var relgramsPath, outputPath = ""
    var minTupleFreq = 0
    val parser = new OptionParser() {
      arg("relgramsPath", "hdfs input path", {str => relgramsPath = str})
      arg("outputPath", "hdfs output path", { str => outputPath = str })
      opt("minTupleFreq", "min freq.", {str => minTupleFreq = str.toInt})
    }
    if (!parser.parse(args)) return

    val badString = RegexUtils.hasSpecialCharsExcept(":")
    def badRelgram(rg:Relgram) = {
      def badTuple(tuple:RelationTuple) = {
        badString(tuple.arg1) || badString(tuple.arg2)
      }
      badTuple(rg.first) || badTuple(rg.second)
    }
    def filter(urgc:UndirRelgramCounts) = {
      val count = CollectionUtils.maxOrElse(urgc.bitermCounts.values, 0)
      count >= minTupleFreq && !badRelgram(urgc.rgc.relgram)
    }
    val tuples = loadRelgrams(relgramsPath, filter)
    export(outputPath, tuples)

  }

}

