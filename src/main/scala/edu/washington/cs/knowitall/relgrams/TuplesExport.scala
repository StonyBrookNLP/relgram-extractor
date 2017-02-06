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
import edu.washington.cs.knowitall.relgrams.utils.RegexUtils

object TuplesExport extends ScoobiApp{

  val logger = LoggerFactory.getLogger(this.getClass)


  def loadRelationTupleCounts(tuplesPath:String,
                              filter:(RelationTupleCounts => Boolean)) = {
    import RelationTupleCounts._
    TextInput.fromTextFile(tuplesPath)
             .flatMap(fromSerializedString)
             .filter(filter)
  }


  def export(outputPath: String, tuples: DList[RelationTupleCounts]) {

    def exportHeadCounts(map:scala.collection.mutable.Map[String, Int]) = map.toSeq.sortBy(c => -c._2).take(5).map(c => c._1 + ":" + c._2).mkString(",")

    def export(tc:RelationTupleCounts) = {
     "%s\t%s\t%s\t%s\t%s\t%d".format(tc.tuple.arg1, exportHeadCounts(tc.tuple.arg1HeadCounts),
                                     tc.tuple.rel,
                                     tc.tuple.arg2, exportHeadCounts(tc.tuple.arg2HeadCounts),
                                     tc.count)
    }

    try{
      persist(TextOutput.toTextFile(tuples.map(export), outputPath))
    }catch{
      case e:Exception => logger.error("Failed to persist tuples: " + e.getStackTraceString)
    }

  }

  def run() {

    var tuplesPath, outputPath = ""
    var minTupleFreq = 0
    val parser = new OptionParser() {
      arg("tuplesPath", "hdfs input path", {str => tuplesPath = str})
      arg("outputPath", "hdfs output path", { str => outputPath = str })
      opt("minTupleFreq", "min freq.", {str => minTupleFreq = str.toInt})
    }
    if (!parser.parse(args)) return

    val badString = RegexUtils.hasSpecialCharsExcept(":")
    def filter(tc:RelationTupleCounts) = {
      tc.count >= minTupleFreq && !(badString(tc.tuple.arg1) || badString(tc.tuple.arg2))
    }
    val tuples = loadRelationTupleCounts(tuplesPath, filter)
    export(outputPath, tuples)

  }

}

