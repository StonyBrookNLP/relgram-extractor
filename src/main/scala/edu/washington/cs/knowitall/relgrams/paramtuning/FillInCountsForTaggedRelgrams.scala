package edu.washington.cs.knowitall.relgrams.paramtuning

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/8/13
 * Time: 3:55 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import java.io.{PrintWriter, File}
import edu.washington.cs.knowitall.relgrams.utils.FileUtil._
import scopt.mutable.OptionParser
import edu.washington.cs.knowitall.relgrams.solr.SolrSearchWrapper

object FillCountsForRelgrams {

  val logger = LoggerFactory.getLogger(this.getClass)

  def loadTaggedRelgrams(file:File, hasHeader:Boolean) = {
    import edu.washington.cs.knowitall.relgrams.utils.FileUtil._
    getLines(file)
      .drop(if(hasHeader) 1 else 0)
      .flatMap(TaggedRelgram.fromTaggedFileString(_))
  }

  def measure(smoothingDelta:Double, windowAlpha:Double)(trgc:TaggedRelgramWithCounts): Double = {
    0.0
  }

  def auc(values: Iterable[(Double, TaggedRelgramWithCounts)]) = {
    var rank = 0
    var numCorrect = 0
    values.toSeq.sortBy(kv => kv._1).map(kv => kv._2).map(trgc => {
      if (trgc.taggedRelgram.relgramLabel == 1) numCorrect = numCorrect + 1
      rank = rank + 1
      (numCorrect/rank).toDouble
    }).sum
  }

  def main(args:Array[String]){
    var taggedRelgramsFile, solrURL, solrDocURL, outputFile = ""
    var hasHeader = false
    val parser = new OptionParser() {
      arg("taggedRelgramsFile", "hdfs input path", {str => taggedRelgramsFile = str})
      arg("solrurl", "Solr url.", { str => solrURL = str})
      arg("solrDocurl", "SolrDocUrl.", { str => solrDocURL = str})
      arg("outputFile", "Output file.", { str => outputFile = str})
      opt("hasHeader", "Input file has a header row.", {str => hasHeader = str.toBoolean})
    }
    def validate() = {
      assert(solrURL != "" && solrDocURL != "", "Solr URLs cannot be empty.")
    }
    if (!parser.parse(args)) return
    validate()
    val solr = new SolrSearchWrapper(solrURL, solrDocURL)
    val file = new File(taggedRelgramsFile)
    assert(file.isFile, "Not a file: " + taggedRelgramsFile)


    val taggedRelgrams = loadTaggedRelgrams(file, hasHeader).toSeq

    logger.info("Number of tagged relgrams: " + taggedRelgrams.size)

    val fromTaggedRelgram = TaggedRelgramWithCounts.fromTaggedRelgram(solr, filterReportingVerbs = false)_

    //val filteredRelgrams = taggedRelgrams.filter(tr => !hasSomeBadTuple(tr))
    //logger.info("Number of relgrams with no bad tuples: " + filteredRelgrams.size)

    //filteredRelgrams.foreach(tr => logger.info(tr.first + "\t" + tr.second + "\t" +
    //                                          tr.isFirstBad + "\t" + tr.isSecondBad + "\t" + tr.relgramLabel))

    val trgcs = taggedRelgrams.flatMap(fromTaggedRelgram(_)).toSeq
    logger.info("Number of relgrams with counts: " + trgcs.size)


    val writer = new PrintWriter(outputFile)
    trgcs.foreach(trgc => writer.println(trgc.serialize))
    writer.close

  }
}
