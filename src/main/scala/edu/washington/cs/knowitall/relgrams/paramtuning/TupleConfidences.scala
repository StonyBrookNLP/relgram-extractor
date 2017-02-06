package edu.washington.cs.knowitall.relgrams.paramtuning

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/10/13
 * Time: 4:11 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.relgrams.{TypedTuplesRecord, RelationTuple}
import collection.mutable
import scopt.mutable.OptionParser
import edu.washington.cs.knowitall.relgrams.solr.SolrSearchWrapper
import java.io.PrintWriter
import scalaz.std.tuple

object TupleConfidences {

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args:Array[String]){

    var scoredTaggedRelgramsFile, outputFile = ""
    var solrURL, solrDocURL = ""
    var removeBadTuples = false
    var numRecords = 100
    val parser = new OptionParser() {
      arg("scoredTaggedRelgramsFile", "hdfs input path", {str => scoredTaggedRelgramsFile = str})
      arg("solrurl", "Solr url.", { str => solrURL = str})
      arg("solrDocurl", "SolrDocUrl.", { str => solrDocURL = str})
      arg("outputFile", "Output file.", { str => outputFile = str})
      opt("removeBadTuples", "Remove bad tuples.", {str => removeBadTuples = str.toBoolean})
      opt("numRecords", "Number of records to sample confidences from.", {str => numRecords = str.toInt})
    }
    if(!parser.parse(args)) return

    val solr = new SolrSearchWrapper(solrURL, solrDocURL)
    val strgcs = ScoredTaggedRelgramWithCounts.fromFile(args(0))
    var firsts = new mutable.HashMap[String, (Int, RelationTuple)]
    var seconds = new mutable.HashMap[String, (Int, RelationTuple)]
    strgcs.foreach(x => {
      firsts += x.trgc.taggedRelgram.first.prettyString -> new Tuple2[Int, RelationTuple](x.trgc.taggedRelgram.isFirstBad, x.trgc.taggedRelgram.first)
      seconds += x.trgc.taggedRelgram.second.prettyString -> new Tuple2[Int, RelationTuple](x.trgc.taggedRelgram.isSecondBad, x.trgc.taggedRelgram.second)
    })


    val writer = new PrintWriter(outputFile, "UTF-8")

    exportConfidence(firsts.toMap, solr, writer, "first", numRecords)
    exportConfidence(seconds.toMap, solr, writer, "second", numRecords)

    writer.close
  }


  def exportConfidence(firsts: Map[String, (Int, RelationTuple)],
                       solr: SolrSearchWrapper,
                       writer: PrintWriter,
                       firstOrSecond: String, numRecords:Int) {
    def medianConfs(doubles:Seq[Double]) = {
      val idx = doubles.size/2
      if (idx < doubles.size) doubles(idx) else 0.0

    }
    firsts.foreach(fvar => {
      val label = fvar._2._1
      val ftuple = fvar._2._2
      logger.info("Processing tuple: " + ftuple.tabSeparatedTuple)
      var records: Seq[TypedTuplesRecord] = solr.recordsMatchingTuple(ftuple, numRecords)
      def relOnlyTuple(rtuple:RelationTuple) = RelationTuple.fromArg1RelArg2("", rtuple.rel, "")
      def capitalize(arg:String) = if (!arg.startsWith("type:")) arg.capitalize else arg
      if (records.size < numRecords){
        records ++= solr.recordsMatchingTuple(RelationTuple.fromArg1RelArg2(capitalize(ftuple.arg1), ftuple.rel, capitalize(ftuple.arg2)), numRecords)
        if (records.size < numRecords) records ++= solr.recordsMatchingTuple(relOnlyTuple(ftuple), ftuple, numRecords) //(ftuple, 1000)
      }

      val confidences = records.map(tuple => tuple.confidence)
      if (!confidences.isEmpty) {
        var extrSents = List[(String, String)]()
        val median = medianConfs(confidences)
        val max = confidences.max
        val min = confidences.min
        val avg = confidences.sum/confidences.size.toDouble
        var hashes = Set[Int]()
        records.sortBy(tuple => -tuple.confidence).foreach(tuple => {
          val sentence = tuple.sentence
          val extr = tuple.arg1 + "\t" + tuple.rel + "\t" + tuple.arg2
          if (!hashes.exists(tuple.hashes)){
            extrSents :+= new Tuple2[String, String](extr, sentence)
          }
          hashes ++= tuple.hashes
        })
        //val top5tuples = tuples.sortBy(tuple => -tuple.confidence).take(5)
        //val sentences = top5tuples.map(tuple => tuple.sentence).take(5)

        writer.println(firstOrSecond + "\t" + median + "\t" + max + "\t" + min + "\t" + avg +  "\t" + label + "\t" + ftuple.tabSeparatedTuple)
        (0 until 5).foreach(k => {
          if (extrSents.size > k){
            val extr = extrSents(k)._1
            val sentence = extrSents(k)._2
            writer.println("extr" + "\t" + extr + "\t" + sentence)
          }
        })
      } else {
        println("Failed to find confidences for tuple: " + ftuple)
      }

    })
  }
}
