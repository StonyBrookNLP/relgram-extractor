package edu.washington.cs.knowitall.relgrams.paramtuning

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/5/13
 * Time: 4:44 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import scopt.mutable.OptionParser
import io.Source
import edu.washington.cs.knowitall.relgrams._
import measures.TopTuplesScoobiApp
import util.Random
import java.io.{File, PrintWriter}
import utils.{MapUtils, Prepositions, RegexUtils, Pronouns}
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import edu.washington.cs.knowitall.relgrams.solr.SolrSearchWrapper
import scopt.mutable.OptionParser
import scala.Some


object TopRelgramsSampler {

  val logger = LoggerFactory.getLogger(this.getClass)

  val tupleLineRe = """(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*)""".r

  var badTupleFinders: Iterable[(RelationTuple => Boolean)] = Seq[(RelationTuple => Boolean)]()

  def loadTuples(file:String) = {
    def toRelationTuple(string:String) = {
      try {
        val tupleLineRe(m1:String, m2:String, m3:String, arg1:String,rel:String, arg2:String) = string
        Some((m1.toDouble, RelationTuple.fromArg1RelArg2(arg1, rel, arg2)))
      } catch {
        case e:Exception => {
          logger.error("Failed to parse line: " + string)
          logger.error(e.getStackTraceString)
          None
        }
      }
    }
    Source.fromFile(file, "UTF-8").getLines().flatMap(line => toRelationTuple(line))
  }

  def isBadTuple(tuple:RelationTuple) = badTupleFinders.exists(finder => finder(tuple))


  def sort(tuples:Iterator[(Double, RelationTuple)]) = tuples.toSeq.sortBy(x => -x._1).map(x => x._2)

  def sample(tuples:Seq[RelationTuple], size:Int) = Random.shuffle(tuples).take(size)

  def exportTuples(path:String, index:Int, tuples:Seq[RelationTuple]) = {
    val selectedTuplesFile = path + File.separator + "bin-%d-tuples.txt".format(index)
    val writer = new PrintWriter(selectedTuplesFile, "UTF-8")
    tuples.foreach(tuple => writer.println(tuple.tabSeparatedTuple))
    writer.close
  }

  def toExportString(maff:(Measures, AffinityMeasures)) = {

    def measureString(maff:(Measures, AffinityMeasures)) = "%.6f".format(maff._2.firstUndir.conditional)
    def withArgs(tuple:RelationTuple, arg1Counts:Map[String, Int], arg2Counts:Map[String, Int]):String = {
      "%s\t%s\t%s\t%s\t%s".format(tuple.arg1,
        MapUtils.toCountsString(arg1Counts, ",", ":"),
        tuple.rel,
        tuple.arg2,
        MapUtils.toCountsString(arg2Counts, ",", ":"))
    }

    def relgramString(maff:(Measures, AffinityMeasures)):String ={
      def relgramStringWithArgs(relgram:Relgram, argCounts:ArgCounts):String = {
        withArgs(relgram.first, argCounts.firstArg1Counts.toMap, argCounts.firstArg2Counts.toMap) + "\t" +
        withArgs(relgram.second, argCounts.secondArg1Counts.toMap, argCounts.secondArg2Counts.toMap) + "\t" +
        maff._1.serialize
      }
      relgramStringWithArgs(maff._1.urgc.rgc.relgram, maff._1.urgc.rgc.argCounts)
    }
    def frequenciesString(maff:(Measures, AffinityMeasures)):String = maff._1.frequenciesString
    relgramString(maff) + "\t" + frequenciesString(maff) + "\t" + measureString(maff)
  }

  def isExactMatch(queryTuple:RelationTuple, candidate:RelationTuple) = queryTuple.isIdenticalTo(candidate)

  def isValidRelgram(queryTuple:RelationTuple, maff:(Measures, AffinityMeasures)) = {
      isExactMatch(queryTuple, maff._1.urgc.rgc.relgram.first) &&
      !isBadTuple(maff._1.urgc.rgc.relgram.second) &&
      TopTuplesScoobiApp.countsAreValid(maff._1)
  }

  def isValidRelgram(firstTuple:RelationTuple, secondTuple:RelationTuple, maff:(Measures, AffinityMeasures)) = {
    isExactMatch(firstTuple, maff._1.urgc.rgc.relgram.first) &&
    isExactMatch(secondTuple, maff._1.urgc.rgc.relgram.second) &&
    !isBadTuple(maff._1.urgc.rgc.relgram.second) &&
    TopTuplesScoobiApp.countsAreValid(maff._1)
  }

  def findRelgrams(solr:SolrSearchWrapper, topk:Int)(queryTuple:RelationTuple) = {
    solr.search(queryTuple)//same as using (queryTuple, dummyTuple, false)
        .filter(maff => isValidRelgram(queryTuple, maff))
        .sortBy(maff => -maff._2.firstUndir.conditional)
        .take(topk)
  }

  def findAndExportRelgramsForTuples(solr:SolrSearchWrapper,
                                     tuples: Seq[RelationTuple],
                                     topk:Int,
                                     index:Int,
                                     outputDir: String) {
    val outputFile = outputDir + File.separator + "bin-%d-relgrams.txt".format(index)
    val writer = new PrintWriter(outputFile, "UTF-8")
    val fr = findRelgrams(solr, topk)_
    tuples.foreach(tuple => {
      println("Bin: %d Finding relgrams for Tuple:(%s, %s, %s)".format(index, tuple.arg1, tuple.rel, tuple.arg2))
      val relgrams = fr(tuple)
      println("Found: %d".format(relgrams.size))
      relgrams.foreach(maff => writer.println(toExportString(maff)))
      println
    })
    writer.close
  }

  def main(args:Array[String]){
    var tuplesFile, outputDir = ""
    var solrURL = ""
    var solrDocURL = ""
    var solr:SolrSearchWrapper = null
    var windowAlpha = 0.9
    var smoothingDelta = 1000.0
    var minFreq = 0
    var topk = 0
    var numTuples = 20
    var numBins = 3
    var seed = 0


    import Pronouns._
    def pronounFilter(tuple:RelationTuple) = isPronoun(tuple.arg1) || isPronoun(tuple.arg2)

    import RegexUtils._
    def numbersFilter(tuple:RelationTuple) = hasNumber(tuple.arg1) || hasNumber(tuple.arg2)

    val relStopWords = Set[String]("say", "tell")
    val stoprels = Prepositions.preps.map(prep => "be %s".format(prep)) ++ Prepositions.preps
    val specialCharsMatch = hasSpecialCharsExcept(" ")
    def stopRelsFilter(tuple:RelationTuple) = {
      import RegexUtils._
      (stoprels.contains(tuple.rel) ) ||
        (tuple.rel.split(" ").toSet exists relStopWords) ||
          specialCharsMatch(tuple.rel)
    }

    def getBadTupleFinders(string:String):Iterable[(RelationTuple => Boolean)] = {
      val pf = pronounFilter _
      val nf = numbersFilter _
      val sr = stopRelsFilter _
      string.split(",").flatMap(name => name match {
        case "pronouns" => Some(pf)
        case "numbers" => Some(nf)
        case "stoprels" => Some(sr)
        case _ => None
      })
    }


    def setSolrWrapper(solrURL:String, solrDocURL:String) {
      solr = new SolrSearchWrapper(solrURL, solrDocURL)
    }


    val parser = new OptionParser() {
      arg("tuplesFile", "hdfs input path", {str => tuplesFile = str})
      arg("solrurl", "Solr url.", { str => solrURL = str})
      arg("solrDocurl", "SolrDocUrl.", { str => solrDocURL = str})
      arg("outputDir", "Output directory.", { str => outputDir = str})
      opt("seed", "Random generator seed.", {str => seed = str.toInt})
      opt("numTuples", "Number of random tuples to sample.", {str => numTuples = str.toInt})
      opt("numBins", "Number of bins to sample tuples from. Default: %d".format(numBins), {str => numBins = str.toInt})
      //opt("windowAlpha", "alpha decay for combining measures from different windows.", {str => windowAlpha = str.toDouble})
      //opt("smoothingDelta", "smoothing delta.", {str => smoothingDelta = str.toDouble})
      opt("topk", "Number of relgrams to output for each tuple.", {str => topk = str.toInt})
      opt("badTupleFilters", "Filters for removing bad tuples: comma separated list of [pronoun, numbers, stoprels]", {str => badTupleFinders = getBadTupleFinders(str)} )
    }
    def validate() = {
      assert(new File(outputDir).isDirectory, "Not a valid directory specified for option: --outputDir=%s".format(outputDir))
      assert(solrURL != "" && solrDocURL != "", "Solr URLs cannot be empty.")
    }
    if (!parser.parse(args)) return
    validate()
    setSolrWrapper(solrURL, solrDocURL)
    Random.setSeed(seed)
    val sortedTuples = sort(loadTuples(tuplesFile).filter(x => !isBadTuple(x._2)))
    val binsize = sortedTuples.size/numBins
    val bins = sortedTuples.grouped(binsize).zipWithIndex
    bins.foreach( binIndex => {
      val bin = binIndex._1
      val index = binIndex._2
      val randomTuples = sample(bin, numTuples)
      println("Bin: %d Size: %d #Selected: %d".format(index, bin.size, randomTuples.size))
      exportTuples(outputDir, index, randomTuples)
      findAndExportRelgramsForTuples(solr, randomTuples, topk, index, outputDir)
    })
  }

}
