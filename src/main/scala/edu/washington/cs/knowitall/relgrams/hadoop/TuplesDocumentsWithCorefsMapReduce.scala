package edu.washington.cs.knowitall.relgrams.hadoop

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/28/13
 * Time: 11:14 AM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.util.GenericOptionsParser


import org.apache.hadoop.io.{LongWritable, Text}
import org.apache.hadoop.fs.Path
import edu.washington.cs.knowitall.relgrams.{TuplesDocumentWithCorefMentions, TuplesDocument, TuplesDocumentGenerator}

import scala.Some
import org.apache.hadoop.mapreduce.{Job, Mapper}
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat
import org.apache.hadoop.mapreduce.lib.input.{FileInputFormat, TextInputFormat}



class CorefMapper extends Mapper[LongWritable, Text, Text, Text] {
  val logger = LoggerFactory.getLogger(this.getClass)
  var tgen:TuplesDocumentGenerator = null
  var maxDocSize = 100
  var maxSentenceLength = 75



  override def setup(context:Mapper[LongWritable,Text, Text, Text] #Context){
    val corefTimeoutMs = context.getConfiguration.get("corefTimeoutMs", "0").toInt
    maxDocSize = context.getConfiguration.get("maxDocSize", "100").toInt
    maxSentenceLength = context.getConfiguration.get("maxSentenceLength", "75").toInt
    tgen = new TuplesDocumentGenerator(corefTimeoutMs)
  }

  def close() {}

  def corefDocument(string: String): Option[TuplesDocumentWithCorefMentions] = {
    TuplesDocument.fromString(string) match {
      case Some(document: TuplesDocument) => if (document.tupleRecords.size <= maxDocSize) {
        tgen.getTuplesDocumentWithCorefMentionsBlocks(document, maxSentenceLength)
      } else {
        logger.info("Ignoring document: %s with size %d".format(document.docid, document.tupleRecords.size))
        None
      }
      case None => {
        logger.error("Failed to build document from line: " + string)
        None
      }
    }
  }

  override protected  def map(
    key:LongWritable,
    value:Text,
    context: org.apache.hadoop.mapreduce.Mapper[LongWritable, Text, Text, Text]#Context) {
    context.getCounter("coref", "docs").increment(1)//("mygroup", "jeff").increment(1)
    corefDocument(value.toString) match {
      case Some(document:TuplesDocumentWithCorefMentions) => context.write(new Text(document.toString), new Text(""))
      case None => logger.error("Failed to extract coref document from line: " + value.toString.split("\t").headOption)
    }
  }

}

object TuplesDocumentsWithCorefsMapReduce {

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args:Array[String]){
    val conf = new Configuration()
    val otherArgs = new GenericOptionsParser(conf, args).getRemainingArgs

    println("Args: " + otherArgs.mkString(","))
    val inputPath = otherArgs(0)
    val outputPath = otherArgs(1)
    println("Input path: " + inputPath)
    println("Output path: " + outputPath)

    val ejob = new Job(conf)
    //ejob.setMaxMapTaskFailuresPercent(100)
    //ejob.setMaxMapAttempts(4)
     //mapred.skip.map.max.skip.records=1
    ejob.setJarByClass(classOf[CorefMapper])
    ejob.setInputFormatClass(classOf[TextInputFormat]) //LzoTextInputFormat])


    ejob setOutputKeyClass classOf[Text]
    ejob setOutputValueClass classOf[Text]

    //Input: <docname, <extraction sentenceWords record1__DOCEXTR_DELIM__extraction sentenceWords record2__DOCEXTR_DELIM__extraction sentenceWords record3>
    //Output: List of rel-view grams <vType, first + second + hashes + count>

    ejob setMapperClass classOf[CorefMapper]

    ejob.setNumReduceTasks(0)

    FileInputFormat.addInputPath(ejob, new Path(inputPath))
    FileOutputFormat.setOutputPath(ejob, new Path(outputPath))

    ejob.waitForCompletion(true)


  }
}
