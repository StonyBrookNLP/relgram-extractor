package edu.washington.cs.knowitall.relgrams.paramtuning

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/11/13
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import scopt.mutable.OptionParser
import scalaz.std.util.parsing.combinator.parser
import io.Source
import edu.washington.cs.knowitall.relgrams.RelationTuple
import java.io.File

import edu.washington.cs.knowitall.common.Analysis._
import edu.washington.cs.knowitall.relgrams.utils.AnalysisHelper
import org.jfree.ui.RectangleEdge

object AnnotatedTuple{


  def fromFile(file:File) = {
    //requires(file.exists(), "Annotated tuple file doesn't exist @: " + file.getAbsolutePath)
    Source.fromFile(file).getLines().flatMap(line => fromString(line)).toSeq
  }
  def fromString(string:String) = {
    val iter = string.split("\t").iterator
    val tuplePos = iter.next
    val median = iter.next.toDouble
    val max = iter.next.toDouble
    val min = iter.next.toDouble
    val average = iter.next.toDouble
    val label = iter.next.toInt
    val tuple = RelationTuple.fromArg1RelArg2(iter.next, iter.next, iter.next)
    Some(new AnnotatedTuple(tuple, label, median, max, min, average, tuplePos))
  }
  val communicationVerbs = Set[String]("say", "tell")
  def isCommunicationRelation(rel: String) = rel.split(" ") exists communicationVerbs
}

case class AnnotatedTuple(tuple:RelationTuple, label:Int, median:Double, max:Double, min:Double, average:Double, tuplePos:String){

  import AnnotatedTuple._
  def agg(aggType:String, isCommunicationRel:Boolean, belowThreshold:Double = 1.0):Double =   {
    if (isCommunicationRel) return 0.0
    return aggType match {
      case "max+average" => this.max + this.average
      case "median" => this.median
      case "min" => this.min
      case "max" => this.max
      case "average" => this.average
      case "avg-below" => if(this.average < belowThreshold) 0.0 else this.average
      case _ => -1.0
    }
  }

  def hasCommunicationRelation = isCommunicationRelation(tuple.rel)



}

object TupleConfidenceEval {

  val logger = LoggerFactory.getLogger(this.getClass)


  import AnalysisHelper._
  import AnnotatedTuple._
  import scalala.library.Plotting._
  def main(args:Array[String])  {
    var confidencesFile, outputFile = ""
    val parser = new OptionParser() {
      arg("confidencesFile", "hdfs input path", {str => confidencesFile = str})
      arg("outputFile", "Output file.", { str => outputFile = str})
    }
    if(!parser.parse(args)) return
    val tuples = fromFile(new File(confidencesFile))
    plotPrecisionRecall(tuples, outputFile)
  }


  def plotPrecisionRecall(tuples: Seq[AnnotatedTuple], outputFile: String) {
    //Set up plot.
    plot.hold = false
    plot.legend = true
    plot.title = "Finding Mal-formed Tuples"
    plot.xaxis.setLabel("Recall")
    plot.yaxis.setLabel("Precision")
    plot.hold = true
    plot.plot.getRangeAxis.setRange(0.0, 1.0)

    plot.chart.getLegend.setPosition(RectangleEdge.RIGHT)
    //Plot for each type of aggregate measure.
    val aggTypes = "avg-below"::"median" :: "min" :: "max" :: "average" :: "max+average":: Nil
    //aggTypes.foreach(aggType => plotPrecisionRecall(aggType, tuples, false))
    aggTypes.foreach(aggType => {
      val comm = aggType.equals("average")
      plotPrecisionRecall(aggType, tuples, comm)
    })

    //Save plot
    plot.hold = false
    saveas(outputFile)
  }

  def plotPrecisionRecall(aggType: String, tuples: Seq[AnnotatedTuple], comm:Boolean) {
    def agg(aggType:String)(tuple:AnnotatedTuple, isCommunicationRel:Boolean):Double =   {
      if (isCommunicationRel) return 0.0
      return aggType match {
        case "max+average" => tuple.max + tuple.average
        case "median" => tuple.median
        case "min" => tuple.min
        case "max" => tuple.max
        case "average" => tuple.average
        case "avg-below" => if(tuple.average < 0.5) 0.0 else tuple.average
        case _ => -1.0
      }
    }
    //val aggVal = agg(aggType) _
    val sortedLabels = tuples.sortBy(tuple => tuple.agg(aggType, false, belowThreshold = 0.5)).map(tuple => tuple.label == 1)
    println("Number of instances: " + sortedLabels.size )
    println("Number of true instances: " + sortedLabels.filter(x => x == true).size)
    val points = precisionRecallPoints(sortedLabels)

    addXYPlot(points, aggType)
    if(comm){
      val csortedLabels = tuples.sortBy(tuple => tuple.agg(aggType, isCommunicationRelation(tuple.tuple.rel), belowThreshold = 0.5)).map(tuple => tuple.label == 1)
      val commPoints = precisionRecallPoints(csortedLabels)
      addXYPlot(commPoints, aggType + "+ Comm.")
    }

  }
  def addXYPlot(series: Seq[(Double, Double)], name: String) {
    def toRecArray(points: Seq[(Double, Double)]) = points.map(x => x._1).toArray
    def toPrecArray(points: Seq[(Double, Double)]) = points.map(x => x._2).toArray
    plot(toRecArray(series), toPrecArray(series), name = name)
  }
}
