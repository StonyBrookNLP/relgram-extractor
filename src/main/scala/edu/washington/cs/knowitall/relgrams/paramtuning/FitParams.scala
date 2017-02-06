package edu.washington.cs.knowitall.relgrams.paramtuning

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/7/13
 * Time: 8:30 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import java.io.{PrintWriter, File}

import org.slf4j.LoggerFactory

import edu.washington.cs.knowitall.relgrams.measures.MeasureFunctions
import edu.washington.cs.knowitall.common.Analysis
import edu.washington.cs.knowitall.relgrams.utils.Pairable._
import edu.washington.cs.knowitall.common.Analysis._
import scopt.mutable.OptionParser
import edu.washington.cs.knowitall.relgrams.RelationTuple
import scalala.library.Plotting._
import scopt.mutable.OptionParser
import scala.Some

object FitParams {

  val logger = LoggerFactory.getLogger(this.getClass)

  def loadTaggedRelgramCounts(file:File) = {
    import edu.washington.cs.knowitall.relgrams.utils.FileUtil._
    getLines(file).flatMap(TaggedRelgramWithCounts.fromSerializedString(_))
  }

  def measure(measureType:String, smoothingDelta:Double, windowAlpha:Double)(trgc:TaggedRelgramWithCounts): Double = {

    val firstConditionals = MeasureFunctions.conditionals(trgc.bitermCounts, smoothingDelta, trgc.firstCounts)
    val windowMeasures = if (measureType.equals("scp")){
      val secondConditionals = MeasureFunctions.conditionals(trgc.bitermCounts, smoothingDelta, trgc.secondCounts)
      (firstConditionals pairElements secondConditionals).map(fs => fs._1 * fs._2)
    }else if (measureType.equals("pmi")){
      MeasureFunctions.pmis(trgc.bitermCounts, smoothingDelta, trgc.firstCounts, trgc.secondCounts)
    }else{
      firstConditionals
    }
    MeasureFunctions.combine(windowMeasures, windowAlpha)
  }

  def measure(measureType:String, smoothingDelta:Double, windowAlpha:Double,
              confMethod:String, aggType:String, filterThreshold:Double,
              f:Double = 0.0, s:Double = 0.0, r:Double = 1.0)(first:AnnotatedTuple, second:AnnotatedTuple, trgc:TaggedRelgramWithCounts): Double = {

    val firstConf = first.agg(aggType, first.hasCommunicationRelation, belowThreshold = 1.0)
    val secondConf = second.agg(aggType, second.hasCommunicationRelation, belowThreshold = 1.0)


    if(confMethod.equals("filter")
      && (firstConf < filterThreshold || secondConf < filterThreshold)){
      return 0.0
    }

    val firstConditionals = MeasureFunctions.conditionals(trgc.bitermCounts, smoothingDelta, trgc.firstCounts)
    val windowMeasures = if (measureType.equals("scp")){
      val secondConditionals = MeasureFunctions.conditionals(trgc.bitermCounts, smoothingDelta, trgc.secondCounts)
      (firstConditionals pairElements secondConditionals).map(fs => fs._1 * fs._2)
    }else if (measureType.equals("pmi")){
      MeasureFunctions.pmis(trgc.bitermCounts, smoothingDelta, trgc.firstCounts, trgc.secondCounts)
    }else if (measureType.equals("cp")){
      firstConditionals
    }else{
      Seq[Double]()
    }
    val rmeasure = MeasureFunctions.combine(windowMeasures, windowAlpha)
    confMethod match {
      case "weight" => f * firstConf + s * secondConf + r * rmeasure
      case "filter" => rmeasure
      case _ => rmeasure
    }
  }

  //Stole from Michael's common code.
  def areaUnderCurve(points: Seq[(Double, Double)]) = {
    val it = points.iterator.buffered
    var cur = (0.0, 1.0)
    var area = 0.0
    while (it.hasNext) {
      // save last point
      val (lastYld, lastPrc) = cur

      // increment iterator
      cur = it.next
      val (yld, prc) = cur

      area += 0.5 * (yld - lastYld) * (prc + lastPrc)
    }
    area
  }
  def auc(sortedRelgrams: Iterable[(Double, TaggedRelgramWithCounts)]) = {

    val total = sortedRelgrams.size
    val totalCorrect =   sortedRelgrams.map(pair => if(pair._2.taggedRelgram.relgramLabel == 1) 1 else 0).sum
    val instanceLabels = sortedRelgrams.map(x => (x._1, x._2.taggedRelgram.relgramLabel))
    val (points, actualCorrect) = precisionRecallPoints(instanceLabels, totalCorrect)

    val auc = areaUnderCurve(points)
    //val precision = actualCorrect.toDouble/total.toDouble
    //logger.info("Actual Correct:%d Total:%d Precision@50:%.4f AUC:%.4f".format(actualCorrect, total, precision, auc))

    (auc, points)
  }


  def precisionRecallPoints(sortedLabels:Seq[Boolean], totalCorrect:Int): Seq[(Double, Double)] = {
    def toRecall(ac:Int, tc:Int) = ac.toDouble/tc.toDouble
    precisionYield(sortedLabels).map(py => (toRecall(py._1, totalCorrect), py._2))
  }
  def precisionRecallPoints(instanceLabels: Iterable[(Double, Int)], totalCorrect: Int): (Seq[(Double, Double)], Int) = {

    import Analysis._
    val booleanLabels = instanceLabels.map(_._2 == 1).toSeq
    val actualCorrect = booleanLabels.filter(x => x==true).size
    //def toRecall(ac:Int, tc:Int) = ac.toDouble/tc.toDouble
    val points: Seq[(Double, Double)] = precisionRecallPoints(booleanLabels, totalCorrect)//precisionYield(booleanLabels).map(py => (toRecall(py._1, totalCorrect), py._2))
    (points, actualCorrect)
  }

  def gridSearchPlots(paramMetrics:Seq[ParamsMetric], outputDir:String) = {

    import scalala.library.Plotting._
    val groupedByDeltas = paramMetrics.groupBy(pm => pm.delta)
    plot.hold = false
    plot.legend = true
    var i = 0
    plot.xaxis.setLabel("Window Alpha")
    plot.yaxis.setLabel("Area under curve")
    groupedByDeltas.toSeq
                   .sortBy(group => group._1).foreach(group => {
      val metrics = group._2.toSeq
      plot(metrics.map(m => m.alpha).toArray,
           metrics.map(m => m.metric).toArray,
           name = "Delta: %.0f".format(group._1))//, lines = true, shapes = false)
      plot.hold = true
      i = i + 1
    })
    //val pointer = new XYPointerAnnotation("Best AUC Configuration: Deltas:%d,%d,%d and Alpha: %.0f)".format(0, 10, 100, 0.5), 0.5, 0.9017, 3.0 * scala.math.Pi / 4.0)//("Best Bid", millis, 163.0, 3.0 * Math.PI / 4.0);
    //plot.plot.addAnnotation(pointer)
    val outputFile = outputDir + File.separator + "delta-alpha.pdf"
    saveas(outputFile)
    plot.hold = false
  }

  case class ParamsMetric(delta:Double, alpha:Double,
                          aggType:String,
                          filterThreshold:Double,
                          metric:Double,
                          precRecallPoints:Seq[(Double, Double)]){

  }

  def precisionRecallPlots(paramMetrics: Seq[ParamsMetric], outputDir: String)  {
    val top5Aucs = paramMetrics.sortBy(pm => -pm.metric).take(1)
    val baseline = paramMetrics.filter(pm => pm.aggType.equals("none")).sortBy(pm => -pm.metric).head



    val filename = "filter-nofilter-aucs.pdf"
    plotTop5AUCs(top5Aucs, baseline, outputDir, filename)
    /**
    import scalala.library.Plotting._
    val groupedByDeltas = paramMetrics.groupBy(pm => (pm.filterThreshold, pm.delta))
    groupedByDeltas.toSeq.sortBy(group => group._1).foreach(group => {
      val filename =  "ft-%.1f-delta-%.0f.pdf".format(group._1._1, group._1._2)
      logger.info("Plotting " + filename)
      val metrics = group._2
      plotPrecisionRecallFixedDelta(metrics, outputDir, filename)
    })

    val groupedByAlphas = paramMetrics.groupBy(pm => (pm.filterThreshold, pm.alpha))
    groupedByAlphas.toSeq.sortBy(group => group._1).foreach(group => {
      val filename =  "ft-%.1f-alpha-%.1f.pdf".format(group._1._1, group._1._2)
      logger.info("Plotting " + filename)
      val metrics = group._2
      plotPrecisionRecallFixedAlpha(metrics, outputDir, filename)
    })     */

    /**val groupedByDeltaAlphas = paramMetrics.groupBy(pm => (pm.aggType, pm.delta, pm.alpha))
    groupedByDeltaAlphas.toSeq.sortBy(group => group._1).foreach(group => {
      val filename =  "aggType-%s-delta-%d-alpha-%.1f.pdf".format(group._1._1, group._1._2.toInt, group._1._3)
      logger.info("Plotting " + filename)
      val metrics = group._2
      plotPrecisionRecallFixedAlphaAndDelta(metrics, outputDir, filename)
    }) */

  }

  def plotTop5AUCs(metrics:Seq[FitParams.ParamsMetric], baseline:ParamsMetric, outputDir:String, filename:String) {
    import scalala.library.Plotting._
    plot.hold = false
    plot.title = "Tuple confidence thresholds"
    plot.legend = true
    plot.xaxis.setLabel("Recall")
    plot.yaxis.setLabel("Precision")
    plot.plot.getRangeAxis.setRange(0.0, 1.0)
    metrics.sortBy(pm => -pm.metric).foreach(pm => {
      val xarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._1).toArray
      val yarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._2).toArray
      plot(xarr, yarr, name = "%s-Delta-%d-Alpha-%.1f-Thresh-%.1f".format(pm.aggType, pm.delta.toInt, pm.alpha, pm.filterThreshold))
      plot.hold = true
    })

    val pm = baseline
    val xarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._1).toArray
    val yarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._2).toArray
    plot(xarr, yarr, name = "Delta-%d-Alpha-%.1f-Thresh-%.1f".format(pm.delta.toInt, pm.alpha, pm.filterThreshold))
    plot.hold = true


    val outputFile = outputDir + File.separator + filename
    saveas(outputFile)
    plot.hold = false
  }

  def plotPrecisionRecallFixedAlphaAndDelta(metrics: Seq[FitParams.ParamsMetric], outputDir: String, filename: String) {
    import scalala.library.Plotting._
    plot.hold = false
    plot.title = "Tuple confidence thresholds"
    plot.legend = true
    plot.xaxis.setLabel("Recall")
    plot.yaxis.setLabel("Precision")
    plot.plot.getRangeAxis.setRange(0.0, 1.0)
    metrics.sortBy(pm => pm.filterThreshold)
      //.filter(pm => (pm.alpha * 10).toInt % 2 == 1)
      .foreach(pm => {
      val xarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._1).toArray
      val yarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._2).toArray
      plot(xarr, yarr, name = "Conf-Threshold: %.1f".format(pm.filterThreshold))
      plot.hold = true
    })
    val outputFile = outputDir + File.separator + filename
    saveas(outputFile)
    plot.hold = false
  }


  def plotPrecisionRecallFixedDelta(metrics: Seq[FitParams.ParamsMetric], outputDir: String, filename: String) {
    import scalala.library.Plotting._
    plot.hold = false
    plot.legend = true
    plot.title = "Varying Alphas"
    plot.xaxis.setLabel("Recall")
    plot.yaxis.setLabel("Precision")
    plot.plot.getRangeAxis.setRange(0.0, 1.0)
    metrics.sortBy(pm => pm.alpha)
      .filter(pm => (pm.alpha * 10).toInt % 2 == 1)
      .foreach(pm => {
      val xarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._1).toArray
      val yarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._2).toArray
      plot(xarr, yarr, name = "Alpha: %.1f".format(pm.alpha))
      plot.hold = true
    })
    val outputFile = outputDir + File.separator + filename
    saveas(outputFile)
    plot.hold = false
  }

  def plotPrecisionRecallFixedAlpha(metrics: Seq[FitParams.ParamsMetric], outputDir: String, filename: String) {
    import scalala.library.Plotting._
    plot.hold = false
    plot.title = "Varying Deltas"
    plot.legend = true
    plot.xaxis.setLabel("Recall")
    plot.yaxis.setLabel("Precision")
    plot.plot.getRangeAxis.setRange(0.0, 1.0)

    metrics.sortBy(pm => pm.delta)
      .foreach(pm => {
      val xarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._1).toArray
      val yarr = pm.precRecallPoints.sortBy(x => x._1).map(x => x._2).toArray
      plot(xarr, yarr, name = "Delta: %.0f".format(pm.delta))
      plot.hold = true
    })
    val outputFile = outputDir + File.separator + filename
    saveas(outputFile)
    plot.hold = false
  }

  def main(args:Array[String]){
    var taggedRelgramsFile, tupleConfidencesFile, outputDir = ""
    var topk = 50
    var deltas:Seq[Double] = 0::10.0::100.0::500.0::1000.0::10000.0::100000.0::Nil
    var alphas:Seq[Double] = (1 until 11).map(i => (i.toDouble*10.0/100.0))
    var aggTypes = "avg-below"::"median" :: "min" :: "max" :: "average" :: "max+average":: Nil
    var filterThresholds:Seq[Double] = (0 until 10).map(i => i.toDouble/10.0)
    var removeBadTuples = false
    var measureType = "psf"

    val parser = new OptionParser() {
      arg("taggedRelgramsCountsFile", "hdfs input path", {str => taggedRelgramsFile = str})
      arg("tupleConfidencesFile", "Tuple confidences file.", {str => tupleConfidencesFile = str})
      arg("outputDir", "Output directory.", { str => outputDir = str})
      opt("removeBadTuples", "Remove bad tuples.", {str => removeBadTuples = str.toBoolean})
      opt("topk", "Number of relgrams to output for each tuple.", {str => topk = str.toInt})
      def toDoubles(string:String) = string.split(",").map(_.toDouble)
      opt("windowAlphas", "alpha decay for combining measures from different windows.", {str => alphas = toDoubles(str)})
      opt("smoothingDeltas", "smoothing delta.", {str => deltas = toDoubles(str)})
      opt("measureType", "scp or psf", {str => measureType = str})

      opt("aggTypes", "aggregate confidence types.", {str => aggTypes = str.split(",").toList})
    }


    def validate() = {
      assert(new File(outputDir).isDirectory, "Not a valid directory specified for option: --outputDir=%s".format(outputDir))
      assert(new File(tupleConfidencesFile).isFile, "Not a file: " + tupleConfidencesFile)
      assert(new File(taggedRelgramsFile).isFile, "Not a file: " + taggedRelgramsFile)
    }
    if (!parser.parse(args)) return
    validate()

    val confidences = loadConfidences(tupleConfidencesFile)
    logger.info("Number of confidence assigned tuples: " + confidences.size)

    val relgrams = loadRelgrams(taggedRelgramsFile, confidences, removeBadTuples)
    logger.info("Number of tagged relgrams: " + relgrams.size)

    val outputFile = outputDir + File.separator + "param-metrics.txt"
    val writer = new PrintWriter(outputFile, "UTF-8")
    var paramMetrics = Seq[ParamsMetric]()
    deltas.foreach(delta => {
      alphas.foreach(alpha => {
        aggTypes.foreach(aggType => {
          //val aggType = "max"
          filterThresholds.foreach(filterThreshold => {
            val measureFunc = measure(measureType, delta, alpha, "filter", aggType, filterThreshold)_
            val sorted = relgrams.map(trgc => {
              (measureFunc(trgc._1, trgc._2, trgc._3),trgc)
            }).toSeq
              .sortBy(kv => -kv._1)
              .map(x => (x._1, x._2._3))
            val (metric, precRecallPoints) = auc(sorted)
            paramMetrics :+= new ParamsMetric(delta, alpha, aggType, filterThreshold, metric, precRecallPoints)
            val confwriter = new PrintWriter(outputDir + File.separator + "delta-%.4f-alpha-%.4f-agg-%s-ft-%.2f.txt".format(delta, alpha, aggType, filterThreshold))
            sorted.foreach(kv => confwriter.println(kv._1 + "\t" + kv._2.serialize))
            confwriter.close
            writer.println("%d\t%.2f\t%s\t%.2f\t%.4f".format(delta.toInt, alpha, aggType, filterThreshold, metric))
          })
        })
        val measureFunc = measure(measureType, delta, alpha, "nofilter", "none", 0.0)_
        val sorted = relgrams.map(trgc => {
          (measureFunc(trgc._1, trgc._2, trgc._3),trgc)
        }).toSeq
          .sortBy(kv => -kv._1)
          .map(x => (x._1, x._2._3))
        val (metric, precRecallPoints) = auc(sorted)
        paramMetrics :+= new ParamsMetric(delta, alpha, "none", 0.0, metric, precRecallPoints)
        val confwriter = new PrintWriter(outputDir + File.separator + "delta-%.4f-alpha-%.4f-agg-%s-ft-%.2f.txt".format(delta, alpha, "none", 0.0))
        sorted.foreach(kv => confwriter.println(kv._1 + "\t" + kv._2.serialize))
        confwriter.close
        writer.println("%d\t%.2f\t%s\t%.2f\t%.4f".format(delta.toInt, alpha, "none", 0.0, metric))
      })
    })
    writer.close

    precisionRecallPlots(paramMetrics, outputDir)
    //import scalala.library.Plotting._
    //plot.hold = false
    //gridSearchPlots(paramMetrics, outputDir)
    paramMetrics.sortBy(x => -x.metric).take(10).foreach(pm => {
      logger.info("Delta=%d, Alpha=%.1f, Agg=%s, Thresh=%.1f, AUC=%.4f".format(pm.delta.toInt, pm.alpha, pm.aggType, pm.filterThreshold, pm.metric))
    })
  }

  def addConfidences(relgram:TaggedRelgramWithCounts, confs:Map[String, AnnotatedTuple]): Option[(AnnotatedTuple, AnnotatedTuple, TaggedRelgramWithCounts)] = {
    //def getConf(tuple:RelationTuple): Option[AnnotatedTuple] = confs.get(tupleKey(tuple))
    val firstOption = confs.get(tupleKey(relgram.taggedRelgram.first))//getConf(relgram.taggedRelgram.first)
    val secondOption = confs.get(tupleKey(relgram.taggedRelgram.second))//getConf(relgram.taggedRelgram.second)
    if (firstOption == None) logger.info("Failed to get  confidence tuple for first: " + tupleKey(relgram.taggedRelgram.first))
    (firstOption, secondOption) match {
      case (Some(first:AnnotatedTuple), Some(second:AnnotatedTuple)) => {
        //logger.info("matched first: " + first.tuple.tabSeparatedTuple + " second: " + second.tuple.tabSeparatedTuple)
        Some((first, second, relgram))
      }
      case _ => {
        logger.error("First or Second tuple missing for relgram: " + relgram)
        None
      }
    }
  }

  def loadRelgrams(taggedRelgramsFile: String, confidences:Map[String, AnnotatedTuple], removeBadTuples: Boolean) = {

    val file = new File(taggedRelgramsFile)
    val relgrams = loadTaggedRelgramCounts(file).toList
    var relgramsWithConfs = List[(AnnotatedTuple, AnnotatedTuple, TaggedRelgramWithCounts)]()
    relgrams.foreach(relgram => {
      addConfidences(relgram, confidences) match {
        case Some(x:(AnnotatedTuple, AnnotatedTuple, TaggedRelgramWithCounts)) => relgramsWithConfs :+= x
        case _ => logger.error("Failed to obtain annotated tuple for relgram: " + relgram.taggedRelgram.first.prettyString + "\t" + relgram.taggedRelgram.second.prettyString)
      }
    })
    logger.info("Relgrams: " + relgrams.size)
    logger.info("With Confs: " + relgramsWithConfs.size)
    if (removeBadTuples)
      relgramsWithConfs.filter(rc => !rc._3.taggedRelgram.hasSomeBadTuple())
    else
      relgramsWithConfs
  }

  def tupleKey(tuple: RelationTuple) = (tuple.arg1.trim + "," + tuple.rel.trim + "," + tuple.arg2.trim).toLowerCase

  def loadConfidences(tupleConfidencesFile: String) =  {
    var file = new File(tupleConfidencesFile)
    AnnotatedTuple.fromFile(file).map(tuple => (tupleKey(tuple.tuple) -> tuple)).toMap
  }

}
