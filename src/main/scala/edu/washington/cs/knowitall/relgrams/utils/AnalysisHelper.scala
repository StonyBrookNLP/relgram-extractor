package edu.washington.cs.knowitall.relgrams.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/11/13
 * Time: 3:07 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory

import edu.washington.cs.knowitall.common.Analysis._

object AnalysisHelper {

  val logger = LoggerFactory.getLogger(this.getClass)

  /**
   * Computes precision/recall assuming that the number of true values in the sorted labels is the
   * total number of true instances.
   * @param sortedLabels
   * @return
   */
  def precisionRecallPoints(sortedLabels:Seq[Boolean]): Seq[(Double, Double)] = {

    val totalCorrect = sortedLabels.filter(label => label == true).size
    precisionRecallPoints(sortedLabels, totalCorrect)
  }
  def precisionRecallPoints(sortedLabels:Seq[Boolean], totalCorrect:Int): Seq[(Double, Double)] = {
    def toRecall(ac:Int, tc:Int) = ac.toDouble/tc.toDouble
    precisionYield(sortedLabels).map(py => (toRecall(py._1, totalCorrect), py._2))
  }

}
