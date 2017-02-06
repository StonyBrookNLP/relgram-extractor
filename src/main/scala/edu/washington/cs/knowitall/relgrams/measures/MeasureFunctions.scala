package edu.washington.cs.knowitall.relgrams.measures

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/4/13
 * Time: 3:33 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.relgrams.utils.Pairable
import collection.Map




object MeasureFunctions {
    val logger = LoggerFactory.getLogger(this.getClass)
  def weightedAverage(weights:Seq[Double], values:Seq[Double]) = {
    require(weights.size == values.size, "Weights and values size must be the same. Actual weights:%d, values:%d".format(weights.size, values.size))
    require(weights.sum != 0, "weights cannot sum to zero: %s".format(weights.mkString(",")))
    import Pairable._
    ((weights pairElements values).map(wv => wv._1 * wv._2).sum)/weights.sum
  }
  def combine(values:Seq[Double], combineAlpha:Double) = {
    if(!values.isEmpty){
      val weights = (1 until values.size+1).map(i => math.pow(combineAlpha, i))
      //logger.info("Weights: " + weights.map(w => "%.6f".format(w)).mkString(","))
      weightedAverage(weights, values)
    }else{
      0.0
    }
  }

  def scp(countsMap:Map[Int, Int], smoothingDelta:Double, firstCounts:Int, secondCounts:Int):Seq[Double] = {
    val firstConditionals = MeasureFunctions.conditionals(countsMap, smoothingDelta, firstCounts)
    val secondConditionals = MeasureFunctions.conditionals(countsMap, smoothingDelta, secondCounts)
    import Pairable._
    (firstConditionals pairElements secondConditionals).map(fs => fs._1 * fs._2)
  }

  def conditionals(countsMap: Map[Int, Int], smoothingDelta: Double, firstCounts: Int): Seq[Double] = {
    countsMap.toSeq.sortBy(kv => kv._1).map(kv => kv._2.toDouble / (smoothingDelta + firstCounts.toDouble))
  }

  def pmis_2(countsMap: Map[Int, Int], smoothingDelta: Double, firstCounts: Int, secondCounts:Int): Seq[Double] = {
    countsMap.toSeq.sortBy(kv => kv._1).map(kv => math.log(kv._2.toDouble/(firstCounts + secondCounts).toDouble))
  }

  def pmis(countsMap: Map[Int, Int], smoothingDelta: Double, firstCounts: Int, secondCounts:Int): Seq[Double] = {
    countsMap.toSeq.sortBy(kv => kv._1).map(kv => math.log(kv._2.toDouble/(firstCounts + secondCounts + 2 * smoothingDelta).toDouble))
  }
  /**
  def pmi(association:Association, vocabSize:Double,  delta:Double, totalRels:Double):Double = {

    var b_given_a = (association.joint + delta)/(association.a_count + delta*vocabSize)
    var b_marginal = (association.b_count + delta * vocabSize)/(totalRels + delta *vocabSize*vocabSize)
    val min_count =  Math.min(association.a_count, association.b_count)
    return Math.log(b_given_a/b_marginal) * association.joint/(association.joint+1.0) * min_count/(min_count + 1.0)

  }    */


}
