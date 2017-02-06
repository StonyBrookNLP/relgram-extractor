package edu.washington.cs.knowitall.relgrams.paramtuning

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/10/13
 * Time: 1:48 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import java.io.PrintWriter

object CompareMeasures {
  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args:Array[String]){
    val cp = args(0)
    val scp = args(1)
    val pmi = args(2)
    val outputFile = args(3)

    def toSortedLabels(path:String) = ScoredTaggedRelgramWithCounts.fromFile(path).filter(x => x.trgc.taggedRelgram.hasSomeBadTuple()).toSeq.sortBy(x => -x.score).map(x => x.trgc.taggedRelgram.isGoodRelgram)

    val cprelgrams = toSortedLabels(cp)
    val totalCorrect = cprelgrams.filter(c => c == true).size
    val scprelgrams = toSortedLabels(scp)
    val pmirelgrams = toSortedLabels(pmi)
    val cppoints = FitParams.precisionRecallPoints(cprelgrams, totalCorrect).sortBy(x => x._1)
    val scppoints = FitParams.precisionRecallPoints(scprelgrams, totalCorrect).sortBy(x => x._1)
    val pmipoints = FitParams.precisionRecallPoints(pmirelgrams, totalCorrect).sortBy(x => x._1)
    import scalala.library.Plotting._
    plot.hold = false
    plot.legend = true
    plot.xaxis.setLabel("Recall")
    plot.yaxis.setLabel("Precision")

    def toRecArray(points:Seq[(Double, Double)]) = points.map(x => x._1).toArray
    def toPrecArray(points:Seq[(Double, Double)]) = points.map(x => x._2).toArray
    plot.hold = true
    plot(toRecArray(cppoints), toPrecArray(cppoints), name = "P(S|F)")
    plot(toRecArray(scppoints), toPrecArray(scppoints), name = "SCP")
    plot(toRecArray(pmipoints), toPrecArray(pmipoints), name = "PMI")
    saveas(outputFile)
  }
}
