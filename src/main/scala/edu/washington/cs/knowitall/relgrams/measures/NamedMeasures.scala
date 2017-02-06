package edu.washington.cs.knowitall.relgrams.measures

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/4/13
 * Time: 3:15 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.relgrams.{Measures, WindowedMeasure}
import util.matching.Regex
import util.matching.Regex.Match
import com.nicta.scoobi.core.WireFormat
import java.io.{DataOutput, DataInput}

object NamedMeasures{
  val nsep = "_NSEP_"
  val msep = "_MSEP_"

  val msepRe = """%s""".format(nsep).r
  val nsepRe = """(.*?)%s(.*)""".format(nsep).r

  def fromNamedMeasureValString(string:String): Option[(String, WindowedMeasure)] = {
    nsepRe.findFirstMatchIn(string) match {
      case Some(m:Match) => {
        val name = m.group(0)
        val windowMeasuresString = m.group(1)
        WindowedMeasure.fromSerializedString(windowMeasuresString) match {
          case Some(wm:WindowedMeasure) => Some(name -> wm)
          case _ => None
        }
      }
      case _ => None
    }
  }
  def fromSerializedString(string:String) = {
    msepRe.split(string).map(mval => fromNamedMeasureValString(mval))
  }

  def fromMeasures(measures:Measures, smoothingDelta:Double, windowAlpha:Double):Option[NamedMeasures] = {
    val bitermCounts = measures.urgc.bitermCounts.toMap
    val bigramCounts = measures.urgc.rgc.counts.toMap
    val firstCounts = measures.firstCounts
    val secondCounts = measures.secondCounts
    val conditionals = getDirUndirConditionals(smoothingDelta,  windowAlpha, bigramCounts, bitermCounts)_
    val measuresMap = (conditionals("first", firstCounts) ++ conditionals("second", secondCounts)).toMap
    Some(new NamedMeasures(measuresMap))
  }


  def getDirUndirConditionals(smoothingDelta:Double, windowAlpha:Double,bigramCounts: Map[Int, Int], bitermCounts: Map[Int, Int])(prefix:String, firstCounts: Int) = {

    import edu.washington.cs.knowitall.relgrams.measures.MeasureFunctions._
    var nameMeasures = Seq[(String, WindowedMeasure)]()
    val firstUndirConditionals = conditionals(bitermCounts, smoothingDelta, firstCounts)
    val firstUndirConditional = combine(firstUndirConditionals, windowAlpha)
    nameMeasures = nameMeasures :+("undir.%s.conditional".format(prefix), new WindowedMeasure(firstUndirConditionals, firstUndirConditional))

    val firstDirConditionals = conditionals(bigramCounts, smoothingDelta, firstCounts)
    val firstDirConditional = combine(firstDirConditionals, windowAlpha)
    nameMeasures = nameMeasures :+("dir.%s.conditional".format(prefix), new WindowedMeasure(firstDirConditionals, firstDirConditional))

    nameMeasures
  }
}
case class NamedMeasures(measures:Map[String, WindowedMeasure]){

  import NamedMeasures._

  def serialize():String = measures.map(m => m._1 + nsep + m._2.serialize).mkString(msep)

  override def toString() :String = measures.map(m => m._1 + nsep + m._2.toString).mkString(msep)
}



