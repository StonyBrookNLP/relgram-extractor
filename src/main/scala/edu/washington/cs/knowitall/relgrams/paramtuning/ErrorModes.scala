package edu.washington.cs.knowitall.relgrams.paramtuning

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/10/13
 * Time: 9:27 AM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import io.Source
import edu.washington.cs.knowitall.relgrams.utils.RegexUtils

import RegexUtils._
import TaggedRelgramWithCounts._
import java.io.PrintWriter

object ErrorModes {

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args:Array[String]){
    val file = args(0)
    val outfile = args(1)
    val writer = new PrintWriter(outfile, "UTF-8")
    val scoredRelgrams: List[ScoredTaggedRelgramWithCounts] = ScoredTaggedRelgramWithCounts.fromFile(file)
    scoredRelgrams.groupBy(x => x.trgc.taggedRelgram.relgramLabel)
                  .foreach(group => {

      println("Group: " + group._1)
      val relgrams = group._1 match {
        case 1 => group._2.sortBy(_.score)
        case 0 => group._2.sortBy(-_.score)
      }
      println("Relgrams size:  " + relgrams.size)
      relgrams.take(100).foreach(relgram => {
        writer.println(group._1 + "\t" + relgram.score + "\t" + relgram.trgc.toPrettyString)
      })

    })
    writer.close
  }


}
