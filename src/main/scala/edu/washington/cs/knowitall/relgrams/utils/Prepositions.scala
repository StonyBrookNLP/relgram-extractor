package edu.washington.cs.knowitall.relgrams.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/6/13
 * Time: 12:06 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
object Prepositions {
  val logger = LoggerFactory.getLogger(this.getClass)
  val preps = Set[String]("a","abaft","aboard","about","above","absent","across","afore","after","against","along",
                          "alongside","amid","amidst","among","amongst","an","anenst","apropos","apud","around","as",
                          "aside","astride","at","athwart","atop","barring","before","behind","below","beneath",
                          "beside","besides","between","beyond","but","by", "circa","concerning","despite","down",
                          "during","except","excluding","failing","following", "for","forenenst","from","given","in",
                          "including","inside","into","lest","like","mid","midst","minus","modulo","near","next",
                          "notwithstanding","of","off","on","onto","opposite","out","outside","over","pace","past",
                          "per","plus","pro","qua","regarding","round","sans","save","since","than","through",
                          "throughout","till","times","to","toward","towards","under","underneath","unlike","until",
                          "unto","up","upon","versus","via","vice","with","within","without","worth")

  def isPreposition(word:String) = preps.contains(word)
}
