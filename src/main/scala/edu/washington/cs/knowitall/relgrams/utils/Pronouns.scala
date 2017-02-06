package edu.washington.cs.knowitall.relgrams.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/6/13
 * Time: 11:12 AM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
object Pronouns {
  val logger = LoggerFactory.getLogger(this.getClass)
  val pronouns = Set[String]("he", "she", "they", "it", "we", "they", "i", "you")
  def isPronoun(word:String) = pronouns.contains(word)
}
