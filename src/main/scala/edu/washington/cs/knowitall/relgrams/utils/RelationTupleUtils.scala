package edu.washington.cs.knowitall.relgrams.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 6/20/13
 * Time: 8:41 AM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.relgrams.{Relgram, RelationTuple}
import edu.washington.cs.knowitall.relgrams.utils.Pronouns._

object RelationTupleUtils {
  val logger = LoggerFactory.getLogger(this.getClass)

  def toVerb(string:String) = {
    string.split(" ").filter(x => !(x.equals("be") || Prepositions.isPreposition(x))).mkString(" ")
  }

  def toVerbOption(string:String) = {
    val tokens = string.split(" ").filter(x => !(x.equals("be") || Prepositions.isPreposition(x))).filter(!_.trim.isEmpty)
    if(!tokens.isEmpty){
      Some(tokens.mkString(" "))
    }else{
      None
    }
  }

  val reportingVerbs = Set("say", "tell", "ask", "report", "describe")

  def notReportingRelation(rel:String):Boolean = !(rel.split(" ") exists reportingVerbs)
  def notReportingRelation(tuple:RelationTuple):Boolean = notReportingRelation(tuple.rel)
  def noReportingRelations(relgram:Relgram) = notReportingRelation(relgram.first) && notReportingRelation(relgram.second)
  def stripVar(arg:String) = arg.replaceAll("XVAR:", "").replaceAll("YVAR:", "")
  def notPronoun(arg:String) = !isPronoun(stripVar(arg))
  def hasNoPronounArg(tuple:RelationTuple): Boolean = notPronoun(tuple.arg1) && notPronoun(tuple.arg2)
  def hasNoPronounArg(relgram:Relgram): Boolean = hasNoPronounArg(relgram.first) && hasNoPronounArg(relgram.second)


}