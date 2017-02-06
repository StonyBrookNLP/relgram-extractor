package edu.washington.cs.knowitall.relgrams.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/6/13
 * Time: 11:21 AM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import util.matching.Regex

object RegexUtils {
  val logger = LoggerFactory.getLogger(this.getClass)

  val hasNumberRe = """[0-9]""".r
  def hasNumber(string:String) = hasNumberRe.findFirstMatchIn(string).isDefined

  def matchesRe(re:Regex)(string:String) = re.findFirstMatchIn(string) != None
  val specialCharsExcept = """[^0-9a-zA-Z%s]"""
  def hasSpecialCharsExcept(except:String): (String) => Boolean = {
    val re = specialCharsExcept.format(except).r
    matchesRe(re)_
  }

  def replaceSpecialCharsExcept(string:String, except:String, replaceString:String) = {
    string.replaceAll(specialCharsExcept.format(except), replaceString)
  }


  val firstSepRest = """(.*?)%s(.*)"""

  val firstTabRestRe = firstSepRest.format("\t").r

  val untilLastSepRestRe = """(.*)%s(.*)"""

  val untilLastTabRestRe = untilLastSepRestRe.format("\t").r

  def firstRestSplit(string:String):(String, String) = {
    val firstTabRestRe(first:String, rest:String) = string
    return (first, rest)

  }

  def firstSepRestSplit(string:String, sep:String):(String, String) = {
    val splitRe = firstSepRest.format(sep).r
    val splitRe(first:String, rest:String) = string
    return (first, rest)

  }

  def nthField(string:String, sep:String, n:Int):Option[String] = nthField(string.split(sep), n)

  def nthField(splits: Array[String], n: Int): Option[String] = {
    if (splits.size > n) {
      return Some(splits(n))
    }
    return None
  }


  def toDoubles(x:Seq[String]):Seq[Double] = x.map(y => y.toDouble)

}
