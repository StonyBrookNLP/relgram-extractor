package edu.washington.cs.knowitall.relgrams.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/7/13
 * Time: 8:39 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import java.io.File
import io.Source

object FileUtil {

  def getLines(file:File) = Source.fromFile(file).getLines

  def getLines(path:String) = Source.fromFile(path).getLines


}
