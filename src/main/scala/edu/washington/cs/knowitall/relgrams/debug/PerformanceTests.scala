package edu.washington.cs.knowitall.relgrams.debug

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/4/13
 * Time: 12:33 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory

object PerformanceTests {
    val logger = LoggerFactory.getLogger(this.getClass)

  def main(args:Array[String]){
    splitVersusCountTest
  }
  def splitVersusCountTest = {
    val string = "This is a test string of some size. And some other size      as well."
    var start = System.currentTimeMillis()
    var sum = 0
    (0 until 10000).foreach(i =>{
      val size = string.split(" ").size
      sum += size
    })
    println("Sum: " + sum)
    var end = System.currentTimeMillis()
    var time = (end-start)/1000.00
    println("Time for split: %.6f".format(time))

    start = System.currentTimeMillis()
    sum = 0
    (0 until 10000).foreach(i =>{
      val size = string.count(p => p == ' ')
      sum += size
    })
    println("Sum: " + sum)
    end = System.currentTimeMillis()
    time = (end-start)/1000.00
    println("Time for count: %.6f".format(time))


  }
}
