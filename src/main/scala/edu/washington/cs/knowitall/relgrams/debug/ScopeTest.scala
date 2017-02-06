package edu.washington.cs.knowitall.relgrams.debug

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/9/13
 * Time: 3:22 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory

object ScopeTest {

  val logger = LoggerFactory.getLogger(this.getClass)


  def main(args:Array[String]){


    val seq = (0 until 5).toSeq
    val (powers, count) = powersAndCount(seq)
    logger.info("Final count after return: " + count)
    logger.info("Sum of powers: " + powers.sum)
  }

  def powersAndCount(seq: Range) = {
    var count: Int = 0
    val powers = seq.map(i => i * 2).map(k => {
      if (k % 2 == 0) {
        count = count + 1
        logger.info("Count: " + count)
      }
      k * k
    })
    logger.info("Final count before return: " + count)
    (powers, count)
  }
}
