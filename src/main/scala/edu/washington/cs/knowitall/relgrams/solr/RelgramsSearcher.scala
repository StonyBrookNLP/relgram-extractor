package edu.washington.cs.knowitall.relgrams.solr

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/14/13
 * Time: 3:14 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import org.apache.solr.client.solrj.impl.{XMLResponseParser, HttpSolrServer}
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.relgrams.{Relgram, AffinityMeasures, Measures, RelationTuple}
import edu.washington.cs.knowitall.relgrams.solr.RelgramsSolrUtil._
import scala.Some
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument
import scala.actors.Futures._
import scala.Some
import org.apache.solr.client.solrj.response.QueryResponse

object RelgramsSearcher {

  val logger = LoggerFactory.getLogger(this.getClass)
  def getInstance(url:String, timeoutMS:Long = 10000) = {
    logger.info("Solr Relgrams Url: " + url)
    val server = new HttpSolrServer(url)
    server.setParser(new XMLResponseParser)
    logger.info("Initialized relgram server: " + server.ping())
    new RelgramsSearcher(server, timeoutMS)
  }
}

class RelgramsSearcher(server:HttpSolrServer, timeoutMS:Long) {
  val logger = LoggerFactory.getLogger(this.getClass)

  val reportingVerbs = Set("say", "tell", "ask", "report", "describe")
  def isNotAReportingVerb(relgram:Relgram) = ! (relgram.second.rel.split(" ") exists reportingVerbs)
  def noReportingVerbs(string:String):Boolean = noReportingVerbs(string.split(" "))
  def noReportingVerbs(words:Iterable[String]):Boolean = !(words exists reportingVerbs)

  def runWithTimeout(timeoutMs: Long)(f: => QueryResponse): Option[QueryResponse] = {
    awaitAll(timeoutMs, future(f)).head match {
      case Some(x:Any) => Some(x.asInstanceOf[QueryResponse])
      case None => {
        None
      }
    }
  }



  def search(firstTuple:RelationTuple,
             secondTuple:RelationTuple = dummyTuple,
             exact:Boolean = false,
             filterReportingVerbs:Boolean = false): Seq[Measures] = {

    def exactMatch(p:SolrDocument) = {
      def tupleMatch(qtuple:RelationTuple, arg1:String, rel:String, arg2:String): Boolean = {
        if(qtuple.isIdenticalTo(dummyTuple)){
          return true
        }
        qtuple.arg1.equalsIgnoreCase(arg1) && qtuple.rel.equalsIgnoreCase(rel) && qtuple.arg2.equalsIgnoreCase(arg2)
      }
      tupleMatch(firstTuple, p.getFieldValue("farg1").toString, p.getFieldValue("frel").toString, p.getFieldValue("farg2").toString) &&
        tupleMatch(secondTuple, p.getFieldValue("sarg1").toString, p.getFieldValue("srel").toString, p.getFieldValue("sarg2").toString)
    }
    toSolrQuery(firstTuple, secondTuple) match {
      case Some(solrQuery:SolrQuery) => {
        logger.debug("SolrQuery: " + solrQuery)
        def queryResponse:QueryResponse = {
          server.query(solrQuery)
        }
        val search = runWithTimeout(timeoutMS) _

        val response = if(timeoutMS > 0) search(queryResponse) else Some(queryResponse)
        response match {//server.query(solrQuery)
          case Some(results:QueryResponse) => {
            logger.debug("Query: %s returned %d solr documents.".format(solrQuery.toString, results.getResults.size))
            results.getResults
              .filter(p => !exact || exactMatch(p)) // Check if tuple matches query tuple exactly.
              .filter(p  => !filterReportingVerbs || (noReportingVerbs(p.getFieldValue("frel").toString) && noReportingVerbs(p.getFieldValue("srel").toString)))
              .flatMap(result => {
              Measures.fromSerializedString(result.getFirstValue("serialize").toString)
            })//.filter(p => !filterReportingVerbs || isNotAReportingVerb(p.urgc.rgc.relgram))
          }
          case None => {
            logger.error("Query: %s timed out. No results returned.".format(solrQuery.toString))
            Seq[Measures]()
          }
        }
      }
      case None => Seq[Measures]()
    }
  }

/**  def search(firstTuple:RelationTuple,
             secondTuple:RelationTuple = dummyTuple,
             filterReportingVerbs:Boolean = false): Seq[Measures] = {

    toSolrQuery(firstTuple, secondTuple) match {
      case Some(solrQuery:SolrQuery) => {
        logger.info("SolrQuery: " + solrQuery)
        val results = server.query(solrQuery)
        logger.info("Query: %s returned %d solr documents.".format(solrQuery.toString, results.getResults.size))
        results.getResults
          .filter(p  => !filterReportingVerbs || (noReportingVerbs(p.getFieldValue("frel").toString) && noReportingVerbs(p.getFieldValue("srel").toString)))
          .flatMap(result => {
            Measures.fromSerializedString(result.getFirstValue("serialize").toString)
        })//.filter(p => !filterReportingVerbs || isNotAReportingVerb(p.urgc.rgc.relgram))
      }
      case None => Seq[Measures]()
    }
  }   */
}
