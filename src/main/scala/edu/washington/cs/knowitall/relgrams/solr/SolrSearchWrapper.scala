package edu.washington.cs.knowitall.relgrams.solr

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 3/12/13
 * Time: 12:21 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.relgrams.web.{MeasureName, RelgramsQuery}
import org.apache.solr.client.solrj.{SolrQuery, SolrServer}
import org.apache.solr.client.solrj.impl.{XMLResponseParser, HttpSolrServer}

import scala.collection.JavaConversions._
import edu.washington.cs.knowitall.relgrams._
import RelgramsSolrUtil._
import TupleDocumentsSolrUtil._

import javax.xml.stream.XMLResolver
import org.apache.solr.common.SolrDocument
import org.apache.solr.client.solrj.response.QueryResponse
import scala.Some
import collection.mutable
import collection.mutable.ArrayBuffer


object SolrSearchWrapper{
  val logger = LoggerFactory.getLogger(this.getClass)
  def solrServerInstance(url:String) = {
    logger.info("Solr Relgrams Url: " + url)
    val server = new HttpSolrServer(url)
    server.setParser(new XMLResponseParser)
    logger.info("Initialized relgram server: " + server.ping())
    server
  }
}

class SolrSearchWrapper {

  var docServer:HttpSolrServer = null
  var server:HttpSolrServer = null
  val logger = LoggerFactory.getLogger(this.getClass)
  def this(solrBaseUrl:String, solrDocUrl:String) = {
    this()
    setupRelgramsServer(solrBaseUrl)
    setupDocServer(solrDocUrl)
  }

  import SolrSearchWrapper._
  def setupRelgramsServer(solrBaseUrl: String) {
    server = solrServerInstance(solrBaseUrl)
  }

  def setupDocServer(solrDocUrl: String) {
    logger.info("SolrDocUrl: " + solrDocUrl)
    try{
      docServer = solrServerInstance(solrDocUrl)
    } catch {
      case e:Exception => {
        logger.error("Failed to set up doc server for url: " + solrDocUrl)
        logger.error("Doc search services will throw exceptions.")
      }
    }
  }

  def findDocumentTuples(docid: String, sort:Boolean = true) = {
    val solrQuery = new SolrQuery
    solrQuery.setQuery("*.*")
    solrQuery.setRows(10000)
    solrQuery.addFilterQuery("docid:" + docid)
    val records = docServer.query(solrQuery).getResults.flatMap(result => {
      TypedTuplesRecord.fromString(result.getFieldValue("serialize").toString)
    })
    if(sort) records.sortBy(record => (record.sentid, record.extrid)) else records

  }



  def findTypedTuplesRecord(id:String):Option[TypedTuplesRecord] = {
    val solrQuery = new SolrQuery
    solrQuery.setQuery("*.*")
    solrQuery.setRows(10000)
    solrQuery.addFilterQuery("id:%s".format(id))
    println("SolrQuery: " + solrQuery.toString)
    val results = docServer.query(solrQuery)
    toTypedTuplesRecords(results, id)

  }

  def toTypedTuplesRecords(results: QueryResponse, id: String): Option[TypedTuplesRecord] = {
    results.getResults.find(x => x.getFieldValue("id").toString.equals(id)) match {
      case Some(idresult: SolrDocument) => {
        TypedTuplesRecord.fromString(idresult.getFieldValue("serialize").toString)
      }
      case _ => None
    }
  }

  def findDocsContainingTuple(arg1:String, rel:String, arg2:String) = {
    val solrQuery: SolrQuery = typedRecordsQuery(arg1, rel, arg2, 100000)
    if (solrQuery.getFilterQueries.size > 0){
      println("Solr Query: "  + solrQuery.toString)
      val results = docServer.query(solrQuery)
      Some(results.getResults.map(result => result.getFieldValue("docid").toString).toSet)
    }else{
      None
    }
  }

  def recordsMatchingTuple(queryTuple:RelationTuple, filterTuple:RelationTuple, numResults:Int): Seq[TypedTuplesRecord] = {
    var records = List[TypedTuplesRecord]()
    val blockSize = 10000
    val query = typedRecordsQuery(queryTuple.arg1, queryTuple.rel, queryTuple.arg2, blockSize)

    (0 until 10).foreach( start => {
      logger.info("Page: " + start + " Matching records size: " + records.size)
      query.setStart(start*blockSize)
      query.setRows(blockSize)
      val results = docServer.query(query)
      def matchesFilterTuple(record:TypedTuplesRecord, filterTuple:RelationTuple):Boolean = {
        if(!record.relHead.trim.equals(filterTuple.rel)) return false
        val arg1s = record.arg1Head::Nil ++ record.arg1Types.map(a => a.toLowerCase)
        val arg2s = record.arg2Head::Nil ++ record.arg2Types.map(a => a.toLowerCase)
        for(arg1 <- arg1s){
          if(arg1.equals(filterTuple.arg1)){
            for (arg2 <- arg2s){
              if(arg2.equals(filterTuple.arg2)) return true
            }
          }
        }
        return false
      }
      results.getResults.iterator.foreach(result => {
        if(records.size < numResults){
          TypedTuplesRecord.fromString(result.getFieldValue("serialize").toString) match {
            case Some(record:TypedTuplesRecord) => {
              if(matchesFilterTuple(record, filterTuple)) {
                records :+= record
              }
            }
            case _ =>
          }
        }else{
          return records
        }
      })
    })
    return records

  }

  def recordsMatchingTuple(tuple:RelationTuple, numResults:Int):Seq[TypedTuplesRecord] = recordsMatchingTuple(tuple.arg1, tuple.rel, tuple.arg2, numResults)

  def recordsMatchingTuple(arg1:String, rel:String, arg2:String,  numResults:Int): Seq[TypedTuplesRecord] = {
    val query = typedRecordsQuery(arg1, rel, arg2, numResults)
    docServer.query(query)
             .getResults
             .flatMap(result => TypedTuplesRecord.fromString(result.getFieldValue("serialize").toString))
             .toSeq
             .take(numResults)
  }

  val reportingVerbs = Set("say", "tell", "ask", "report", "describe")
  def isNotAReportingVerb(relgram:Relgram) = ! (relgram.second.rel.split(" ") exists reportingVerbs)

  def search(firstTuple:RelationTuple,
             secondTuple:RelationTuple = dummyTuple,
             filterReportingVerbs:Boolean = false): Seq[(Measures, AffinityMeasures)] = {
  //def search(firstTuple:RelationTuple, secondTuple:RelationTuple, filterReportingVerbs:Boolean): Seq[(Measures, AffinityMeasures)] = {
    toSolrQuery(firstTuple, secondTuple) match {
      case Some(solrQuery:SolrQuery) => {
        logger.info("SolrQuery: " + solrQuery)
        val results = server.query(solrQuery)
        logger.info("Query: %s returned %d solr documents.".format(solrQuery.toString, results.getResults.size))
        results.getResults
          .flatMap(result => {
          val moption = Measures.fromSerializedString(result.getFirstValue("serialize").toString)
          val affOption =  AffinityMeasures.fromSerializedString(result.getFirstValue("affinities").toString)
          (moption, affOption) match {
            case (Some(m:Measures), Some(aff:AffinityMeasures)) => Some((m, aff))
            case _ => None
          }
        }).filter(p => !filterReportingVerbs || isNotAReportingVerb(p._1.urgc.rgc.relgram))
      }
      case None => Seq[(Measures, AffinityMeasures)]()
    }
  }

}

object SolrSearchWrapperTest{
  val logger = LoggerFactory.getLogger(this.getClass)
  def main(args:Array[String]){
    val url = args(0)
    val docurl = args(1)
    val testid = args(2)
    val server = new SolrSearchWrapper(url, docurl)

    /**val query = new RelgramsQuery(RelationTuple.fromArg1RelArg2("", "pay", ""),MeasureName.bigram, 10, 0.5, 0.5, "ARG1RELARG2", "conditional", "both")
    logger.info("relgramsQuery: " + query.toHTMLString)
    val results = server.search(query)
    logger.info(results.mkString(","))
     */
    server.findTypedTuplesRecord(testid) match {
      case Some(record:TypedTuplesRecord) => logger.info("Found record: " + record.toString)
      case None => logger.info("Failed to find record for id: " + testid)
    }

  }
}