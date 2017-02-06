package edu.washington.cs.knowitall.relgrams.solr

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/14/13
 * Time: 3:05 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import org.apache.solr.client.solrj.SolrQuery

object TupleDocumentsSolrUtil {

  def typedRecordsQuery(arg1: String, rel: String, arg2: String, numResults:Int): SolrQuery = {
    val solrQuery = new SolrQuery()
    solrQuery.setQuery("*:*")
    solrQuery.setRows(numResults)
    def isEmpty(string: String) = string.isEmpty

    if (!isEmpty(arg1)) {
      if (!arg1.contains("type:"))
        solrQuery.addFilterQuery( """arg1Head:%s""".format(arg1))
      else
        solrQuery.addFilterQuery( """arg1Types:"%s"""".format(arg1.replaceAll("type:", "Type:")))
    } //"farg1:" + query.relationTuple.arg1)
    if (!isEmpty(rel)) solrQuery.addFilterQuery( """relHead:"%s"""".format(rel))
    if (!isEmpty(arg2)) {
      if (!arg2.contains("type:"))
        solrQuery.addFilterQuery( """arg2Head:%s""".format(arg2))
      else
        solrQuery.addFilterQuery( """arg2Types:"%s"""".format(arg2.replaceAll("type:", "Type:")))
    }
    solrQuery
  }


}
