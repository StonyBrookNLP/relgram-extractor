package edu.washington.cs.knowitall.relgrams.solr

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/14/13
 * Time: 3:02 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import edu.washington.cs.knowitall.relgrams.RelationTuple
import org.apache.solr.client.solrj.SolrQuery
import edu.washington.cs.knowitall.relgrams.utils.RegexUtils

object RelgramsSolrUtil {

  val dummyTuple = RelationTuple.fromArg1RelArg2("", "", "")

  def toSolrQuery(firstTuple:RelationTuple):Option[SolrQuery] =  toSolrQuery(firstTuple, dummyTuple)

  def toSolrQuery(firstTuple:RelationTuple, secondTuple:RelationTuple):Option[SolrQuery] = {
    val solrQuery = new SolrQuery()
    solrQuery.setQuery("*:*")
    solrQuery.setRows(10000)
    def isEmpty(string:String) = string.isEmpty

    def scrub(string:String) = RegexUtils.replaceSpecialCharsExcept(string, ":", " ")

    if (!isEmpty(firstTuple.arg1)) solrQuery.addFilterQuery("""farg1:"%s"""".format(scrub(firstTuple.arg1)))
    if (!isEmpty(firstTuple.rel)) solrQuery.addFilterQuery("""frel:"%s"""".format(scrub(firstTuple.rel)))
    if (!isEmpty(firstTuple.arg2)) solrQuery.addFilterQuery("""farg2:"%s"""".format(scrub(firstTuple.arg2)))
    if (!isEmpty(secondTuple.arg1)) solrQuery.addFilterQuery("""sarg1:"%s"""".format(scrub(secondTuple.arg1)))
    if (!isEmpty(secondTuple.rel)) solrQuery.addFilterQuery("""srel:"%s"""".format(scrub(secondTuple.rel)))
    if (!isEmpty(secondTuple.arg2)) solrQuery.addFilterQuery("""sarg2:"%s"""".format(scrub(secondTuple.arg2)))
    if (solrQuery.getFilterQueries.size > 0){
      Some(solrQuery)
    }else{
      None
    }

    //solrQuery.addSort("farg1", SolrQuery.ORDER.asc )
    //solrQuery.addSort("frel", SolrQuery.ORDER.asc )
    //solrQuery.addSort("farg2", SolrQuery.ORDER.asc )
  }
}
