package edu.washington.cs.knowitall.relgrams.solr

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 3/24/13
 * Time: 8:43 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import com.nicta.scoobi.application.ScoobiApp
import scopt.mutable.OptionParser
import edu.washington.cs.knowitall.relgrams.{TuplesDocument, TuplesDocumentWithCorefMentions}
import com.nicta.scoobi.core.DList
import com.nicta.scoobi.io.text.{TextOutput, TextInput}
import org.apache.solr.client.solrj.impl.{XMLResponseParser, HttpSolrServer}
import org.slf4j.LoggerFactory
import org.apache.solr.common.SolrInputDocument
import collection.mutable

import com.nicta.scoobi.Persist._
import io.Source
import java.io.{FilenameFilter, File}

class ToSolrTuplesDocument(solrServer:HttpSolrServer) {
  val logger = LoggerFactory.getLogger(this.getClass)
  def addToIndex(id:Int, document:TuplesDocument) {
    document.tupleRecords.foreach(record => {
    val solrDoc = new SolrInputDocument
    solrDoc.addField("id", document.docid + "-" + record.sentid + "-" + record.extrid)
    solrDoc.addField("docid", document.docid)
    solrDoc.addField("serialize", record.toString)
    solrDoc.addField("sentence", record.sentence)
    solrDoc.addField("arg1", record.arg1)
    solrDoc.addField("arg1Head", record.arg1Head)
    solrDoc.addField("rel", record.rel)
    solrDoc.addField("relHead", record.relHead)
    solrDoc.addField("arg2", record.arg2)
    solrDoc.addField("arg2Head", record.arg2Head)
    solrDoc.addField("arg1Types", record.arg1Types.mkString(","))
    solrDoc.addField("arg2Types", record.arg2Types.mkString(","))
    solrServer.add(solrDoc)
    })
  }

}


object IndexTupleDocuments extends ScoobiApp{

  def run() {
    var inputPath, solrURL, outputPath = ""
    val parser = new OptionParser() {
      arg("inputPath", "hdfs input path", {str => inputPath = str})
      arg("solr", "solr server path", {str => solrURL = str})
      arg("outputPath", "hdfs output path", { str => outputPath = str })
    }
    if (!parser.parse(args)) return
    import TuplesDocumentWithCorefMentions._
    println("This is run once....")
    println("inputPath: " + inputPath)
    println("solr: " + solrURL)
    println("outputPath: " + outputPath)
    def loadTupleDocuments(inputPath: String):DList[TuplesDocumentWithCorefMentions] = {
      TextInput.fromTextFile(inputPath)
        .flatMap(x => TuplesDocumentWithCorefMentions.fromString(x))
    }
    val toSolrs = new mutable.HashMap[Thread, ToSolrTuplesDocument] with mutable.SynchronizedMap[Thread, ToSolrTuplesDocument]
    def getToSolr(solrURL:String) = {
      val solrServer = new HttpSolrServer(solrURL)
      solrServer.setParser(new XMLResponseParser())
      new ToSolrTuplesDocument(solrServer)
    }

    val tupleDocuments = loadTupleDocuments(inputPath)
    val out:DList[String] = tupleDocuments.flatMap(td => {
      val toSolr = toSolrs.getOrElseUpdate(Thread.currentThread(), getToSolr(solrURL))
      toSolr.addToIndex(td.tuplesDocument.docid.hashCode, td.tuplesDocument)
      None
    })
    persist(TextOutput.toTextFile(out, outputPath))

  }
}

object IndexTupleDocumentsLocal {

  val logger = LoggerFactory.getLogger(this.getClass)
  def main(args:Array[String]) {
    var inputPath = ""
    val parser = new OptionParser() {
      arg("inputPath", "hdfs input path", {str => inputPath = str})
      arg("solr", "solr server path", {str => solrURL = str})
    }
    if (!parser.parse(args)) return
    println("inputPath: " + inputPath)
    println("solr: " + solrURL)
    processFiles(new File(inputPath))
  }

  val toSolrs = new mutable.HashMap[Thread, ToSolrTuplesDocument] with mutable.SynchronizedMap[Thread, ToSolrTuplesDocument]
  var solrURL = ""
  def getToSolr(): ToSolrTuplesDocument = getToSolr(solrURL)
  def getToSolr(solrURL:String): ToSolrTuplesDocument = {
    val solrServer = new HttpSolrServer(solrURL)
    solrServer.setParser(new XMLResponseParser())
    new ToSolrTuplesDocument(solrServer)
  }
    val dotFilesFilter = new FilenameFilter{
      def accept(dir: File, name: String): Boolean = !(name.equals(".") || name.equals(".."))
    }
  def processFiles(dir:File){
    def processFile(file:File) = {
      Source.fromFile(file).getLines().toSeq.par.foreach(line => {
        TuplesDocumentWithCorefMentions.fromString(line) match {
          case Some(td:TuplesDocumentWithCorefMentions) => {
            val toSolr = toSolrs.getOrElseUpdate(Thread.currentThread(), getToSolr)
            toSolr.addToIndex(td.tuplesDocument.docid.hashCode, td.tuplesDocument)
          }
          case None => println("Failed to construct TuplesDocumentWithCorefMentions from string: " + line)
        }

      })
    }
    val files = if(dir.isDirectory) dir.listFiles(dotFilesFilter) else (dir::Nil).toArray
    if(files != null){
      files.map(file => file.isDirectory match {
        case true => processFiles(file)
        case false => processFile(file)
      })
    }
  }

}
