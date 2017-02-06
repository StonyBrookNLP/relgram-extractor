package edu.washington.cs.knowitall.relgrams.web

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 3/12/13
 * Time: 11:54 AM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import unfiltered.request.{HttpRequest, GET, Path}

import edu.washington.cs.knowitall.relgrams._

import edu.washington.cs.knowitall.relgrams.solr.SolrSearchWrapper
import unfiltered.response.{Html, ResponseString}
import scopt.mutable.OptionParser
import collection.mutable
import scala.xml.{XML, Node, Elem, NodeBuffer}
import util.matching.Regex.Match
import unfiltered.netty.ReceivedMessage
import collection.mutable.ArrayBuffer
import scala.io.Source
import org.jboss.netty.handler.codec.http.HttpHeaders


object MeasureName extends Enumeration("bigram", "biterm", "psgf", "pfgs", "psgf_undir", "pmi", "npmi", "pmi_undir", "npmi_undir",
  "psgf_combined", "pfgs_combined",  "psgf_undir_combined", "pfgs_undir_combined",
  "pmi_combined", "npmi_combined", "pmi_undir_combined", "npmi_undir_combined",
  "pfgs_undir", "avg_psgf_pfgs",
  "scp", "scp_undir", "scp_combined", "scp_undir_combined"){


  type MeasureName = Value
  val bigram,
  biterm,
  psgf, pfgs,
  psgf_undir,
  pmi, npmi,
  pmi_undir, npmi_undir,
  psgf_combined, pfgs_combined, psgf_undir_combined, pfgs_undir_combined,
  pmi_combined, npmi_combined,
  pmi_undir_combined, npmi_undir_combined,
  pfgs_undir,
  avg_psgf_pfgs, scp, scp_undir, scp_combined, scp_undir_combined = Value

}


import MeasureName._
class RelgramsQuery(val relationTuple:RelationTuple,
                    val measure:MeasureName,
                    val measureIndex:Int,
                    val alpha:Double,
                    val smoothingDelta:Double,
                    val outputView:String,
                    val sortBy:String,
                    val equalityOption:String,
                    val typeOption:String,
                    val filterReportingVerbs:Boolean,
                    val minFreq:Int){

  override def toString:String = "%s\t%s\t%d\t%s\t%s\t%s\t%s\t%s\t%d".format(relationTuple.toString(), measure, measureIndex, outputView, sortBy, equalityOption, typeOption, filterReportingVerbs, minFreq)
  def toHTMLString(relationTuple:RelationTuple):String = "(%s;%s;%s)".format(relationTuple.arg1, relationTuple.rel, relationTuple.arg2)
  def toHTMLString:String = "%s\t%s\t%d\t%s\t%s\t%s\t%s\t%s\t%d".format(toHTMLString(relationTuple), measure, measureIndex, outputView, sortBy, equalityOption, typeOption, filterReportingVerbs, minFreq)

}


object ReqHelper{
  def getDocids(req: HttpRequest[Any]) = getParamValue("docids", req)


  def getParamValue(paramName:String, req:HttpRequest[Any]):String = req.parameterNames.contains(paramName) match {
    case true => req.parameterValues(paramName)(0).trim
    case false => ""
  }
  def getParamValues(paramName:String, req:HttpRequest[Any]):Seq[String] = req.parameterNames.contains(paramName) match {
    case true => req.parameterValues(paramName).map(x => x.trim)
    case false => Seq[String]()
  }
  def getEqualityOption(req:HttpRequest[Any]) = {
    val values = getParamValues("equalityOption", req)
    if (values.size > 1)
      "both"
    else if(values.size == 1)
      values(0)
    else
      ""

  }

  def getTypeOption(req:HttpRequest[Any]) = {
    val values = getParamValues("typeOption", req)
    if (values.size > 1)
      "both"
    else if(values.size == 1)
      values(0)
    else
      ""

  }

  def getDocid(req: HttpRequest[Any]) = getParamValue("docid", req)

  def getIds(req: HttpRequest[Any]):String = getParamValue("ids", req)
  //def getFirstIds(req: HttpRequest[Any]):String = getParamValue("firstids", req)
  //def getSecondIds(req: HttpRequest[Any]):String = getParamValue("secondids", req)

  def getSortBy(req:HttpRequest[Any]) = getParamValue("sortBy", req)
  def getArg1(req:HttpRequest[Any]) = getParamValue("arg1", req)
  def getRel(req:HttpRequest[Any]) = getParamValue("rel", req)
  def getArg2(req:HttpRequest[Any]) = getParamValue("arg2", req)
  def getMeasure(req:HttpRequest[Any]) = try { MeasureName.withName(getParamValue("measure", req)) } catch { case  e:Exception => MeasureName.psgf }
  def getAlpha(req:HttpRequest[Any]) = try {getParamValue("alpha", req).toDouble} catch { case e:Exception => -1.0}
  def getSmoothingDelta(req:HttpRequest[Any]) = try {getParamValue("delta", req).toDouble} catch { case e:Exception => -1.0}

  def getIndex(req:HttpRequest[Any]) = try { getParamValue("k", req).toInt } catch { case e:Exception => 50}

  def getOutputView(req:HttpRequest[Any]) = getParamValue("v", req)

  def getMinFreq(req:HttpRequest[Any]) = try { getParamValue("minFreq", req).toInt } catch { case e:Exception => 1 }


  def getFilterReportingVerbs(req:HttpRequest[Any]) = try {
    getParamValue(HtmlHelper.reportingVerbParamName, req).toBoolean } catch {case e:Exception => true}
  def getRelgramsQuery(req:HttpRequest[Any]) = {
    new RelgramsQuery(RelationTuple.fromArg1RelArg2(getArg1(req),getRel(req),getArg2(req)),
                          getMeasure(req),
                          getIndex(req),
                          getAlpha(req),
                          getSmoothingDelta(req),
                          getOutputView(req),
                          getSortBy(req),
                          getEqualityOption(req),
                          getTypeOption(req),
                          getFilterReportingVerbs(req),
                          getMinFreq(req))
  }
}

object HtmlHelper{

  def viewOptions(query:RelgramsQuery) = {
    var anySelected, arg1relselected, relarg2selected, arg1relarg2selected = ""
    query.outputView match {
      case "Any" => anySelected = "selected"
      case "ARG1REL" => arg1relselected = "selected"
      case "RELARG2" => relarg2selected = "selected"
      case "ARG1RELARG2" => arg1relarg2selected = "selected"
      case _ => anySelected = "selected"

    }
    "<select name=\"v\">\n" +
      "<option value=\"Any\" " +  anySelected + " >Any</option>\n" +
      "<option value=\"ARG1REL\" " +  arg1relselected + " >ARG1REL</option>\n"  +
      "<option value=\"RELARG2\" " + relarg2selected + ">RELARG2</option>\n" +
      "<option value=\"ARG1RELARG2\" " + arg1relarg2selected + ">ARG1RELARG2</option>\n" +
      "</select><br/><br/><br/>\n"

  }

  def optionString(measureName:String, selectedString:String, displayName:String) = {
    "<option value=\"%s\" %s>%s</option>\n".format(measureName, selectedString, displayName)//
  }
  def measureOptions(query: RelgramsQuery): String = {

    var bigramSelected, bitermSelected, psgfSelected, psgfUndirSelected, pmiSelected, npmiSelected = ""
    var psgfCombinedSelected,psgfUndirCombinedSelected,pmiCombinedSelected, npmiCombinedSelected = ""
    query.measure match {
      case MeasureName.bigram => bigramSelected = "selected"
      case MeasureName.biterm => bitermSelected = "selected"
      case MeasureName.psgf => psgfSelected = "selected"
      case MeasureName.psgf_undir => psgfUndirSelected = "selected"
      case MeasureName.pmi => pmiSelected = "selected"
      case MeasureName.npmi => npmiSelected = "selected"
      case MeasureName.psgf_combined => psgfCombinedSelected = "selected"
      case MeasureName.psgf_undir_combined => psgfUndirCombinedSelected = "selected"
      case MeasureName.pmi_combined => pmiCombinedSelected = "selected"
      case MeasureName.npmi_combined => npmiCombinedSelected = "selected"

      case _ => bigramSelected = "selected"

    }


    "<select name=\"measure\">\n" +
      optionString(MeasureName.bigram.toString, bigramSelected, "#(F,S)") +
      optionString(MeasureName.biterm.toString, bitermSelected, "#(F,S) + #(S,F)") +
      optionString(MeasureName.psgf.toString, psgfSelected, "Dir: P(S|F)") +
      optionString(MeasureName.psgf_undir.toString, psgfUndirSelected, "UnDir: P(S|F)") +
      optionString(MeasureName.pmi.toString, pmiSelected, "PMI") +
      optionString(MeasureName.npmi.toString, npmiSelected, "NPMI") +
      optionString(MeasureName.psgf_combined.toString, psgfCombinedSelected, "Combined Dir: P(S|F)") +
      optionString(MeasureName.psgf_undir_combined.toString, psgfUndirCombinedSelected, "Combined UnDir: P(S|F)") +
      optionString(MeasureName.pmi_combined.toString, pmiCombinedSelected, "Combined PMI") +
      optionString(MeasureName.npmi_combined.toString, npmiCombinedSelected, "Combined NPMI") +
      "</select> Measure<br/><br/><br/>\n"
  }

  def typeConstraintCheckBoxes(query:RelgramsQuery):String = {

    var typeSelected = ""
    var notypeSelected = ""
    query.typeOption match {
      case "type" => typeSelected = "checked"
      case "notype" => notypeSelected = "checked"
      case "both" => {typeSelected = "checked"; notypeSelected = "checked"}
      case _ => {typeSelected = "checked"; notypeSelected = "checked"}
    }
    val inputs = """<input type="checkbox" name="typeOption" value="type" %s>Typed</input>
                    <input style="margin-left: 40px" type="checkbox" name="typeOption" value="notype" %s>Heads</input>
                 """.format(typeSelected, notypeSelected)
    inputs.toString
  }
  def equalityCheckBoxes(query:RelgramsQuery):String = {

    var equalitySelected = ""
    var noequalitySelected = ""
    query.equalityOption match {
      case "equality" => equalitySelected = "checked"
      case "noequality" => noequalitySelected = "checked"
      case "both" => {equalitySelected = "checked"; noequalitySelected = "checked"}
      case _ => {equalitySelected = "checked"; noequalitySelected = "checked"}
    }
    val inputs = """<input type="checkbox" name="equalityOption" value="equality" %s>Equality</input>
                    <input style="margin-left: 30px" type="checkbox" name="equalityOption" value="noequality" %s>No Equality</input>
                 """.format(equalitySelected, noequalitySelected)
    inputs.toString
  }



  def sortByOptions(query:RelgramsQuery):String = {

    var conditionalSelected = ""
    var fsSelected = ""
    var sfSelected = ""
    var fSelected = ""
    var sSelected = ""
    var fssfSelected = ""
    var kfssfSelected = ""

    query.sortBy match {
      case "conditional" => conditionalSelected = "selected"
      case "fs" => fsSelected = "selected"
      case "sf" => sfSelected = "selected"
      case "fs+sf" => fssfSelected = "selected"
      case "kfs+sf" => kfssfSelected = "selected"
      case "f" => fSelected = "selected"
      case "s" => sSelected = "selected"
      case _ => fsSelected = "selected"
    }

    val header = "<b>Sort By:</b><br/>"
    header + "<select name =\"sortBy\">" +
    optionString("kfs+sf", kfssfSelected, "#k(FS+SF)") +
    optionString("fs+sf", fssfSelected, "#50(FS+SF)") +
    optionString("conditional", conditionalSelected, "P(S|F)") +
    optionString("fs", fsSelected, "#50(FS)") +
    optionString("sf", sfSelected, "#50(SF)") +
    optionString("f", fSelected, "#F") +
    optionString("s", sSelected, "#S") +
    "</select>\n"

  }
  val windows = Seq[Int](1,5,10,20,30,40,50)
  def mesureIndexOptions(query: RelgramsQuery, minFreq:Int): String = {

    "<b>Co-occurrence Window:</b><br/><select name=\"k\">" +
      windows.map(i => {
        val selected = if (i == query.measureIndex) "selected" else ""
        """<option value="%d" %s>%d</option>\n""".format(i, selected, i)
      }).mkString("\n") +
      "</select> Show relgrams that occur at least " +
      """<input name="minFreq" size="2" value="%d"/>""".format(query.minFreq)  +
      " times within this window\n"
  }


  def alphaBox(query:RelgramsQuery, alpha:Double) = {
    val value = if(query.alpha > 0) query.alpha else alpha
    """<input name=alpha value="%.2f"> Alpha (Used to combine measures from different windows) </input><br/>""".format(value)
  }

  def deltaBox(query:RelgramsQuery, delta:Double) = {
    val value = if(query.alpha > 0) query.smoothingDelta else delta
    """<input name=delta value="%.2f"> Delta (Smoothing factor hack: 10,100,1000 etc. Higher values discount more.)</input><br/>""".format(value)
  }


  def scrubHTML(string:String) = {
    import org.apache.commons.lang.StringEscapeUtils.escapeHtml
    escapeHtml(string)
  }

  def usage(exampleURL1:String, exampleURL2:String, exampleURL3:String) = {<span style="font-size: 12">
              <b>Usage:</b> Can be used to find rel-grams whose first tuple matches the fields specified below.
              <br/>
              <b>Querying:</b><br/>
              <span>a) By default, tuples that contain ANY of the words or their stems in the corresponding fields are returned.</span><br/>
              <span>b) Quotes around words causes the input string to be treated as a phrase.</span><br/>
              <b>Examples:</b> <a href={exampleURL1}>(X:[person], arrest, )</a>
              <a href={exampleURL2}>([person], graduate, )</a>
              <a href={exampleURL3}>(,pitch,)</a>
              <br/>
              </span>}

  def examples(exampleURL1:String, exampleURL2:String) = {
    <span class="form">
      <b>Examples:</b>
      <ol>
      <li><a href={exampleURL1}>([person], arrest, X:[person])</a></li>
      <li><a href={exampleURL2}>([organization], pass, bill)</a></li>
      </ol>
    </span>
  }

  var reportingVerbParamName: String = "reportingVerbs"
  def filterReportingVerbs(query:RelgramsQuery) = {
    val checked = query.filterReportingVerbs match {
      case true => "checked"
      case false => ""
    }
    val header = "<b>Filtering options:</b><br/>"
    header + """<input type="checkbox" name="%s" value="true" %s>Remove reporting verb based relations. (e.g. say, say in, tell)</input>""".format(reportingVerbParamName, checked)
  }
  val formCSS = "<style type=\"text/css\">\nform{\nfont-family: Verdana;\nfont-size: 14px;\nbackground-color: #efefef;\nwidth: 600px;\npadding: 10px;\nborder: 1}\n    </style>"
  def createForm(query:RelgramsQuery, host:String, port:Int): String = {

    //val exampleURL = """http://%s:%s/relgrams?arg1="xvar+type+person"&rel="die+in"&arg2="type+time+unit"&sortBy=fs&equalityOption=equality&search=search""".format(host, port)
    val arg1 = scrubHTML(query.relationTuple.arg1)
    val rel = scrubHTML(query.relationTuple.rel)
    val arg2 = scrubHTML(query.relationTuple.arg2)

    val base = """http://%s:%s""".format(host, port)
    val exampleURL1 =  base + """/relgrams?arg1=person&rel=arrest&arg2=xvar%3Atype%3Aperson&sortBy=fs&typeOption=type&typeOption=notype&equalityOption=equality&k=1&minFreq=1&search=Search"""
    val exampleURL2 = base + """/relgrams?arg1=organization&rel=pass&arg2=bill&sortBy=fs&typeOption=type&typeOption=notype&equalityOption=equality&equalityOption=noequality&k=50&minFreq=1&reportingVerbs=true&search=Search""".format(host, port)


    var loginForm:String = formCSS + "\n<div class=\"form\">"

    loginForm += "<form action=\"relgrams\">\n"
    loginForm += "<h4>Relgrams Search</h4>\n"
    //loginForm += "<textarea name=original rows=10 cols=40>" + document + "</textarea><br/>"
    loginForm += "<input name=arg1 size=\"50\" value=\"%s\"> Arg1</input><br/>\n".format(arg1)
    loginForm += "<input name=rel size=\"50\" value=\"%s\"> Rel</input><br/>\n".format(rel)
    loginForm += "<input name=arg2 size=\"50\" value=\"%s\"> Arg2</input><br/>\n".format(arg2)
    loginForm += "<br/>"
    loginForm += sortByOptions(query)
    loginForm += "<br/>"
    loginForm += "<br/>"

    loginForm += "<b>Argument constraints:</b>"
    loginForm += "<br/>"
    loginForm += typeConstraintCheckBoxes(query).toString//equalityOptions(query
    loginForm += "<br/>"
    loginForm += equalityCheckBoxes(query).toString//equalityOptions(query)
    loginForm += "<br/>"
    loginForm += "<br/>"
    loginForm += mesureIndexOptions(query, query.minFreq)
    loginForm += "<br/>"
    loginForm += "<br/>"
    loginForm += filterReportingVerbs(query)
    loginForm += "<br/>"
    loginForm += "<br/>"
    loginForm += "<input name=search type=\"submit\" value=\"Search\"/>\n"//<span style="padding-left:300px"></span>
    loginForm += "<br/>"
    loginForm += examples(exampleURL1, exampleURL2)//usage(exampleURL1, exampleURL2, exampleURL3)
    loginForm += "</form>\n"
    loginForm += "</div>"
    //println(loginForm)
    loginForm
  }

  /**loginForm += viewOptions(query)
    loginForm += measureOptions(query)
    loginForm += mesureIndexOptions(query)

    loginForm += alphaBox(query, 0.5)
    loginForm += deltaBox(query, 10)
    */




}

object RelgramsViewerFilter {//extends unfiltered.filter.Plan {
  val logger = LoggerFactory.getLogger(this.getClass)

  var solrManager:SolrSearchWrapper = null//new SolrSearchWrapper("http://rv-n15.cs.washington.edu:10000/solr/mar24-relgrams", "http://rv-n15.cs.washington.edu:10000/solr/mar24-tupledocuments")

  def wrapHtml(content:String) = "<html>" + content + "</html>"

  val dummyTuple = RelationTuple.fromArg1RelArg2("", "", "")
  def search(query: RelgramsQuery):(String, Seq[(Measures, AffinityMeasures)]) = (query.toHTMLString, solrManager.search(query.relationTuple, dummyTuple, query.filterReportingVerbs))

  def findDocumentTuples(docid:String, sort:Boolean = true) = solrManager.findDocumentTuples(docid)

  def findTuples(ids:Iterable[String]) = ids.flatMap(id => findTuple(id))
  def findTuple(id:String) = solrManager.findTypedTuplesRecord(id)

  val dropCSS ="span.dropt {border-bottom: thin dotted; background: #ffeedd;}\nspan.dropt:hover {text-decoration: none; background: #ffffff; z-index: 6; }\nspan.dropt span {position: absolute; left: -9999px;\n  margin: 20px 0 0 0px; padding: 3px 3px 3px 3px;\n  border-style:solid; border-color:black; border-width:1px; z-index: 6;}\nspan.dropt:hover span {left: 2%; background: #ffffff;} \nspan.dropt span {position: absolute; left: -9999px;\n  margin: 4px 0 0 0px; padding: 3px 3px 3px 3px; \n  border-style:solid; border-color:black; border-width:1px;}\nspan.dropt:hover span {margin: 20px 0 0 170px; background: #ffffff; z-index:6;} "

  val cssSoft = "<style type=\"text/css\">table.soft {\n\tborder-spacing: 0px;}\n.soft th, .soft td {\n\tpadding: 5px 30px 5px 10px;\n\tborder-spacing: 0px;\n\tfont-size: 90%;\n\tmargin: 0px;}\n.soft th, .soft td {\n\ttext-align: left;\n\tbackground-color: #e0e9f0;\n\tborder-top: 1px solid #f1f8fe;\n\tborder-bottom: 1px solid #cbd2d8;\n\tborder-right: 1px solid #cbd2d8;}\n.soft tr.head th {\n\tcolor: #fff;\n\tbackground-color: #90b4d6;\n\tborder-bottom: 2px solid #547ca0;\n\tborder-right: 1px solid #749abe;\n\tborder-top: 1px solid #90b4d6;\n\ttext-align: center;\n\ttext-shadow: -1px -1px 1px #666666;\n\tletter-spacing: 0.15em;}\n.soft td {\n\ttext-shadow: 1px 1px 1px #ffffff;}\n.soft tr.even td, .soft tr.even th {\n\tbackground-color: #3E698E;}\n.soft tr.head th:first-child {\n\t-webkit-border-top-left-radius: 5px;\n\t-moz-border-radius-topleft: 5px;\n\tborder-top-left-radius: 5px;}\n.soft tr.head th:last-child {\n\t-webkit-border-top-right-radius: 5px;\n\t-moz-border-radius-topright: 5px;\n\tborder-top-right-radius: 5px;}</style>"
  val cssFinancial ="<style type=\"text/css\">/* financial or timetable */\n\nbody {\n\tfont-family: Arial, Verdana, sans-serif;\n\tcolor: #111111;}\n\ntable.financial {\n\twidth: 1400px;}\n\n.financial th, .financial td {\n\tpadding: 7px 10px 10px 10px;}\n.financial th {\n\tbackground-color: #c3e6e5;\tletter-spacing: 0.1em;\n\tfont-size: 90%;\n\tborder-bottom: 2px solid #111111;\n\tborder-top: 1px solid #999;\n\ttext-align: left;}\n\n.financial tr.even {\n\tbackground-color: #efefef;}\n\n.financial tr:hover {\n\tbackground-color: #c3e6e5;}\n\n.financial tfoot td {\n\tborder-top: 2px solid #111111;\n\tborder-bottom: 1px solid #999;}\n\n.money {\n\ttext-align: right;}</style>"
  val tableTags = "<table class=\"financial\">\n%s\n</table>\n"
  //def headerRow(measure:MeasureName.MeasureName) = "<tr><td><b>First (F)</b></td><td><b>Second (S)</b></td><td><b>%s</b></td><td><b>%s</b></td><td><b>%s</b></td><td><b>%s</b></td><td><b>%s</b></td></tr>".format("Measure", "#(F,S)", "#(S,F)", "#(F,*)", "#(S,*)")

  def wrapResultsTableTags(content:String) = {
    tableTags.format(content)
  }

  val rowTags = "<tr>%s<tr>\n"
  def toResultsRow(measureName:String, query:RelgramsQuery, rank:Int, measures:Measures, affinities:AffinityMeasures, even:Boolean):String = resultsRowContent(measureName, query, rank, measures, affinities, even).toString

  def fromTabToColonFmt(text:String) = {
    "(%s)".format(text.replaceAll("\t", "; "))
  }


  val typeRe= """(.*?)[tT]ype:(.*)$""".r
  def getTypeName(text:String) = typeRe.findFirstMatchIn(text) match {
    case Some(m:Match) => m.group(2).replaceAll(""":Number""", "")
    case _ => ""
  }
  def displayTypeText(text: String): String = {
    val m = typeRe.findFirstMatchIn(text)
    if (m != None) {
      val before = m.get.group(1)
      val typeText = m.get.group(2).replaceAll(""":Number""", "")
      "%s[%s]".format(before, typeText)
    }else{
      text
    }
  }
  def toDisplayText(tuple:RelationTuple):(String, String, String)= (displayTypeText(tuple.arg1).replaceAll("""XVAR""", "X"), tuple.rel, displayTypeText(tuple.arg2).replaceAll("""XVAR""", "X"))

  var debug = false
  def relationWithRelArgs(first:RelationTuple,
                          firstArg1Counts:mutable.Map[String, Int],
                          firstArg2Counts:mutable.Map[String, Int],
                          query:RelgramsQuery): List[Seq[Node] with Serializable]  = {
    relationWithRelArgs(first,
                        firstArg1Counts.toArray.sortBy(x => -x._2).mkString(","),
                        firstArg2Counts.toArray.sortBy(x => -x._2).mkString(","),
                        query)
  }

  def paramString(query:RelgramsQuery) = {
    def typeOptions = {
      query.typeOption match {
        case "both" => "typeOption=type&typeOption=notype"
        case "type" => "typeOption=type"
        case "notype" => "typeOPtion=notype"
        case _ => "typeOption="
      }

    }

    def equalityOptions = {
      query.equalityOption match {
        case "both" => "equalityOption=equality&equalityOption=noequality"
        case "equality" => "equalityOption=equality"
        case "noequality" => "equalityOption=noequality"
        case _ => "equalityOption="
      }
    }
    """sortBy=%s&%s&%s&k=%d&minFreq=%d&reportingVerbs=%s""".format(query.sortBy.replaceAll("""\+""", "%2B"), typeOptions, equalityOptions, query.measureIndex, query.minFreq, query.filterReportingVerbs)
  }

  val xvartypeargsstyle,typeargsstyle,xvarargstyle="font-weight: bold; color: #585858"
  val relstyle = "font-weight: bold; color: #0B614B"
  val argstyle = ""
  def relationWithRelArgs(tuple:RelationTuple, arg1TopArgs:String, arg2TopArgs:String,  query:RelgramsQuery): List[Seq[Node] with Serializable] = {

    val (arg1:String, rel:String, arg2:String) = toDisplayText(tuple)

    def isType(argText:String) = argText.contains("type:") ||  argText.contains("Type:")
    def argStyle(argText:String) = {
      if (isType(argText) || argText.contains("XVAR"))
        xvarargstyle
      else
        argstyle
    }
    val sentence = "Ids: %s\n\nSentence: %s".format(tuple.ids.toSeq.sortBy(id => id).take(3).mkString(","), tuple.sentences.toSeq.sortBy(s => s).headOption.getOrElse("No sentence found."))
    val arg1Style = argStyle(tuple.arg1)
    val arg2Style = argStyle(tuple.arg2)
    val url = "/tuple?ids=%s".format(tuple.ids.mkString(","))
    val toprelationsURL = "/toprelations/tuple?arg1=%s&rel=%s&arg2=%s".format(tuple.arg1, tuple.rel, tuple.arg2)


    val runasQueryURL = "/relgrams?arg1=%s&rel=%s&arg2=%s&%s".format(tuple.arg1.replaceAll(":", "%3A"), tuple.rel, tuple.arg2.replaceAll(":", "%3A"), paramString(query))
    val lastTD: Elem = if(debug){
      <td><span style="margin-right:2px"> <a TITLE="Run tuple as query." href={runasQueryURL}>[{"<<"}]</a></span><a TITLE="Find top relations in documents containing tuple" href={toprelationsURL}>[?]</a></td>
    }
    else {
      <td TITLE="Run tuple as query."><a href={runasQueryURL}>[{"<<"}]</a></td>
    }
    val out: NodeBuffer = <td TITLE={arg1TopArgs} witdh="5%"><span style={arg1Style}>{arg1}</span></td><td TITLE={sentence} witdh="25%"><span style={relstyle}>{rel}</span></td><td TITLE={arg2TopArgs} witdh="5%"><span style={arg2Style}>{arg2}</span></td>;
    //val out: NodeBuffer = <td TITLE={arg1TopArgs} witdh="5%"><span style={arg1Style}>{arg1}</span></td><td TITLE={sentence} witdh="25%"><a href={url}><span style={relstyle}>{rel}</span></a></td><td TITLE={arg2TopArgs} witdh="5%"><span style={arg2Style}>{arg2}</span></td>;
    out::lastTD::Nil
  }
  def resultsRowContent(measureName:String, query:RelgramsQuery, rank:Int, measures:Measures, affinities:AffinityMeasures, even:Boolean) = {
    val measureVal = "%.4f".format(getMeasure(measureName, query.measureIndex, measures, affinities))
    val undirRGC = measures.urgc
    val rgc = undirRGC.rgc
    val fscount = maxOrElse(undirRGC.rgc.counts.values, 0)
    val bitermCount = maxOrElse(undirRGC.bitermCounts.values, 0)
    val bitermCountsAtK = undirRGC.bitermCounts.getOrElse(query.measureIndex, 0)
    val sfcount = bitermCount - fscount
    var farg1Counts: mutable.Map[String, Int] = rgc.argCounts.firstArg1Counts
    var farg2Counts: mutable.Map[String, Int] = rgc.argCounts.firstArg2Counts

    var tfarg1Counts = farg1Counts.filter(x => !farg2Counts.contains(x._1))
    if(tfarg1Counts.isEmpty) tfarg1Counts = farg1Counts

    var tfarg2Counts = farg2Counts.filter(x => !farg1Counts.contains(x._1))
    if(tfarg2Counts.isEmpty) tfarg2Counts = farg2Counts


    var sarg1Counts: mutable.Map[String, Int] = rgc.argCounts.secondArg1Counts
    var sarg2Counts: mutable.Map[String, Int] = rgc.argCounts.secondArg2Counts

    var tsarg1Counts = sarg1Counts.filter(x => !sarg2Counts.contains(x._1))
    if(tsarg1Counts.isEmpty) tsarg1Counts = sarg1Counts

    var tsarg2Counts = sarg2Counts.filter(x => !sarg1Counts.contains(x._1))
    if(tsarg2Counts.isEmpty) tsarg2Counts = sarg2Counts

    /** FOX ME:::::::::::::::::::::**/
    //tsarg1Counts = new mutable.HashMap[String, Int]()
    //tsarg2Counts = new mutable.HashMap[String, Int]()
    /** FOX ME:::::::::::::::::::::**/

    val valStyle = "text-align: center"
    val evenString = if(even) "even" else ""
    <tr class={evenString}>
      <td>{rank}</td>
      {relationWithRelArgs(rgc.relgram.first, tfarg1Counts, tfarg2Counts, query)}
      {relationWithRelArgs(rgc.relgram.second, tsarg1Counts, tsarg2Counts, query)}
      <td style={valStyle} width="5%">{bitermCountsAtK}</td>
      <td style={valStyle} width="5%">{bitermCount}</td>
      <td style={valStyle} width="5%">{measureVal}</td>
      <td style={valStyle} width="5%">{fscount}</td>
      <td style={valStyle} width="5%">{sfcount}</td>
      <td style={valStyle} width="5%">{measures.firstCounts}</td>
      <td style={valStyle} width="5%">{measures.secondCounts}</td>
    </tr>


  }

  def headerRow(measure:MeasureName.MeasureName, k:Int) = {

    def debugRow = if(debug) {
      <th colspan="2" width="0.0%"></th>
    }else {
      <th  width="0.0%"/><th width="0.0%"/>
    }
    val headElem: Elem = <thead>
      <tr>
         <th width="5%"></th>
         <th width="35%" colspan="4" style="text-align:center">First (F)</th>
         <th width="35%" colspan="4" style="text-align:center">Second (S)</th>
         <th width="5%" title="#k(FS+FS): Number of times F and S occur within k tuples of each other.">Query window:#{k}(FS+SF)</th>
         <th width="5%" title="#50(FS+FS): times F and S occur within 50 tuples of each other.">#50(FS+SF)</th>
         <th width="5%" title="P(S|F): Conditional probability of seeing S within 50 tuples of F.">P(S|F)</th>
         <th width="5%" title="#50(FS): Number of times S follows F within 50 tuples.">#50(FS)</th>
         <th width="5%" title="#50(SF): Number of times F follows S within 50 tuples.">#50(SF)</th>
         <th width="5%">#F</th>
         <th width="5%">#S</th>
      </tr>
    </thead>
    headElem.toString
  }


  def hasTypedEquality(relgram:Relgram):Boolean = hasTypedEquality(relgram.first) || hasTypedEquality(relgram.second)
  def hasTypedEquality(tuple:RelationTuple):Boolean = hasTypedEquality(tuple.arg1) || hasTypedEquality(tuple.arg2)
  def hasTypedEquality(arg:String):Boolean = hasEquality(arg) && hasType(arg)

  def hasType(relgram:Relgram):Boolean = hasType(relgram.first) && hasType(relgram.second)
  def hasType(tuple:RelationTuple):Boolean = hasType(tuple.arg1) && hasType(tuple.arg2)
  def hasType(string:String):Boolean = typeRe.findFirstMatchIn(string) != None

  def hasNoTypedArg(tuple:RelationTuple):Boolean = !hasType(tuple.arg1) && !hasType(tuple.arg2)

  def hasEquality(relgram:Relgram):Boolean = hasEquality(relgram.first) || hasEquality(relgram.second)
  def hasEquality(tuple:RelationTuple):Boolean = hasEquality(tuple.arg1) || hasEquality(tuple.arg2)
  def hasEquality(arg:String): Boolean =arg.contains("XVAR")

  def agreesWithEqualityOption(equalityOption: String, tuple: (Measures, AffinityMeasures)): Boolean = equalityOption match {
    //case "typedequality" => hasEquality(tuple._1.urgc.rgc.relgram) && hasType(tuple._1.urgc.rgc.relgram)
    case "equality" => hasEquality(tuple._1.urgc.rgc.relgram)
    case "noequality" => !hasEquality(tuple._1.urgc.rgc.relgram)
    //case "typednoequality" => !hasEquality(tuple._1.urgc.rgc.relgram) && hasType(tuple._1.urgc.rgc.relgram)
    case _ => true
  }

  def agreesWithTypeOption(typeOption: String, tuple: (Measures, AffinityMeasures)): Boolean = typeOption match {
    //case "typedequality" => hasEquality(tuple._1.urgc.rgc.relgram) && hasType(tuple._1.urgc.rgc.relgram)
    case "type" => hasType(tuple._1.urgc.rgc.relgram.second)
    case "notype" => hasNoTypedArg(tuple._1.urgc.rgc.relgram.second)
    //case "typednoequality" => !hasEquality(tuple._1.urgc.rgc.relgram) && hasType(tuple._1.urgc.rgc.relgram)
    case _ => true
  }

  def hasTypeQuantity(tuple:RelationTuple):Boolean = tuple.arg1.contains("Type:quantity") || tuple.arg2.contains("Type:quantity")
  def hasTypeQuantity(relgram:Relgram):Boolean = hasTypeQuantity(relgram.first) || hasTypeQuantity(relgram.second)

  //var minFreq = 0
  def aboveThreshold(urgc:UndirRelgramCounts, window:Int, minFreq:Int) = {
    urgc.bitermCounts.getOrElse(window, 0) >= minFreq
  }//maxOrElse(urgc.bitermCounts.values, 0) > minFreq

  def isIdentityRelgram(relgram:Relgram) = {
    relgram.first.isIdenticalTo(relgram.second)
  }

  def findEqualityVar(tuple:RelationTuple) = if(tuple.arg1.contains("XVAR")) tuple.arg1 else if(tuple.arg2.contains("XVAR")) tuple.arg2 else ""
  def equalityTypesAgree(relgram: Relgram) = {
    val fvar = findEqualityVar(relgram.first)
    val svar = findEqualityVar(relgram.second)
    val ftypeName = getTypeName(fvar)
    val stypeName = getTypeName(svar)
    ftypeName.equals(stypeName)
  }

  def hasPronounArgs(relgram: Relgram):Boolean = hasPronounArgs(relgram.first) || hasPronounArgs(relgram.second)
  def hasPronounArgs(tuple:RelationTuple):Boolean = isPronounArg(tuple.arg1.trim) || isPronounArg(tuple.arg2.trim)
  val pronouns = Set("he", "she", "they", "it", "i", "we")
  def isPronounArg(arg:String) = pronouns.contains(arg)


  def summary(results: Seq[(Measures, AffinityMeasures)]): String = {
    val numResults = results.size
    "Found %d rel-grams <br/>".format(numResults)
  }

  def renderSearchResults(measureName:String, query:RelgramsQuery, results:(String, Seq[(Measures, AffinityMeasures)])) = {
    var even = false
    cssSoft + cssFinancial +
    summary(results._2) +
    wrapResultsTableTags(headerRow(query.measure, query.measureIndex) + "\n<br/>\n<tbody>" +
                         results._2.zipWithIndex.map(marank => {
                           even = !even
                           val ma = marank._1
                           val rank = marank._2 + 1
                           toResultsRow(measureName, query, rank, ma._1, ma._2, even)
                         }).mkString("\n") + "</tbody>")
  }



  def isNonEmptyQuery(query: RelgramsQuery): Boolean = {
    query.relationTuple.arg1.size > 0 || query.relationTuple.rel.size > 0 || query.relationTuple.arg2.size > 0
  }

  def sortByMeasure(tuple: (Measures, AffinityMeasures), sortBy:String, measureIndex:Int) = {
    val measure = getMeasure(sortBy, measureIndex, tuple)
    0-measure
  }


  def getMeasure(measureName: String, measureIndex:Int, tuple: (Measures, AffinityMeasures)): Double = {
    val (measures, affinities) = tuple
    getMeasure(measureName, measureIndex, measures, affinities)
  }

  //def maxOrElse(values:Iterable[Int], elseValue:Int):Int = if(!values.isEmpty) values.max else elseValue

  def getMeasure(measureName:String, measureIndex:Int, measures:Measures, affinities:AffinityMeasures):Double = measureName match {

    case "conditional" => affinities.firstUndir.conditional
    case "fs" => maxOrElse(measures.urgc.rgc.counts.values, 0).toDouble
    case "sf" => (maxOrElse(measures.urgc.bitermCounts.values, 0) - maxOrElse(measures.urgc.rgc.counts.values, 0)).toDouble
    case "fs+sf" => maxOrElse(measures.urgc.bitermCounts.values, 0).toDouble
    case "kfs+sf" => measures.urgc.bitermCounts.getOrElse(measureIndex, 0).toDouble
    case "f" => measures.firstCounts.toDouble
    case "s" => measures.secondCounts.toDouble
    case _ => affinities.firstUndir.conditional
  }

  def frequencies(measures:Measures) = "%d\t%d\t%d\t%d".format(maxOrElse(measures.urgc.bitermCounts.values, 0),maxOrElse(measures.urgc.rgc.counts.values, 0), measures.firstCounts, measures.secondCounts)
  def maxOrElse(counts:Iterable[Int], orElse:Int) = if(!counts.isEmpty) counts.max else orElse
  def countsAreValid(measures: Measures) = {
    val maxBitermCount = maxOrElse(measures.urgc.bitermCounts.values, 0)
    val maxBigramCount = maxOrElse(measures.urgc.rgc.counts.values, 0)
    val out = maxBitermCount <= measures.firstCounts && maxBitermCount <= measures.secondCounts && maxBigramCount <= measures.firstCounts && maxBigramCount <= measures.secondCounts
    if(!out) logger.error("Invalid counts for relgram: " + measures.urgc.rgc.relgram.prettyString + "\t" + frequencies(measures))
    out
  }

  def pruneResults(query:RelgramsQuery, results:Seq[(Measures, AffinityMeasures)]) = {
    def applyFilters(in:Seq[(Measures, AffinityMeasures)]) = in.filter(ma => !isIdentityRelgram(ma._1.urgc.rgc.relgram))
                                                               .filter(ma => !hasPronounArgs(ma._1.urgc.rgc.relgram))
                                                               .filter(ma => aboveThreshold(ma._1.urgc, query.measureIndex, query.minFreq))
                                                               .filter(ma => agreesWithEqualityOption(query.equalityOption, ma))
                                                               .filter(ma => agreesWithTypeOption(query.typeOption, ma))
                                                               .filter(ma => !hasTypeQuantity(ma._1.urgc.rgc.relgram))
                                                               .filter(ma => equalityTypesAgree(ma._1.urgc.rgc.relgram))
                                                               .filter(ma => countsAreValid(ma._1))

    def removeDuplicates(in:Seq[(Measures, AffinityMeasures)]) = {
      var keys = Set[String]()
      def key(m:Measures) = m.urgc.rgc.relgram.prettyString
      in.flatMap(x => {
        val k= key(x._1)
        if(!keys.contains(k)) {
          keys += k
          Some(x)
        } else None
      })
    }
    removeDuplicates(applyFilters(results))
  }

  var host = "localhost"
  var port = 10000

  def findDocsContainingTuple(arg1:String, rel:String, arg2:String) = {
    solrManager.findDocsContainingTuple(arg1, rel, arg2)
  }
  def toTupleHtml(records:Iterable[TypedTuplesRecord]): Elem = {
    var docids = records.map(rec => rec.docid).toSet.mkString(",")
    val topRelationsUrl = "/toprelations?docids=%s".format(docids)
    def toIndexedTupleHtml(recs:Iterable[TypedTuplesRecord]) = {
      recs.zipWithIndex.map(idrecord => {
        toTupleHtml(idrecord._2, idrecord._1)
      })
    }
    <div>
    <a href={topRelationsUrl}>Top relations in {docids}</a><br/>
    <b>Extractions:</b>
    <table class="financial">
      {toIndexedTupleHtml(records)}
    </table>
    </div>
  }
  def toTupleHtml(index:Int, record: TypedTuplesRecord): NodeBuffer = {
    val url = "/document?docid=%s".format(record.docid)
    val indexStr = "%d)".format(index)
    val id = record.docid + "-" + record.sentid + "-" + record.extrid
    val sentence = record.sentence
    <thead>
     <tr><th>{indexStr} <a href={url}>{id}</a></th><th colspan="2">{sentence}</th></tr>
    </thead>
     <tr class="even"><td>{record.arg1Head}</td><td>{record.relHead}</td><td>{record.arg2Head}</td></tr>
     <tr class="even"><td>{record.arg1}</td><td>{record.rel}</td><td>{record.arg2}</td></tr>
     <tr class="even"><td>{record.arg1Types.mkString(",")}</td><td></td><td>{record.arg2Types.mkString(",")}</td></tr>
     <tr><td/><td/><td/></tr>
     <tr><td/><td/><td/></tr>

  }

  def toGroupHtml(groupedRecords: Map[String, Seq[TypedTuplesRecord]]): Seq[Elem] = {
    val breakElement: Elem = <br/>
    var index = 1
    def size(x:String) = groupedRecords(x).map(x => x.docid).toSeq.distinct.size
    def topKeys = {
      groupedRecords.keys.toSeq.sortBy(key => -size(key)).take(100)
    }
    def topKeyElements ={
      <ol>
        {topKeys.map(x => <li>{x} [{size(x)}]</li>)}
      </ol>
    }

    topKeyElements::Nil ++ topKeys.flatMap(key => {
      val records = groupedRecords(key)
      val diffDocs = records.map(x => x.docid).toSeq.distinct.size
      val keyElement: Elem = <span>{index}) <b>Relation:</b>{key} ({diffDocs})</span>
      val recordsHtml: Elem = toTupleHtml(records)
      keyElement::breakElement::recordsHtml::breakElement::Nil
    })
  }

  def toGroupedRow(arg1s:Seq[(String, String)], rel:String, arg2s:Seq[(String, String)]) = {
    def argSpans(args:Seq[(String, String)]) = {
      args.map(arg => <span title={arg._2}>{arg._1}</span>)
    }
    <tr>
    <td>{argSpans(arg1s)}</td><td>{rel}</td><td>{argSpans(arg2s)}</td>
    </tr>
  }
  def collapsedResults(measuresName: String, query: RelgramsQuery, tuple: (String, Seq[(Measures, AffinityMeasures)])) = {
    val grouped = tuple._2.groupBy(k => k._1.urgc.rgc.relgram.second.rel)
    val sorted = grouped.toSeq.sortBy(x => -x._2.map(y => getMeasure(measuresName, query.measureIndex, y)).max)
    <table>

      {toGroupRows(sorted)}
    </table>

  }

  var mainPage = "src/main/resources/relgrams.html"
  def projectPage()  = Source.fromFile(mainPage).getLines().mkString("\n").replaceAll("_SEARCH_URL_", "http://%s:%s/relgrams".format(host, port))

  def toGroupRows(sorted: Seq[(String, Seq[(Measures, AffinityMeasures)])]) = {
    sorted.map(x => {
      val ma = x._2
      println("x._1: " + x._1)
      var arg1s = new ArrayBuffer[(String, String)]()
      var arg2s = new ArrayBuffer[(String, String)]()
      def arg1AndCounts(z: (Measures, AffinityMeasures)) = (z._1.urgc.rgc.relgram.second.arg1, z._1.urgc.rgc.relgram.second.arg1HeadCounts.toSeq.sortBy(kv => -kv._2).map(kv => kv._1 + "(" + kv._2 + ")").mkString(","))
      def arg2AndCounts(z: (Measures, AffinityMeasures)) = (z._1.urgc.rgc.relgram.second.arg2, z._1.urgc.rgc.relgram.second.arg2HeadCounts.toSeq.sortBy(kv => -kv._2).map(kv => kv._1 + "(" + kv._2 + ")").mkString(","))
      ma.foreach(y => {
        arg1s += arg1AndCounts(y)
        arg2s += arg2AndCounts(y)
      })
      <tr><td><b>
        Relation: {x._1}
      </b></td><td></td><td></td>
      </tr> ++ toGroupedRow(arg1s.toSeq, x._1, arg2s.toSeq)
    })
  }

  val intentVal = unfiltered.netty.cycle.Planify {
    case req @ GET(Path("/debug/relgrams")) => {
      debug = true
      handleRelgrams(req)
    }
    case req @ GET(Path("/relgrams")) => {
      debug = false
      handleRelgrams(req)
      //"" + "Arg1: " + relgramsQuery.toHTMLString) )
    }

    case req @ GET(Path("/relgramscollapse")) => {
      val relgramsQuery = ReqHelper.getRelgramsQuery(req)
      logger.info("Query: " + relgramsQuery)
      val sortBy = ReqHelper.getSortBy(req)
      val results = if (isNonEmptyQuery(relgramsQuery)) search(relgramsQuery) else ("", Seq[(Measures, AffinityMeasures)]())
      val sortedResults = results._2.sortBy(ma => sortByMeasure(ma, sortBy, relgramsQuery.measureIndex))
      val prunedResults = pruneResults(relgramsQuery, sortedResults)
      ResponseString(wrapHtml(HtmlHelper.createForm(relgramsQuery, host, port) + "<br/>"
        + collapsedResults("conditional", relgramsQuery, (results._1, prunedResults))))
      //"" + "Arg1: " + relgramsQuery.toHTMLString) )
    }

    case req @ GET(Path("/tuple")) => {
      val ids = ReqHelper.getIds(req).split(",")
      val tuples = findTuples(ids)
      val tupleHtml = <div><b>Tuples</b><br/>{toTupleHtml(tuples)}</div>
      ResponseString(wrapHtml(cssSoft + cssFinancial + "\n" + tupleHtml.toString))
    }
    case req @ GET(Path("/document")) => {
      val docid = ReqHelper.getDocid(req)
      val tuples = findDocumentTuples(docid)
      val docHtml = <div><b>Document Tuples</b><br/>{toTupleHtml(tuples)}</div>
      ResponseString(wrapHtml(cssSoft + cssFinancial + "\n" + docHtml.toString))
    }

    case req @ GET(Path("/toprelations")) => {
      val docids = ReqHelper.getDocids(req).split(",")
      val docRecords = docids.map(docid => findDocumentTuples(docid)).toSeq
      val groupedRecords: Map[String, Seq[TypedTuplesRecord]] = docRecords.flatten.groupBy(record => record.relHead)
      val groupHtml = <div><b>Grouped Tuples</b><br/><br/>{toGroupHtml(groupedRecords)}</div>
      ResponseString(wrapHtml(cssSoft + cssFinancial + "\n" + groupHtml.toString))
    }

    case req @ GET(Path("/toprelgrams")) => {
      ResponseString(wrapHtml(""))
    }
    case req @ GET(Path("/")) => {

      val xml = XML.loadString(projectPage)
      val response = Html(xml)
      response
    }
    case req @ GET(Path("/toprelations/tuple")) => {
      val arg1 = ReqHelper.getArg1(req)
      val rel = ReqHelper.getRel(req)
      val arg2 = ReqHelper.getArg2(req)
      findDocsContainingTuple(arg1, rel, arg2) match {
        case Some(docids:Set[String]) => {
          println("Number of docids: " + docids.size)
          val docRecords = docids.take(100).toSeq.par.map(docid => findDocumentTuples(docid, sort=false))
          val groupedRecords = docRecords.seq.flatten.groupBy(record => record.relHead)
          val groupHtml = <div><b>Grouped Tuples</b><br/><br/>{toGroupHtml(groupedRecords)}</div>
          ResponseString(wrapHtml(cssSoft + cssFinancial + "\n" + groupHtml.toString))
        }
        case _ => ResponseString(wrapHtml("No documents found for (%s, %s, %s)".format(arg1, rel, arg2)))
      }
      //val docids = ReqHelper.getDocids(req).split(",")

    }

  }


  def handleRelgrams(req: HttpRequest[ReceivedMessage]): ResponseString = {
    val relgramsQuery = ReqHelper.getRelgramsQuery(req)
    logger.info("Query: " + relgramsQuery)
    val sortBy = ReqHelper.getSortBy(req)
    val results = if (isNonEmptyQuery(relgramsQuery)) search(relgramsQuery) else ("", Seq[(Measures, AffinityMeasures)]())
    val sortedResults = results._2.sortBy(ma => sortByMeasure(ma, sortBy, relgramsQuery.measureIndex))
    val prunedResults = pruneResults(relgramsQuery, sortedResults)
    ResponseString(wrapHtml(HtmlHelper.createForm(relgramsQuery, host, port) + "<br/>"
      + renderSearchResults("conditional", relgramsQuery, (results._1, prunedResults))))
  }

  def main(args:Array[String]){

    //var port = 10000
    var relgramsURL, docsURL = ""
    //var mainPage = "src/resources/relgrams.html"

    val parser = new OptionParser() {
      arg("solrURL", "hdfs input path", {str => relgramsURL = str})
      arg("solrDocURL", "hdfs input path", {str => docsURL = str})
      arg("mainPage", "local path", {str => mainPage = str})
      opt("port", "port to run on.", {str => port = str.toInt})
      opt("host", "port to run on.", {str => host = str})
      opt("debug", "debug version?", {str => debug = str.toBoolean})
    }
    if (!parser.parse(args)) return
    println("Main page: " + mainPage)
    println("host:port: " + host + ":" + port)
    solrManager =  new SolrSearchWrapper(relgramsURL, docsURL)
    unfiltered.netty.Http(port, host).plan(intentVal).run()
  }

}
