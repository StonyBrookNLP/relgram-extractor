package edu.washington.cs.knowitall
package tool
package coref




/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 4/6/13
 * Time: 3:41 PM
 * To change this template use File | Settings | File Templates.
 */
import org.slf4j.LoggerFactory
import java.io.File
import java.io.PrintWriter

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.mapAsJavaMap
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.JavaConversions.seqAsJavaList

import common.ling.Word
import edu.stanford.nlp.dcoref.CorefChain
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.util.CoreMap
import edu.washington.cs.knowitall.common.Resource.using


class TuplesCorefResolver extends CoreferenceResolver {
  lazy val corenlp = {
    val props = new java.util.Properties()
    props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    props.put("dcoref.maxdist", "50")
    new StanfordCoreNLP(props)
  }

  // create a lookup
  override def clusters(text: String): Map[Mention, List[Mention]] = {


    val document = new Annotation(text);
    // run the Stanford pipeline
    corenlp.annotate(document);

    // an array of arrays, where the first dimension is sentences
    // and the second is tokens
    val tokens: Array[Array[CoreLabel]] = document.get[java.util.List[CoreMap], SentencesAnnotation](classOf[SentencesAnnotation]).map { sentence =>
      sentence.get[java.util.List[CoreLabel], TokensAnnotation](classOf[TokensAnnotation]).toList.toArray
    }.toArray

    // stanford is doing some WEIRD stuff, look at the JavaDoc for get
    // somehow Java handles this without having to specify the types.
    val coremaps = document.get[java.util.Map[java.lang.Integer, CorefChain], CorefChainAnnotation](classOf[CorefChainAnnotation])

    (for ((k, chain) <- coremaps) yield {
      val representitive = chain.getRepresentativeMention
      val mentions = chain.getMentionsInTextualOrder

      (new Mention(representitive.mentionSpan, tokens(representitive.sentNum - 1)(representitive.startIndex - 1).beginPosition), mentions.map(m =>
        new Mention(m.mentionSpan, tokens(m.sentNum - 1)(m.startIndex - 1).beginPosition)).toList)
    })(scala.collection.breakOut)
  }

  def resolve(text: String, transform: (String, String) => String): String = {
    val substitutions = this.substitutions(text).map { case Substitution(from, to) =>
      Substitution(from, to.copy(text = from.text + "[" + to.text + "]"))
    }

    CoreferenceResolver.substitute(text, substitutions)
  }
}
