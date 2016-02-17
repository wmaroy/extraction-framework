package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.{DBpediaDatasets, Quad}
import org.dbpedia.extraction.wikiparser.PageNode
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.destinations.QuadBuilder
import java.net.URI
import scala.language.reflectiveCalls

/**
 * Extracts sameAs links for resources with themselves. Only makes sense when serialization is
 * configured such that subjects are IRIs and objects are URIs (or vice versa).
 */
class UriSameAsIriExtractor(
  context : {
    def ontology : Ontology
    def language : Language
  }
)
extends PageNodeExtractor
{
  private val language = context.language

  val sameAsProperty = context.ontology.properties("owl:sameAs")
  
  val quad = QuadBuilder(context.language, DBpediaDatasets.UriSameAsIri, sameAsProperty, null) _

  override val datasets = Set(DBpediaDatasets.UriSameAsIri)

  override def extract(page: PageNode, subjectUri: String, pageContext: PageContext): Seq[Quad] =
  {
    // only extract triple if IRI is actually different from URI
    if (new URI(subjectUri).toASCIIString() == subjectUri) Seq.empty
    else Seq(quad(subjectUri, subjectUri, page.sourceUri))
  }
}