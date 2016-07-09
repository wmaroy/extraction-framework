package org.dbpedia.extraction.mappings

import scala.language.reflectiveCalls
import org.dbpedia.extraction.destinations.{DBpediaDatasets, Quad}
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser.{PageNode, TextNode}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Lukas Faber, Sebastian Serth and Stephan Haarmann on 28.05.2016.
*/
class CommonsResourceExtractor (
  context : {
    def ontology : Ontology
    def language : Language
  }
) extends PageNodeExtractor{

  private val propertyUri = context.ontology.properties("owl:sameAs");
  private val commonsBaseUri = "http://commons.dbpedia.org/resource/%s"

  override val datasets = Set(DBpediaDatasets.PageLinks)

  override def extract(node : PageNode, subjectUri : String, pageContext : PageContext) : Seq[Quad] ={

    val quads = new ArrayBuffer[Quad]()

    for { template <- InfoboxExtractor.collectTemplates(node)
      if template.title.decoded == "Commons"
    }
    {
      if (template.children.isEmpty){
        return Seq(new Quad(context.language, DBpediaDatasets.PageLinks, subjectUri, propertyUri,
          String.format(commonsBaseUri, node.title.encoded.asInstanceOf[String]), null, null))
      } else{
        var commonsPageTitle = template.children.head.children.head.asInstanceOf[TextNode].text
        return Seq(new Quad(context.language, DBpediaDatasets.PageLinks, subjectUri, propertyUri,
          String.format(commonsBaseUri, commonsPageTitle), null, null))
      }
    }
    Seq.empty
  }
}
