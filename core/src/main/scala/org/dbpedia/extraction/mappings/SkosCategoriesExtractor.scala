package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.{Graph, DBpediaDatasets, Quad}
import org.dbpedia.extraction.wikiparser.{PageNode, Node, InternalLinkNode, WikiTitle}
import org.dbpedia.extraction.ontology.datatypes.Datatype
import org.dbpedia.extraction.wikiparser.impl.wikipedia.Namespaces
import org.dbpedia.extraction.ontology.{Ontology, OntologyNamespaces}
import org.dbpedia.extraction.util.Language

/**
 * Extracts information about which concept is a category and how categories are related using the SKOS Vocabulary.
 */
class SkosCategoriesExtractor( context : {
                                   def ontology : Ontology
                                   def language : Language }  ) extends Extractor
{
    private val language = context.language.wikiCode

    private val rdfTypeProperty = context.ontology.getProperty("rdf:type").get
    private val skosConceptClass = context.ontology.getClass("skos:Concept").get
    private val skosPrefLabelProperty = context.ontology.getProperty("skos:prefLabel").get
    private val skosBroaderProperty = context.ontology.getProperty("skos:broader").get
    private val skosRelatedProperty = context.ontology.getProperty("skos:related").get

    override def extract(node : PageNode, subjectUri : String, pageContext : PageContext) : Graph =
    {
        if(node.title.namespace != WikiTitle.Namespace.Category) return new Graph()

        var quads = List[Quad]()

        quads ::= new Quad(context.language, DBpediaDatasets.SkosCategories, subjectUri, rdfTypeProperty, skosConceptClass.uri, node.sourceUri)
        quads ::= new Quad(context.language, DBpediaDatasets.SkosCategories, subjectUri, skosPrefLabelProperty, node.title.decoded, node.sourceUri, new Datatype("xsd:string"))

        for(link <- collectCategoryLinks(node))
        {
            val property =
                if(link.retrieveText.getOrElse("").startsWith(":"))
                {
                    skosRelatedProperty
                }
                else
                {
                    skosBroaderProperty
                }

            quads ::= new Quad(context.language, DBpediaDatasets.SkosCategories, subjectUri, property, getUri(link.destination), link.sourceUri)
        }

        new Graph(quads)
    }

    private def collectCategoryLinks(node : Node) : List[InternalLinkNode] =
    {
        node match
        {
            case linkNode : InternalLinkNode if linkNode.destination.namespace == WikiTitle.Namespace.Category => List(linkNode)
            case _ => node.children.flatMap(collectCategoryLinks)
        }
    }

    private def getUri(destination : WikiTitle) : String =
    {
        val categoryNamespace = Namespaces.getNameForNamespace(context.language, WikiTitle.Namespace.Category)
        
        OntologyNamespaces.getResource(categoryNamespace + ":" + destination.encoded, language)
        //OntologyNamespaces.getUri(categoryNamespace + ":" + destination.encoded, OntologyNamespaces.DBPEDIA_INSTANCE_NAMESPACE)
    }
}