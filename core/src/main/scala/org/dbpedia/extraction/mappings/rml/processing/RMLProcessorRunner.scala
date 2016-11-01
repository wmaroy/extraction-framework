package org.dbpedia.extraction.mappings.rml.processing

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, ObjectOutputStream}
import java.util

import be.ugent.mmlab.rml.config.RMLConfiguration
import be.ugent.mmlab.rml.core.StdRMLEngine
import be.ugent.mmlab.rml.model.dataset.{RMLDataset, StdRMLDataset}
import be.ugent.mmlab.rml.model.{RMLMapping, TriplesMap}
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.dbpedia.extraction.destinations.{DBpediaDatasets, Quad}
import org.dbpedia.extraction.mappings.rml.util.RMLOntologyUtil
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser.{InternalLinkNode, TemplateNode}
import org.openrdf.model.Value
import org.openrdf.model.impl.URIImpl
import org.openrdf.rio.RDFFormat

/**
  *
  * Runs the RML Processor
  *
  */
class RMLProcessorRunner(mappings: RMLMapping) {

  def process(templateNode: TemplateNode, triplesMap: TriplesMap, subjectUri: String, context : { def language : Language
                                                                                                  def ontology: Ontology}) : Seq[Quad] = {

    triplesMap.getSubjectMap.setConstantValue(new URIImpl(subjectUri))

    val parameters = new util.HashMap[String, String]()
    val exeTriplesMap = List[String](triplesMap.getName)
    val engine = new StdRMLEngine()
    val dataset : RMLDataset = new StdRMLDataset()
    val templateNodeHashMap = convertTemplateNodeToMap(templateNode)

    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)

    oos.writeObject(templateNodeHashMap)
    oos.flush()
    oos.close()

    val is = new ByteArrayInputStream(baos.toByteArray());

    engine.generateRDFTriples(dataset, mappings, parameters, exeTriplesMap.toArray, is)

    val triplesOutputStream = new ByteArrayOutputStream()
    dataset.dumpRDF(triplesOutputStream, RDFFormat.TURTLE)
    val triplesInputStream = new ByteArrayInputStream(triplesOutputStream.toByteArray)

    val model = ModelFactory.createDefaultModel()
    model.read(triplesInputStream, null, "TURTLE")

    val statementIterator = model.listStatements()
    var list = List()
    while(statementIterator.hasNext) {

      val statement = statementIterator.nextStatement()

      val objectUri = if(statement.getObject.isResource) {
        statement.getObject.asResource().getURI
      } else if(statement.getObject.isLiteral) {
        statement.getObject.asLiteral().getDatatypeURI
      } else {
        throw new RuntimeException(statement.getSubject.getURI + " has no valid object")
      }

      val ontolog = context.ontology
      val quad = new Quad(context.language, DBpediaDatasets.OntologyPropertiesLiterals, statement.getSubject.getURI,
        RMLOntologyUtil.loadOntologyPropertyFromIRI(statement.getPredicate.getURI, context),
        objectUri, templateNode.sourceUri)
      println(quad.toString())
    }


    Seq.empty

  }

  private def convertTemplateNodeToMap(templateNode: TemplateNode) : util.HashMap[String,String] = {
    val hashMap = new util.HashMap[String,String]()
    val keyset = templateNode.keySet
    for(key <- keyset) {
      val node = templateNode.property(key).get
      if(node.children.size == 1) {
        hashMap.put(key, node.children.head.retrieveText.get.replaceAll("\n",""))
      } else {
        var found = false;
        var i = 0
        while(!found && i < node.children.size) {
          if(node.children(i).isInstanceOf[InternalLinkNode]) {
            val internalLinkNode = node.children(i).asInstanceOf[InternalLinkNode]
            hashMap.put(key, internalLinkNode.destination.decoded)
            found = true
          }
          i += 1
        }

      }
    }
    return hashMap
  }

}

