package org.dbpedia.extraction.mappings.rml.processing

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, ObjectOutputStream}
import java.util

import be.ugent.mmlab.rml.core.StdRMLEngine
import be.ugent.mmlab.rml.model.RDFTerm.PredicateMap
import be.ugent.mmlab.rml.model.dataset.{RMLDataset, StdRMLDataset}
import be.ugent.mmlab.rml.model.{PredicateObjectMap, RMLMapping, TriplesMap}
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.dbpedia.extraction.dataparser._
import org.dbpedia.extraction.destinations.{DBpediaDatasets, Quad}
import org.dbpedia.extraction.mappings.Redirects
import org.dbpedia.extraction.mappings.rml.util.RMLOntologyUtil
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.ontology.datatypes.Datatype
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser.impl.simple.{Matcher, SimpleWikiParser, Source}
import org.dbpedia.extraction.wikiparser.{ExternalLinkNode, InternalLinkNode, TemplateNode}
import org.eclipse.rdf4j.model.impl.URIImpl
import org.eclipse.rdf4j.rio.RDFFormat
import scala.collection.JavaConverters._

import scala.language.reflectiveCalls

/**
  *
  * Runs the RML Processor
  * //TODO: refactor this class!
  */
class RMLProcessorRunner(mappings: RMLMapping) {

  def process(templateNode: TemplateNode, triplesMap: TriplesMap, subjectUri: String, context : { def language : Language
                                                                                                  def ontology: Ontology
                                                                                                  def redirects: Redirects}) : Seq[Quad] = {

    println(templateNode.title.decoded)

    /**
      *  Setting up the processor
      */
    triplesMap.getSubjectMap.setConstantValue(new URIImpl(subjectUri))

    val parameters = new util.HashMap[String, String]()
    val exeTriplesMap = List[String](triplesMap.getName)
    val engine = new StdRMLEngine()
    val dataset : RMLDataset = new StdRMLDataset()
    val templateNodeHashMap = convertTemplateNodeToMap(templateNode)

    /**
      * Setting up the dataset stream
      */
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)

    oos.writeObject(templateNodeHashMap)
    oos.flush()
    oos.close()

    val is = new ByteArrayInputStream(baos.toByteArray())

    val running_start = System.nanoTime()

    /**
      * Running the processor
      */

    engine.generateRDFTriples(dataset, mappings, parameters, exeTriplesMap.toArray, is)

    val running_elapsed = System.nanoTime() - running_start
    println("Running RML Mapper engine: " + running_elapsed / 1000000000.0 +" seconds")


    val processing_start = System.nanoTime()

    /**
      * Processing the output of the processor
      */
    val triplesOutputStream = new ByteArrayOutputStream()
    dataset.dumpRDF(triplesOutputStream, RDFFormat.TURTLE)
    val triplesInputStream = new ByteArrayInputStream(triplesOutputStream.toByteArray)

    val model = ModelFactory.createDefaultModel()
    model.read(triplesInputStream, null, "TURTLE")

    val processing_elapsed = System.nanoTime()
    println("Processing output: " + processing_elapsed / 1000000000.0 +" seconds")


    val generating_start = System.nanoTime()

    /**
      * Iterating over the output and generating Quads
      */
    val statementIterator = model.listStatements()
    var seq = Seq.empty[Quad]
    while(statementIterator.hasNext) {

      val statement = statementIterator.nextStatement()

      // extract object value
      val objectValue = if(statement.getObject.isResource) {
        statement.getObject.asResource().toString
      } else if(statement.getObject.isLiteral) {
        statement.getObject.asLiteral().getString
      } else {
        throw new RuntimeException(statement.getSubject.getURI + " has no valid object")
      }

      // extract predicate value
      val ontologyProperty = RMLOntologyUtil.loadOntologyPropertyFromIRI(statement.getPredicate.getURI, context)

      //TODO: check for all mapping datastructures if datatype and mapDataset is calculated like this <- this is not the case
      //TODO: Datasets need to be applied correctly, solution need to be found!
      // extract datatype from predicate uri
      val datatype = if(ontologyProperty.range.isInstanceOf[Datatype]) ontologyProperty.range.asInstanceOf[Datatype] else null
      var mapDataset = if (datatype == null) DBpediaDatasets.OntologyPropertiesObjects else DBpediaDatasets.OntologyPropertiesLiterals
      // if the triple is a geo coordinate
      if(ontologyProperty.name == "geo:lat" || ontologyProperty.name == "geo:lon") mapDataset = DBpediaDatasets.OntologyPropertiesGeo


      // generate quad
      val quad = new Quad(context.language, mapDataset, statement.getSubject.getURI, ontologyProperty,
        objectValue, templateNode.sourceUri, datatype)

      seq :+= quad

    }

    val generating_elapsed = System.nanoTime()
    println("Generating quads: " + generating_elapsed/ 1000000000.0 +" seconds")

    seq

  }

  /**
    * Convert the template node to HashMap that can be processed by the RML processor
    *
    * @param templateNode
    * @return
    */
  private def convertTemplateNodeToMap(templateNode: TemplateNode) : util.HashMap[String,String] = {
    val hashMap = new util.HashMap[String,String]()
    val keyset = templateNode.keySet
    for(key <- keyset) {
      val node = templateNode.property(key).get
      hashMap.put(key, node.toWikiText)
    }

    hashMap
  }

}

