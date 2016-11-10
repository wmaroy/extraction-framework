package org.dbpedia.extraction.mappings.rml.processing

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, ObjectOutputStream}
import java.util

import be.ugent.mmlab.rml.core.StdRMLEngine
import be.ugent.mmlab.rml.model.dataset.{RMLDataset, StdRMLDataset}
import be.ugent.mmlab.rml.model.{RMLMapping, TriplesMap}
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.dbpedia.extraction.dataparser.DateTimeParser
import org.dbpedia.extraction.destinations.{DBpediaDatasets, Quad}
import org.dbpedia.extraction.mappings.Redirects
import org.dbpedia.extraction.mappings.rml.util.RMLOntologyUtil
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.ontology.datatypes.Datatype
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser.{ExternalLinkNode, InternalLinkNode, TemplateNode}
import org.eclipse.rdf4j.model.impl.URIImpl
import org.eclipse.rdf4j.rio.RDFFormat

import scala.language.reflectiveCalls

/**
  *
  * Runs the RML Processor
  *
  */
class RMLProcessorRunner(mappings: RMLMapping) {

  def process(templateNode: TemplateNode, triplesMap: TriplesMap, subjectUri: String, context : { def language : Language
                                                                                                  def ontology: Ontology
                                                                                                  def redirects: Redirects}) : Seq[Quad] = {

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

    val is = new ByteArrayInputStream(baos.toByteArray())

    engine.generateRDFTriples(dataset, mappings, parameters, exeTriplesMap.toArray, is)

    val triplesOutputStream = new ByteArrayOutputStream()
    dataset.dumpRDF(triplesOutputStream, RDFFormat.TURTLE)
    val triplesInputStream = new ByteArrayInputStream(triplesOutputStream.toByteArray)

    val model = ModelFactory.createDefaultModel()
    model.read(triplesInputStream, null, "TURTLE")

    val statementIterator = model.listStatements()
    var seq = Seq.empty[Quad]
    while(statementIterator.hasNext) {

      val statement = statementIterator.nextStatement()

      val objectUri = if(statement.getObject.isResource) {
        statement.getObject.asResource().toString
      } else if(statement.getObject.isLiteral) {
        statement.getObject.asLiteral().getString
      } else {
        throw new RuntimeException(statement.getSubject.getURI + " has no valid object")
      }

      val ontologyProperty = RMLOntologyUtil.loadOntologyPropertyFromIRI(statement.getPredicate.getURI, context)

      //TODO: check for all mapping datastructures if datatype and mapDataset is calculated like this
      val datatype = if(ontologyProperty.range.isInstanceOf[Datatype]) ontologyProperty.range.asInstanceOf[Datatype] else null
      val mapDataset = if (datatype == null) DBpediaDatasets.OntologyPropertiesObjects else DBpediaDatasets.OntologyPropertiesLiterals


      //TODO: writing values should get it's own def
      val value = ontologyProperty.range match {
        case dt : Datatype => dt.name match {
          case "xsd:date" =>
          {
            val parser = new DateTimeParser(context, dt)
            parser.findDate(objectUri).get.toString
          }
          case "xsd:gYear" =>
          {
            val parser = new DateTimeParser(context, dt)
            parser.findDate(objectUri).get.toString
          }
          case "xsd:gYearMonth" =>
          {
            val parser = new DateTimeParser(context, dt)
            parser.findDate(objectUri).get.toString
          }
          case "xsd:gMonthDay" =>
          {
            val parser = new DateTimeParser(context, dt)
            parser.findDate(objectUri).get.toString
          }
          case other => objectUri
        }
        case other => objectUri
      }



      val quad = new Quad(context.language, mapDataset, statement.getSubject.getURI, ontologyProperty,
        value, templateNode.sourceUri, datatype)

      seq :+= quad

    }

    seq

  }

  private def convertTemplateNodeToMap(templateNode: TemplateNode) : util.HashMap[String,String] = {
    val hashMap = new util.HashMap[String,String]()
    val keyset = templateNode.keySet
    for(key <- keyset) {
      val node = templateNode.property(key).get
      if(node.children.size == 1) {
        hashMap.put(key, node.children.head.retrieveText.get.replaceAll("\n", ""))
      } else {
        var found = false;
        var i = 0
        while(!found && i < node.children.size) {
          if(node.children(i).isInstanceOf[InternalLinkNode]) {
            val internalLinkNode = node.children(i).asInstanceOf[InternalLinkNode]
            hashMap.put(key, internalLinkNode.destination.resourceIri)
            found = true
          }
          i += 1
        }

      }
    }

    return hashMap
  }

}

