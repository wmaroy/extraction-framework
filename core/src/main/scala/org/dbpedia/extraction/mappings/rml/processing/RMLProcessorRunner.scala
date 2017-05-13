package org.dbpedia.extraction.mappings.rml.processing

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, ObjectOutputStream}
import java.net.{URLDecoder, URLEncoder}
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
import org.dbpedia.extraction.wikiparser.impl.wikipedia.Namespaces
import org.dbpedia.extraction.wikiparser.{ExternalLinkNode, InternalLinkNode, TemplateNode}
import org.eclipse.rdf4j.model.impl.URIImpl
import org.eclipse.rdf4j.rio.RDFFormat

import scala.collection.JavaConversions
import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.language.reflectiveCalls

/**
  *
  * Runs the RML Processor
  * //TODO: refactor this class!
  */
class RMLProcessorRunner(mappings: Map[String, RMLMapping]) {

  def process(templateNode: TemplateNode, mappingName : String, subjectUri: String, context : { def language : Language
                                                                                                  def ontology: Ontology
                                                                                                  def redirects: Redirects}) : Seq[Quad] = {

    RMLProcessorRunner.count += 1

    RMLProcessorRunner.context = context
    RMLProcessorRunner.mappings = mappings


    val templateNodeHashMap = convertTemplateNodeToMap(templateNode)
    val regex = ".*/".r
    templateNodeHashMap.put("wikititle", regex.replaceAllIn(subjectUri, ""))


    /**
      * Running the processor
      */
    val start = System.nanoTime()
    RMLProcessorRunner.addToCache(templateNodeHashMap, mappingName)
    val seq = if(RMLProcessorRunner.isFull(mappingName)) {

      processFromCache(mappingName, RMLProcessorRunner._fastCache, context)

    } else Seq()
    var delta = System.nanoTime() - start
    RMLProcessorRunner.RMLProcessorCounter += delta

    seq


  }

  private def processFromCache(mappingName : String, cache : HashMap[String, Seq[util.HashMap[String,String]]], context :{ def language : Language
                                                                    def ontology: Ontology
                                                                    def redirects: Redirects} ) : Seq[Quad] = {

    /**
      *  Setting up the processor
      */

    val parameters = new util.HashMap[String, String]()
    val triplesMap = "http://en.dbpedia.org/resource/" + mappingName
    val exeTriplesMap = List[String](triplesMap)
    val engine = new StdRMLEngine()
    val dataset : RMLDataset = new StdRMLDataset()

    /**
      * Setting up the dataset stream
      */
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(new util.ArrayList(JavaConversions.seqAsJavaList(cache.get(mappingName).get)))
    oos.flush()
    oos.close()
    val is = new ByteArrayInputStream(baos.toByteArray)

    engine.generateRDFTriples(dataset, mappings(mappingName), parameters, exeTriplesMap.toArray, is)
    RMLProcessorRunner.flush(mappingName)
    RMLProcessorRunner.flushCount += 1

    /**
      * Processing the output of the processor
      */
    val triplesOutputStream = new ByteArrayOutputStream()
    dataset.dumpRDF(triplesOutputStream, RDFFormat.TURTLE)
    val triplesInputStream = new ByteArrayInputStream(triplesOutputStream.toByteArray)
    val model = ModelFactory.createDefaultModel()
    model.read(triplesInputStream, null, "TURTLE")



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

      val subjectURI = statement.getSubject.getURI
      val subjectURIDecoded = URLDecoder.decode(subjectURI, "UTF-8") // The RMLProcessor encodes this uri

      // extract predicate value
      val ontologyProperty = RMLOntologyUtil.loadOntologyPropertyFromIRI(statement.getPredicate.getURI, context)

      val quad = if(ontologyProperty != null) {

        //TODO: Datasets need to be applied correctly, solution need to be found!

        val datatype = try {
          val regex = ".*/".r
          val name = regex.replaceAllIn(statement.getObject.asLiteral.getDatatype.getURI, "")
          val dt = RMLOntologyUtil.loadOntologyDataType(name , context)
          dt
        } catch {
          case e: Exception => ontologyProperty.range match {
            case dt: Datatype => dt
            case _ => null
          }
        }

        var mapDataset = if (datatype == null) DBpediaDatasets.OntologyPropertiesObjects else DBpediaDatasets.OntologyPropertiesLiterals
        // if the triple is a geo coordinate
        if (ontologyProperty.name == "geo:lat" || ontologyProperty.name == "geo:lon") mapDataset = DBpediaDatasets.OntologyPropertiesGeo

        // generate quad
        val quad = new Quad(context.language, mapDataset, subjectURIDecoded, ontologyProperty,
          objectValue, null, datatype)

        quad

      } else {

        // if the ontology is not in DBpedia!

        val datatype = if(statement.getObject.isResource) {
          null
        } else {
          statement.getObject.asLiteral().getDatatypeURI
        }

        // generate quad
        val quad = new Quad(context.language.toString, DBpediaDatasets.OntologyPropertiesLiterals.toString, subjectURIDecoded, statement.getPredicate.getURI,
          objectValue, null, datatype)

        quad
      }

      seq :+= quad

    }

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
      val pattern = ".*?=".r
      hashMap.put(key, pattern replaceFirstIn(node.toWikiText, ""))
    }

    hashMap
  }

}

// experimental ...

object RMLProcessorRunner {

  var context : { def language : Language
    def ontology: Ontology
    def redirects: Redirects} = new {
      def ontology: Ontology = null
      def language: Language = null
      def redirects : Redirects = null
  }

  var mappings : Map[String,RMLMapping] = null

  var flushCount = 0

  val cacheLimit = 1000

  private var _cacheMap = new HashMap[String, Seq[util.HashMap[String, String]]]
  private var _fastCache = new HashMap[String, Seq[util.HashMap[String, String]]]

  def cacheMap : HashMap[String, Seq[util.HashMap[String,String]]] = {
    _cacheMap
  }

  def addToCache(infoboxContent : util.HashMap[String,String], infoboxName : String) = {
    if(_fastCache.contains(infoboxName)) {
      val updatedSequence = _fastCache.get(infoboxName).get :+ infoboxContent
      _fastCache = _fastCache + (infoboxName -> updatedSequence)
    } else {
      val updatedSequence = if (_cacheMap.contains(infoboxName)) {
        _cacheMap.get(infoboxName).get :+ infoboxContent
      } else {
        Seq(infoboxContent)
      }
      if (updatedSequence.size > 100) {
        _fastCache = _fastCache + (infoboxName -> updatedSequence)
        _cacheMap = _cacheMap - infoboxName
      } else {
        _cacheMap = _cacheMap + (infoboxName -> updatedSequence)
      }
    }
  }

  private def processFromCache(mappingName : String, cache : HashMap[String, Seq[util.HashMap[String,String]]], context :{ def language : Language
    def ontology: Ontology
    def redirects: Redirects} ) : Seq[Quad] = {

    /**
      *  Setting up the processor
      */

    val parameters = new util.HashMap[String, String]()
    val triplesMap = "http://en.dbpedia.org/resource/" + mappingName
    val exeTriplesMap = List[String](triplesMap)
    val engine = new StdRMLEngine()
    val dataset : RMLDataset = new StdRMLDataset()

    /**
      * Setting up the dataset stream
      */
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(new util.ArrayList(JavaConversions.seqAsJavaList(cache.get(mappingName).get)))
    oos.flush()
    oos.close()
    val is = new ByteArrayInputStream(baos.toByteArray)

    engine.generateRDFTriples(dataset, mappings(mappingName), parameters, exeTriplesMap.toArray, is)
    RMLProcessorRunner.flush(mappingName)
    RMLProcessorRunner.flushCount += 1

    /**
      * Processing the output of the processor
      */
    val triplesOutputStream = new ByteArrayOutputStream()
    dataset.dumpRDF(triplesOutputStream, RDFFormat.TURTLE)
    val triplesInputStream = new ByteArrayInputStream(triplesOutputStream.toByteArray)
    val model = ModelFactory.createDefaultModel()
    model.read(triplesInputStream, null, "TURTLE")



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

      val subjectURI = statement.getSubject.getURI
      val subjectURIDecoded = URLDecoder.decode(subjectURI, "UTF-8") // The RMLProcessor encodes this uri

      // extract predicate value
      val ontologyProperty = RMLOntologyUtil.loadOntologyPropertyFromIRI(statement.getPredicate.getURI, context)

      val quad = if(ontologyProperty != null) {

        //TODO: Datasets need to be applied correctly, solution need to be found!

        val datatype = try {
          val regex = ".*/".r
          val name = regex.replaceAllIn(statement.getObject.asLiteral.getDatatype.getURI, "")
          val dt = RMLOntologyUtil.loadOntologyDataType(name , context)
          dt
        } catch {
          case e: Exception => ontologyProperty.range match {
            case dt: Datatype => dt
            case _ => null
          }
        }

        var mapDataset = if (datatype == null) DBpediaDatasets.OntologyPropertiesObjects else DBpediaDatasets.OntologyPropertiesLiterals
        // if the triple is a geo coordinate
        if (ontologyProperty.name == "geo:lat" || ontologyProperty.name == "geo:lon") mapDataset = DBpediaDatasets.OntologyPropertiesGeo

        // generate quad
        val quad = new Quad(context.language, mapDataset, subjectURIDecoded, ontologyProperty,
          objectValue, null, datatype)

        quad

      } else {

        // if the ontology is not in DBpedia!

        val datatype = if(statement.getObject.isResource) {
          null
        } else {
          statement.getObject.asLiteral().getDatatypeURI
        }

        // generate quad
        val quad = new Quad(context.language.toString, DBpediaDatasets.OntologyPropertiesLiterals.toString, subjectURIDecoded, statement.getPredicate.getURI,
          objectValue, null, datatype)

        quad
      }

      seq :+= quad

    }

    seq
  }

  def isFull(infoboxName : String): Boolean = {
    if(_fastCache.contains(infoboxName)) {
      _fastCache.get(infoboxName).get.size > cacheLimit
    } else false
  }

  def flush(infoboxName : String) = {
    _fastCache = _fastCache - infoboxName
  }

  def flushRemaining() : Seq[Quad] = {
    val seq_1  = _cacheMap.flatMap(tuple => {
      println("name: " + tuple._1 + "; " + " left: " + tuple._2.size)
      processFromCache(tuple._1, _cacheMap, context)
    })
    val seq_2 = _fastCache.flatMap(tuple => {
      println("name: " + tuple._1 + "; " + " left: " + tuple._2.size)
      processFromCache(tuple._1, _fastCache, context)
    })

    seq_1.toSeq ++ seq_2.toSeq
  }

  var RMLProcessorCounter = 0L

  var JenaCounter = 0L

  var ProcessorRunnerCounter = 0L

  var count = 0

  def printResult() = {
    print("RML Processor: " + RMLProcessorCounter / count + "\n")
    print("Jena: " + JenaCounter / count + "\n")
    print("Total: " + ProcessorRunnerCounter / count + "\n")
  }

}

