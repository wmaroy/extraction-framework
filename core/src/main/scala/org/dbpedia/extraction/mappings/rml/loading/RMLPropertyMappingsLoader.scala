package org.dbpedia.extraction.mappings.rml.loading

import be.ugent.mmlab.rml.model.{PredicateObjectMap, TriplesMap}
import org.dbpedia.extraction.mappings.rml.util.RMLOntologyUtil
import org.dbpedia.extraction.mappings._
import org.dbpedia.extraction.ontology.{Ontology, OntologyProperty, RdfNamespace}
import org.dbpedia.extraction.util.Language

import scala.collection.JavaConverters._
import scala.language.reflectiveCalls


/**
  * Loads property mappings of a single mapping (triples map)
  */
object RMLPropertyMappingsLoader {

  /**
    * Loads all property mappings from a triples map
    */
  def loadPropertyMappings(triplesMap: TriplesMap, context:{def ontology: Ontology
                                                            def language: Language
                                                            def redirects: Redirects}) : List[PropertyMapping] =

  {

      var propertyMappings = List[PropertyMapping]()
      val predicateObjectMaps = triplesMap.getPredicateObjectMaps.asScala

      for (predicateObjectMap : PredicateObjectMap <- predicateObjectMaps) {
          val propertyMapping = loadPropertyMapping(predicateObjectMap, context)
          if(propertyMapping != null) {
            propertyMappings ::= propertyMapping
          }
      }

      propertyMappings

  }

  /**
    * Loads property mapping
    */
  def loadPropertyMapping(predicateObjectMap: PredicateObjectMap, context: {def ontology: Ontology
                                                                            def language: Language
                                                                            def redirects: Redirects}) : PropertyMapping =
  {




    predicateObjectMap.getDCTermsType match {

      case "simplePropertyMapping" => {

        val predicateMap = predicateObjectMap.getPredicateMaps.asScala.head
        val ontologyProperty = RMLOntologyUtil.loadOntologyPropertyFromIRI(predicateMap.getConstantValue.stringValue(), context)
        val isObjectMap = predicateObjectMap.getObjectMaps.size() != 0

        val templateProperty = if(isObjectMap) {
          val objectMap = predicateObjectMap.getObjectMaps.asScala.head
          objectMap.getReferenceMap.getReference
        } else {
          getParameterRef(predicateObjectMap, "property")
        }

        val select = if(isObjectMap) null else {
          getParameterRef(predicateObjectMap, "select")
        }

        val prefix = if(isObjectMap) null else {
          getParameterRef(predicateObjectMap, "prefix")
        }

        val suffix = if(isObjectMap) null else {
          getParameterRef(predicateObjectMap, "suffix")
        }

        val transform = if(isObjectMap) null else {
          getParameterRef(predicateObjectMap, "transform")
        }

        val factor = if(isObjectMap) 1.0 else {
          val _factor = getParameterRef(predicateObjectMap, "factor")
          if(_factor != null) _factor.toDouble else 1.0
        }

        if(ontologyProperty != null) {

          val dataType = RMLOntologyUtil.loadOntologyDataTypeFromIRI(ontologyProperty.range.name, context)
          new SimplePropertyMapping(templateProperty, ontologyProperty, select, prefix, suffix, transform, dataType, context.language, factor, context)

        } else {

          null

        }

      }

      case "startDateIntervalMapping" => {
        loadStartDateIntervalMapping(predicateObjectMap, context)
      }
      case "endDateIntervalMapping" => {
          null
      }


      case "constantMapping" => {
        val predicateMap = predicateObjectMap.getPredicateMaps.asScala.head
        val ontologyProperty = RMLOntologyUtil.loadOntologyPropertyFromIRI(predicateMap.getConstantValue.stringValue(), context)
        val isObjectMap = predicateObjectMap.getObjectMaps.size() != 0

        val value = if(isObjectMap) {
          val objectMap = predicateObjectMap.getObjectMaps.asScala.head
          objectMap.getConstantValue.stringValue()
        } else {
          getParameterRef(predicateObjectMap, "value")
        }

        val unit = if(isObjectMap) null else {
          getParameterRef(predicateObjectMap, "unit")
        }

        if(ontologyProperty != null) {

          val dataType = RMLOntologyUtil.loadOntologyDataTypeFromIRI(ontologyProperty.range.name, context)
          new ConstantMapping(ontologyProperty, value, dataType, context)

        } else {

          null

        }

      }

      case "latitudeMapping" => {
        null
      }

      case "longitudeMapping" => {
        null
      }

      case "intermediateNodeMapping" => {
        null
      }

      case null => {
        null
      }
    }

  }

  private def getParameterRef(predicateObjectMap: PredicateObjectMap, param: String) : String =
  {
    val functionTermMap = predicateObjectMap.getFunctionTermMaps.asScala.head
    if(functionTermMap.getParameterRefs.asScala.contains(RdfNamespace.DBF.namespace + param + "Parameter")) {
      functionTermMap.getParameterRefs.asScala(RdfNamespace.DBF.namespace + param + "Parameter")
    } else {
      null
    }
  }

  private def loadStartDateIntervalMapping(predicateObjectMap: PredicateObjectMap, context: {def ontology: Ontology
                                                                                            def language: Language
                                                                                            def redirects: Redirects}) : DateIntervalMapping =
  {
    val predicateMap = predicateObjectMap.getPredicateMaps.asScala.head
    val startOntologyProperty = RMLOntologyUtil.loadOntologyPropertyFromIRI(predicateMap.getConstantValue.stringValue(), context)
    var endOntologyProperty: OntologyProperty = null

    val templateProperty = getParameterRef(predicateObjectMap, "startDate")
    for (pom <- predicateObjectMap.getOwnTriplesMap.getPredicateObjectMaps.asScala) {

      // search for end pom
      val isCorrespondingOntology = pom.getDCTermsType == "endDateIntervalMapping" &&
        pom.getPredicateMaps.asScala.head.getConstantValue.stringValue().replaceAll("(?i)end", "") ==
          startOntologyProperty.uri.replaceAll("(?i)start", "")
      val isSameTemplateProperty = if (isCorrespondingOntology) {
        val endDateTemplateProperty = getParameterRef(pom, "endDate")
        templateProperty == endDateTemplateProperty
      } else false
      if (isCorrespondingOntology && isSameTemplateProperty) {
        val predicateMap = pom.getPredicateMaps.asScala.head
        endOntologyProperty = RMLOntologyUtil.loadOntologyPropertyFromIRI(predicateMap.getConstantValue.stringValue(), context)
      }
    }
    if (startOntologyProperty != null && endOntologyProperty != null) {
      new DateIntervalMapping(templateProperty,startOntologyProperty,endOntologyProperty, context)
    } else null
  }
}
