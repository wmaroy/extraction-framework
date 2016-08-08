package org.dbpedia.extraction.mappings.rml.loading

import be.ugent.mmlab.rml.model.std.StdConditionPredicateObjectMap
import be.ugent.mmlab.rml.model.{PredicateObjectMap, RMLMapping, TriplesMap}
import org.dbpedia.extraction.mappings._
import org.dbpedia.extraction.mappings.rml.util.RMLOntologyUtil
import org.dbpedia.extraction.ontology.{Ontology, OntologyClass, OntologyProperty, RdfNamespace}
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser.{TableNode, TemplateNode}

import scala.collection.JavaConverters._
import scala.collection.immutable.List
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.language.reflectiveCalls

/**
  * Responsible for loading mappings from parsed RML documents
  */
object RMLMappingsLoader {

  /**
    * Loads mappings from an RMLMapping context
    */
  def load(context: {
              def ontology : Ontology
              def language : Language
              def redirects: Redirects
              def mappingDoc : RMLMapping }) : Mappings =
  {

      val templateMappings = new HashMap[String, Extractor[TemplateNode]]
      val tableMappings = new ArrayBuffer[Extractor[TableNode]] //now still empty

      val triplesMapCollection = context.mappingDoc.getTriplesMaps.asScala //get triplesmaps from mapping document
      for (triplesMap: TriplesMap <- triplesMapCollection) {
        if(triplesMap.getDCTermsType == "templateMapping") {
          println("Loading: " + triplesMap.getName)
          try {
            templateMappings.put(triplesMap.getName().replaceAll(".*/Mapping_en:", "").replaceAll("_", " "), loadTemplateMapping(triplesMap, context))
          } catch {
            case e : IllegalArgumentException => {
              println(e)
              println("Failed to load Template Mapping: " + triplesMap.getName)
            }
          }
        } else if(triplesMap.getDCTermsType == "conditionalMapping") {
          println("Loading: " + triplesMap.getName)
          try {
            templateMappings.put(triplesMap.getName().replaceAll(".*/Mapping_en:", "").replaceAll("_", " "), loadConditionalMapping(triplesMap, context))
          } catch {
            case e : IllegalArgumentException => {
              println(e)
              println("Failed to load Conditional Mapping: " + triplesMap.getName)
            }
          }
        }
      }

      new Mappings(templateMappings.toMap,tableMappings.toList)

  }

  /**
    * Loads a template mapping from a triples map
    */
  private def loadTemplateMapping(triplesMap: TriplesMap, context : {
                                                            def ontology: Ontology
                                                            def language: Language
                                                            def redirects: Redirects}) : TemplateMapping =
  {

      val mapToClass: OntologyClass = RMLOntologyUtil.loadMapToClassOntology(triplesMap, context)
      val correspondingClass: OntologyClass = RMLOntologyUtil.loadCorrespondingClassOntology(triplesMap, context)
      val correspondingProperty: OntologyProperty = RMLOntologyUtil.loadCorrespondingPropertyOntology(triplesMap, context)
      val propertyMappingList: List[PropertyMapping] = RMLPropertyMappingsLoader.loadPropertyMappings(triplesMap, context)

      new TemplateMapping(mapToClass, correspondingClass, correspondingProperty, propertyMappingList, context)

  }

  private def loadConditionalMapping(triplesMap: TriplesMap, context : {
                                                            def ontology: Ontology
                                                            def language: Language
                                                            def redirects: Redirects}) : ConditionalMapping =
  {
    val conditions = loadConditions(triplesMap, context)
    val defaultMappings = RMLPropertyMappingsLoader.loadPropertyMappings(triplesMap, context)

    new ConditionalMapping(conditions,defaultMappings)
  }

  private def loadConditions(triplesMap: TriplesMap, context : {
                                                            def ontology: Ontology
                                                            def language: Language
                                                            def redirects: Redirects}) : List[ConditionMapping] =
  {
    val poms = triplesMap.getPredicateObjectMaps.asScala
    var conditionMappings: List[ConditionMapping] = List[ConditionMapping]()
    for(pom <- poms) {
      val predicate = pom.getPredicateMaps.asScala.head.getConstantValue.toString

      //TODO fix this in RMLModel: accessing conditionalPoms and determine DCTermsType from conditionalPoms
      if(pom.getDCTermsType == null &&  predicate ==  RdfNamespace.RDF.namespace + "type") {
        val conditionPom = pom.asInstanceOf[StdConditionPredicateObjectMap]
        conditionMappings = loadConditionPoms(conditionPom, conditionMappings, context)
      }
    }

    conditionMappings
  }

  private def loadConditionPoms(conditionPom: StdConditionPredicateObjectMap, mappings: List[ConditionMapping], context : {
                                                            def ontology: Ontology
                                                            def language: Language
                                                            def redirects: Redirects}) : List[ConditionMapping] =
  {
    var _mappings = mappings

    val condition = conditionPom.getConditions.asScala.head //TODO: get equalConditions
    val fallbacks = conditionPom.getFallbackPOMs.asScala.toList

    val correspondingClass = null
    val correspondingProperty = null
    val mapToClass = RMLOntologyUtil.loadMapToClassOntologyViaType(conditionPom, context)
    val propertyMappings = RMLPropertyMappingsLoader.loadPropertyMappingsFromList(fallbacks, context)

    val templateMapping = new TemplateMapping(mapToClass, correspondingClass, correspondingProperty, propertyMappings, context)


    //TODO: fill in value, property and operator field for condition
    val value = null
    
    val property = null
    val operator = null

    var nextConditionPom: StdConditionPredicateObjectMap = null
    var nextFallbacks: List[PredicateObjectMap] = List[PredicateObjectMap]()
    for(fallback <- fallbacks)
    {
      if(fallback.getPredicateMaps.asScala.head.getConstantValue == RdfNamespace.RDF.namespace + "type")
      {
        nextConditionPom = fallback.asInstanceOf[StdConditionPredicateObjectMap]
      } else {
        nextFallbacks ::= fallback
      }

    }

    _mappings ::= new ConditionMapping(property, operator, value, templateMapping)

    if(nextConditionPom != null) {
      loadConditionPoms(nextConditionPom, _mappings, context)
    } else {
      _mappings
    }
  }


}
