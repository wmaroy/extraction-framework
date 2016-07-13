package org.dbpedia.extraction.mappings.rml.loading

import be.ugent.mmlab.rml.model.RDFTerm.ObjectMap
import be.ugent.mmlab.rml.model.std.StdConditionObjectMap
import be.ugent.mmlab.rml.model.{RMLMapping, TriplesMap}
import org.dbpedia.extraction.mappings._
import org.dbpedia.extraction.mappings.rml.util.RMLOntologyUtil
import org.dbpedia.extraction.ontology.{Ontology, OntologyClass, OntologyProperty}
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
    val defaultMappings = loadTemplateMapping(triplesMap,context).mappings

    new ConditionalMapping(conditions,defaultMappings)
  }

  private def loadConditions(triplesMap: TriplesMap, context : {
                                                            def ontology: Ontology
                                                            def language: Language
                                                            def redirects: Redirects}) : List[ConditionMapping] =
  {
    val set : Iterable[ObjectMap] = triplesMap.getPredicateObjectMaps.asScala.head.getObjectMaps.asScala
    for(objectMap <- set) {
      if(objectMap.isInstanceOf[StdConditionObjectMap]) {
        println("found")
      }
    }

    null
  }

}
