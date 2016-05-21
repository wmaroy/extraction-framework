package org.dbpedia.extraction.mappings.rml

import be.ugent.mmlab.rml.model.{RMLMapping, TriplesMap}
import org.dbpedia.extraction.mappings.{Extractor, Mappings, PropertyMapping, TemplateMapping}
import org.dbpedia.extraction.ontology.{Ontology, OntologyClass, OntologyProperty}
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser.{TableNode, TemplateNode}
import collection.JavaConverters._
import scala.collection.immutable.List
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
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
              def mappingDoc : RMLMapping }) : Mappings =
  {

      val templateMappings = new HashMap[String, Extractor[TemplateNode]]
      val tableMappings = new ArrayBuffer[Extractor[TableNode]] //now still empty
      val triplesMapCollection = context.mappingDoc.getTriplesMaps.asScala

      for (triplesMap: TriplesMap <- triplesMapCollection) {
        templateMappings.put(triplesMap.getShortName, loadTemplateMapping(triplesMap, context))
      }

      new Mappings(templateMappings.toMap,tableMappings.toList)

  }

  /**
    * Loads a template mapping from a triples map
    */
  private def loadTemplateMapping(triplesMap: TriplesMap, context : {
                                                            def ontology: Ontology
                                                            def language: Language }) : TemplateMapping =
  {

      val mapToClass: OntologyClass = RMLOntologyLoader.loadMapToClassOntology(triplesMap, context)
      val correspondingClass: OntologyClass = RMLOntologyLoader.loadCorrespondingClassOntology(triplesMap, context)
      val correspondingProperty: OntologyProperty = RMLOntologyLoader.loadCorrespondingPropertyOntology(triplesMap, context)
      val propertyMappingList: List[PropertyMapping] = RMLPropertyMappingsLoader.loadPropertyMappings(triplesMap, context)

      new TemplateMapping(mapToClass, correspondingClass, correspondingProperty, propertyMappingList, context)

  }

}
