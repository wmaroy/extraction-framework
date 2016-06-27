package org.dbpedia.extraction.server.resources.rml.model

import java.util.Date

import org.apache.jena.rdf.model.Resource
import org.dbpedia.extraction.mappings.{ConditionalMapping, GeoCoordinatesMapping, IntermediateNodeMapping, _}
import org.dbpedia.extraction.ontology.datatypes.Datatype

/**
  * Class that adds rml mappings to a ModelWrapper
  */
class RMLModelMapper(modelWrapper: RMLModelWrapper) {


  def addSimplePropertyMapping(mapping: SimplePropertyMapping) =
  {
    val uniqueString = createUniquePredicateObjectMapString("SimplePropertyMapping/" + mapping.templateProperty)
    val predicateObjectMap = modelWrapper.addPredicateObjectMapToModel(uniqueString)

    //add predicate
    modelWrapper.addPropertyAsPropertyToResource(predicateObjectMap, Prefixes("rr") + "predicate", mapping.ontologyProperty.uri)

    //add object map with rml reference
    val objectMap = modelWrapper.addBlankNode()
    modelWrapper.addResourceAsPropertyToResource(predicateObjectMap, Prefixes("rr") + "objectMap", objectMap)
    modelWrapper.addLiteralAsPropertyToResource(objectMap, Prefixes("rml") + "reference", mapping.templateProperty)

    //add unit if present
    if(mapping.unit != null) addUnitToPredicateObjectMap(predicateObjectMap, mapping.unit)

    modelWrapper.addPredicateObjectMapUriToTriplesMap(uniqueString)
  }

  def addCalculateMapping(mapping: CalculateMapping) =
  {
    //TODO: implement
  }

  def addCombineDateMapping(mapping: CombineDateMapping) =
  {
    //TODO: implement
  }

  def addDateIntervalMapping(mapping: DateIntervalMapping) =
  {
    val uniqueString = createUniquePredicateObjectMapString(mapping.templateProperty + "/" + mapping.startDateOntologyProperty.name + "/" + mapping.endDateOntologyProperty.name)
    val dateIntervalPom = modelWrapper.addPredicateObjectMapToModel(uniqueString)
    modelWrapper.addResourceAsPropertyToResource(modelWrapper.triplesMap, Prefixes("rr") + "predicateObjectMap", dateIntervalPom)

    val object1 = modelWrapper.addBlankNode()
    modelWrapper.addLiteralAsPropertyToResource(object1, Prefixes("rml") + "reference", mapping.templateProperty)
    modelWrapper.addPredicateObjectMapToResource(dateIntervalPom, mapping.startDateOntologyProperty.uri, object1)

    val object2 = modelWrapper.addBlankNode()
    modelWrapper.addLiteralAsPropertyToResource(object2, Prefixes("rml") + "reference", mapping.templateProperty)
    modelWrapper.addPredicateObjectMapToResource(dateIntervalPom, mapping.endDateOntologyProperty.uri, object2)
  }

  def addGeoCoordinatesMapping(mapping: GeoCoordinatesMapping) =
  {
    if(mapping.coordinates != null) {
      val objectMap1 = modelWrapper.addBlankNode()
      modelWrapper.addLiteralAsPropertyToResource(objectMap1, Prefixes("rr") + "parentTriplesMap", mapping.coordinates)
      modelWrapper.addPredicateObjectMapToMainTriplesMap(Prefixes("dbo") + "coordinates", objectMap1)

    } else if (mapping.latitude != null && mapping.longitude != null) {
      //TODO: implement
    }
  }

  def addConditionalMapping(mapping: ConditionalMapping) =
  {
    //TODO: implement
  }

  def addIntermediateNodeMapping(mapping: IntermediateNodeMapping) =
  {

    //create the predicate object map
    val templateString = "IntermediateNodeMapping/" + mapping.nodeClass.name + "/" + mapping.correspondingProperty.name
    val uniqueString = createUniquePredicateObjectMapString(templateString)
    val predicateObjectMap = modelWrapper.addPredicateObjectMapToModel(uniqueString)

    modelWrapper.addPropertyAsPropertyToResource(predicateObjectMap, Prefixes("rr") + "predicate", mapping.correspondingProperty.uri)
    modelWrapper.addPredicateObjectMapUriToTriplesMap(uniqueString)


    //create the triples map
    val subjectMap = modelWrapper.addResource(createUniquePredicateObjectMapString(templateString + "/SubjectMap"), Prefixes("rr") + "SubjectMap")
    modelWrapper.addPropertyAsPropertyToResource(subjectMap, Prefixes("rr") + "constant", "tobeDefined")
    val triplesMap = modelWrapper.addTriplesMapToModel(createUniquePredicateObjectMapString(templateString + "/TriplesMap"), subjectMap)

    val objectMap = modelWrapper.addBlankNode()
    modelWrapper.addResourceAsPropertyToResource(objectMap, Prefixes("rr") + "parentTriplesMap", triplesMap)
    modelWrapper.addResourceAsPropertyToResource(predicateObjectMap, Prefixes("rr") + "objectMap", objectMap)

    //create the mappings

  }

  private def createUniquePredicateObjectMapString(name : String): String =
  {
    "http://mappings.dbpedia.org/wiki/" + modelWrapper.wikiTitle.encodedWithNamespace + "/" + name
  }

  private def addUnitToPredicateObjectMap(predicateObjectMap: Resource, unit : Datatype): Unit =
  {
    val objectMap = modelWrapper.addBlankNode()
    modelWrapper.addPropertyAsPropertyToResource(objectMap, Prefixes("rr") + "parentTriplesMap", unit.uri)
    modelWrapper.addResourceAsPropertyToResource(predicateObjectMap, Prefixes("rr") + "objectMap", objectMap)
  }
  
}