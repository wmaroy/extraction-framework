package org.dbpedia.extraction.mappings.rml.util

import be.ugent.mmlab.rml.model.TriplesMap
import org.dbpedia.extraction.ontology.datatypes.Datatype
import org.dbpedia.extraction.ontology.{Ontology, OntologyClass, OntologyProperty}
import org.openrdf.model.URI

import scala.language.reflectiveCalls

/**
  * Loading ontology data from given ontology class instance
  */
object RMLOntologyUtil {

  // "class" is the rml value
  private final val mapToClass: String = "class"

  def loadMapToClassOntology(triplesMap: TriplesMap, context: {def ontology : Ontology}): OntologyClass = {
      loadTriplesMapOntologyClass(triplesMap, mapToClass, context)
  }

  def loadCorrespondingPropertyOntology(triplesMap: TriplesMap, context: {def ontology : Ontology}): OntologyProperty = {
      null //TODO: ? not defined in the rml mappings
  }

  def loadCorrespondingClassOntology(triplesMap: TriplesMap, context: {def ontology : Ontology}): OntologyClass = {
      null //TODO: ? not defined in the rml mappings
  }

  def loadOntologyClass(ontologyClassName : String, context: {def ontology: Ontology}): OntologyClass = {
      try {
        context.ontology.classes(ontologyClassName)
      } catch {
        case _: NoSuchElementException => throw new IllegalArgumentException("Ontology class not found: " + ontologyClassName)
      }

  }

  def loadOntologyProperty(ontologyPropertyName: String, context: {def ontology: Ontology}): OntologyProperty = {
      try {
        context.ontology.properties(ontologyPropertyName)
      } catch {
        case _ : NoSuchElementException => throw new IllegalArgumentException("Ontology property not found: " + ontologyPropertyName)
      }
  }

  def loadOntologyDataType(ontologyDataTypeName: String, context: {def ontology: Ontology}): Datatype = {
    try {
      context.ontology.datatypes(ontologyDataTypeName)
    } catch {
      case _ : NoSuchElementException => null
    }
  }

  def loadOntologyPropertyFromIRI(ontologyIRI : String, context : {def ontology: Ontology}): OntologyProperty = {
      //TODO: change in RMLProcessor for looking up local ontology property
      val localOntologyPropertyName = ontologyIRI.replaceAll(".*/","")
      try {
        loadOntologyProperty(localOntologyPropertyName, context)
      } catch {
        case _ : IllegalArgumentException => println("Skipping Ontology Property: " + localOntologyPropertyName); null
      }
  }

  def loadOntologyDataTypeFromIRI(ontologyIRI : String, context : { def ontology : Ontology}) : Datatype = {
      val localOntologyDataTypeName = ontologyIRI.replaceAll(".*/","")
      loadOntologyDataType(localOntologyDataTypeName, context)
  }

  // private defs

  private def loadTriplesMapOntologyClass(triplesMap: TriplesMap, ontologyType : String, context: {def ontology : Ontology}): OntologyClass = {
      val ontologyClassName = loadTriplesMapOntologyClassName(triplesMap)
      loadOntologyClass(ontologyClassName, context)
  }

  private def loadTriplesMapOntologyClassName(triplesMap: TriplesMap): String = {
      triplesMap.getSubjectMap.getClassIRIs.toArray.head.asInstanceOf[URI].getLocalName
  }


}
