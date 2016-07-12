package org.dbpedia.extraction.mappings.rml

import org.dbpedia.extraction.mappings._
import org.dbpedia.extraction.mappings.rml.loading.RMLMappingsLoader
import org.dbpedia.extraction.mappings.rml.util.{ContextCreator}
import org.dbpedia.extraction.util.Language
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

/**
  * Testing RMLMappingsLoader
  */

@RunWith(classOf[JUnitRunner])
class RMLMappingsLoaderTest extends FlatSpec with Matchers
{

  // language
  val languageEN = Language.English

  // xml mapping file location
  val xmlMappingPath = "src/test/resources/org/dbpedia/extraction/mappings/rml/infobox_person.xml"

  // rml mapping file location
  val rmlDocumentPath = "src/test/resources/org/dbpedia/extraction/mappings/rml/infobox_person.rml"

  // testing RMLMappingsLoader && MappingsLoader
  val rmlTemplateMappings = RMLMappingsLoader.load(ContextCreator.createRMLContext(rmlDocumentPath, languageEN)).templateMappings
  val xmlTemplateMappings = MappingsLoader.load(ContextCreator.createXMLContext(xmlMappingPath, languageEN)).templateMappings

  // printing output of both versions
  println("**** RML MAPPINGS ****")

  var simple = 0
  var date = 0
  var intermediate = 0
  var geo = 0

  for(mapping <- rmlTemplateMappings.head._2.asInstanceOf[TemplateMapping].mappings) {
    mapping.getClass.getSimpleName match {
      case "SimplePropertyMapping" => simple += 1
      case "DateIntervalMapping" => date += 1
      case "IntermediateNodeMapping" => intermediate += 1
      case "GeoCoordinatesMapping" => geo += 1
    }
  }

  println("SimplePropertyMappings: " + simple)
  println("DateIntervalMappings: " + date)
  println("IntermediateNodeMappings: " + intermediate)
  println("GeoCoordinateMappings: " + geo)

  simple = 0
  date = 0
  intermediate = 0
  geo = 0

  println("\n\n**** XML MAPPINGS ****")
  for(mapping <- xmlTemplateMappings.head._2.asInstanceOf[TemplateMapping].mappings) {
    mapping.getClass.getSimpleName match {
      case "SimplePropertyMapping" => simple += 1
      case "DateIntervalMapping" => date += 1
      case "IntermediateNodeMapping" => intermediate += 1
      case "GeoCoordinatesMapping" => geo += 1
    }
  }

  println("SimplePropertyMappings: " + simple)
  println("DateIntervalMappings: " + date)
  println("IntermediateNodeMappings: " + intermediate)
  println("GeoCoordinateMappings: " + geo)

  println("\nEnd of test.\n\n\n")




}
