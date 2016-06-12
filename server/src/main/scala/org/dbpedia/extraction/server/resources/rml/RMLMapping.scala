package org.dbpedia.extraction.server.resources.rml

/**
  * RMLMappings converted from DBpedia mappings using a triple store
  */
abstract class RMLMapping {

  def printAsNTriples: Unit

  def printAsTurtle: Unit
}
