package org.dbpedia.extraction.mappings.rml

import collection.JavaConverters._
import be.ugent.mmlab.rml.model.{PredicateObjectMap, RMLMapping, TriplesMap}
import org.dbpedia.extraction.mappings.rml.loading.RMLParser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class RMLParserTest extends FlatSpec with Matchers
{

    "RMLParser" should "return RMLMapping" in
    {
        parse() shouldBe a [RMLMapping]
    }

    "RMLParser" should "find Triples Maps with a dcterms:type" in
    {
        val document = parse()

        var counter = 0
        for (triplesMap: TriplesMap <- document.getTriplesMaps.asScala;
             predicateObjectMap: PredicateObjectMap <- triplesMap.getPredicateObjectMaps.asScala) {
            if(predicateObjectMap.getDCTermsType != null) {
                counter += 1
            }
        }
        println("\nAmount of Predicate Object Maps with dcterm:type that were found: " + counter)
    }

    "RMLParser " should "find multiple RML in a folder" in {

        val dir = "/home/wmaroy/github/rml-xml-to-zip"
        val loadedMappings = RMLParser.parseFromDir(dir)

        println(loadedMappings.size + " mappings were loaded.")
        println(loadedMappings)
    }

    def parse(): RMLMapping =
    {
        val pathToDocument = "src/test/resources/org/dbpedia/extraction/mappings/rml/infobox_person.rml"
        val rmlMapping = RMLParser.parseFromFile(pathToDocument)
        return rmlMapping

    }
}

