package org.dbpedia.extraction.mappings.rml.util

import java.util

import be.ugent.mmlab.rml.model.{RMLMapping, TriplesMap}

import scala.collection.JavaConverters._

/**
  * Wraps around an RML Mapping to provide extra functions
  */
class RMLMappingWrapper(mapping : RMLMapping) {


  //TODO: store necessary triples maps only

  private val triplesMapSet : Set[String] = filterTriplesMap(mapping.getTriplesMaps.asScala)

  /**
    * Gets the uri of the triples map if this exists in a mapping
    * @param name
    * @return
    */
  def getTriplesMap(name: String) : Option[String] = {
    val transformedTitle = parsedName(name)
    if (triplesMapSet.contains(transformedTitle)) {
      Some(transformedTitle)
    } else None
  }

  /**
    * Parses template title, should be done differently =/
    *
    * @param name
    * @return
    */
  private def parsedName(name : String) = {
    val title = "http://en.dbpedia.org/resource/Mapping_en:" + name
    val encodedTitle = title.replaceAll(" ", "_")
    encodedTitle
  }

  /**
    * Filter the good triples maps, those which represent a valid infobox mapping
    * @param triplesMaps
    * @return
    */
  private def filterTriplesMap(triplesMaps : Iterable[TriplesMap]) : Set[String] = {
    val filtered = triplesMaps.flatMap {
      case x if x.getSubjectMap == null => None
      case x if x.getSubjectMap.getStringTemplate == null => None
      case x if x.getSubjectMap.getStringTemplate.equals("http://en.dbpedia.org/resource/{wikititle}") => Some(x.getName)
      case x => None
    }
    filtered.toSet
  }


}
