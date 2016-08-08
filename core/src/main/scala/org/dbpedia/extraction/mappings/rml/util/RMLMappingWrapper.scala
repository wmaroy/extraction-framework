package org.dbpedia.extraction.mappings.rml.util

import be.ugent.mmlab.rml.model.{RMLMapping, TriplesMap}
import scala.collection.JavaConverters
/**
  * Wraps around an RML Mapping to provide extra functions
  */
class RMLMappingWrapper(mapping : RMLMapping) {

  def getTriplesMap(name: String) : Option[TriplesMap] = {
    val triplesMaps = mapping.getTriplesMaps
    val it = triplesMaps.iterator()
    while(it.hasNext) {
      val triplesMap = it.next()
      val triplesMapName = triplesMap.getName.replace("http://en.dbpedia.org/resource/Mapping_en:", "")
      val decodedName = name.replace(" ", "_")
      if(triplesMapName.equals(decodedName)) {
        return Some(triplesMap)
      }
    }
    None
  }

}
