package org.dbpedia.extraction.destinations

import java.io.{File}

import org.dbpedia.extraction.util.RichFile.wrapFile
import org.dbpedia.extraction.destinations.formatters.Formatter
import org.dbpedia.extraction.util.{DateFinder, IOUtils, Finder}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Chile on 9/29/2016.
  */
object DestinationUtils {
  def createDestination(finder: Finder[File], date: String, datasets: Seq[Dataset], formats: Map[String, Formatter]) : Destination = {
    val destination = new ArrayBuffer[Destination]()
    for ((suffix, format) <- formats) {
      val datasetDestinations = new mutable.HashMap[String, Destination]()
      for (dataset <- datasets) {
        val file = finder.file(date, dataset.name.replace('_', '-')+'.'+suffix)
        datasetDestinations(dataset.name) = new WriterDestination(() => IOUtils.writer(file), format)
      }
      destination += new DatasetDestination(datasetDestinations)
    }
    new CompositeDestination(destination.toSeq: _*)
  }

  def createDestination(finder: DateFinder[File], datasets: Seq[Dataset], formats: Map[String, Formatter]) : Destination = {
    createDestination(finder.finder, finder.date, datasets, formats)
  }
}
