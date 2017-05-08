package org.dbpedia.extraction.dump.extract

import java.util.logging.{Level, Logger}

import org.dbpedia.extraction.destinations.Destination
import org.dbpedia.extraction.mappings.WikiPageExtractor
import org.dbpedia.extraction.sources.{Source, WikiPage}
import org.dbpedia.extraction.util.SimpleWorkers
import org.dbpedia.extraction.wikiparser.Namespace
import org.dbpedia.util.Exceptions
import org.slf4j.LoggerFactory

/**
 * Executes a extraction.
 *
 * @param extractor The Extractor
 * @param source The extraction source
 * @param namespaces Only extract pages in these namespaces
 * @param destination The extraction destination. Will be closed after the extraction has been finished.
 * @param label user readable label of this extraction job.
 */
class ExtractionJob(extractor: WikiPageExtractor, source: Source, namespaces: Set[Namespace], destination: Destination, label: String, description: String)
{
  private val logger = Logger.getLogger(getClass.getName)

  private val progress = new ExtractionProgress(label, description)

  private val workers = SimpleWorkers { page: WikiPage =>
    var success = false
    try {
      if (namespaces.contains(page.title.namespace)) {
        //val graph = extractor(parser(page))
        val graph = extractor.extract(page)
        destination.write(graph)
      }
      success = true
    } catch {
      case ex: Exception => logger.log(Level.WARNING, "error processing page '"+page.title+"': "+Exceptions.toString(ex, 200))
    }
    progress.countPage(success)
  }
  
  def run(): Unit =
  {

    progress.start()
    
    destination.open()

    workers.start()


    process()

    
    workers.stop()
    
    destination.close()
    
    progress.end()
  }

  def process() : Unit = {
    var count = 0
    for (page <- source) {
      count += 1
      workers.process(page)
      if(count % 100 == 0) {
        println(count + " pages done")
      }
    }
  }
  
}
