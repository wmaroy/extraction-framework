package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.Quad
import org.dbpedia.extraction.wikiparser._

/**
 * TODO: get rid of this class... get initial URI from page...?
 */
class RootExtractor(val extractor: Extractor[PageNode])
{
    /**
     * Processes a wiki page and returns the extracted data.
     *
     * @param page The source page
     * @return A graph holding the extracted data
     */
    final def apply(page : PageNode) : Seq[Quad] =
    {
      //Generate the page URI
      val uri = page.title.language.resourceUri.append(page.title.decodedWithNamespace)

      //Extract
      extractor.extract(page, uri, new PageContext())
    }
}
