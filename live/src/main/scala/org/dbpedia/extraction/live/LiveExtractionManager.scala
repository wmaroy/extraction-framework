package org.dbpedia.extraction.live.extraction

import xml.Elem

import java.util.Properties;
import java.io.File

import org.dbpedia.extraction.wikiparser._

import java.io._
import org.dbpedia.extraction.sources.{LiveExtractionSource,XMLSource,WikiSource}

/**
 * Created by IntelliJ IDEA.
 * User: Mohamed Morsey
 * Date: May 26, 2010
 * Time: 4:15:28 PM
 * This is the main class responsible for managing the process of live extraction.
 */

object LiveExtractionManager
{
  var wiki : WikiTitle = null;
  var oaiID : String = "";
  var Num=1;

  def extractFromPage(Element :scala.xml.Elem)
    {
      val articlesSource = LiveExtractionSource.fromXML(Element);

      LiveExtractionConfigLoader.startExtraction(articlesSource);

    }

}