package org.dbpedia.extraction.live.extraction

import xml.XML

import java.net.URL
import collection.immutable.ListMap
import java.util.Properties
import java.io.File
import java.util.logging.{Level, Logger}
import java.awt.event.{ActionListener, ActionEvent}
import org.dbpedia.extraction.mappings._
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.sources.{WikiSource, Source}
import org.dbpedia.extraction.wikiparser.{WikiParser, WikiTitle}
import org.dbpedia.extraction.destinations.LiveUpdateDestination
import org.dbpedia.extraction.live.job.LiveExtractionJob
import org.dbpedia.extraction.live.helper.{ExtractorStatus, LiveConfigReader}
import org.dbpedia.extraction.live.statistics.StatisticsData
import org.dbpedia.extraction.live.main.Main
import org.dbpedia.extraction.live.core.LiveOptions
import org.dbpedia.extraction.dump.extract.ExtractionJob
import org.dbpedia.extraction.wikiparser.Namespace


/**
 * Created by IntelliJ IDEA.
 * User: Mohamed Morsey
 * Date: May 26, 2010
 * Time: 3:10:00 PM
 * This object loads the configuration information from the config file and also performs the actual step of extracting
 * the required triples from the wikipage.
 */

object LiveExtractionConfigLoader extends ActionListener
{
  //This number is used for filling the recentlyUpdatedInstances array which is in Main class, this array is used
  //for statistics
  private var instanceNumber = 0;

  //    private var config : Config = null;
  private var extractors : List[RootExtractor] = null;
  private var CurrentJob : ExtractionJob = null;
  private var reloadOntologyAndMapping = true;
  private var ontologyAndMappingsUpdateTime : Long = 0;
  val logger = Logger.getLogger("LiveExtractionConfigLoader");

  /** Ontology source */
  val ontologySource = WikiSource.fromNamespaces(
    namespaces = Set(Namespace.OntologyClass, Namespace.OntologyProperty),
    url = new URL(Language.Mappings.apiUri),
    language = Language.Mappings );

  /** Mappings source */
  val mappingsSource =  WikiSource.fromNamespaces(
    namespaces = Set(Namespace.mappings(Language.apply(LiveOptions.options.get("language")))),
    url = new URL(Language.Mappings.apiUri),
    language = Language.Mappings );

  println ("COMMONS SOURCE = " + LiveOptions.options.get("commonsDumpsPath"));

  val commonsSource = null;

  def actionPerformed(e: ActionEvent) =
  {
    reloadOntologyAndMapping = true;
  }

  def reload(t : Long) =
  {
    if (t > ontologyAndMappingsUpdateTime)
      reloadOntologyAndMapping = true;
  }

  /**
   * Retrieves the dump stream for a specific language edition.
   */
  private def getDumpFile(dumpDir : File, wikiPrefix : String) : File =    //wikiPrefix is language prefix (and can be 'commons')
  {
    val wikiDir = new File(dumpDir + "/" + wikiPrefix)
    if(!wikiDir.isDirectory) throw new Exception("Dump directory not found: " + wikiDir)

    //Find most recent dump date
    val date = wikiDir.list()
      .filter(_.matches("\\d{8}"))
      .sortWith(_.toInt > _.toInt)
      .headOption.getOrElse(throw new Exception("No dump found for Wiki: " + wikiPrefix))

    val articlesDump = new File(wikiDir + "/" + date + "/" + wikiPrefix.replace('-', '_') + "wiki-" + date + "-pages-articles.xml")
    if(!articlesDump.isFile) throw new Exception("Dump not found: " + articlesDump)

    articlesDump
  }


  /**
   * Loads the configuration and creates extraction jobs for all configured languages.
   *
   * @param configFile The configuration file
   * @return Non-strict Traversable over all configured extraction jobs i.e. an extractions job will not be created until it is explicitly requested.
   */
  def startExtraction(articlesSource : Source) : Unit =
  {
    //        if(config.multihreadingMode)
    if(LiveConfigReader.multihreadingMode)
    {
      val extractionJobs = convertExtractorMapToScalaMap(LiveConfigReader.extractorClasses).keySet.view.map(createExtractionJob(articlesSource))

      for(extractionJob <- extractionJobs)
      {
        extractionJob.start();
      }
    }
    else
    {
      startSingleThreadExtraction(articlesSource)(Language.apply(LiveOptions.options.get("language")));
    }
  }

  def isMultithreading():Boolean =
  {
    //    config.multihreadingMode;
    LiveConfigReader.multihreadingMode;
  }

  /**
   * Creates an extraction job for a specific language.
   */

  private def createExtractionJob(articlesSource : Source)(language : Language) : LiveExtractionJob =
  {

    // In case of multi-threading
    //Extractor
    if(extractors == null || reloadOntologyAndMapping)
    {
      extractors = LoadOntologyAndMappings(articlesSource, language);
      logger.log(Level.INFO, "Ontology and mappings reloaded");
      reloadOntologyAndMapping = false;
    }

    new LiveExtractionJob(null, articlesSource, language, "Extraction Job for " + language.wikiCode + " Wikipedia");

  }


  private def startSingleThreadExtraction(articlesSource : Source)(language : Language):Unit =
  {
    // In case of single threading
    //Extractor
    if(extractors==null || reloadOntologyAndMapping)
    {
      extractors = LoadOntologyAndMappings(articlesSource, language);
      logger.log(Level.INFO, "Ontology and mappings reloaded");
      reloadOntologyAndMapping = false;
    }

    var liveDest : LiveUpdateDestination = null;
    val parser = WikiParser()

    for (cpage <- articlesSource.map(parser))
    {

      if(cpage.title.namespace == Namespace.Main ||
        cpage.title.namespace == Namespace.File ||
        cpage.title.namespace == Namespace.Category)
      {
        liveDest = new LiveUpdateDestination(cpage.title, language.locale.getLanguage(),
          cpage.id.toString)

        liveDest.setPageID(cpage.id);
        liveDest.setOAIID(LiveExtractionManager.oaiID);

        //Add triples generated from active extractors
        extractors.foreach(extractor => {
          println(extractor.getClass())
          var RequiredGraph = extractor(cpage);

          //When the DBpedia framework is updated, there is a class called "RootExtractor", which contain internally the
          // required extractor, so we should get it from inside
          liveDest.write(RequiredGraph, extractor.extractor.getClass().getName());
        });

        //Remove triples generated from purge extractors
        liveDest.removeTriplesForPurgeExtractors();

        //Keep triples generated from keep extractors
        liveDest.retainTriplesForKeepExtractors();

        liveDest.close();

        logger.log(Level.INFO, "page number " + cpage.id + " has been processed");

        //Updating information needed for statistics

        // TODO: Remove statistics for now, we have triple store for basic statistics
        //StatisticsData.addItem(cpage.title.decoded, strDBppage,strWikipage,0, System.currentTimeMillis)
      }

    }
  }

  //This method loads the ontology and mappings
  //@param  articlesSource  The source of the wikipage article
  //@param  language  The required language
  //    private def LoadOntologyAndMappings(articlesSource: Source, language: Language): Extractor = {
  private def LoadOntologyAndMappings(articlesSource: Source, language: Language): List[RootExtractor] = {

    ontologyAndMappingsUpdateTime = System.currentTimeMillis
    val extractorClasses = convertExtractorListToScalaList(LiveConfigReader.getExtractors(language, ExtractorStatus.ACTIVE))
    org.dbpedia.extraction.live.extractor.LiveExtractor.load(ontologySource, mappingsSource, articlesSource, commonsSource,
            extractorClasses, language)
  }

  /**
   * Converts a givin java list to scala list
   * @ param  list  The required java list
   * @return  The newly generated scala list
   */
  private def convertExtractorListToScalaList(list : java.util.List[Class[Extractor]]): List[Class[Extractor]] =
  {
    var extractorList =  List[Class[Extractor]]();

    val listiterator = list.iterator();
    while(listiterator.hasNext){
      extractorList = extractorList ::: List[Class[Extractor]](listiterator.next());
    }
    println(extractorList);
    extractorList;
  }

  private def convertExtractorMapToScalaMap(map: java.util.Map[Language, java.util.List[Class[Extractor]]]):
  Map[Language, List[Class[Extractor]]] =
  {
    var extractorMap =  Map[Language, List[Class[Extractor]]]();

    val mapIterator = map.entrySet.iterator();
    while(mapIterator.hasNext){
      var pairs = mapIterator.next();
      extractorMap += pairs.getKey() -> convertExtractorListToScalaList(pairs.getValue());
    }
    println(extractorMap);
    extractorMap;

  }

  private class Config(config : Properties)
  {

    /** Languages */
    if(config.getProperty("languages") == null)
      throw new IllegalArgumentException("Property 'languages' is not defined.")
    private val languages = config.getProperty("languages").split("\\s+").map(_.trim).toList
      .map(code => Language.getOrElse(code, throw new IllegalArgumentException("Invalid language: '" + code + "'")))

    /** The period of updating ontology and mappings**/
    if(config.getProperty("updateOntologyAndMappingsPeriod") == null)
      throw new IllegalArgumentException("Property 'updateOntologyAndMappingsPeriod' is not defined.");
    val updateOntologyAndMappingsPeriod = config.getProperty("updateOntologyAndMappingsPeriod").toInt;
    println("updateOntologyAndMappingsPeriod = " + updateOntologyAndMappingsPeriod);

    if(config.getProperty("multihreadingMode") == null) throw new IllegalArgumentException("Property 'multihreadingMode' is not defined.")
    val multihreadingMode = config.getProperty("multihreadingMode").toBoolean;

    /** Extractor classes */
    val extractors = loadExtractorClasses()

    /** Ontology source */
    val ontologySource = WikiSource.fromNamespaces(namespaces = Set(Namespace.OntologyClass, Namespace.OntologyProperty),
      url = new URL("http://mappings.dbpedia.org/api.php"),
      language = Language.apply(LiveOptions.options.get("language")) );

    /** Mappings source */
    val mappingsSource =  WikiSource.fromNamespaces(namespaces = Set(Namespace.mappings(Language.apply(LiveOptions.options.get("language")))),
      url = new URL("http://mappings.dbpedia.org/api.php"),
      language = Language.apply(LiveOptions.options.get("language")) );

    /**
     *  Loads the extractors classes from the configuration.
     *
     * @return A Map which contains the extractor classes for each language
     */
    private def loadExtractorClasses() : Map[Language, List[Class[Extractor]]] =
    {
      //Load extractor classes
      if(config.getProperty("extractors") == null) throw new IllegalArgumentException("Property 'extractors' not defined.")
      val stdExtractors = loadExtractorConfig(config.getProperty("extractors"))

      //Create extractor map
      var extractors = ListMap[Language, List[Class[Extractor]]]()
      for(language <- languages) extractors += ((language, stdExtractors))

      //Load language specific extractors
      val LanguageExtractor = "extractors\\.(.*)".r

      for(LanguageExtractor(code) <- config.stringPropertyNames.toArray;
          language = Language.getOrElse(code, throw new IllegalArgumentException("Invalid language: " + code));
          if extractors.contains(language))
      {
        extractors += ((language, stdExtractors ::: loadExtractorConfig(config.getProperty("extractors." + code))))
      }

      extractors
    }

    /**
     * Parses a enumeration of extractor classes.
     */
    private def loadExtractorConfig(configStr : String) : List[Class[Extractor]] =
    {
      configStr.split("\\s+").map(_.trim).toList
        .map(className => ClassLoader.getSystemClassLoader().loadClass(className))
        .map(_.asInstanceOf[Class[Extractor]])
    }
  }

  private class LiveConfig(){
    val xmlConfigTree = XML.loadFile("./live.config");
    var currentNode = xmlConfigTree \\ "extractor";
    println(currentNode.toString)
  }
}
