package org.dbpedia.extraction.server.stats

import java.io.File
import java.util.logging.Logger

import org.dbpedia.extraction.destinations.{Dataset,DBpediaDatasets}
import org.dbpedia.extraction.util.RichFile.wrapFile
import org.dbpedia.extraction.util.StringUtils.prettyMillis
import org.dbpedia.extraction.util.{Finder,Language}
import org.dbpedia.extraction.util.Language.wikiCodeOrdering
import org.dbpedia.extraction.wikiparser.Namespace

/**
 * Script to gather statistics about templates and properties:
 * - which templates exist?
 * - which parameters (properties) does each template have?
 * - on how many pages / how often in total is a template used?
 * - how often is each paramater (property) used?
 * 
 * Needs the following files (where xx is the wiki file prefix and yyyymmdd is a date):
 * 
 * xxwiki-yyyymmdd-article-templates.ttl
 * xxwiki-yyyymmdd-infobox-test.ttl
 * xxwiki-yyyymmdd-transitive-redirects.ttl
 * xxwiki-yyyymmdd-template-parameters.ttl
 * 
 * They are generated by the following extractors:
 * 
 * org.dbpedia.extraction.mappings.RedirectExtractor
 * org.dbpedia.extraction.mappings.InfoboxExtractor
 * org.dbpedia.extraction.mappings.TemplateParameterExtractor
 * 
 * All DBpedia URIs in these files must use specific domains, even en.dbpedia.org.
 * 
 * .ttl.bz2 / .ttl.gz input files are also allowed.
 *
 * TODO: The extraction framework should be flexible and configurable enough that
 * it can write simpler formats besides N-Triples. This class would be MUCH simpler and faster
 * if it had to read simple text files without N-Triples and URI-encoding.
 */
object CreateMappingStats
{
    val logger = Logger.getLogger(getClass.getName)
    
    def main(args: Array[String])
    {
        require (args != null && args.length >= 3, "need at least four args: input dir, output dir, file suffix ('.gz', '.bz2' or ''), pretty-printing flag. may be followed by list of language codes.")
        
        val inputDir = new File(args(0))
        
        val statsDir = new File(args(1))
        
        val suffix = args(2).trim

        val pretty = args(3).toBoolean
        
        // Use all remaining args as language codes or comma or whitespace separated lists of codes
        var languages: Seq[Language] = for(arg <- args.drop(4); lang <- arg.split("[,\\s]"); if (lang.nonEmpty)) yield Language(lang)
          
        // if no languages are given, use all languages for which a mapping namespace is defined
        if (languages.isEmpty) languages = Namespace.mappings.keySet.toSeq
        
        languages.sorted.par.foreach(language =>  {
          
            val millis = System.currentTimeMillis()
            
            logger.info("creating statistics for "+language.wikiCode)
            
            val finder = new Finder[File](inputDir, language, "wiki")
            
            // Note: org.dbpedia.extraction.dump.extract.Extraction.Complete = "extraction-complete"
            // TODO: move that constant to core, or use config value
            val date = finder.dates("extraction-complete").last
            
            def inputFile(dataset: Dataset): File = {
              finder.file(date, dataset.name.replace('_', '-')+".ttl"+suffix)
            }
            
            // extracted by org.dbpedia.extraction.mappings.RedirectExtractor
            val redirects = inputFile(DBpediaDatasets.Redirects)
            
            // extracted by org.dbpedia.extraction.mappings.ArticleTemplatesExtractor
            val articleTemplates = inputFile(DBpediaDatasets.ArticleTemplates)
            // extracted by org.dbpedia.extraction.mappings.TemplateParameterExtractor
            val templateParameters = inputFile(DBpediaDatasets.TemplateParameters)
            // extracted by org.dbpedia.extraction.mappings.InfoboxExtractor
            val infoboxTest = inputFile(DBpediaDatasets.InfoboxTest)
            
            val builder = new MappingStatsBuilder(statsDir, language, pretty)
    
            builder.buildStats(redirects, articleTemplates, templateParameters, infoboxTest)
            
            // load them right back to check that the format is ok
            new MappingStatsManager(statsDir, language)
            
            logger.info("created statistics for "+language.wikiCode+" in "+prettyMillis(System.currentTimeMillis - millis))
        })
    }
}
