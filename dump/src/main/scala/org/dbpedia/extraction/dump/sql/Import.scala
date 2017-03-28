package org.dbpedia.extraction.dump.sql

import java.io._
import org.dbpedia.extraction.dump.download.Download
import org.dbpedia.extraction.sources.XMLSource
import org.dbpedia.extraction.util._
import org.dbpedia.extraction.util.ConfigUtils.parseLanguages
import org.dbpedia.extraction.util.RichFile.wrapFile
import org.dbpedia.extraction.wikiparser.Namespace
import scala.io.Codec
import scala.collection.mutable.Set
import java.util.Properties
import scala.io.Source

object Import {
  
  def main(args: Array[String]) : Unit = {
    
    val baseDir = new File(args(0))
    val tablesFile = new File(args(1))
    val url = args(2)
    val requireComplete = args(3).toBoolean
    val fileName = args(4)
    val importThreads = args(5).toInt

    // Use all remaining args as keys or comma or whitespace separated lists of keys
    val languages = parseLanguages(baseDir, args.drop(6))
    
    val source = Source.fromFile(tablesFile)(Codec.UTF8)
    val tables =
    try source.getLines.mkString("\n")
    finally source.close()
    
    //With the new change in Abstract extractor we need all articles TODO FIX this sometime soon and use only categories
    val namespaces = Set(Namespace.Template, Namespace.Category, Namespace.Main, Namespace.Module)
    val namespaceList = namespaces.map(_.name).mkString("[",",","]")

      org.dbpedia.extraction.util.Workers.work(SimpleWorkers(importThreads, importThreads){ language : Language =>      //loadfactor: think about disk read speed and mysql processes

        val info = new Properties()
        info.setProperty("allowMultiQueries", "true")
        val conn = new com.mysql.jdbc.Driver().connect(url, info)
        try {
          val finder = new Finder[File](baseDir, language, "wiki")
          val date = finder.dates(fileName).last

          val completeExists = finder.file(date, Download.Complete) match {
            case None => false
            case Some(x) => x.exists()
          }

          if (requireComplete && completeExists) {
            finder.file(date, fileName) match {
              case None =>
              case Some(file) => {
                val database = finder.wikiName

                println(language.wikiCode + ": importing pages in namespaces " + namespaceList + " from " + file + " to database " + database + " on server URL " + url)

                /*
            Ignore the page if a different namespace is also given for the title.
            http://gd.wikipedia.org/?curid=4184 and http://gd.wikipedia.org/?curid=4185&redirect=no
            have the same title "Teamplaid:Gàidhlig", but they are in different namespaces: 4184 is
            in the template namespace (good), 4185 is in the main namespace (bad). It looks like
            MediaWiki can handle this somehow, but when we try to import both pages from the XML dump
            into the database, MySQL rightly complains about the duplicate title. As a workaround,
            we simply reject pages for which the <ns> namespace doesn't fit the <title> namespace.
            */
                val source = XMLSource.fromReader(() => IOUtils.reader(file), language, title => title.otherNamespace == null && namespaces.contains(title.namespace))

                val stmt = conn.createStatement()
                try {
                  stmt.execute("DROP DATABASE IF EXISTS " + database + "; CREATE DATABASE " + database + " CHARACTER SET binary; USE " + database + ";")
                  stmt.execute(tables)
                }
                finally stmt.close()

                val pages = new Importer(conn, language).process(source)

                println(language.wikiCode + ": imported " + pages + " pages in namespaces " + namespaceList + " from " + file + " to database " + database + " on server URL " + url)
              }
            }
          }
        }
          finally conn.close()
      }, languages.toList)
  }
}

