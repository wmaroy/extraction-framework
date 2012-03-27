package org.dbpedia.extraction.destinations

import formatters.NTriplesFormatter
import java.net.URLEncoder
import collection.mutable.HashMap
import java.io._

/**
 * A destination which writes all statements to files.
 * This class is thread-safe.
 *
 * @param formatter The formatter used to serialize the statements. If no formatter is provided, the statements are written using the N-Triples format.
 * @param baseDir The base directory of the output files. If no base directory is provided, the output is written to current directory.
 * @param filePattern The pattern according to which the statements are split into different files by their dataset.
 */
class FileDestination(formatter : Formatter = new NTriplesFormatter(),
                      baseDir : File = new File("."),
                      filePattern : (Dataset => String), append : Boolean = false) extends Destination
{
    if (! baseDir.isDirectory() && ! baseDir.mkdirs()) throw new IOException("cannot create base dir "+baseDir)

    private val writers = HashMap[String, Writer]()

    private var closed = false

    override def write(graph : Graph) : Unit = synchronized
    {
        if(closed) throw new IllegalStateException("Trying to write to a closed destination")

        for((dataset, quads) <- graph.quadsByDataset)
        {
            val writer = getWriter(dataset)

            for(quad <- quads)
            {
                formatter.write(quad, writer)
            }
        }
    }

    override def close() = synchronized
    {
        if(!closed)
        {
            for(writer <- writers.values)
            {
                formatter.writeFooter(writer)
                writer.close()
            }
            closed = true
        }
    }

    private def getWriter(dataset : Dataset) : Writer =
    {
        val fileName = filePattern(dataset)
        writers.getOrElseUpdate(fileName, createWriter(fileName))
    }
    
    private def createWriter(fileName : String) : Writer=
    {
        val file = new File(baseDir, fileName)
        mkdirs(file.getParentFile)
    
        val stream = new FileOutputStream(file, append)
        val writer = new OutputStreamWriter(stream, "UTF-8")
        formatter.writeHeader(writer)
    
        writer
    }
    
    private def mkdirs(dir : File) : Unit =
    {
      if (! dir.isDirectory && ! dir.mkdirs) throw new IOException("directory "+dir+" does not exist and cannot be created")
    }
      
}
