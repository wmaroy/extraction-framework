package org.dbpedia.extraction.scripts

import java.io._
import org.dbpedia.extraction.util.RichFile.wrapFile
import IOUtils._
import scala.collection.mutable.{ArrayBuffer}
import scala.Console._
import org.dbpedia.extraction.util.StringUtils._
import scala.collection.SortedSet
import java.lang.Boolean

/**
 * Split multistream Wikipedia dumps (e.g. [1]) into size-configurable chunks
 * [1] http://dumps.wikimedia.org/enwiki/latest/enwiki-latest-pages-articles-multistream.xml.bz2
 * Note: This script only works with multistream dumps!
 *
 * Usage:
 * ../run WikipediaDumpSplitter /path/to/multistream/dump/enwiki-latest-pages-articles-multistream.xml.bz2
 * /path/to/mulstistream/dump/index/enwiki-latest-pages-articles-multistream-index.txt.bz2 /output/directory 64
 */
object WikipediaDumpSplitter {

  def main(args: Array[String]) {

    require(args != null && args.length >= 4,
      "need three args: " +
        /*0*/ "wikipedia dump file (must be in the multistream format), " +
        /*1*/ "streams index file, " +
        /*2*/ "output directory, " +
        /*3*/ "approx. chunk size (in MB)")

    // Collect arguments
    val dump = new File(args(0))
    require(dump.isFile() && dump.canRead(), "Please specify a valid dump file!")

    /**
     * The streams index contains the boundaries of each bz2 stream
     */
    val index = new File(args(1))
    require(index.isFile() && index.canRead(), "Please specify a valid streams index!")

    val output = new File(args(2))
    require(output.isDirectory() && output.canWrite(), "Please specify a valid output directory")

    val chunkSize = args(3).toInt * 1024 * 1024 // Let the exception flow
    require(chunkSize > 0, "Please specify a positive integer")

    // Whether to replace the chunk files if they already exist
    val replace = try {
      Boolean.parseBoolean(args(4))
    } catch {
      case _ : Exception => true // default value
    }

    // Prepare initial data
    val i = dump.name.lastIndexOf('.')
    val chunkNamePrefix = dump.name.substring(0, i)
    val chunkNameExtension = dump.name.substring(i + 1)
    var chunkNumber = 0
    var chunk : File = null

    var lines = 0
    val start = System.nanoTime

    /**
     * The streams index file has the following format
     *
     * offset:pageid:title
     *
     * offset represents the bytes offset of the bzip2 stream from the start of the dump file
     *
     * The other parameters are irrelevant for our uses
     */
    var offsets = SortedSet[Long]()
    var offsetsSeq = Seq[Long]()

    // Offsets cache file
    val indexCache = new File(index.getParentFile.getAbsolutePath, index.name + ".obj")

    // We should collect stream offsets
    if (indexCache.exists()) {

      val inputStream = new ObjectInputStream(new FileInputStream(indexCache))
      try
      {
        offsetsSeq = inputStream.readObject().asInstanceOf[Seq[Long]]
      }
      finally
      {
        inputStream.close()
      }

    } else {

      readLines(index) { line =>

        lines += 1

        // Should we use?
        // - line.takeWhile(_ != ':')
        // - line.split(":")(0)
        offsets += line.substring(0, line.indexOf(':')).toLong

        if (lines % 10000 == 0) log(lines, offsets.size, start)
      }

      offsetsSeq = offsets.toSeq

      val outputStream = new ObjectOutputStream(new FileOutputStream(indexCache))
      try
      {
        outputStream.writeObject(offsetsSeq)
      }
      finally
      {
        outputStream.close()
      }
    }

    // Save the Mediawiki XML header
    chunk = startChunk(output, chunkNamePrefix, "header", chunkNameExtension)
    copyToChunk(dump, chunk, 0, offsetsSeq(0))

    // Process chunks
    var low = offsetsSeq(0)
    val boundaries = new ArrayBuffer[(Long,Long)]()

    offsetsSeq.drop(1).foreach { offset =>
      if (offset - low >= chunkSize) {
        boundaries.append((low,offset))
        low = offset
      }
    }

    // Add the last chunk
    if (dump.length() - low > 0) {
      boundaries.append((low, dump.length()))
    }

    val digits = (boundaries.size + 1).toString.length

    for ((low,high) <- boundaries) {
      chunkNumber += 1
      chunk = startChunk(output, chunkNamePrefix, chunkId(chunkNumber, digits), chunkNameExtension)
      if (replace) {
        copyToChunk(dump, chunk, low, high - low)
      }
    }
  }

  private def chunkId(current: Int, digits: Int): String = {
    current.toString.reverse.padTo(digits, "0").reverse.mkString
  }

  private def startChunk(dir: File, prefix: String, chunk: String, extension: String): File = {
    new File(dir, prefix + '.' + chunk + '.' + extension)
  }

  private def copyToChunk(from: File, to: File, offset: Long, chunkSize: Long): Long = {

    err.println("Generating chunk " + to.name + " from file " + from.name)

    val fromChannel = new FileInputStream(from).getChannel
    val toChannel = new FileOutputStream(to).getChannel

    try {
      fromChannel.transferTo(offset, chunkSize, toChannel)
    } finally {
      fromChannel.close()
      toChannel.close()
      err.println("Done generating chunk " + to.name + " from file " + from.name)
    }
  }

  private def log(lines: Int, collected: Int, start: Long): Unit = {
    val nanos = System.nanoTime - start
    err.println("processed " + lines + " lines, collected " + collected + " offsets - " + prettyMillis(nanos / 1000000) + " (" +
      (nanos.toFloat/lines) + " nanos per line)")
  }
}
