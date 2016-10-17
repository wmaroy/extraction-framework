package org.dbpedia.extraction.util

import java.net._

import org.apache.commons.lang3.StringEscapeUtils
import org.dbpedia.util.text.uri.UriDecoder

object UriUtils
{
    private val knownSchemes = Set("http", "https", "ftp")

    private val knownPrefixes = knownSchemes.map(_ + "://")

    def hasKnownScheme(uri: String) : Boolean = knownPrefixes.exists(uri.startsWith(_))

    /**
     * TODO: comment
     */
    def cleanLink( uri : URI ) : Option[String] =
    {
      if (knownSchemes.contains(uri.getScheme)) Some(uri.normalize.toString) 
      else None
    }

    /**
     * Relativizes the given parent URI against a child URI.
     *
     * @param parent
     * @param child
     * @return path from parent to child
     * @throws IllegalArgumentException if parent is not a parent directory of child.
     */
    def relativize( parent : URI, child : URI ) : URI =
    {
        val path = parent.relativize(child)
        if (path eq child) throw new IllegalArgumentException("["+parent+"] is not a parent directory of ["+child+"]")
        path
    }

  def createUri(uri: String): URI ={
    // unescape all \\u escaped characters
    val input = StringEscapeUtils.unescapeJava(uri)

    // Here's the list of characters that we re-encode (see WikiUtil.iriReplacements):
    // "#%<>?[\]^`{|}

    // we re-encode backslashes and we currently can't decode Turtle, so we disallow it
    if (input.contains("\\"))
      throw new IllegalArgumentException("URI contains backslash: [" + input + "]")
    new URI(StringUtils.escape(input, StringUtils.replacements('%', "\"<>[\\]^`{|}")))
  }

  /**

    */
    def toDbpediaUri(uri: String): URI = {
      val sb = new java.lang.StringBuilder()
      val input = StringUtils.replaceChars(sb, StringEscapeUtils.unescapeJava(uri), " \u00A0\u200E\u200F\u2028\u202A\u202B\u202C\u3000", "_").toString
      val respos = input.indexOf("dbpedia.org/resource/") + 21
      var pos = 0
      if(respos > 20)
      {
        val query = input.indexOf('?')
        val fragment = input.indexOf('#')
        val prelude = input.substring(0, respos)
        val resource = encodeAndClean(
          if(query > respos)
          input.substring(respos, query)
        else if (fragment > respos)
          input.substring(respos, fragment)
        else
          input.substring(respos)
        )

        val qu = if(query > respos){
          if(fragment > query)
            "?" + encodeAndClean(input.substring(query+1, fragment))
          else
            "?" + encodeAndClean(input.substring(query+1))
        } else ""

        val frag = if(fragment > respos)
            "#" + encodeAndClean(input.substring(fragment+1))
          else ""

        new URI(prelude + resource + qu + frag)
      }
      else
        createUri(input)
    }

  /**
    * decodes (ASCII) uris and transforms them into iris with the DBpedia naming rules
    *
    * @param uri
    * @return
    */
  def uriToIri(uri: String): String = {
    val urii = toDbpediaUri(uri)
    uriToIri(urii)
  }

  /**
    * see uriToIri(uri: String)
    *
    * @param uri
    * @return
    */
  def uriToIri(uri: URI): String = {
      // re-encode URI according to our own rules
      uri.getScheme + "://" +
        uri.getAuthority +
        decode(uri.getPath)  +
        (if(uri.getQuery != null) "?" + decode(uri.getQuery) else "")+
        (if(uri.getFragment != null) "#" + decode(uri.getFragment) else "")
  }

  private def encodeAndClean(uriPart: String): String={
    var decoded = uriPart
    while(UriDecoder.decode(decoded) != decoded)
      decoded = UriDecoder.decode(decoded)
    StringUtils.escape(decoded, StringUtils.replacements('%', "<>\"#%?[\\]^`{|}"))
  }

  private def decode(uriPart: String): String={
    var decoded = uriPart
    while(UriDecoder.decode(decoded) != decoded)
      decoded = UriDecoder.decode(decoded)

    decoded.replaceAll("[<>#%\\?\\[\\\\\\]]", "_")
  }

  def encodeUriComponent(comp: String): String={
    URLEncoder.encode(comp, "UTF-8")
      .replaceAll("\\+", "%20")
      .replaceAll("\\%21", "!")
      .replaceAll("\\%27", "'")
      .replaceAll("\\%28", "(")
      .replaceAll("\\%29", ")")
      .replaceAll("\\%7E", "~")
  }
}
