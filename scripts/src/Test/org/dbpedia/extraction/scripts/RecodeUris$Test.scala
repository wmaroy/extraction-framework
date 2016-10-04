package org.dbpedia.extraction.scripts

import org.dbpedia.extraction.util.UriUtils
import org.scalatest._

/**
  * Created by Chile on 10/4/2016.
  */
class RecodeUris$Test extends FunSuite {

  test("uriToIri") {
    info(UriUtils.uriToIri("http://dbpedia.org/resource/Robert_Sch\\u00F6ller"))
    info(UriUtils.uriToIri("http://dbpedia.org/resource/Robert_Schöller"))
    info(UriUtils.uriToIri("http://dbpedia.org/resource/Mírzá_`Abbás_Núrí"))
    info(UriUtils.uriToIri("http://dbpedia.org/resource/Robert_Sch\\u00F6ller?oldid=702093022#absolute-line=13&template=Infobox_scientist&property=field&mapped="))
    info(UriUtils.uriToIri("http://de.dbpedia.org/resource/Robert_Sch%C3%B6ller?oldid=702093022#absolute-line=13&template=Infobox_scienti%C3%B6st&property=field&mapped="))
    info(UriUtils.uriToIri("http://dbpedia.org/resource/Robert_Sch%C3%B6ller?oldid=702093022#absolute-line=13&template=Infobox_scienti%C3%B6st&property=field&mapped="))
  }
}
