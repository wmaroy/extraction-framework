package org.dbpedia.extraction.ontology.datatypes

import org.dbpedia.extraction.ontology.{OntologyType, OntologyNamespaces}
import org.dbpedia.extraction.util.Language

/**
 * Base class of all data types.
 *
 * @param name The name of this datatype e.g. xsd:float
 * @param labels The labels of this datatype. Map: LanguageCode -> Label
 * @param comments Comments describing this datatype. Map: LanguageCode -> Comment
 */
class Datatype(name : String, labels : Map[Language, String], comments : Map[Language, String]) extends OntologyType(name, labels, comments)
{
    /**
     * Constructs a datatype when no label is explicitly defined.
     *
     * @param name The name of this datatype
     */
    def this(name : String) = this(name, Map(Language.Default -> name), Map())

    /**
     * The URI of this datatype
     */
    override val uri = OntologyNamespaces.getUri(name, OntologyNamespaces.DBPEDIA_DATATYPE_NAMESPACE)

    lazy val isExternalProperty = !uri.startsWith(OntologyNamespaces.DBPEDIA_DATATYPE_NAMESPACE)
}
