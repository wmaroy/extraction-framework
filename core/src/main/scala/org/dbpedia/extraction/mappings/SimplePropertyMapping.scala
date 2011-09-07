package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.ontology.datatypes._
import org.dbpedia.extraction.dataparser._
import org.dbpedia.extraction.destinations.{Graph, DBpediaDatasets, Quad, IriRef, TypedLiteral}
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.ontology._
import java.lang.IllegalArgumentException
import org.dbpedia.extraction.wikiparser.TemplateNode
import org.dbpedia.extraction.ontology.{OntologyDatatypeProperty, OntologyNamespaces, OntologyClass, OntologyProperty}

class SimplePropertyMapping( val templateProperty : String, //TODO IntermediaNodeMapping and CreateMappingStats requires this to be public. Is there a better way?
                             ontologyProperty : OntologyProperty,
                             unit : Datatype,
                             private var language : Language,
                             factor : Double,
                             context : {
                                 def ontology : Ontology
                                 def redirects : Redirects  // redirects required by DateTimeParser and UnitValueParser
                                 def language : Language } ) extends PropertyMapping
{
    if(language == null) language = context.language

    validate()

    /**
     * Validates the mapping
     */
    private def validate()
    {
        ontologyProperty match
        {
            case datatypeProperty : OntologyDatatypeProperty =>
            {
                //Check if unit is compatible to the range of the ontology property
                (unit, datatypeProperty.range) match
                {
                    case (dt1 : UnitDatatype, dt2 : UnitDatatype) => require(dt1.dimension == dt2.dimension,
                        "Unit must conform to the dimension of the range of the ontology property")

                    case (dt1 : UnitDatatype, dt2 : DimensionDatatype) => require(dt1.dimension == dt2,
                        "Unit must conform to the dimension of the range of the ontology property")

                    case (dt1 : DimensionDatatype, dt2 : UnitDatatype) => require(dt1 == dt2.dimension,
                        "The dimension of unit must match the range of the ontology property")

                    case (dt1 : DimensionDatatype, dt2 : DimensionDatatype) => require(dt1 == dt2,
                        "Unit must match the range of the ontology property")

                    case _ if unit != null => require(unit == ontologyProperty.range, "Unit must be compatible to the range of the ontology property")
                    case _ =>
                }
            }
            case _ =>
        }

        if(language != context.language)
        {
            require(ontologyProperty.isInstanceOf[OntologyDatatypeProperty],
                "Language can only be specified for datatype properties")

            require(ontologyProperty.range.uri == "http://www.w3.org/2001/XMLSchema#string",
                "Language can only be specified for string datatype properties")
        }
    }
    
    private val parser : DataParser = ontologyProperty.range match
    {
        //TODO
        case c : OntologyClass if ontologyProperty.name != "foaf:homepage" => new ObjectParser(context)
        case c : OntologyClass =>
        {
            checkMultiplicationFactor("foaf:homepage")
            new LinkParser()
        }
        case dt : UnitDatatype => new UnitValueParser(context, if(unit != null) unit else dt, multiplicationFactor = factor)
        case dt : DimensionDatatype => new UnitValueParser(context, if(unit != null) unit else dt, multiplicationFactor = factor)
        case dt : EnumerationDatatype =>
        {
            checkMultiplicationFactor("EnumerationDatatype")
            new EnumerationParser(dt)
        }
        case dt : Datatype => dt.name match
        {
            case "xsd:integer" => new IntegerParser(context, multiplicationFactor = factor)
            case "xsd:positiveInteger"    => new IntegerParser(context, multiplicationFactor = factor, validRange = (i => i > 0))
            case "xsd:nonNegativeInteger" => new IntegerParser(context, multiplicationFactor = factor, validRange = (i => i >=0))
            case "xsd:nonPositiveInteger" => new IntegerParser(context, multiplicationFactor = factor, validRange = (i => i <=0))
            case "xsd:negativeInteger"    => new IntegerParser(context, multiplicationFactor = factor, validRange = (i => i < 0))
            case "xsd:double" => new DoubleParser(context, multiplicationFactor = factor)
            case "xsd:float" => new DoubleParser(context, multiplicationFactor = factor)
            case "xsd:string" =>
            {
                checkMultiplicationFactor("xsd:string")
                StringParser
            }
            case "xsd:anyURI" =>
            {
                checkMultiplicationFactor("xsd:anyURI")
                new LinkParser(false)
            }
            case "xsd:date" =>
            {
                checkMultiplicationFactor("xsd:date")
                new DateTimeParser(context, dt)
            }
            case "xsd:gYear" =>
            {
                checkMultiplicationFactor("xsd:gYear")
                new DateTimeParser(context, dt)
            }
            case "xsd:gYearMonth" =>
            {
                checkMultiplicationFactor("xsd:gYearMonth")
                new DateTimeParser(context, dt)
            }
            case "xsd:gMonthDay" =>
            {
                checkMultiplicationFactor("xsd:gMonthDay")
                new DateTimeParser(context, dt)
            }
            case "xsd:boolean" =>
            {
                checkMultiplicationFactor("xsd:boolean")
                BooleanParser
            }
            case name => throw new IllegalArgumentException("Not implemented range " + name + " of property " + ontologyProperty)
        }
        case dt => throw new IllegalArgumentException("Property " + ontologyProperty + " does have invalid range " + dt)
    }

    private def checkMultiplicationFactor(datatypeName : String)
    {
        if(factor != 1)
        {
            throw new IllegalArgumentException("multiplication factor cannot be specified for " + datatypeName)
        }
    }
    
    override def extract(node : TemplateNode, subjectUri : String, pageContext : PageContext) : Graph =
    {
        var graph = new Graph()

        for(propertyNode <- node.property(templateProperty) if propertyNode.children.size > 0)
        {
            for( parseResult <- parser.parsePropertyNode(propertyNode, !ontologyProperty.isFunctional) )
            {
                val g = parseResult match
                {
                    case (value : Double, unit : UnitDatatype) => writeUnitValue(node, value, unit, subjectUri, propertyNode.sourceUri)
                    case value => writeValue(value, subjectUri, propertyNode.sourceUri)
                }
                graph = graph.merge(g)
            }
        }
        
        graph
    }

    private def writeUnitValue(node : TemplateNode, value : Double, unit : UnitDatatype, subjectUri : String, sourceUri : String) : Graph =
    {
        //TODO better handling of inconvertible units
        if(unit.isInstanceOf[InconvertibleUnitDatatype])
        {
            val quad = new Quad(DBpediaDatasets.OntologyProperties, new IriRef(subjectUri), new IriRef(ontologyProperty), new TypedLiteral(value.toString, unit), new IriRef(sourceUri))
            return new Graph(quad)
        }

        //Write generic property
        val stdValue = unit.toStandardUnit(value)
        val quad = new Quad(DBpediaDatasets.OntologyProperties, new IriRef(subjectUri), new IriRef(ontologyProperty), new TypedLiteral(stdValue.toString, new Datatype("xsd:double")), new IriRef(sourceUri))
        var graph = new Graph(quad)

        //Write specific properties
        for(classAnnotation <- node.annotation(TemplateMapping.CLASS_ANNOTATION);
            currentClass <- classAnnotation.asInstanceOf[List[OntologyClass]])
        {
            for(specificPropertyUnit <- context.ontology.specializations.get((currentClass, ontologyProperty)))
            {
                 val outputValue = specificPropertyUnit.fromStandardUnit(stdValue)
                 val propertyUri = OntologyNamespaces.DBPEDIA_SPECIFICPROPERTY_NAMESPACE + currentClass.name + "/" + ontologyProperty.name
                 val quad = new Quad(DBpediaDatasets.SpecificProperties, new IriRef(subjectUri),
                                     new IriRef(propertyUri), new TypedLiteral(outputValue.toString, specificPropertyUnit), new IriRef(sourceUri))
                 graph = graph.merge(new Graph(quad))
            }
        }

        graph
    }

    private def writeValue(value : Any, subjectUri : String, sourceUri : String) : Graph =
    {
        val datatype = if(ontologyProperty.range.isInstanceOf[Datatype]) ontologyProperty.range.asInstanceOf[Datatype] else null

        val quad = new Quad(DBpediaDatasets.OntologyProperties, new IriRef(subjectUri),
                             new IriRef(ontologyProperty), new TypedLiteral(value.toString, datatype), new IriRef(sourceUri))

        new Graph(quad)
    }
}
