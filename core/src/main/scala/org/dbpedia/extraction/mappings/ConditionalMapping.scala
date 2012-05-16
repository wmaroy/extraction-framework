package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.wikiparser.{Node, TemplateNode}
import org.dbpedia.extraction.destinations.Quad
import org.dbpedia.extraction.dataparser.StringParser

class ConditionalMapping( 
  val cases : List[ConditionMapping], // must be public val for statistics
  val defaultMappings : List[PropertyMapping] // must be public val for statistics
)
extends Mapping[TemplateNode]
{
    override def extract(node: TemplateNode, subjectUri : String, pageContext : PageContext) : Seq[Quad] =
    {
      for(condition <- cases)
      {
        if (condition.matches(node)) {
          val graph = condition.extract(node, subjectUri, pageContext)
          // template mapping sets instance URI
          val instanceURI = node.annotation(TemplateMapping.INSTANCE_URI_ANNOTATION)
            .getOrElse(throw new IllegalArgumentException("missing instance URI"))
            .asInstanceOf[String]
          return graph ++ defaultMappings.flatMap(_.extract(node, instanceURI, pageContext))
        }
      }
      Seq.empty
    }
}

class ConditionMapping(
  templateProperty : String,
  operator : String,
  value : String,
  val mapping : Mapping[TemplateNode] // must be public val for statistics
) 
extends Mapping[TemplateNode]
{
    /** Check if templateProperty is defined */
    require(operator == "otherwise" || templateProperty != null, "templateProperty must be defined")
    /** Check if given operator is supported */
    require(List("isSet", "equals", "contains", "otherwise").contains(operator), "Invalid operator: " + operator +". Supported operators: isSet, equals, contains, otherwise")
    /** Check if value is defined */
    require(operator == "otherwise" || operator == "isSet" || value != null, "Value must be defined")

    def extract(node : TemplateNode, subjectUri : String, pageContext : PageContext) : Seq[Quad] =
    {
       mapping.extract(node, subjectUri, pageContext)
    }

    def matches(node : TemplateNode) : Boolean =
    {
      if (operator == "otherwise") true
      else {
        val property = node.property(templateProperty).getOrElse(return false)
        val propertyText = StringParser.parse(property).getOrElse("").toLowerCase.trim

        operator match
        {
            case "isSet" => ! propertyText.isEmpty
            // FIXME: toLowerCase must use correct language locale
            case "equals" => propertyText == value.toLowerCase
            // FIXME: toLowerCase must use correct language locale
            case "contains" => propertyText.contains(value.toLowerCase)
            case _ => false
        }
      }
    }
}
