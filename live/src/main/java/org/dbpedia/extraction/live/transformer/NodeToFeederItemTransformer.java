package org.dbpedia.extraction.live.transformer;

import org.apache.commons.collections15.Transformer;
import org.apache.log4j.Logger;
import org.dbpedia.extraction.live.feeder.FeederItem;
import org.dbpedia.extraction.live.util.DBPediaXPathUtil;
import org.dbpedia.extraction.live.util.ExceptionUtil;
import org.dbpedia.extraction.live.util.XMLUtil;
import org.dbpedia.extraction.live.util.XPathUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

/**
 * Created with IntelliJ IDEA.
 * User: Dimitris Kontokostas
 * Date: 11/19/12
 * Time: 8:38 PM
 * An iterator which takes an iterator of nodes and creates converts them to a FeederItem.
 */
public class NodeToFeederItemTransformer implements Transformer<Node, FeederItem>
{
	private static Logger logger = Logger.getLogger(NodeToFeederItemTransformer.class);

	public FeederItem transform(Node node)
	{
        Document document = null;
		try {
			if (node == null)
				return null;

            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            document = dbf.newDocumentBuilder().newDocument();
            Node clone = document.importNode(node, true);
            document.appendChild(clone);

            String tmpID = XPathUtil.evalToString(document, DBPediaXPathUtil.getOAIIdentifierExpr());
            long nodeItemID = tmpID.equals("")? 0 : Long.parseLong(tmpID.substring(tmpID.lastIndexOf(":")+1));
            String nodeItemName =XPathUtil.evalToString(document, DBPediaXPathUtil.getTitleExpr())                      ;
            String nodeModificationDate = XPathUtil.evalToString(document, DBPediaXPathUtil.getTimestampExpr());
            boolean nodeDeleted = XPathUtil.evalToString(document, DBPediaXPathUtil.getOAIIsRecordDeletedExpr()).equals("deleted");
            // TODO add this for debugging, remove it later
            String xml = XMLUtil.toString(document);

            return new FeederItem(nodeItemID, nodeItemName, nodeModificationDate, nodeDeleted, xml);
		}
		catch (Exception e) {
			logger.warn(ExceptionUtil.toString(e));
		}

		return null;
	}
}
