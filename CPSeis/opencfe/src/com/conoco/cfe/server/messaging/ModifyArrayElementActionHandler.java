// ModifyElementsActionHandler.java

package com.conoco.cfe.server.messaging;

import com.conoco.xml.StringArray;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * An acion handler for "ModifyElements" action.
 */
public class ModifyArrayElementActionHandler 
  implements XmlActionHandler {
  
  /**
   * Returns the keyword attribute name.
   * 
   * @return the keyword attribute name
   */
  public String getKeywordAttributeName() {
    return KEYWORD;
  }
  
  /**
   * Returns a boolean to indicate the requirement of a keyword attribute.
   * 
   * @return the boolean value for keyword attribute
   */
  public boolean isKeywordRequired() {
    return true;
  }

  /**
   * Returns the value attribute name.
   *
   * @return the value attribute name
   */
  public String getValueAttributeName() {
    return VALUE;
  }
  
  /**
   * Returns a boolean value that indicates whether a value attribute
   * is required or not.
   * 
   * @return the boolean indicating the requirement of a value attribute
   */
  public boolean isValueRequired() {
    return true;
  }

  /**
   * This method is invoked when a "ModifyArrayElement" node is 
   * encountered in an XML document.
   *
   * @param node the DOM tree node to be processed
   * @param cfeApi the wrapper class providing access to the CFE API 
   * @throws com.conoco.cfe.server.messaging.MessagingException if the keyword, 
   *  startElement or the value attributes are not found in the node                          
   */
  public void performAction(Node node, CfeApi cfeApi) 
    throws MessagingException {

    if (node != null) {
      NamedNodeMap attributes = node.getAttributes();

      int windowId;
      String keyword = null;
      String[] values = null;
      int start;
      int end;

      Node windowIdNode = attributes.getNamedItem(WINDOW_ID);

      if (windowIdNode == null) {
        windowId = 0;
      } else {
        windowId = Integer.parseInt(windowIdNode.getNodeValue());
      }

      if (isKeywordRequired()) {
        Node keywordNode = attributes.getNamedItem(getKeywordAttributeName());
        if (keywordNode == null) {
          throw (new MessagingException("Could not find '" +
                          getKeywordAttributeName() + "' attribute"));
        } else {
          keyword = keywordNode.getNodeValue();
        }
      }

      if (isValueRequired()) {
        Node valueNode = attributes.getNamedItem(getValueAttributeName());
        if (valueNode == null) {
          throw (new MessagingException("Could not find '" +
                          getValueAttributeName() + "' attribute"));
        } else {
          //values = new String[1];
          //values[0] = valueNode.getNodeValue();
          values = StringArray.parseStringArray(valueNode.getNodeValue());
        }
      }

      Node startNode = attributes.getNamedItem(START_ELEMENT);
      if (startNode == null) {
        throw (new MessagingException("Could not find '" +
                        START_ELEMENT + "' attribute"));
      } else {
        start = Integer.parseInt(startNode.getNodeValue());
      }

      Node endNode = attributes.getNamedItem(END_ELEMENT);
      if (endNode == null) {
        end = -1;
      } else {
        end = Integer.parseInt(endNode.getNodeValue());
      }

      try {
        cfeApi.setArray(windowId, node.getNodeName(), keyword, values, start, end);
      } catch (MessagingException me) {
        System.out.println("Messaging exception: " + me);
        //me.printStackTrace();
      }
    }
  }
}