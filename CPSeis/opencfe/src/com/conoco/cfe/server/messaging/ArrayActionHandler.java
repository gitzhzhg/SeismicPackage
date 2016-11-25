// ArrayActionHandler.java

package com.conoco.cfe.server.messaging;

import com.conoco.xml.StringArray;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;


/**
 * An action handler which gets invoked when client messages
 * related to arrays are encountered by the message parser.
 */
public class ArrayActionHandler 
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
   * Returns a boolean variable that indicates whether keyword
   * is required or not.
   * 
   * @return the boolean indicating the requirement of keyword attribute
   */
  public boolean isKeywordRequired() {
    return true;
  }

  /**
   * Returns the value attribute name. Since this is an array, 
   * it returns "elements" as the value.
   * 
   * @return the value attribute name
   */
  public String getValueAttributeName() {
    return ELEMENTS;
  }
  
  /**
   * Returns a boolean variable that indicates whether a value 
   * attribute is required.
   * 
   * @return the boolean that indicates whether a value attribute is required
   */
  public boolean isValueRequired() {
    return true;
  }

  /**
   * This method is invoked by the message parser.
   * 
   * @param node the DOM document node that is to be processed
   * @param cfeApi the wrapper class that provides access to the CFE API   
   * @throws com.conoco.cfe.server.messaging.MessagingException if the keyword
   * attribute is not found                         
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