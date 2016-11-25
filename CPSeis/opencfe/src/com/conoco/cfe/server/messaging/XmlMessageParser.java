// XMLMessageParser.java

package com.conoco.cfe.server.messaging;

//import com.ibm.xml.parsers.DOMParser;
import org.apache.xerces.parsers.DOMParser;

import java.io.CharArrayReader;
import java.io.IOException;

import java.util.Hashtable;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * This is the interface used to parse XML messages into CFE API calls.
 *
 * @see com.conoco.cfe.server.messaging.CfeApi
 */
public class XmlMessageParser 
  implements MessageParser {

  /** 
   * A hashtable of action/action handler pairs, attach actions to XML tags
   *
   * @serial
   */
  Hashtable _xmlHandlers;
  
  /** 
   * The CFE API, providing native access to server functions 
   * 
   * @serial
   */
  CfeApi _cfeApi;

  /**
   * Constructs a new XML message parser.
   */
  public XmlMessageParser() {
    _xmlHandlers = new Hashtable();
  }

  /**
   * Retrieves the CFE API that this message parser sends messages to after 
   * each message has been parsed.
   *
   * @return The CFE API that this message parser sends messages to after 
     *         each message has been parsed
   */
  public CfeApi getCfeApi() {
    return _cfeApi;
  }

  /**
   * Retrieves the CFE API that this message parser sends messages to after 
   * each message has been parsed.
   *
   * @param cfeApi The CFE API that this message parser sends messages to after 
   *             each message has been parsed.
   */
  public void setCfeApi(CfeApi cfeApi) {
    _cfeApi = cfeApi;
  }
  
  /**
   * Skips the text nodes in the DOM tree and returns the first
   * element node for the specified DOM tree node.
   * 
   * @param node the node to be processed
   * @return the first element node under the specified node
   * @throws com.conoco.cfe.server.messaging.MessagingException if 
   * the document cannot be decoded
   */
  private Node findFirstElement(Node node) throws MessagingException {
    while (node.getNodeType() != Document.ELEMENT_NODE) {
      node = node.getNextSibling();
        
      if (node == null) {
        throw (new MessagingException("Document not understood."));
      }
    }
    
    return node;
  }

  /**
   * Parses a message.  After the message has been parsed, this method calls
   * the appropriate methods on the CFE API to send the message to the
   * appropriate CFE algorithm.
   * 
   * @param message the message to be decoded
   * @throws com.conoco.cfe.server.messaging.MessagingException if 
   * an invalid element is encountered 
   */
  public void parseMessage(String message)
    throws MessagingException {

    Document dom = buildDom(message);
    Element root = dom.getDocumentElement();
    root.normalize();
    String rootType = root.getTagName();
    
    if (rootType.equals("Message") || rootType.equals("MESSAGE")) {
      Node request = findFirstElement(root.getFirstChild());
      Node realRequest;
      
      if (request.getNodeName().equals("Request") || request.getNodeName().equals("REQUEST")) {
        realRequest = findFirstElement(request.getFirstChild());
      }
      else {
        throw(new MessagingException("Invalid sub-message tag " + request.getNodeName()));
      }
      
      handleMessage(realRequest.getNodeName(), realRequest);
      
    }
    else {
      throw (new MessagingException("Invalid starting tag " + rootType));
    }
  }

  /**
   * Takes an XML message, identified by an action anme, and performs an action
   * associated with the action name.
   *
   * @param actionName The XML tag name; identifies the action type.
   * @param node The DOM node that corresponds to the action.
   * @throws com.conoco.cfe.server.messaging.MessagingException if 
   * a handler for the node is not found
   */
  protected void handleMessage(String actionName, Node node) throws MessagingException {
    XmlActionHandler actionHandler = (XmlActionHandler)_xmlHandlers.get(actionName.toUpperCase());
    if (actionHandler != null) {
      actionHandler.performAction(node, _cfeApi);
    } else {
      throw(new MessagingException("No handler for node " + actionName));
    }
  }

  /**
   * Registers an XML action (an XML tag and action handler pair) in a hashtable of
   * XML tags.
   *
   * @param actionName Then XML tag name corresponding to a message action.
   * @param actionHandler An action handler for an XML tag.
   */
  protected void registerXmlAction(String actionName, XmlActionHandler actionHandler) {
    _xmlHandlers.put(actionName.toUpperCase(), actionHandler);
  }

  /**
   * Builds a DOM tree for an XML message.
   *
   * @param message An XML message.
   */
  protected Document buildDom(String message) {
    DOMParser parser = new DOMParser();

    try {
      CharArrayReader charArrayReader = new CharArrayReader(message.toCharArray());
      InputSource inputSource = new InputSource(charArrayReader);
      parser.parse(inputSource);
    } catch (SAXException saxException) {
      System.err.println("Error while parsing file: "+saxException.getMessage());
      saxException.printStackTrace();
      return null;
    } catch (IOException ioException) {
      System.err.println("Error while parsing file: "+ioException.getMessage());
      ioException.printStackTrace();
      return null;
    }

    return parser.getDocument();
  }
}