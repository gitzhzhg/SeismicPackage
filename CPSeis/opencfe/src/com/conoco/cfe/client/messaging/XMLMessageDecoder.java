// XMLMessageDecoder.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;

import com.conoco.cfe.client.gui.controller.actions.XmlActionHandler;

//import com.ibm.xml.parsers.DOMParser;
import org.apache.xerces.parsers.DOMParser;

import java.io.CharArrayReader;
import java.io.IOException;

import java.util.Hashtable;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
  
/**
 * Walks the DOM tree of a document that decribes request and
 * reply messages. This class implements the 
 * <code>com.conoco.MessageDecoder</code> interface.
 * The messages passing between the client and the server 
 * are encoded in an XML document which is then parsed and read
 * by this class. This reader then invokes appropriate
 * methods on the message handler object. For the 
 * Document Type Definition (DTD) of the XML document read by this
 * reader, see <code>message.dtd</code>.
 *
 * @see org.w3c.dom
 */
public class XMLMessageDecoder implements MessageDecoder {
  /** 
   * A hashtable of action/action handler pairs, attach 
   * actions to XML tags 
   * 
   * @serial
   */
  Hashtable _xmlHandlers;
  
  /**
   * Constructs a new message decoder object.
   */
  public XMLMessageDecoder() {
  }

  /**
   * Sets the action handlers that handle server replies.
   *
   * @param h the hashtable of action handlers
   */ 
  public void setReplyActionHandlers(Hashtable h) {
    _xmlHandlers = h;
  }
  
  /**
   * Parses a message.  After the message has been parsed, this method calls
   * the appropriate methods on the state machine to send the message to the
   * appropriate state machine algorithm.
   * 
   * @param message the XML document describing the message
   */
  public void decode(String message) {
    if ( message != null ) {
      Console.logMessage(message);
      Document dom = buildDom(message);
      NodeList children = null;
      Element root = null;
      if ( dom != null ) {
        root = dom.getDocumentElement();
      } 
      else {
        return;    
      }
      if ( root != null ) {
        root.normalize();
        String rootType = root.getTagName();
        //handleMessage(rootType, root);
        children = root.getChildNodes();
      } 
      else {
        ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
        d.setType(ErrorInfoWarningDialog.ERROR);
        String[] messages = { "XMLMessageDecoder: Parsing Error; Root is null" };
        d.setMessages(messages);
        d.setSize(200,200);
        d.setVisible(true);
        return;            
      }
      if ( children != null ) {
        for ( int i = 0; i < children.getLength(); i++) {
          if ( children.item(i).getNodeType() == Node.ELEMENT_NODE ) {
            handleReply(children.item(i));  
          }
        } 
      } 
      else {
        ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
        d.setType(ErrorInfoWarningDialog.ERROR);
        String[] messages = { "XMLMessageDecoder: Parsing Error; No root children" };
        d.setMessages(messages);
        d.setSize(200,200);
        d.setVisible(true);
        return;            
      }
    } 
    else {
        ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] messages = { "XMLMessageDecoder: Message is null" };
      d.setMessages(messages);
      d.setSize(200,200);
      d.setVisible(true);
      return;    
    }
  }
  
  /**
   * Handles a single 'Reply' node. This is a protected method called 
   * by the <code>decode(java.lang.String)</code> method. 
   * 
   * @param n the XML document node that corresponds to a reply message
   */
  protected void handleReply(Node n) {
    if ( n.hasChildNodes() ) {
      NodeList children = n.getChildNodes();
      for ( int i = 0; i < children.getLength(); i++) {
        if ( children.item(i).getNodeType() == Node.ELEMENT_NODE ) {
          handleMessage(children.item(i).getNodeName(), children.item(i));  
        }
      }
    }
  }

  /**
   * Takes an XML message, identified by an action name, and performs an action
   * associated with the action name.
   *
   * @param actionName The XML tag name; identifies the action type.
   * @param node The DOM node that corresponds to the action.
   */
  protected void handleMessage(String actionName, Node node) {
    XmlActionHandler actionHandler = (XmlActionHandler) _xmlHandlers.get(actionName.toUpperCase());
    if (actionHandler != null) {
      actionHandler.performAction(node);
    }
  }

  /**
   * Builds a DOM (Document Object Model) for an XML message.
   *
   * @param message An XML message.
   * @return the document object that gives the DOM tree
   */
  protected Document buildDom(String message) {
    DOMParser parser = new DOMParser();
    try {
      CharArrayReader charArrayReader = new CharArrayReader(message.toCharArray());
      InputSource inputSource = new InputSource(charArrayReader);
      parser.parse(inputSource);
    } 
    catch (SAXException saxException) {
      return null;
    } 
    catch (IOException ioException) {
      return null;
    }
    return parser.getDocument();
  }
}
