// AppGUIDecoder.java

package com.conoco.cfe.client.gui;

import com.conoco.cfe.client.application.Console;

import java.io.IOException;

import java.net.URL;
import java.net.MalformedURLException;

import java.util.Hashtable;

//import com.ibm.xml.parsers.DOMParser;
import org.apache.xerces.parsers.DOMParser;
//import com.ibm.xml.parsers.RevalidatingDOMParser;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;


/**
 * Walks the DOM tree of a document that decribes the layout of GUI
 * components and builds these components. The building is carried out
 * by ActionHandler objects created by the GUIBuilder object. 
 *
 * @see com.conoco.client.gui.GUIBuilder
 * @see org.w3c.dom
 * @see gui.dtd
 */
public class AppGUIDecoder implements GUIDecoder {
  
  /**
   * Variable for the action handlers that handle
   * the document nodes in the XML document that
   * describes the GUI layout
   * 
   * @serial
   */
  private Hashtable _actionHandlers;
  
  /**
   * Variable for the parser that will parse the XML document
   * 
   * @serial
   */
  private DOMParser _parser;
  
  /**
   * Constructs a new tree walker object.
   */
  public AppGUIDecoder() {
  }

  /**
   * Sets the file name of the document that will be walked by 
   * this tree walker.
   *
   * @param   file name of the document which will be
   *       walked by this tree walker
   */
  public void setFilename(String name) {
    InputSource input = new InputSource(name);
    process(input);
  }
   
  /**
   * Sets the URL of the document that will be walked by this 
   * tree walker.
   * 
   * @param   URL of the document which will be
   *       walked by this tree walker
   */
  public void setURL(String url_string) {
    try {
      URL url = new URL(url_string);
      
      InputSource input = new InputSource(url.openStream());
      process(input);
    }
    catch (MalformedURLException e) {
      // URL was incorrect
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] message = { "AppGUIDecoder: Malformed URL: " + url_string };
      d.setMessages(message);
      d.setSize(200,200);
      d.setVisible(true);
    }

    catch (IOException e) {
      // Not possible to connect etc
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] message = { "AppGUIDecoder: Unable to connect" };
      d.setMessages(message);
      d.setSize(200,200);
      d.setVisible(true);
    }
  }
  
  /**
   * Sets the action handlers on this decoder.
   * 
   * @param action_handlers   the action handlers that handle
   *               the GUI document nodes
   */
  public void setActionHandlers(Hashtable action_handlers) {
    _actionHandlers = action_handlers;
  }
  
  /**
   * Processes the DOM tree from the specified source.
   * 
   * @param inputSource the source that supplies the XML document tree
   */
  private void process(InputSource inputSource) {
    try {
      _parser = new MacFoobarsDOMParser();
      /* _parser = new DomParser(); */
      _parser.parse(inputSource);
    }    
    catch (SAXParseException err) {
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] message = { "AppGUIDecoder: Parsing error; line " + 
        err.getLineNumber() + ", uri " + err.getSystemId()};
      d.setMessages(message);
      d.setSize(200,200);
      d.setVisible(true);
      return;
    } 
    
    catch (SAXException e) {
      Exception x = e.getException ();
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] message = { "AppGUIDecoder: Parser failed: " + 
        ((x == null) ? e : x).toString ()};
      d.setMessages(message);
      d.setSize(200,200);
      d.setVisible(true);
      return;
    }

    catch (IOException e) {
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] message = { "AppGUIDecoder: Parser failed: " + e.toString()};
      d.setMessages(message);
      d.setSize(200,200);
      d.setVisible(true);
      return;
    } 
    
    // validation could get here
    Document doc = _parser.getDocument();
    
    if (doc != null) {
      Element rootNode = doc.getDocumentElement();
      rootNode.normalize();
      processNode(rootNode);
    }
  }
  
  /**
   * Processes the root node of the document. This method is 
   * called by the <code>setFileName(String)</code> and 
   * <code>setURL(URL)</code> methods.
   */
  public void processNode(Node node) {  
    if ( node != null ) {
      Node current = node.getFirstChild();
        
        while (current != null) {
          if (current.getNodeType() != Node.COMMENT_NODE) {
            ActionHandler handler = (ActionHandler) _actionHandlers.get(current.getNodeName());
            
            if (handler != null) {
              handler.performAction(current);
              if (!handler.isHandlesSubTree()) {
                 processNode(current);
               }
               handler.subTreeComplete();
            }
            else {
               if (!(current.getNodeName() == "#text")) {
               // Error no handler for Node
                 Console.logMessage("AppGUIDecoder: Unknown XML Node: No handler for name=[" + 
                                    current.getNodeName() + "] value= [" + current.getNodeValue()+"]");
               }
            }
          }
          current = current.getNextSibling();
        }
      }
    }  
  
  /**
   * Inner class that extends the revaildating DOM parser
   * to ignore unnecessary text nodes.
   */  
  //class MacFoobarsDOMParser extends RevalidatingDOMParser {
  class MacFoobarsDOMParser extends DOMParser {
    /**
     * Need to override method, to get rid of the unnecessary text nodes
     * between elements
     */
    public void ignorableWhitespace(int i, boolean b) {
      Console.logMessage("RevalidatingDOMParser called from PreferencesReader.java 253.");
    }
  }
}
