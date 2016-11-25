///
/// PreferencesReader.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  6.
///  5. 09-11-2002 SMCook   Added code for WorkstationSetupHandler.
///  4. 09-24-2001 SMCook   Message statements were reporting the wrong class
///                          name (they were informational messages only).
///                         Also, exception messages were made slightly more
///                          useful by reporting what the URL is when problems
///                          occur.
///

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;

//import com.ibm.xml.parsers.DOMParser;
import org.apache.xerces.parsers.DOMParser;
//import com.ibm.xml.parsers.RevalidatingDOMParser;



import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.net.URL;
import java.net.URLConnection;
import java.net.MalformedURLException;

import java.util.Hashtable;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * A class to read the application preferences as specified in the preferences
 * file.
 * 
 * @see com.conoco.cfe.client.messaging.Preferences
 * @see preferences.dtd
 */
public class PreferencesReader {
  /**
   * Declares a variable to hold the preferences file URL.
   * 
   * @serial
   */
  protected String _prefsFileURL;
    
  /**
   * Variable for the action handlers
   * 
   * @serial
   */
  protected Hashtable _handlers;
  
  /**
   * Variable for the object that provides the input to be
   * processed
   * 
   * @serial
   */
  protected InputSource _input;
  
  /**
   * Variable for the parser that will parse the DOM tree
   * 
   * @serial
   */
  protected DOMParser _parser;
  
  /**
   * Constructs a new reader object that reads the preferences file and 
   * constructs a preferences object that can be quesried for the different
   * preferences.
   *
   * @param fileURL the string that describes the URL of the preferences file
   */
  public PreferencesReader(String fileURL) {
    createActionHandlers();
    setPrefsFileURL(fileURL);
  }
  
  /**
   * Creates the action handlers that will handle the 
   * XML nodes in the DOM tree. This is a protected method
   * called by the constructor.
   */
  protected void createActionHandlers() {
    _handlers = new Hashtable();

//Workstation handler should go first, in case later handlers are dependent
    _handlers.put("WorkstationSetup", new WorkstationSetupHandler());
    _handlers.put("BackEndLibPath",   new BackEndLibPathHandler());    
    _handlers.put("DefaultFieldFont", new DefaultFieldFontHandler());
    _handlers.put("DefaultArrayFont", new DefaultArrayFontHandler());
    _handlers.put("DefaultLabelFont", new DefaultLabelFontHandler());
    _handlers.put("HelpMode",         new HelpHandler());
    _handlers.put("LoginServletName", new LoginServletNameHandler());
    _handlers.put("ServerName",       new ServerNameHandler());
    _handlers.put("ServletName",      new ServletNameHandler());
    _handlers.put("ScriptName",       new ScriptNameHandler());
    _handlers.put("WithConsole",      new WithConsoleHandler());
    _handlers.put("XMLDocPath",       new XMLDocPathHandler());    
  }
  
  /**
   * Parses the preferences file and reads the preferences. This is a protected
   * method called by the <code>setPrefFileURL(java.lang.String)</code> method
   * of this class.
   */
  protected void getFileData() {
    URL url = null;
    try {
      url = new URL(_prefsFileURL);
      _input = new InputSource(url.openStream());
    } 
    catch (MalformedURLException en) {
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] messages =
        { "PreferencesReader: Malformed prefs file URL:", _prefsFileURL };
      d.setMessages(messages);
      d.setSize(250,200);
      d.setVisible(true);
      System.exit(1);       //SMCook added
    } 
    catch (IOException ex) {
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] messages =
        { "PreferencesReader: Cannot set up URL connection:", _prefsFileURL };
      d.setMessages(messages);
      d.setSize(250,200);
      d.setVisible(true);
      System.exit(1);       //SMCook added
    }
  }
  
  /**
   * Reads the data from the preferences file.
   */
  protected void readFileData() {
    try {
      _parser = new MacFoobarsDOMParser();
      _parser.parse(_input);
    } 
    catch (SAXParseException err) {
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] message = { "PreferencesReader: Parsing error; line " + 
        err.getLineNumber() + ", uri " + err.getSystemId() };
      d.setMessages(message);
      d.setSize(200,200);
      d.setVisible(true);
      return;
    } 
    catch (SAXException e) {
      Exception x = e.getException ();
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] message = { "PreferencesReader: Parser failed: " + 
        ((x == null) ? e : x).toString ()};
      d.setMessages(message);
      d.setSize(200,200);
      d.setVisible(true);
      return;
    }
    catch (IOException e) {
      ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
      d.setType(ErrorInfoWarningDialog.ERROR);
      String[] message = { "PreferencesReader: Parser failed: " + e.toString()};
      d.setMessages(message);
      d.setSize(200,200);
      d.setVisible(true);
      return;
    } 
    Document doc = _parser.getDocument();
    if (doc != null) {
      Element rootNode = doc.getDocumentElement();
      rootNode.normalize();
      processNode(rootNode);
    }
  }
  
  /**
   * Sets the preferences file to be read by this reader.
   * 
   * @param prefsFile the URL of the preferences file as a string
   */
  public void setPrefsFileURL(String prefsFile) {
    _prefsFileURL = prefsFile;
    getFileData();  
    readFileData();
  }
  
  /**
   * Returns the URL of the preferences file that will be read by this reader.
   * 
   * @return the URL of the preferences file as a string
   */
  public String getPrefsFileURL() {
    return _prefsFileURL;
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
        if ( (current.getNodeType() != Node.COMMENT_NODE) 
          && (current.getNodeType() == Node.ELEMENT_NODE) ) {
          PrefsNodeHandler handler = (PrefsNodeHandler) _handlers.get(current.getNodeName());
          if (handler != null) {
            handler.performAction(current);
             processNode(current);
          }
          else {
               if (!(current.getNodeName() == "#text")) {
               // Error no handler for Node
                 Console.logMessage("PreferencesReader: Unknown XML Node: No handler for name=[" +
                                    current.getNodeName() + "] value= [" + current.getNodeValue()+"]");
               }
          }
        }
        current = current.getNextSibling();
      }
    }
  }  
  
  /**
   * Inner class to implement a new revalidating DOM parser that
   * skips unnecessary text nodes.
   */
  //class MacFoobarsDOMParser extends RevalidatingDOMParser {
    class MacFoobarsDOMParser extends DOMParser {
    /**
     * Need to override method, to get rid of the unnecessary text nodes
     * between elements
     */
    public void ignorableWhitespace(int i, boolean b) {
        Console.logMessage("RevalidatingDOMParser called from PreferencesReader.java 253.");    }
  }
}
