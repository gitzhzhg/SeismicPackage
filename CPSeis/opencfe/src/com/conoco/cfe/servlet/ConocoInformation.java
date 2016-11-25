// ConocoInformation.java

package com.conoco.cfe.servlet;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;

//import com.ibm.xml.parsers.DOMParser;
import org.apache.xerces.parsers.DOMParser;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import org.w3c.dom.CharacterData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * Initializes the servlet startup variables. This class
 * is used by the startup servlet to initialize some
 * startup variables. 
 */
public final class ConocoInformation {
  
  /**
   * Variable for the XML file name that stores the startup 
   * information
   * 
   * @serial
   */
  private final static String parameterFile = "Conoco.xml";
  
  /*
   * These variables arent read in from the file because
   * if we change the code to use others we'll need to reconfigure
   * this anyway.
   */
   
  /**
   * Variable for the server name
   * 
   * @serial
   */
  public static final String POST_VARIABLE_SERVER  = "serverName";
  
  /**
   * Variable for the script name
   * 
   * @serial
   */
    public static final String POST_VARIABLE_SCRIPT  = "scriptName";
    
  /**
   * Variable for client message
   * 
   * @serial
   */
    public static final String POST_VARIABLE_MESSAGE  = "clientMessage";
        
  /**
   * Variable for the user
   * 
   * @serial
   */
    public static final String POST_VARIABLE_USER = "user";
    
  /**
   * Variable for the password
   * 
   * @serial
   */
    public static final String POST_VARIABLE_PASSWORD = "password";
  

  /**
   * Location of scripts directory
   */
    public static String SCRIPT_BASE_DIR;

  /**
   * Location of message.dtd file as a URL
   */
    public static String MESSAGE_DTD;
  
  
  /**
   * This methods reads the Conoco.xml file to obtain the SCRIPT_BASE_DIR
   * and the message dtd file. 
   * 
   * @param path the path of the parameters file
   */
  public static final void load(String path) {
    DOMParser parser = null;
    
    try {
      InputSource xmlDocument = new InputSource(path + File.separator + parameterFile);
      parser = new DOMParser();
      
      parser.parse(xmlDocument);

    } catch (Exception e) { 
      System.out.println("Bad karma: " + e);
    }
    
    Document  doc;
      Node current;
    Element rootNode;
      
      doc = parser.getDocument();
      
      rootNode = doc.getDocumentElement();
      rootNode.normalize();
        
    if (rootNode.hasChildNodes()) {
      current = rootNode.getFirstChild();
        
      do {
        if (current.getNodeType() == Document.ELEMENT_NODE) {
          processNode(current);
        }
        current = current.getNextSibling();
      }
      while (current != null);
    }
  }
    
    /**
     * Processes a given node.
     * 
     * @param node the node to be processed
     */  
  private static void processNode(Node node) {
    Node child;
    CharacterData cdata;
    
    child = node.getFirstChild();
    
    if (child instanceof CharacterData) {
      String value;
      
      cdata = (CharacterData) child;
      value = cdata.getData();
      
      if (node.getNodeName().equals("ScriptBaseDir")) {
        SCRIPT_BASE_DIR = value;
      }
      else
      if (node.getNodeName().equals("MessageDtd")) {
        MESSAGE_DTD = value;
      }
    }
  }
      
  // for testing
  public static void main(String [] args) {
    ConocoInformation.load("/usr/local/apache/jserv/parameters");
    System.out.println("SCRIPT_BASE_DIR = " + ConocoInformation.SCRIPT_BASE_DIR);
    System.out.println("MESSAGE_DTD = " + ConocoInformation.MESSAGE_DTD);
  }
}