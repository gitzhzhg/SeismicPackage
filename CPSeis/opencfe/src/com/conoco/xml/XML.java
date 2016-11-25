// XML.java

package com.conoco.xml;

import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.io.IOException;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * Implementation of a general XML document node.
 */
public class XML implements XMLNode {
  
  /**
   * Variable to store the elements in this node
   * 
   * @serial
   */
  protected Vector _elements;
  
  /**
   * Variable for storing the attributes of this node
   * 
   * @serial
   */
  protected Hashtable _attributes;
  
  /**
   * Variable for the filter
   * 
   * @serial
   */
  protected Filter    _filter;
  
  /**
   * Variable for the node name
   * 
   * @serial
   */
  protected String    _nodeName;
  
  /**
   * A boolean that is set to true if a trailing slash
   * is required; false otherwise
   * 
   * @serial
   */
  protected boolean  _needsTrailingSlash = true;
  
  /**
   * A boolean that is set to true if a return after element
   * is required; false otherwise
   * 
   * @serial
   */
  protected boolean   _needsReturnAfterElement = true;
  
  /**
   * Constructs a new XML document node.
   * 
   * @param nodeName the name of the node
   * @param filter the filter to be used
   */
  public XML(String nodeName, Filter filter) {
    _attributes = new Hashtable();
    _elements   = new Vector();
    _filter     = filter;
    _nodeName   = nodeName;
  }
  
  /**
   * Constructs a new XML document node.
   * 
   * @param nodeName the name of the node 
   */
  public XML(String nodeName) {
    this(nodeName, new CharacterFilter());
  }
  
  /**
   * Returns the node name.
   * 
   * @return the node name
   */
  public String getElementName() {
    return _nodeName;
  }
  
  /**
   * Sets the element name.
   *
   * @param nodeName the desired element name
   */
  public void setElementName(String nodeName) {
    _nodeName = nodeName;
  }
  
  /**
   * Adds an element to this node.
   * 
   * @param node the node to be added
   */
  public void addElement(XMLNode node) {
    _elements.addElement(node);
  }
  
  /**
   * Inserts an element a specified position.
   * 
   * @param node the node to be inserted
   * @param index the position where the node is to be inserted
   */
  public void insertElementAt(XMLNode node, int index) {
    _elements.insertElementAt(node, index);
  }
  
  /**
   * Removes the specified node.
   * 
   * @param node the node to be removed
   */
  public void removeElement(XMLNode node) {
    _elements.removeElement(node);
  }
  
  /**
   * Removes all the elements from this node.
   */
  public void removeAllElements() {
    _elements.removeAllElements();
  }    
  
  /**
   * Returns the number of elements in this node.
   * 
   * @return the element count
   */  
  public int getElementCount() {
    return _elements.size();
  }
  
  /**
   * Returns the element at the specified position.
   * 
   * @param index the position at which the element is desired
   * @return the element at the specified position
   */
  public XMLNode getElementAt(int index) {
    return (XMLNode) _elements.elementAt(index);
  }  
  
  /**
   * Returns the number of attributes this element has.
   * 
   * @return the attribute count
   */
  public int getAttributeCount() {
    return _attributes.size();
  }
  
  /**
   * Returns the attribute keys.
   * 
   * @return the attribute keys as a <code>java.util.Enumeration</code>
   */
  public Enumeration getAttributeKeys() {
    return _attributes.keys();
  }
  
  /**
   * Returns an attribute corresponding to a key.
   * 
   * @param key the key of the desired attribute
   * @return the attribute as a <code>java.lang.String</code>
   */
  public String getAttribute(String key) {
    return (String) _attributes.get(key);
  }
    
  /**
   * Adds an attribute to this element.
   * 
   * @param name the attribute name
   * @param value the attribute value
   */  
  public void addAttribute(String name, String value) {
    _attributes.put(name, value);
  }
  
  /**
   * Sets the boolean variable for the trailing slash.
   *
   * @param trailing the boolean variable for the trailing slash
   */
  public void setTrailingSlash(boolean trailing) {
    _needsTrailingSlash = trailing;
  }
  
  /**
   * Returns the boolean variable that indicates whether a trailing
   * slash is needed or not.
   *
   * @return the boolean value for the trailing slash
   */
  public boolean isTrailingSlash() {
    return _needsTrailingSlash;
  }
  
  /**
   * Sets the boolean variable for the return after element.
   *
   * @param trailing the boolean variable for the return after element   
   */  
  public void setNeedsReturn(boolean trailing) {
    _needsReturnAfterElement = trailing;
  }
  
  /**
   * Returns the boolean variable that indicates whether a return 
   * after element is needed or not.
   *
   * @return the boolean value for the return after element
   */
  public boolean isNeedsReturn() {
    return _needsReturnAfterElement;
  }

  /**
   * Writes the first node to the writer.
   * 
   * @param writer the writer to which the node writes
   */
  protected void writeStartNode(PrintWriter writer) {
    writer.print("<");
    writer.print(_nodeName);
    
    writeAttributes(writer);
    
    if (_elements.size() == 0) {
      if (_needsTrailingSlash) {
        writer.print('/');
      }
    }
    
    if (_needsReturnAfterElement) {
      writer.println('>');
    }
    else {
      writer.print('>');
    }
  }
  
  /**
   * Writes the last node to the writer.
   * 
   * @param writer the writer to which the node writes
   */
  protected void writeEndNode(PrintWriter writer) {
    writer.print('<');
    
    if (_needsTrailingSlash) {
      writer.print('/');
    }
    writer.print(_nodeName);
    
    writer.println('>');
  }
  
  /**
   * Writes the attributes to the writer.
   *
   * @param writer the writer to which the node writes
   */
  protected void writeAttributes(PrintWriter writer) {
    Enumeration keys = _attributes.keys();
    while (keys.hasMoreElements()) {
      String key   = (String) keys.nextElement();
      String value = (String) _attributes.get(key);
      
      writer.print(' ');
      writer.print(key);
      writer.print('=');
      writer.print('"');
      writer.print(value);
      writer.print('"');
    }
  }
  
  /**
   * Writes the elements to a writer.
   * 
   * @param writer the writer to which the node writes
   */
  protected void writeElements(PrintWriter writer) {
    for (int i=0; i < _elements.size(); i++) {
      XMLNode element = (XMLNode) _elements.elementAt(i);
      element.output(writer);
    }
  }
  
  /**
   * Outputs the information in this node,
   * 
   * @param writer   the writer to which the information
   *           is to be sent to
   */
  public void output(PrintWriter writer) {
    writeStartNode(writer);
    
    if (_elements.size() != 0) {
      writeElements(writer);
      writeEndNode(writer);
    }
  }
  
  /**
   * Returns a string representation of this element. 
   * This method is typically invoked after all the elements 
   * with the desired attributes have been added to the 
   * this element. This method returns the XML for this
   * element in the form of a string.
   * 
   * @return the string representing the XML for this element
   */
  public String toString() {
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    PrintWriter pWriter = new PrintWriter(bos);
    
    output(pWriter);
    
    pWriter.flush();
    
    try {
      bos.close();
    }
    catch (IOException e) { }
    
    return bos.toString();
  }
  
  /* for testing */
  public static void main(String [] args) {
    XML rootNode = new XML("Error");
    rootNode.addElement(new TextNode("Hello World"));
    System.out.println(rootNode.toString());
  }
}
    
    
    