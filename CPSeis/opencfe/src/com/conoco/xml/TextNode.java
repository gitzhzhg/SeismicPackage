// TextNode.java

package com.conoco.xml;

import java.io.PrintWriter;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * An XML document node for handling text.
 */
public class TextNode implements XMLNode {
  
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
   * Variable for the text contained by this node.
   * 
   * @serial
   */
  protected String    _text;
  
  /**
   * Boolean variable for indicating whether a return is
   * required or not
   * 
   * @serial
   */
  protected boolean  _noReturn = false;
  
  /**
   * Constructs a new text node.
   * 
   * @param text the text of the text node
   * @param filter the filter to be used
   */
  public TextNode(String text, Filter filter) {
    _filter = filter;
    _text   = text;
  }
  
  /**
   * Constructs a new text node.
   * 
   * @param text the text of the text node
   */  
  public TextNode(String text) {
    this(text, new CharacterFilter());
  }
  
  /**
   * Sets the boolean variable that indicates whether a return
   * is needed or not.
   * 
   * @param b the boolean variable for the return
   */
  public void setNeedsReturn(boolean b) {
    _noReturn = b;
  }
  
  /**
   * Returns a boolean variable for the return.
     * 
   * @param b the boolean variable for the return
   */
  public boolean isNeedsReturn() {
    return _noReturn;
  }
  
  /**
   * Sets the text on this text node.
   * 
   * @param text the text to be set on this node
   */
  public void setText(String text) {
    _text = text;
  }
  
  /**
   * Outputs the text contained by this element.
   * 
   * @param writer the writer to which the text is output
   */
  public void output(PrintWriter writer) {
    if (_noReturn) {
      writer.println(_text);
    }
    else {
      writer.print(_text);
    }
  }
}
    
    
    