// XMLHelper.java

package com.conoco.cfe.client.gui;

import com.conoco.cfe.client.application.Console;

import java.util.Hashtable;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.CharacterData;

/**
 * A helper class that provides some utility methods
 * to help processing of DOM trees.
 * 
 * @see org.w3c.dom
 */
public class XMLHelper {
  
  /**
   * Returns the children of the specified node as an array of <code>
   * org.w3c.dom.Node</code>.
   *
   * @param n the node whose children are desired
   * @return   the children of the specified node; returns null if no
   *    children are found
   */
  public static Node[] getChildren(Node n) {
    NodeList list = n.getChildNodes();
    if ( list == null) {
      return null;
    } 
    else {
      Node[] children = new Node[list.getLength()];
      for (int i =0; i < children.length; ++i) {
        children[i] = list.item(i);
      }
      return children;
    }
  }
  
  /**
   * Prints the information associated with the specified node.
   *
   * @param header user-defined string that will be prepended to the node info
   * @param node the node whose infomation is desired
   */
  public static void printNode(String header, Node node) {
    NamedNodeMap attributes;
      
    Console.logMessage(header + " " + node.getNodeName());
    attributes = node.getAttributes();
        
    if (attributes != null) {
      Node attribute;
      for (int i=0; i < attributes.getLength(); i++) {
        attribute = attributes.item(i);
        Console.logMessage("Attribute: " + attribute.getNodeName() + 
          " = " + attribute.getNodeValue());
      }
    }
    String value = node.getNodeValue();
    if (value != null) {
      value = value.trim();
      if (value.length() != 0) {
        Console.logMessage("Value: " + value);
      }
    }
  }
  
  /**
   * Parses the value of the specified attribute of the specified node 
   * as a floating point number and returns the floating point value.
   *
   * @param node the node whose infomation is desired
   * @param attribute the attribute whose value will be parsed as a float
   */
  public static float getFloatAttributeValue(Node node, String attribute) {
    NamedNodeMap attributes;
    Node valueNode;  
    attributes = node.getAttributes();
    valueNode = attributes.getNamedItem(attribute);
    Float f = new Float(valueNode.getNodeValue());
    return f.floatValue();
    //return Float.parseFloat(valueNode.getNodeValue());
  }
  
  /**
   * Parses the value of the specified attribute of the specified node 
   * as a integer number and returns the integer value.
   *
   * @param node the node whose infomation is desired
   * @param attribute the attribute whose value will be parsed as an integer
   */
  public static int getIntAttributeValue(Node node, String attribute) {
    NamedNodeMap attributes;
    Node valueNode;
    attributes = node.getAttributes();
    valueNode = attributes.getNamedItem(attribute);      
    return Integer.parseInt(valueNode.getNodeValue());
  }
  
  /**
   * Parses the value of the specified attribute of the specified node 
   * as a string and returns the string value.
   *
   * @param node the node whose infomation is desired
   * @param attribute the attribute whose value will be parsed as a string
   */
  public static String getStringAttributeValue(Node node, String attribute) {
    NamedNodeMap attributes;
    Node valueNode;
    attributes = node.getAttributes();
    valueNode = attributes.getNamedItem(attribute);
    return valueNode.getNodeValue();
  }
  
  /**
   * Parses the value of the specified attribute of the specified node 
   * as a string and returns the string value.
   *
   * @param node the node whose infomation is desired
   * @param attribute the attribute whose value will be parsed as a string
   */
  public static String getUpperCaseStringAttributeValue(Node node, String attribute) {
    return getStringAttributeValue(node, attribute).toUpperCase();
  }

  /**
   * Parses the value of the specified attribute of the specified node 
   * as a string and returns the string value.
   *
   * @param node the node whose infomation is desired
   * @param attribute the attribute whose value will be parsed as a string
   */
  public static boolean getBooleanAttributeValue(Node node, String attribute) {
    NamedNodeMap attributes;
    Node valueNode;
    attributes = node.getAttributes();
    valueNode = attributes.getNamedItem(attribute);
    String value = valueNode.getNodeValue();

    if (value.equalsIgnoreCase("yes") || value.equalsIgnoreCase("true")) {
      return true;
    }
    else {
      return false;
    }
  }
  
  /**
   */
  public static String getCharDataString(Node node) {
    CharacterData child = (CharacterData) node.getFirstChild();
    if ( child != null ) { 
      return child.getData();
    } 
    else {
      return null;
    }
  }

  /**
    * Strips white space characters from a text node's character data.
    * All leading and trailing tabs, blanks and new line characters 
   * are stripped.
   *
   * @param charData the character data which has white space that is
   *       to be stripped off
   */
  public static void stripWhiteSpace(CharacterData charData) {
    charData.setData(stripWhiteSpace(charData.getData()));
  }  
  
  /**
   * Strips the white space characters from a string. This is a static
   * utility method called by <code>stripWhiteSpace(org.w3c.dom.CharacterData)
   * </code> method. All leading and trailing tabs, blanks and new line characters 
   * are stripped.
   * 
   * @param str the string whose white space characters need to be purged
   */
  public static String stripWhiteSpace(String str) {
    char[] array = str.toCharArray();
    StringBuffer buf = new StringBuffer();
    boolean leadingBlanks = true;
    int no = 0;
    int mark = 0;
    for (int i =0; i < array.length; i++) {
      if (array[i] == '\t' || array[i] == '\n') {
        if (leadingBlanks) {
          ++no;  
        }
        continue;  
      }
      else if ( array[i] == ' ') {
        if ( leadingBlanks == true) {
          ++no;
          continue;    
        } 
        else {
          buf.append(array[i]);
          mark = i;
        }
      }
      else {
        if ( leadingBlanks == true ) {
          leadingBlanks = false;
        }
        buf.append(array[i]);
        mark = i;
      }
    }
    return buf.toString();
  }
  
  /**
   * Returns the attribute values of a specified node as a 
   * <code>java.util.Hashtable</code>. 
   * 
   * @param n the node whose attributes are desired
   * @return the attributes of the given node as a <code>java.util.Hashtable</code>
   */
  public static Hashtable getAttributesAsHashtable(Node n) {
    NamedNodeMap map = n.getAttributes();
    if ( map == null ) {
      return null;
    }
    Hashtable attrs = new Hashtable();
    for ( int k = 0; k < map.getLength(); ++k) {
      Node current = map.item(k);
      attrs.put(current.getNodeName(), current.getNodeValue());
    }  
    return attrs;
  }
}