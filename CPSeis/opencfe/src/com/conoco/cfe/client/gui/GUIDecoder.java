// GUIDecoder.java

package com.conoco.cfe.client.gui;

import java.util.Hashtable;

/**
 * Interface to define the behaviour of a general tree walker
 * that will walk the DOM tree of an XML document that describes
 * the GUI layout of components.
 */
public interface GUIDecoder {
  
  /**
   * Initiates a decode on the file or url specified
   *
   * @param file_or_url Filename or URL
   *
   */
  public void setFilename(String file);
  
  /**
   * Initiates a decode on the url specified
   *
   * @param url The url location of the XML GUI file
   */
  public void setURL(String url);
  
  /**
   * The hashtable contains a mapping of keyword to ActionHandler instance
   * Each ActionHandler can process a node from an XML DOM tree
   *
   * @param handlers The hashtable of keyword to ActionHandler instances
   */
  public void setActionHandlers(Hashtable handlers);
}