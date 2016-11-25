// XMLMessageHandler.java

package com.conoco.cfe.server.messaging;

import org.w3c.dom.Node;

/**
 * Interface for a general-purpose action handler that
 * handles the nodes in the XML document that describes the
 * cleint requests.
 */
public interface XmlActionHandler extends MessagingConstants {
  
  /**
   * This method is invoked by the message parser.
   * 
   * @param node the DOM document node that is to be processed
   * @param cfeApi the wrapper class that provides access to the CFE API   
   * @throws com.conoco.cfe.server.messaging.MessagingException if 
   * an error takes place while parsing the message node      
   */
  void performAction(Node node, CfeApi cfeApi)
    throws MessagingException;
}