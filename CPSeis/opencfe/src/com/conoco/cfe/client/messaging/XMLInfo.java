// XMLInfo.java

package com.conoco.cfe.client.messaging;

/**
 * A class to declare XML-related constants. The static constants
 * are used by the XML encoder to write XML documents.
 *
 * @see com.conoco.cfe.client.messaging.XMLMessageEncoder
 */
public abstract class XMLInfo {
  /** A constant for declaring the XML version */
  public static String XML_VERSION = "<?xml version=\"1.0\"?>";
    
  /** A constant for declaring the path to the message Document Type Definition */
  public static String MESSAGE_DTD = "message.dtd";
}