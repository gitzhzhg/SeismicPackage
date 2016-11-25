// JNIMessageTransport.java

package com.conoco.cfe.server.messaging;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;

/**
 * A message transporter that glues the message builder, message parser
 * and the CFE API wrapper class together.
 */
public class JNIMessageTransport 
  extends RmiMessageTransport {

  /**
   * Constructs a new message transport class.
   * 
   * @throws java.rmi.RemoteException if a problem in setting up RMI is ecountered
   */
  public JNIMessageTransport() throws RemoteException {
    super();
    XmlMessageBuilder     messageBuilder   = new XmlMessageBuilder();
    CfeXmlMessageParser   messageParser   = new CfeXmlMessageParser();
    JNICfeApi         api        = new JNICfeApi();
    
    setMessageParser(messageParser);
    setMessageBuilder(messageBuilder);
    
    api.setMessageBuilder(messageBuilder);
    messageParser.setCfeApi(api);
  }

  public static void main(String[] args) {
    try {
      if (System.getSecurityManager() == null) {
        System.setSecurityManager(new RMISecurityManager());
      }

      String prefix;
      String suffix;
      
      if (args[0].startsWith("//")) {
        prefix = "";
      } else {
        prefix = "//";
      }

      if (args[0].endsWith("/") || args[1].startsWith("/")) {
        suffix = "";
      } else {
        suffix = "/";
      }
      
      String rmiName = prefix + args[0] + suffix + args[1];

      System.out.println("rmiName="+rmiName);
      try {
        JNIMessageTransport messageTransport = new JNIMessageTransport();
        Naming.rebind(rmiName, messageTransport);
        System.out.println("Successfully bound "+rmiName);
      } catch (Exception e) {
        System.out.println("JNIMessageTransport error: " + e.getMessage());
        e.printStackTrace();
      }
      
    } catch (Exception e) {
      System.err.println("Unknown error: " + e.getMessage());
      e.printStackTrace();
    }
  }
}
                          