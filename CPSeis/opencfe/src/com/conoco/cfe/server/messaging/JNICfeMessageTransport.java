// JNICfeMessageTransport.java

package com.conoco.cfe.server.messaging;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;

/**
 * A message transporter that glues the message builder, message parser
 * and the CFE API wrapper class together.
 */
public class JNICfeMessageTransport 
  extends RmiMessageTransport {

  /**
   * Constructs a new message transport class.
   * 
   * @throws java.rmi.RemoteException if a problem in setting up RMI is ecountered
   */
  public JNICfeMessageTransport(int timeout) throws RemoteException {
    super();
    XmlMessageBuilder   messageBuilder   = new XmlMessageBuilder(timeout);
    CfeXmlMessageParser messageParser   = new CfeXmlMessageParser();
    JNICfeApi       api      = new JNICfeApi();
    
    setMessageParser(messageParser);
    setMessageBuilder(messageBuilder);
    
    api.setMessageBuilder(messageBuilder);
    messageParser.setCfeApi(api);
  }

  /* for testing */
  public static void main(String[] args) {
    if (args.length != 3) {
      System.err.println("Usage: java <classname> <hostname> <sessionid> <timeout>");
      System.exit(0);
    }
    
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
      int timeout;
      
      try {
        timeout = Integer.parseInt(args[2]);
      }
      catch (NumberFormatException nfe) {
        // Set default to 61 minutes
        timeout = 61 * 60 * 1000;
      }
      
      System.out.println("rmiName="+rmiName);
      try {
        JNICfeMessageTransport messageTransport = new JNICfeMessageTransport(timeout);
        Naming.rebind(rmiName, messageTransport);
        System.out.println("Successfully bound "+rmiName);
      } catch (Exception e) {
        System.out.println("JNICfeMessageTransport error: " + e.getMessage());
        e.printStackTrace();
      }
      
    } catch (Exception e) {
      System.err.println("Unknown error: " + e.getMessage());
      e.printStackTrace();
    }
  }
}
                          