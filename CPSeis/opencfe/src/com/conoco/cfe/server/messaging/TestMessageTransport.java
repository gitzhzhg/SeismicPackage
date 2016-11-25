// TestMessageTransport.java

package com.conoco.cfe.server.messaging;

//import com.ibm.xml.parsers.DOMParser;
import org.apache.xerces.parsers.DOMParser;

import java.io.IOException;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * A message transport implementation.
 */
public class TestMessageTransport 
  extends RmiMessageTransport {

  /**
   * Constructs a new message transport object.
   * 
   * @param servletTimeout the timeout period for the servlet
   * @throws java.rmi.RemoteException
   */
  public TestMessageTransport(int servletTimeout) throws RemoteException {
    super();
    XmlMessageBuilder     messageBuilder   = new XmlMessageBuilder(servletTimeout);
    CfeXmlMessageParser   messageParser   = new CfeXmlMessageParser();
    XmlTestCfeApi       api        = new XmlTestCfeApi();
    
    setMessageParser(messageParser);
    setMessageBuilder(messageBuilder);

    DOMParser responseParser = new DOMParser();
    try {
      responseParser.parse(new InputSource("http://mezcal.internal.int.com/Conoco/xml/responses.xml"));
    } catch (SAXException saxe) {
      throw new RemoteException(saxe.toString());
    } catch (IOException ioe) {
      throw new RemoteException(ioe.toString());
    }
    Document responseDom = responseParser.getDocument();
    Element response = responseDom.getDocumentElement();
    response.normalize();

    api.setMessageBuilder(messageBuilder);
    api.setResponseTree(response);
    messageParser.setCfeApi(api);
  }

  /** for testing */
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
      int servletTimeout = Integer.parseInt(args[2]);

      System.out.println("rmiName="+rmiName);
      try {
        TestMessageTransport messageTransport = new TestMessageTransport(servletTimeout);
        Naming.rebind(rmiName, messageTransport);
        System.out.println("Successfully bound "+rmiName);
      } catch (Exception e) {
        System.out.println("TestMessageTransport error: " + e.getMessage());
        e.printStackTrace();
      }
      
    } catch (Exception e) {
      System.err.println("Unknown error: " + e.getMessage());
      e.printStackTrace();
    }
  }
}
                          