// DirectMessageTransport.java

package com.conoco.cfe.server.messaging;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;

/**
 * A message transporter that glues the message builder, message parser
 * and the CFE API wrapper class together.
 */
public class DirectMessageTransport {

  /** 
   * The class that is responsible for parsing messages from the client
   *
   * @serial
   */
  protected MessageParser _messageParser;
  
  /** 
   * The class that is repsonsible for constructing reply messages from the server 
   *
   * @serial
   */
  protected MessageBuilder _messageBuilder;
  

  /**
   * Constructs a new message transport class.
   * 
   * @throws java.rmi.RemoteException if a problem in setting up RMI is ecountered
   */
  public DirectMessageTransport(int timeout) {
    super();
    XmlMessageBuilder   messageBuilder   = new XmlMessageBuilder(timeout);
    CfeXmlMessageParser messageParser   = new CfeXmlMessageParser();
    JNICfeApi       api      = new JNICfeApi();
    
    setMessageParser(messageParser);
    setMessageBuilder(messageBuilder);
    
    api.setMessageBuilder(messageBuilder);
    messageParser.setCfeApi(api);
  }
  
  /**
   * Sets the message parser used by this message transport.
   *
   * @param messageParser The parser that is used to parse messages from the client.
   */
  public void setMessageParser(MessageParser messageParser) {
    _messageParser = messageParser;
  }

  /**
   * Sets the reply message builder used by this message transport.
   *
   * @param messageBuilder The reply message builder used by this message transport.
   */
  public void setMessageBuilder(MessageBuilder messageBuilder) {
    _messageBuilder = messageBuilder;
  }


  /**
   * This is the method that is invoked by the middle tier, through 
   * <code>rmiSendMessage()</code>, to send a message to the server. 
   * This method wraps the <code>sendMessage()</code> method, trapping
   * any exceptions that may occur, and replacing them with RemoteExceptions.
   *
   * @param clientMessage A message from the client that is sent to the server.
   * @return A reply message that will be sent back to the middle tier.
   * @throws java.rmi.RemoteException
   */
  public String sendMessage(String clientMessage) 
    throws Exception {

    if (_messageParser == null) {
      throw new Exception("Message parser is not set on message transport layer.\n" +
                    "The message parser must be set before calling sendMessage()");
    }

    if (_messageBuilder == null) {
      throw new Exception("Message builder is not set on message transport layer.\n" +
                    "The message builder must be set before calling sendMessage()");
    }

    try { 
      _messageParser.parseMessage(clientMessage);
    } catch (MessagingException me) {
      throw new RemoteException(me.toString());
    }
    
    return _messageBuilder.getMessage();
  }


}
                          