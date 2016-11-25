// AppCommController.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.ClientConstants;

import java.util.Hashtable;

/**
 * This class is the middleman between the communications module 
 * that talks with the server and the client's state machine. 
 * The communications controller uses the message encoder, decoder 
 * and the communications module to pass the messages back and 
 * forth between the client and the server.
 */
public class AppCommController   implements CommController {
  /**  
   * Variable for the comms module
   * 
   * @serial
   */
  protected CommModule _commModule;
  
  /**
   * Variable for the message encoder
   * 
   * @serial
   */
  protected MessageEncoder _encoder;
  
  /**
   * Variable for the message decoder
   * 
   * @serial
   */
  protected MessageDecoder _decoder;
  
  /**
   * Constructs a new comms controller.
   */
  public AppCommController() {
  }
  
  /**
   * Sets the comm module.
   * 
   * @param commModule the comm module
   */
  public void setCommModule(CommModule commModule) {
    _commModule = commModule;
  }  
  
  /**
   * Returns the comm module.
   * 
   * @return the comm module 
   */
  public CommModule getCommModule() {
    return _commModule;
  }
      
  /**
   * Sets the message encoder.
   *
   * @param encoder the message encoder
   */
  public void setMessageEncoder(MessageEncoder encoder) {
    _encoder = encoder;  
  }
  
  /**
   * Returns the message encoder.
   * 
   * @return the message encoder set on this comms controller
   */
  public MessageEncoder getMessageEncoder() {
    return _encoder;
  }

  /**
   * Sets the message decoder.
   *
   * @param encoder the message decoder
   */
  public void setMessageDecoder(MessageDecoder decoder) {
    _decoder =   decoder;
  }
  
  /**
   * Sets the reply action handlers. The processing
   * is handled by the action handlers.
   * 
   * @param h the action handlers as a <code>java.util.Hashtable</code>
   */
  public void setReplyActionHandlers(Hashtable h) {
    _decoder.setReplyActionHandlers(h);
  }  
  
  /**
   * Returns the message decoder.
   * 
   * @return the message decoder set on this comms controller
   */
  public MessageDecoder getMessageDecoder() {
    return _decoder;
  }
  
  /**
   * Transmits the message to the server by passing it
   * on to the comm module. This method is typically used
   * for transmitting messages related to components
   * other than arrays and arraysets.
   * 
   * @param windId the window ID
   * @param action the name of the message to be transmitted
   * @param keyword the kewyord of the component this message is related to
   * @param value the value of the message
   */
  public void transmitMessage(int winId, String action, String keyword, String value) {
    MessageEncoder encoder = getMessageEncoder();
    encoder.setValue(winId, action, keyword, value);
    String response =  getCommModule().sendMessage(encoder.encode());
    getMessageDecoder().decode(response);  
  }

  /**
   * Transmits the message to the server by passing it
   * on to the comm module. This method is typically used
   * for transmitting messages related to array and arrayset components.
   * 
   * @param windId the window ID
   * @param action the name of the message to be transmitted
   * @param keyword the kewyord of the component this message is related to
   * @param value the value of the message
   * @param start the start index
   * @param end the end index
   */
  public void transmitMessage(int winId, String action, String keyword, 
    String value, int start, int end) {
    MessageEncoder encoder = getMessageEncoder();
    encoder.setArray(winId, action, keyword, value, start, end);
    String response = getCommModule().sendMessage(encoder.encode());
    getMessageDecoder().decode(response);  
  }
}