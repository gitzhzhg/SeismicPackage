// CommController.java

package com.conoco.cfe.client.messaging;

import java.util.Hashtable;

/**
 * Public interface defining the general functionality of 
 * a communicatins controller. The job of the comm
 * controller is to get messages decoded or encoded 
 * using a decoder and an encoder respectively and 
 * interact with the communications module which actually
 * transmits the messages to the server. 
 * 
 * @see com.conoco.cfe.client.messaging.MessageEncoder
 * @see com.conoco.cfe.client.messaging.MessageDecoder
 * @see com.conoco.cfe.client.messaging.CommModule
 */
public interface CommController {
  /**
   * Sets the commnunications module that transmits the message 
   * to the server.
   * 
   * @param commModule the communications module that interacts with the server
   */
  public void setCommModule(CommModule commModule);
  
  /**
   * Returns the comm module.
   * 
   * @return the comm module 
   */
  public CommModule getCommModule();
    
  /**
   * Sets the message encoder.
   *
   * @param encoder the message encoder
   */
  public void setMessageEncoder(MessageEncoder encoder);
  
  /**
   * Returns the message encoder.
   * 
   * @return the message encoder set on this comms controller
   */
  public MessageEncoder getMessageEncoder();

  /**
   * Sets the message decoder.
   *
   * @param encoder the message decoder
   */
  public void setMessageDecoder(MessageDecoder encoder);
  
  /**
   * Returns the message decoder.
   * 
   * @return the message decoder set on this comms controller
   */
  public MessageDecoder getMessageDecoder();
  
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
  public void transmitMessage(int winId, String action,String keyword, String value);

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
  public void transmitMessage(int winId, String action, String keyword, String value, int start, int end);

  /**
   * Sets the reply action handlers. The processing
   * is handled by the action handlers.
   * 
   * @param h the action handlers as a <code>java.util.Hashtable</code>
   */
  public void setReplyActionHandlers(Hashtable handlers);
}