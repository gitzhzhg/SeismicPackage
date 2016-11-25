// MessageEncoder.java

package com.conoco.cfe.client.messaging;

/**
 * Interface for the message encoder. Implement 
 * this interface to create custom message encoders. 
 * Typically, the clients state machine provides the 
 * encoder with the details of the message to be encoded
 * and retreives the encoded message from it. 
 */
public interface MessageEncoder {
  /**
   * Sends a request to the server for setting a value.
   *
   * @param windowId the id of the window
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param value The value for the action.  This field is ignored for some 
   *        action types.
   */
  public void setValue(int windowId, String action, String keyword, String value);

  /**
   * Sends a request to the server for setting an array of values.
   *
   * @param windowId the id of the window
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param value the changed value 
   * @param index index of the object changed
   */
  public void setArray(int windowId, String action, String keyword, String value, int start, int end);
  
  /**
   * Encodes the message and returns the encoded message
   * as a string.
   * 
   * @return the encoded message as a string
   */
  public String encode();
}