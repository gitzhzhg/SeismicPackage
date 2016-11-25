// GUIMessageHandlerAdapter.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.messaging.MessageEncoder;
import com.conoco.cfe.client.messaging.CommController;
  
/**
 * An adapter class for the GUI message handler. 
 */
public abstract class GUIMessageHandlerAdapter  implements GUIMessageHandler {
  /**
   * Variable for the communications controller; the
   * comms controller is directly invoked by the 
   * action handler to send messages to the server
   * 
   * @serial
   */
  protected CommController _commController;
    
  /**
   * Constructs a new adpater object.
   */
  public GUIMessageHandlerAdapter() {
  }
  
  /**
   * Constructs a new adapter object with the specified
   * comms controller.
   * 
   * @param commController   the comms controller that will be
   *               invoked by this adapter
   */
  public GUIMessageHandlerAdapter(CommController commController) {
    this();
    setCommController(commController);
  }
    
  /**
   * Sets the comms controller object on this adapter.
   * 
   * @param commController   the comms controller that will be
   *               invoked by this adapter
   */
  public void setCommController(CommController commController) {
    _commController = commController;
  }
  
  /**
   * Returns the comms controller object.
   * 
   * @return the comms controller object set on this adapter
   */
  public CommController getCommController() {
    return _commController;
  }
}