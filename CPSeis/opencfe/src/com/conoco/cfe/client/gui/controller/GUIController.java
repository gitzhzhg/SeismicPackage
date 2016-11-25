// GUIController.java

package com.conoco.cfe.client.gui.controller;

import com.conoco.cfe.client.messaging.CommController;

import com.conoco.cfe.utils.ArrayList;

import java.util.Hashtable;

/**
 * An interface for the controller that controls
 * the state of GUI controls. The GUI controller
 * sends requests to the comms controller and receives
 * responses back from the applications controller.
 */
public interface GUIController {
    
  /**
   * Sets the communications controller that is invoked
   * by the GUI controller to send messages to the server.
   * 
   * @param appCommController the comm controller to be set on this 
   *               GUI controller
   */
  public void setCommController(CommController appCommController);
  
  /**
   * Returns the communications controller that is invoked
   * by the state to send messages to the server.
   * 
   * @return the communications controller attached to this state
   */
  public CommController getCommController();

  /**
   * Returns the action handlers that handle the reply messages
   * coming from the server. The action handlers interact directly
   * with the controls to manipulate the client state.
   * 
   * @return the action handlers as a <code>java.util.Hashtable</code>
   */
  public Hashtable getReplyActionHandlers();
}