// ReplyActionHandlerAdapter.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.gui.controller.AppGUIController;

/**
 * An adapter class for action handlers that handle server reply
 * messages.
 */
public abstract class ReplyActionHandlerAdapter
  implements XmlActionHandler {
  
  /**
   * Variable for the action handler helper which is 
   * internal to the the GUI controller 
   * 
   * @serial
   */
  protected AppGUIController.ActionHandlerHelper _helper;
  
  /**
   * Constructs a new adapter.
   */
  protected ReplyActionHandlerAdapter() {
  }
  
  /**
   * Constructs a new adapter with the specified helper.
   * 
   * @param helper the action handler helper
   */
  public ReplyActionHandlerAdapter(AppGUIController.ActionHandlerHelper helper) {
    _helper = helper;
  }  
    
  /**
   * Returns the helper that helps the action handlers.
   * 
   * @return the helper that helps the actin handlers
   */
  public AppGUIController.ActionHandlerHelper getHelper() {
    return _helper;
  }
  
  /**
   * Sets the helper on this adpater.
   * 
   * @param helper   the helper object that helps the adapter 
   *           the server reply
   */
  public void setHelper(AppGUIController.ActionHandlerHelper helper) {
    _helper = helper;
  }
}