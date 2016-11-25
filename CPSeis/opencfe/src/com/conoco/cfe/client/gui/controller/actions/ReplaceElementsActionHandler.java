///
/// ReplaceElementsActionHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5.
///  4. 09-05-2002 SMCook   Added clearSelection() call when replacing elements
///                          to fix "1.4 problem" wherein all cells where
///                          sometimes selected after repaint!  This also fixed
///                          the red/green focus problem on the main gui.
///

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.ArrayGUIControl;
import com.conoco.cfe.client.gui.controls.GUIControl;

import com.conoco.cfe.utils.ArrayList;

import com.conoco.xml.StringArray;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "ModifyElements" action.
 */
public class ReplaceElementsActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
  /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public ReplaceElementsActionHandler(AppGUIController.ActionHandlerHelper helper) {
    super(helper);  
  }
  
  /**
   * Performs the action as specified by the XML node describing the action.
   * This method is invoked by the message decoder. 
   * 
   * @param n the XML node for the message
   */  
  public void performAction(Node n) {
    int pid = XMLHelper.getIntAttributeValue(n, "windowId");
    String keyword = XMLHelper.getUpperCaseStringAttributeValue(n, "keyword");
    String elements = XMLHelper.getStringAttributeValue(n, "elements");

    String[] array = StringArray.parseStringArray(elements);

    ArrayGUIControl arrayControl = (ArrayGUIControl) getHelper().getControl(pid, keyword);
    
    if (arrayControl != null) {
      arrayControl.clearSelection(); //SMCook - fixes bug wherein the array is
                                     //         erroneously all selected
                                     //         during/after repaint in 1.4
      arrayControl.deleteElements(0, arrayControl.getRowCount()-1);
      arrayControl.insertElements(array, 0);
    }
    else {
      //Console.logMessage("ReplaceElementsActionHandler: Couldn't find component " + keyword);
    }
  }        
}
