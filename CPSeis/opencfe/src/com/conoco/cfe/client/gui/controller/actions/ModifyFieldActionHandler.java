///
/// ModifyFieldActionHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4. 11-03-2003 SMCook   Now catches ClassCastException.
///

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.FieldGUIControl;
import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.TextFieldControl;

import com.conoco.cfe.utils.ArrayList;

import java.util.StringTokenizer;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "ModifyField" action.
 */
public class ModifyFieldActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
  /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public ModifyFieldActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    String value = XMLHelper.getStringAttributeValue(n, "value");

    GUIControl control = getHelper().getControl(pid, keyword);
    
    if (control != null) {
      if ( control instanceof TextFieldControl ) {
        ((FieldGUIControl) control).setValue(
                                checkLength((TextFieldControl) control, value));
      } 
      else {
        try {
          ((FieldGUIControl) control).setValue(value);
        } catch(ClassCastException e) {
          System.out.println("ModifyFieldActionHandler: " + e.getMessage());
          Console.logMessage(this, e.getMessage());
        }
      }
    }
    else {
      //Console.logMessage(
      //  "ModifyFieldActionHandler: Couldn't find component " + keyword);
    }
  }        
     
   /**
    * Check if the length is OK.
    *
    * @param control the TextFieldControl
    * @param value   the String to check
    */
   protected String checkLength(TextFieldControl control, String value) {
     if ( value.length() <= control.getColumns() ) {
       return value;
     } 
     else {
       StringBuffer buf = new StringBuffer();
       for ( int i = 0; i < control.getColumns(); i++ ) {
         buf.append("*");  
       }
       return buf.toString();
     }
   }  
}
