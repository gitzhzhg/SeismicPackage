///
/// ShowHelpActionHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  6.
///  5. 10-25-2002 SMCook   HelpWindowControl.show() now doesn't need argument.
///  4. 10-24-2002 SMCook   HelpWindowControl.show() now needs argument.
///                         Also removed unused import statements.
///

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.builder.HelpSection;

import com.conoco.cfe.client.gui.controller.AppGUIController;
import com.conoco.cfe.client.gui.controller.GUIControllerHelper;

import com.conoco.cfe.client.gui.controls.HelpWindowControl;
import com.conoco.cfe.client.gui.controls.WindowControl;

import java.util.Hashtable;

import org.w3c.dom.Node;
import org.w3c.dom.Document;
import org.w3c.dom.CharacterData;

/**
 * Action handler for the "ShowHelp" action.
 */
public class ShowHelpActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
   /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public ShowHelpActionHandler(AppGUIController.ActionHandlerHelper helper) {
    super(helper);  
  }

  /**
   * Performs the action as specified by the XML node describing the action.
   * This method is invoked by the message decoder.
   * 
   * @param n the XML node for the message
   */  
  public void performAction(Node n) {
    String appHelpURL = XMLHelper.getStringAttributeValue(n, "value");    

    WindowControl wc = (WindowControl) _helper.getWindowUnderFocus();
    String winKey = wc.getKeyword();
    int id = _helper.getWindowIdForWindow(wc);
    
    Hashtable h = GUIControllerHelper.getWindowIdToHelpSection();  
    HelpSection hs = (HelpSection) h.get(new Integer(id));
    
    String winHelpTip = hs.getHelpTip(winKey);
    String winHelpText = hs.getHelpText(winKey);

    if ( winHelpText != null ) {
      HelpWindowControl.setProcessHelp(winHelpText);
    } 
    else if ( winHelpTip != null ) {
      HelpWindowControl.setProcessHelp(winHelpTip);
    }
    else {
      HelpWindowControl.setProcessHelp("");
    }
    HelpWindowControl.setAppHelpURL(appHelpURL);
    HelpWindowControl.show();
  }        
}
