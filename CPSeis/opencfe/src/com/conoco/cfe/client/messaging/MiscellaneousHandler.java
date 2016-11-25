///
/// MiscellaneousHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  1. 04-06-2004 SMCook   Initial version.  Added in order to get icps
///                          buttons up and running for the custom code case.
///

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the "Miscellaneous" document 
 * node. 
 */
public class MiscellaneousHandler  implements PrefsNodeHandler {
  /**
   * This method is invoked when the parser
   * comes across a "Miscellaneous" node.
   * 
   * @param n the document node
   */  
  public void performAction(Node n) {
    //String path = XMLHelper.getCharDataString(n);
    //Preferences.setBackEndShortPath(path);
  }    
}
