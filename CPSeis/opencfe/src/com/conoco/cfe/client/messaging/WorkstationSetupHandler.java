///
/// WorkstationSetupHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  3.
///  2. 09-12-2002 SMCook   Removed debug statement.
///  1. 09-11-2002 SMCook   Original version.  Needed to help with platform-
///                          specific issues, e.g. font bugs.
///

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the "WorkstationSetupHandler" document node. 
 */
public class WorkstationSetupHandler  implements PrefsNodeHandler {

  public static final int UNKNOWN    = 0;
  public static final int EXCEED     = 1;
  public static final int LINUX_VM   = 2;
  public static final int SOLARIS_VM = 3;

  private static int code;

  /**
   *
   */
  public static int getCode() {
    return code;
  }

  /**
   * This method is invoked when the parser
   * comes across a "WorkstationSetup" node.
   * 
   * @param n the document node
   */  
  public void performAction(Node n) {
    String setup = XMLHelper.getStringAttributeValue(n, "value");

    if     (setup.equals("Exceed")) {
      code = EXCEED;
    }
    else if(setup.equals("LinuxVM")) {
      code = LINUX_VM;
    }
    else if(setup.equals("SolarisVM")) {
      code = SOLARIS_VM;
    }
    else {
      code = UNKNOWN;
    }
  }

}
