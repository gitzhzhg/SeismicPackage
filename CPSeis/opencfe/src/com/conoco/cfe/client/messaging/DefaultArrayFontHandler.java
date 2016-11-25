///
/// DefaultArrayFontHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5.
///  4. 09-11-2002 SMCook   Removed debug statement.
///                         Added switched translate() call to compensate for
///                          platform-dependent bug in vertical text alignment.
///  3. 08-13-2002 SMCook   Added font scaling capability.
///

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.ClientConstants;

import java.awt.Font;
import java.awt.geom.AffineTransform;

/**
 * A handler for the "DefaultArrayFontHandler" document 
 * node. 
 */
public class DefaultArrayFontHandler extends FontHandler {
  public void setFont(Font f) {

    AffineTransform aff = new AffineTransform();
    aff.setToScale(FONT_HEIGHT_SCALE_FACTOR, FONT_HEIGHT_SCALE_FACTOR);

    switch(WorkstationSetupHandler.getCode()) {

      case WorkstationSetupHandler.SOLARIS_VM:
      case WorkstationSetupHandler.EXCEED:
        //no translation needed
      break;

      case WorkstationSetupHandler.LINUX_VM:
      case WorkstationSetupHandler.UNKNOWN:
        aff.translate(0, 1.5);
      break;

      default:
        System.err.println("unhandled workstation setup case");
        System.exit(1);
    }

    ClientConstants.setDefaultArrayFont(f.deriveFont(aff));
  }    
}
