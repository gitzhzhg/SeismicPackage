///
/// DefaultLabelFontHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5.
///  4. 09-11-2002 SMCook   Removed debug statement.
///  3. 08-13-2002 SMCook   Added font scaling capability.
///

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import java.awt.Font;
import java.awt.geom.AffineTransform;

/**
 * A handler for the "DefaultLabelFontHandler" document 
 * node. 
 */
public class DefaultLabelFontHandler  extends FontHandler {
  public void setFont(Font f) {
    AffineTransform aff = new AffineTransform();
    aff.setToScale(FONT_WIDTH_SCALE_FACTOR, FONT_HEIGHT_SCALE_FACTOR);
    ClientConstants.setDefaultLabelFont(f.deriveFont(aff));
  }    
}
