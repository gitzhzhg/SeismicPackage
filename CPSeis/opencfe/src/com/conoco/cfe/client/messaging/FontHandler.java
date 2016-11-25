///
/// FontHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  7.
///  6. 09-11-2002 SMCook   Settled on 1.08 as best height scaling factor, in
///                          combination with numerous other resize/alignment
///                          adjustments in WindowControl, RowTable, and
///                          DefaultArrayFontHandler.
///  5. 09-09-2002 SMCook   Changed back to 1.00 to allow for user feedback.
///  4. 08-20-2002 SMCook   Changed height scale factor from 1.00 to 1.08.
///  3. 08-13-2002 SMCook   Added font scaling capability.
///

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.gui.XMLHelper;

import java.awt.Font;

import org.w3c.dom.Node;

/**
 * A handler for the "FontHandler" document node. 
 */
public abstract class FontHandler  implements PrefsNodeHandler {
  public abstract void setFont(Font f);

  public static final double FONT_WIDTH_SCALE_FACTOR  = .92857;
  public static final double FONT_HEIGHT_SCALE_FACTOR = 1.08;
  //public static final double FONT_HEIGHT_SCALE_FACTOR = 1.00;

  /**
   * This method is invoked when the parser
   * comes across a "Fonts" node.
   * 
   * @param n the document node
   */  
  public void performAction(Node n) {
    String fontName = XMLHelper.getStringAttributeValue(n, "name");
    int    fontSize = XMLHelper.getIntAttributeValue(n, "size");
    String fontStyle= XMLHelper.getStringAttributeValue(n, "style");
    int    style    = Font.PLAIN;
    if      (fontStyle.equalsIgnoreCase("plain")) {
      style = Font.PLAIN;
    }
    else if (fontStyle.equalsIgnoreCase("bold")) {
      style = Font.BOLD;
    }
    else if (fontStyle.equalsIgnoreCase("italic")) {
      style = Font.ITALIC;
    }
    setFont(new Font(fontName, style, fontSize));
  }    
}
