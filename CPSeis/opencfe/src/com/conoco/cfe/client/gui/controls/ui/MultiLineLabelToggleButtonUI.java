///
/// MultiLineLabelToggleButtonUI.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-18-2001 SMCook   Changed comments so as not to use '///'.
///

package com.conoco.cfe.client.gui.controls.ui;

import java.awt.*;
import java.awt.event.*;

import java.beans.*;

import java.io.Serializable;

import javax.swing.*;

import javax.swing.border.*;

import javax.swing.plaf.*;
import javax.swing.plaf.basic.*;
import javax.swing.plaf.metal.*;
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.ComponentUI;

import javax.swing.text.View;

/**
 * A UI delegate that handles buttons having multi-line 
 * labels.
 */
public class MultiLineLabelToggleButtonUI extends MetalToggleButtonUI {
  private final static MultiLineLabelToggleButtonUI ui = 
                              new MultiLineLabelToggleButtonUI(); 
  private ConocoAbstractButtonUIImpl uiImpl = new ConocoAbstractButtonUIImpl();
    //
    //  Create PLAF
    //
    public static ComponentUI createUI(JComponent c) {
      return ui;
    }
    public static void initialize() {
      // don't hardcode the class name, fetch it dynamically.  This way we can
      // obfuscate.
      String key = "ButtonUI";
      Class cls = ui.getClass();
      String name = cls.getName();
      UIManager.put(key,name);
      UIManager.put(name,cls);
     }
     public Dimension getPreferredSize(JComponent c) {
      return uiImpl.getPreferredSize(c);    
    }
    
    /* These rectangles/insets are allocated once for this shared LabelUI
     * implementation.  Re-using rectangles rather than allocating
     * them in each paint call halved the time it took paint to run.
     */
    private static Rectangle paintIconR = new Rectangle();
    private static Rectangle paintTextR = new Rectangle();
    private static Rectangle paintViewR = new Rectangle();
    private static Insets paintViewInsets = new Insets(0, 0, 0, 0);
    private static Rectangle oPaintIconR = new Rectangle();
    private static Rectangle oPaintTextR = new Rectangle();

    public void paint(Graphics g, JComponent c) {
      AbstractButton b = (AbstractButton) c;
      ButtonModel model = b.getModel();
      String text = b.getText();      
      Icon icon = b.isEnabled() ? b.getIcon() : b.getDisabledIcon();
      if ((icon == null) && (text == null)) {
        return;
      }
      FontMetrics fm = g.getFontMetrics();
      int fontHeight=fm.getHeight();
      int fontAscent=fm.getAscent();
      int numLines = 1;
      int maxline=0;
      String lines[];
      if (text != null) {
        int maxlength=0;
         lines = UIUtility.breakupLines(text);
         numLines = lines.length;
         for (int i=0; i<numLines; i++) {
          int len = SwingUtilities.computeStringWidth(fm,lines[i]);
          if (len > maxlength) {
            maxlength=len;
            maxline=i;
          }
        }
      } 
      else {
        maxline=0;
         lines = new String[1];
         lines[0]=null;
      }
      
      boolean clippedMode = false;
      boolean individualAlign = true;
      paintViewInsets = c.getInsets(paintViewInsets);
      paintViewR.x = paintViewInsets.left;
      paintViewR.y = paintViewInsets.top;
      paintViewR.width =
              c.getWidth() - (paintViewInsets.left + paintViewInsets.right);
      paintViewR.height =
              c.getHeight() - (paintViewInsets.top + paintViewInsets.bottom);
      paintIconR.x = paintIconR.y = paintIconR.width = paintIconR.height = 0;
      paintTextR.x = paintTextR.y = paintTextR.width = paintTextR.height = 0;
       boolean vertical = false;
       if (vertical) {
         String totalWidth = uiImpl.getTotalVerticalWidth(b,numLines);
         clippedMode=false;
         uiImpl.layoutCL(b,fm,totalWidth,icon,paintViewR,paintIconR,paintTextR,
         lines[maxline].length()*fontHeight,clippedMode);
      } 
      else {
         uiImpl.layoutCL(b, fm, lines[maxline], icon, paintViewR, paintIconR,
         paintTextR, fontHeight*numLines, clippedMode);
      }
      if (icon != null) {
          icon.paintIcon(c, g, paintIconR.x, paintIconR.y);
      }
      int textHeight = fontHeight*numLines;
       int textX = paintTextR.x;
       int textY = paintTextR.y + fontAscent;
       int textWidth = paintTextR.width;
       uiImpl.paintButton(g, c, getSelectColor());                
      for (int i=0; i<numLines; i++, textY+=fontHeight) {
        String clipped = lines[i];
        if (individualAlign) {
          oPaintIconR.x=oPaintIconR.y=oPaintIconR.width=oPaintIconR.height=0;
          oPaintTextR.x=oPaintTextR.y=oPaintTextR.width=oPaintTextR.height=0;
          // now get layout, for the maximum line with the total text height
          clipped= uiImpl.layoutCL(b, fm, clipped, icon, paintViewR,
                            oPaintIconR, oPaintTextR,
                            textHeight, clippedMode);
          textX = oPaintTextR.x;
        } 
        else if (clippedMode) {
          /* block mode, now clip the text, if necessary */
          int w=SwingUtilities.computeStringWidth(fm,clipped);
          if (w > textWidth) {
            clipped = UIUtility.getClippedText(clipped,fm,textWidth);
          }
        }
        if (b.isEnabled()) {
          if (uiImpl.paintEnabledText(b, g, clipped, textX, textY)) {
          }
        } 
        else {
          if (uiImpl.paintDisabledText(b, g, clipped, textX, textY)) {
          }
        }
      }
      if (b.isFocusPainted() && b.hasFocus()) {
        // paint UI specific focus
        uiImpl.paintFocus(g,b,paintViewR,paintTextR,paintIconR, getFocusColor());
      }
    }  
  }
