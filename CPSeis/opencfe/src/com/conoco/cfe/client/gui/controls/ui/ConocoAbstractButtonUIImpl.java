// ConocoAbstractButtonUIImpl.java
 
package com.conoco.cfe.client.gui.controls.ui;

import com.conoco.cfe.client.ClientConstants;
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


public final class ConocoAbstractButtonUIImpl {
  private static String DEFAULT_VERTICAL_WIDTH = "W";
  private static String DEFAULT_VERTICAL_SPACE = " ";
   public static String VERTICAL_SPACE = "verticalSpace";
   public static String VERTICAL_WIDTH = "verticalWidth";

  String layoutCL(AbstractButton label,           
                  FontMetrics fontMetrics, 
                  String text, 
                  Icon icon, 
                  Rectangle viewR, 
                  Rectangle iconR, 
                  Rectangle textR,
                  int textHeight,
                  boolean clippedMode) {
    return UIUtility.layoutCompoundLabel(label,
                                         fontMetrics,
                                         text,
                                         icon,
                                         label.getVerticalAlignment(),
                                         label.getHorizontalAlignment(),
                                         label.getVerticalTextPosition(),
                                         label.getHorizontalTextPosition(),
                                         viewR,
                                         iconR,
                                         textR,
                                         0,
                                         textHeight,
                                         clippedMode);
  }
  
  String getTotalVerticalWidth(JComponent label, int numLines) {
    String space = getVerticalSpace(label);
    char[] spaceArr = space.toCharArray();
    int spaceLen = spaceArr.length;
    String width = getVerticalWidth(label);
    char[] widthArr = width.toCharArray();
    int widthLen = widthArr.length;
    char[] totalWidth = new char[spaceLen*numLines+widthLen*(numLines-1)];
    for (int i=0; i<numLines; i++) {
      System.arraycopy(widthArr,0,totalWidth,i*(spaceLen+widthLen),widthLen);
      if (i!=numLines-1) {
        System.arraycopy(spaceArr,0,totalWidth,i*(
        spaceLen+widthLen)+widthLen,spaceLen);
      }
    }
    return new String(totalWidth);
  }
  
  String getVerticalSpace(JComponent label) {
    String val = (String)label.getClientProperty(VERTICAL_SPACE);
    if (val == null) {
      return DEFAULT_VERTICAL_SPACE;
    } 
    else {
        return val;
      }
    }

  String getVerticalWidth(JComponent label) {
    String val = (String)label.getClientProperty(VERTICAL_WIDTH);
    if (val == null) {
      return DEFAULT_VERTICAL_WIDTH;
    }
    else {
      return val;
    }
  }

  void paintIcon(Graphics g, JComponent c, Rectangle iconRect){
  }

  void paintFocus(Graphics g, AbstractButton b,
                   Rectangle viewRect, Rectangle textRect, 
                     Rectangle iconRect, Color focusColor){
    Rectangle focusRect = new Rectangle();
    String text = b.getText();
    boolean isIcon = b.getIcon() != null;
    // If there is text
    if ( text != null && !text.equals( "" ) ) {
      if ( !isIcon ) {
        focusRect.setBounds( textRect );
      }
      else {
        focusRect.setBounds( iconRect.union( textRect ) );
      }
    }
    // If there is an icon and no text
    else if ( isIcon ) {
      focusRect.setBounds( iconRect );
    }
    g.setColor(focusColor);
    g.drawRect((focusRect.x-1), (focusRect.y-1),
                  focusRect.width+1, focusRect.height+1);
    }

    /**
     */
   void paintButton(Graphics g, JComponent c, Color selectColor) {
    AbstractButton b = (AbstractButton) c;
    ButtonModel model = b.getModel();
    if ( c instanceof JButton ) {
       // perform UI specific press action
       if (model.isArmed() && model.isPressed()) {
         paintButtonPressed(g,b,selectColor); 
       }
    } 
    else if ( b instanceof JToggleButton ) {
      // perform UI specific press action
      if (model.isArmed() && model.isPressed() || model.isSelected()) {
        paintButtonPressed(g,b,selectColor); 
      }
    }
  }
    
  /**
   * Paint clippedText at textX, textY with the labels foreground color.
   * Return true if the accelerator Character was printed
   * 
   * @see #paint
   * @see #paintDisabledText
   */
  boolean paintEnabledText(AbstractButton b, 
                            Graphics g, String s, int textX, 
                               int textY) {
    g.setColor(b.getForeground());
    return UIUtility.drawString(g, s, '\0', textX, textY);
  }
  
  /**
   * Paint clippedText at textX, textY with background.lighter() and then 
   * shifted down and to the right by one pixel with background.darker().
   * Return true if the accelerator Character was printed
   * 
   * @see #paint
   * @see #paintEnabledText
   */
  boolean paintDisabledText(AbstractButton b, 
              Graphics g, String s, int textX, 
                                int textY) {
    Color background = b.getBackground();
    g.setColor(background.brighter());
    /*res=*/UIUtility.drawString(g, s, '\0', textX, textY);
    g.setColor(background.darker());
    return UIUtility.drawString(g, s, '\0', textX + 1, textY + 1);
  }
  
  void paintButtonPressed(Graphics g, AbstractButton b, Color selectColor) {
    if ( b.isContentAreaFilled() ) {
      Dimension size = b.getSize();
      g.setColor(selectColor);
      g.fillRect(0, 0, size.width, size.height);
    }
  }
      
  public Dimension getPreferredSize(JComponent c) {
    Rectangle iconR = new Rectangle();
    Rectangle textR = new Rectangle();
    Rectangle viewR = new Rectangle();
    Insets viewInsets = new Insets(0, 0, 0, 0);
    
    AbstractButton label = (AbstractButton)c;
    String text = label.getText();
    Icon icon = label.getIcon();
    Insets insets = label.getInsets(viewInsets);
    Font font = label.getFont();
  
    int dx = insets.left + insets.right;
    int dy = insets.top + insets.bottom;
  
    if ((icon == null) && ((text == null) || 
             ((text != null) && (font == null)))) {
      return new Dimension(dx, dy);
    }
    else if ((text == null) || ((icon != null) && (font == null))) {
      return new Dimension(icon.getIconWidth() + dx, 
      icon.getIconHeight() + dy);
    }
    else {
      FontMetrics fm = c.getFontMetrics(font);
      //FontMetrics fm = Toolkit.getDefaultToolkit().getFontMetrics(font);
      int fontHeight = fm.getHeight();
      String lines[] = UIUtility.breakupLines(text);
      int numLines = lines.length;
      String maxline = "";
      int maxwidth=0;
      for (int i=0; i<numLines; i++) {
        int w = fm.stringWidth(lines[i]);
        if (w > maxwidth) {
          maxline = lines[i];
          maxwidth = w;
        }
      }
      iconR.x = iconR.y = iconR.width = iconR.height = 0;
      textR.x = textR.y = textR.width = textR.height = 0;
      viewR.x = dx;
      viewR.y = dy;
      viewR.width = viewR.height = Short.MAX_VALUE;
      boolean vertical = false;
      boolean clippedMode=false;
      if (vertical) {
        String totalWidth = getTotalVerticalWidth(label,numLines);
        layoutCL(label,fm,totalWidth,icon,viewR,iconR,textR,
         maxline.length()*fontHeight,clippedMode);
       } 
      else {
        layoutCL(label,fm,maxline,icon,viewR,iconR,textR,
         numLines*fontHeight,clippedMode);
      }
      int x1 = Math.min(iconR.x, textR.x);
      int x2 = Math.max(iconR.x + iconR.width, textR.x + textR.width);
      int y1 = Math.min(iconR.y, textR.y);
      int y2 = Math.max(iconR.y + iconR.height, textR.y + textR.height);
      Dimension rv = new Dimension(x2 - x1, y2 - y1);
      rv.width += dx;
      rv.height += dy;
      return new Dimension((int) (rv.width*1.0), rv.height);
    }
  }
}