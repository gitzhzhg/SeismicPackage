// UIUtility.java
//UIUtility.java
package com.conoco.cfe.client.gui.controls.ui;

import java.awt.*;
import java.lang.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.text.View;

/**
 * A utility class that provides some static methods 
 * that are used by the UI classes.
 */
public class UIUtility implements SwingConstants {
  /**
   * A variable for the line separator
   * 
   * @serial
   */
  private static String LINE_SEPARATOR = System.getProperty("line.separator");
  
   /**
    * A variable for the line separator length
    * 
    * @serial
    */
   private static int LINE_SEPARATOR_LEN = LINE_SEPARATOR.length();

   /**
    * StringTokenizer(text,"\n") really does a "\n+" which is not what we want.
    * We also want it be based on the line.separator property.  So create our
    * own version.  We first attempt to divide by line.separator, then by "\n".
    * Ideally, we'd prefer to just break up by line.separator, but we need to
    * handle text that was defined in a properties file, with embedded \n.
    */
   public static String[] breakupLines(String text) {
     int len = text.length();
     if (len == 0) {
      return new String[] {""};
    } 
    else {
      Vector data = new Vector(10);
      int start=0;
      int i=0;
      while (i<len) {
        if (text.startsWith(LINE_SEPARATOR,i)) {
            data.addElement(text.substring(start,i));
            start=i+LINE_SEPARATOR_LEN;
            i=start;
        } 
        else if (text.charAt(i) == '\n') {
          data.addElement(text.substring(start,i));
          start=i+1;
          i=start;
        } 
        else {
          i++;
        }
      }
      if (start != len) {
        data.addElement(text.substring(start));
      }
      int numlines = data.size();
      String lines[] = new String[numlines];
      data.copyInto(lines);
      return lines;
    }
  }

  /**   
   * Computes and returns the location of the icons origin, the
   * location of origin of the text baseline, and a possibly clipped
   * version of the compound labels string.  Locations are computed
   * relative to the viewR rectangle.
   */
  public static String layoutCompoundLabel(JComponent c,
                                           FontMetrics fm,
                                           String text,
                                           Icon icon,
                                           int verticalAlignment,
                                           int horizontalAlignment,
                                           int verticalTextPosition,
                                           int horizontalTextPosition,
                                           Rectangle viewR,
                                           Rectangle iconR,
                                           Rectangle textR,
                                           int textIconGap,
                                           int textHeight,
                                           boolean clipIt
                                           ) {

    /* Initialize the icon bounds rectangle iconR.*/
    if (icon != null) {
      iconR.width = icon.getIconWidth();
      iconR.height = icon.getIconHeight();
    }
    else {
      iconR.width = iconR.height = 0;
    }

    /* Initialize the text bounds rectangle textR.  If a null
     * or and empty String was specified we substitute "" here
     * and use 0,0,0,0 for textR.
     */
    boolean textIsEmpty = (text == null) || text.equals("");
    View v = null;
    if (textIsEmpty) {
      textR.width = textR.height = 0;
      text = "";
    }
    else {
      v = (c != null) ? (View) c.getClientProperty("html") : null;
      if (v != null) {
        textR.width = (int) v.getPreferredSpan(View.X_AXIS);
        textR.height = (int) v.getPreferredSpan(View.Y_AXIS);
      } 
      else {
        textR.width = SwingUtilities.computeStringWidth(fm,text);        
        // store the text height rather than calling fm.getHeight()
        textR.height = textHeight;
      }
    }

    /* Unless both text and icon are non-null, we effectively ignore
     * the value of textIconGap.  The code that follows uses the
     * value of gap instead of textIconGap.
     */
    int gap = (textIsEmpty || (icon == null)) ? 0 : textIconGap;
    if (!textIsEmpty) {
      /* If the label text string is too wide to fit within the available
       * space "..." and as many characters as will fit will be
       * displayed instead.
       */
      int availTextWidth;
      if (horizontalTextPosition == CENTER) {
        availTextWidth = viewR.width;
      }
      else {
        availTextWidth = viewR.width - (iconR.width + gap);
      }
      //clip the text (if asked and required)
      if (clipIt && textR.width > availTextWidth) {
        if (v != null) {
          textR.width = availTextWidth;
        } 
        else {
          text = getClippedText(text,fm,availTextWidth);
          textR.width = SwingUtilities.computeStringWidth(fm,text);
        }
      }
    }

    /* Compute textR.x,y given the verticalTextPosition and
     * horizontalTextPosition properties
     */
    if (verticalTextPosition == TOP) {
      if (horizontalTextPosition != CENTER) {
        textR.y = 0;
      }
      else {
        textR.y = -(textR.height + gap);
      }
    }
    else if (verticalTextPosition == CENTER) {
      textR.y = (iconR.height / 2) - (textR.height / 2);
    }
    else { // (verticalTextPosition == BOTTOM)
      if (horizontalTextPosition != CENTER) {
        textR.y = iconR.height - textR.height;
      }
      else {
        textR.y = (iconR.height + gap);
      }
    }
    if (horizontalTextPosition == LEFT) {
      textR.x = -(textR.width + gap);
    }
    else if (horizontalTextPosition == CENTER) {
      textR.x = (iconR.width / 2) - (textR.width / 2);
    }
    else { // (horizontalTextPosition == RIGHT)
      textR.x = (iconR.width + gap);
    }

    /* labelR is the rectangle that contains iconR and textR.
     * Move it to its proper position given the labelAlignment
     * properties.
     *
     * To avoid actually allocating a Rectangle, Rectangle.union
     * has been inlined below.
     */
    int labelR_x = Math.min(iconR.x, textR.x);
    int labelR_width = Math.max(iconR.x + iconR.width,
                                textR.x + textR.width) - labelR_x;
    int labelR_y = Math.min(iconR.y, textR.y);
    int labelR_height = Math.max(iconR.y + iconR.height,
                                 textR.y + textR.height) - labelR_y;
    int dx, dy;
    if (verticalAlignment == TOP) {
      dy = viewR.y - labelR_y;
    }
    else if (verticalAlignment == CENTER) {
      dy = (viewR.y + (viewR.height / 2)) - (labelR_y + (labelR_height / 2));
    }
    else { // (verticalAlignment == BOTTOM)
      dy = (viewR.y + viewR.height) - (labelR_y + labelR_height);
    }
    if (horizontalAlignment == LEFT) {
      dx = viewR.x - labelR_x;
    }
    else if (horizontalAlignment == RIGHT) {
      dx = (viewR.x + viewR.width) - (labelR_x + labelR_width);
    }
    else { // (horizontalAlignment == CENTER)
      dx = (viewR.x + (viewR.width / 2)) -
      (labelR_x + (labelR_width / 2));
    }

    /* Translate textR and glypyR by dx,dy.
     */
    textR.x += dx;
    textR.y += dy;
    iconR.x += dx;
    iconR.y += dy;
    return text;
  }

  public static String getClippedText(String text,
                                      FontMetrics fm,
                                      int availTextWidth) {
    String clipString = "...";
    int totalWidth = SwingUtilities.computeStringWidth(fm,clipString);
    int nChars;
    for(nChars = 0; nChars < text.length(); nChars++) {
      totalWidth += fm.charWidth(text.charAt(nChars));
      if (totalWidth > availTextWidth) {
        break;
      }
    }
    return text.substring(0, nChars) + clipString;
  }

  /*  copied and modified from BasicGraphicsUtils
   *
   *  Draw a string with the graphics g at location (x,y) just like
   *  g.drawString() would.  The first occurence of underlineChar in text will
   *  be underlined. The matching is not case sensitive.  Returns true if the
   *  underlined character was drawn.  
   */
  public static boolean drawString(Graphics g,
                                   String text,
                                   int underlinedChar,
                                   int x,
                                   int y) {
    char lc,uc;
    int index=-1,lci,uci;
    if(underlinedChar != '\0') {
      uc = Character.toUpperCase((char)underlinedChar);
      lc = Character.toLowerCase((char)underlinedChar);
      uci = text.indexOf(uc);
      lci = text.indexOf(lc);
      if(uci == -1) {
        index = lci;
      }
      else if(lci == -1) {
        index = uci;
      }
      else {
        index = (lci < uci) ? lci : uci;
      }
    }
    g.drawString(text,x,y);
    if(index != -1) {
      FontMetrics fm = g.getFontMetrics();
      int underlineRectX = x + fm.stringWidth(text.substring(0,index));
      int underlineRectY = y;
      int underlineRectWidth = fm.charWidth(text.charAt(index));
      int underlineRectHeight = 1;
      g.fillRect(underlineRectX,underlineRectY + fm.getDescent() - 1,
       underlineRectWidth,underlineRectHeight);
    }
    return (index != -1);
  }
}