// MultiLineLabelUI.java

package com.conoco.cfe.client.gui.controls.ui;

import java.awt.*;
import java.beans.*;
import java.util.*;

import javax.swing.*;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.BasicLabelUI;
import javax.swing.text.View;

/**
 * To add multiline label support to your swing applications, just add this
 * static call to your main method.  Note, you only need to do this once, even
 * if you change LookAndFeel as the UIManager knows not to overwrite the user
 * defaults.  Moreover, it uses the current L&F foreground/background colors
 * <p><pre>
 *        MultiLineLabelUI.initialize();
 * </pre><p>
 * There are two alignment modes, BLOCK and INDIVIDUAL, which can be set with
 * the MULTI_ALIGNMENT_MODE property.  If not specified or set to null, the
 * default is INDIVIDUAL.
 * <p><pre>
 *     label.putClientProperty(MULTI_ALIGNMENT_MODE,BLOCK);
 *     label.putClientProperty(MULTI_ALIGNMENT_MODE,INDIVIDUAL);
 * </pre></p>
 * By default, text is clipped.  This can be changed using the CLIPPED_MODE
 * property.
 * <p><pre>
 *     label.putClientProperty(CLIPPED_MODE,CLIPPED);
 *     label.putClientProperty(CLIPPED_MODE,NOT_CLIPPED);
 * </pre></p>
 * You can also specify if the label should be drawn vertically, by using the
 * ORIENTATION_MODE property.  
 * <p><pre>
 *     label.putClientProperty(ORIENTATION_MODE,HORIZONTAL);
 *     label.putClientProperty(ORIENTATION_MODE,VERTICAL);
 * </pre></p>
 * In vertical mode, MULTI_ALIGNMENT_MODE can be used for each column of text.
 * However, the CLIPPED_MODE property is ignored.  
 * @author Albert L. M. Ting
 */
public class MultiLineLabelUI extends BasicLabelUI {
  public static String MULTI_ALIGNMENT_MODE = "multiAlignmentMode";
  public static String BLOCK = "block";
  public static String INDIVIDUAL = "individual";

  public static String CLIPPED_MODE = "clippedMode";
  public static String CLIPPED = "clipped";
  public static String NOT_CLIPPED = "notClipped";

  public static String ORIENTATION_MODE = "orientationMode";
  public static String HORIZONTAL = "horizontal";
  public static String VERTICAL = "vertical";

  public static String VERTICAL_SPACE = "verticalSpace";
  public static String VERTICAL_WIDTH = "verticalWidth";

  protected static MultiLineLabelUI SINGLETON = new MultiLineLabelUI();

  private static String DEFAULT_ALIGNMENT_MODE = INDIVIDUAL;
  private static String DEFAULT_CLIPPED_MODE = CLIPPED;

  private static String DEFAULT_VERTICAL_WIDTH = "W";
  private static String DEFAULT_VERTICAL_SPACE = " ";

  public static void initialize() {
    // don't hardcode the class name, fetch it dynamically.  This way we can
    // obfuscate.
    String key = "LabelUI";
    Class cls = SINGLETON.getClass();
    String name = cls.getName();
    UIManager.put(key,name);
    UIManager.put(name,cls);
  }

  public void installUI(JComponent c) { 
    super.installUI(c);
  }

  public void uninstallUI(JComponent c) { 
    super.uninstallUI(c);
  }
    
  public static ComponentUI createUI(JComponent c) {
    return SINGLETON;
  }

  /**
   * Change the default alignment mode
   */
  public static void setDefaultAlignmentMode(String mode) {
    DEFAULT_ALIGNMENT_MODE = mode;
  }
  /**
   * Change the default clipped mode
   */
  public static void setDefaultClippedMode(String mode) {
    DEFAULT_CLIPPED_MODE = mode;
  }
  /**
   * Change the default vertical space.  Ideally, we would like to specify in
   * pixel sizes the space for each column of text.  That would require more
   * changes to SwingUtilities.layoutCompoundLabel than we'd like.  Instead,
   * we specify the space with a string template.
   */
  public static void setDefaultVerticalSpace(String space) {
    DEFAULT_VERTICAL_SPACE = space;
  }
  /**
   * Change the default vertical width
   */
  public static void setDefaultVerticalWidth(String width) {
    DEFAULT_VERTICAL_WIDTH = width;
  }

  protected boolean isIndividualAlign(JLabel label) {
    String val = (String)label.getClientProperty(MULTI_ALIGNMENT_MODE);
    if (val == null) {
      return DEFAULT_ALIGNMENT_MODE.equals(INDIVIDUAL);
    } 
    else {
      return val.equals(INDIVIDUAL);
    }
  }

  protected boolean isClippedMode(JLabel label) {
    String val = (String)label.getClientProperty(CLIPPED_MODE);
    if (val == null) {
      return DEFAULT_CLIPPED_MODE.equals(CLIPPED);
    } 
    else {
      return val.equals(CLIPPED);
    }
  }

  protected boolean isVerticalMode(JLabel label) {
    String val = (String)label.getClientProperty(ORIENTATION_MODE);
    if (val == null) {
      return false;
    } 
    else {
      return val.equals(VERTICAL);
    }
  }

  protected String getVerticalSpace(JLabel label) {
    String val = (String)label.getClientProperty(VERTICAL_SPACE);
    if (val == null) {
      return DEFAULT_VERTICAL_SPACE;
    } 
    else {
      return val;
    }
  }

  protected String getVerticalWidth(JLabel label) {
    String val = (String)label.getClientProperty(VERTICAL_WIDTH);
    if (val == null) {
      return DEFAULT_VERTICAL_WIDTH;
    } 
    else {
      return val;
    }
  }

  public void propertyChange(PropertyChangeEvent e) {
    if (e.getPropertyName().equals(MULTI_ALIGNMENT_MODE) ||
      e.getPropertyName().equals(CLIPPED_MODE) ||
      e.getPropertyName().equals(VERTICAL_SPACE) ||
      e.getPropertyName().equals(VERTICAL_WIDTH)) {
      JLabel label = (JLabel) e.getSource();
      label.repaint();
    } 
    else {
      super.propertyChange(e);
    }
  }

  /* override BasicLableUI version, since we need to pass textHeight 
   * forwards the call to UIUtility.layoutCompoundLabel().
   * This method is here to shorten the method call a little.
   */
  protected String layoutCL(JLabel label,           
                            FontMetrics fontMetrics, 
                            String text, 
                            Icon icon, 
                            Rectangle viewR, 
                            Rectangle iconR, 
                            Rectangle textR,
                            int textHeight,
                            boolean clippedMode)  {
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
                                         label.getIconTextGap(),
                                         textHeight,
                                         clippedMode);
  }

  /**
   * Paint clippedText at textX, textY with the labels foreground color.
   * Return true if the accelerator Character was printed
   * 
   * @see #paint
   * @see #paintDisabledText
   */
  protected boolean paintEnabledText(JLabel l, 
                                    Graphics g, String s, int textX, 
                                    int textY, int accChar) {
    g.setColor(l.getForeground());
    return UIUtility.drawString(g, s, accChar, textX, textY);
  }

  /**
   * Paint clippedText at textX, textY with background.lighter() and then 
   * shifted down and to the right by one pixel with background.darker().
   * Return true if the accelerator Character was printed
   * 
   * @see #paint
   * @see #paintEnabledText
   */
  protected boolean paintDisabledText(JLabel l, Graphics g, String s, 
                                      int textX, int textY, int accChar) {
    Color background = l.getBackground();
    g.setColor(background.brighter());
    /*res=*/UIUtility.drawString(g, s, accChar, textX, textY);
    g.setColor(background.darker());
    return UIUtility.drawString(g, s, accChar, textX + 1, textY + 1);
  }

  protected String getTotalVerticalWidth(JLabel label, int numLines) {
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

  /* These are shared between instances in paint() below, as allocating
   * Rectangles doubled the time that method took to run. */
  private Rectangle iconR = new Rectangle();
  private Rectangle textR = new Rectangle();
  private Rectangle viewR = new Rectangle();
  private static Insets viewInsets = new Insets(0, 0, 0, 0);
  public Dimension getPreferredSize(JComponent c) {
    JLabel label = (JLabel)c;
    String text = label.getText();
    Icon icon = label.getIcon();
    Insets insets = label.getInsets(viewInsets);
    Font font = label.getFont();
    int dx = insets.left + insets.right;
    int dy = insets.top + insets.bottom;
    if ((icon == null) && 
          ((text == null) || 
             ((text != null) && (font == null)))) {
      return new Dimension(dx, dy);
    }
    else if ((text == null) || ((icon != null) && (font == null))) {
      return new Dimension(icon.getIconWidth() + dx, 
                           icon.getIconHeight() + dy);
    }
    else {
      //++SBE
      FontMetrics fm = c.getFontMetrics(font);
      int fontHeight = fm.getHeight();
      String lines[] = UIUtility.breakupLines(text);
      int numLines = lines.length;
      String maxline = "";
      int maxwidth=0;
      boolean clippedMode = isClippedMode(label);
      boolean vertical = isVerticalMode(label);
      for (int i=0; i<numLines; i++) {
        int w = fm.stringWidth(lines[i]);
        if (w > maxwidth) {
          maxline = lines[i];
          maxwidth = w;
        }
      }
      //--SBE
      iconR.x = iconR.y = iconR.width = iconR.height = 0;
      textR.x = textR.y = textR.width = textR.height = 0;
      viewR.x = dx;
      viewR.y = dy;
      viewR.width = viewR.height = Short.MAX_VALUE;

      //++SBE
      if (vertical) {
        String totalWidth = getTotalVerticalWidth(label,numLines);
        clippedMode=false;
        layoutCL(label,fm,totalWidth,icon,viewR,iconR,textR,
         maxline.length()*fontHeight,clippedMode);
      } 
      else {
        layoutCL(label,fm,maxline,icon,viewR,iconR,textR,
         numLines*fontHeight,clippedMode);
      }
      //--SBE
      int x1 = Math.min(iconR.x, textR.x);
      int x2 = Math.max(iconR.x + iconR.width, textR.x + textR.width);
      int y1 = Math.min(iconR.y, textR.y);
      int y2 = Math.max(iconR.y + iconR.height, textR.y + textR.height);
      Dimension rv = new Dimension(x2 - x1, y2 - y1);
      rv.width += dx;
      rv.height += dy;
      return rv;
    }
  }

  /**
   * @return getPreferredSize(c)
   */
  public Dimension getMinimumSize(JComponent c) {
    return super.getMinimumSize(c);
  }

  /**
   * @return getPreferredSize(c)
   */
  public Dimension getMaximumSize(JComponent c) {
    return super.getMaximumSize(c);
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
    JLabel label = (JLabel)c;
    String text = label.getText();
    Icon icon = label.isEnabled() ? label.getIcon() : label.getDisabledIcon();
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
    boolean clippedMode = isClippedMode(label);
    boolean vertical = isVerticalMode(label);
    boolean individualAlign=isIndividualAlign(label);
    paintViewInsets = c.getInsets(paintViewInsets);
    paintViewR.x = paintViewInsets.left;
    paintViewR.y = paintViewInsets.top;
    paintViewR.width =
              c.getWidth() - (paintViewInsets.left + paintViewInsets.right);
    paintViewR.height =
              c.getHeight() - (paintViewInsets.top + paintViewInsets.bottom);
    paintIconR.x = paintIconR.y = paintIconR.width = paintIconR.height = 0;
    paintTextR.x = paintTextR.y = paintTextR.width = paintTextR.height = 0;
    // now get layout, for the maximum line with the total text height
    if (vertical) {
      String totalWidth = getTotalVerticalWidth(label,numLines);
      clippedMode=false;
      layoutCL(label,fm,totalWidth,icon,paintViewR,paintIconR,paintTextR,
               lines[maxline].length()*fontHeight,clippedMode);
    } 
    else {
      layoutCL(label, fm, lines[maxline], icon, paintViewR, paintIconR,
               paintTextR, fontHeight*numLines, clippedMode);
    }
    if (icon != null) {
      icon.paintIcon(c, g, paintIconR.x, paintIconR.y);
    }
    if (text != null) {
      int accChar = label.getDisplayedMnemonic();
      int textX = paintTextR.x;
      int textY = paintTextR.y + fontAscent;
      int textWidth = paintTextR.width;
      View v = (View) c.getClientProperty("html");
      if (v != null) {
        // use View renderer
        v.paint(g, paintTextR);
        } 
        else if (vertical) {
          // vertical text
          String space = getVerticalSpace(label);
          String width = getVerticalWidth(label);
          int pixelSpace = fm.stringWidth(space);
          int pixelWidth = fm.stringWidth(width);
          int pixelColumnWidth = pixelSpace+pixelWidth;
          char[] oneChar = new char[1];
          int i,j,k,strlen,offset;
          String oneCharStr;
          String line;
          for (i=0; i<numLines; i++, textX+=pixelColumnWidth) {
            line = lines[i];
            strlen = line.length();
            if (individualAlign) {
              oPaintIconR.x=oPaintIconR.y=oPaintIconR.width=oPaintIconR.height=0;
              oPaintTextR.x=oPaintTextR.y=oPaintTextR.width=oPaintTextR.height=0;
              /* relayout each line. Note, we're only concerned about y location.
               * so it doesn't matter what text we use to do the layout
               */
              line = layoutCL(label, fm, space, icon, paintViewR,
                  oPaintIconR, oPaintTextR,
                  strlen*fontHeight, clippedMode);
              textY = oPaintTextR.y + fontAscent;
            }
            for (j=0, k=textY; j<strlen; j++, k+=fontHeight) {
              oneChar[0] = lines[i].charAt(j);
              oneCharStr = new String(oneChar);
              // now center the character in the column
              offset = (pixelWidth-fm.stringWidth(oneCharStr))/2;
              if (label.isEnabled()) {
                if (paintEnabledText(label, g, oneCharStr, textX+offset, k, accChar)) {
                  accChar = '\0';
                }
              } 
              else {
                if (paintDisabledText(label, g, oneCharStr, textX+offset, k, accChar)) {
                  accChar = '\0';
                }
              }
            }
          }
        } 
        else {
          // horizontal text
          int textHeight = fontHeight*numLines;
          for (int i=0; i<numLines; i++, textY+=fontHeight) {
            String clipped = lines[i];
            if (individualAlign) {
              oPaintIconR.x=oPaintIconR.y=oPaintIconR.width=oPaintIconR.height=0;
              oPaintTextR.x=oPaintTextR.y=oPaintTextR.width=oPaintTextR.height=0;
              // now get layout, for the maximum line with the total text height
              clipped= layoutCL(label, fm, clipped, icon, paintViewR,
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
            if (label.isEnabled()) {
              if (paintEnabledText(label, g, clipped, textX, textY,accChar)) {
                accChar = '\0';
              }
            } 
            else {
              if (paintDisabledText(label, g, clipped, textX, textY,accChar)) {
                accChar = '\0';
              }
            }
          }
        }
      }
    }
  }