// HelpSection.java

package com.conoco.cfe.client.gui.builder;

import java.util.Hashtable;

/**
 * A wrapper class that maintains help tips and help texts
 * for controls in a window control.
 */
public class HelpSection {
  
  /**
   * A lookup table for help tip
   * 
   * @serial
   */
  protected Hashtable _keywordToHelpTip;
  
  /**
   * A lookup table for help text
   * 
   * @serial
   */
  protected Hashtable _keywordToHelpText;
    
  /**
   * Constructs a new help section object
   */
  public HelpSection() {
    _keywordToHelpTip = new Hashtable();
    _keywordToHelpText = new Hashtable();
  }
  
  /**
   * Returns the help tip for the specified control.
   * 
   * @param keyword   the keyword of the control whose
   *           help tip is desired
   * @return the control's help tip
   */
  public String getHelpTip(String keyword) {
    return (String) _keywordToHelpTip.get(keyword);
  }
  
  /**
   * Returns the help tip for the specified control.
   * 
   * @param keyword   the keyword of the control whose
   *           help tip is desired
   * @return the control's help tip
   */
  public String getHelpText(String keyword) {
    return (String) _keywordToHelpText.get(keyword);
  }

  /**
   * Puts the help tip for a specified control in this
   * help section.
   * 
   * @param keyword the keyword of the control
   * @param tip the tip to be registered
   */
  public void putHelpTip(String keyword, String tip) {
    _keywordToHelpTip.put(keyword, tip);  
  }

  /**
   * Puts the help text for a specified control in this
   * help section.
   * 
   * @param keyword the keyword of the control
   * @param text the text to be registered
   */
  public void putHelpText(String keyword, String text) {
    _keywordToHelpText.put(keyword, text);  
  }
}