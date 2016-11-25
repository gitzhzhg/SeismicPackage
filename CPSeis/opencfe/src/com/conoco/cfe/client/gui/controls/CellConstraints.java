// CellConstraints.java

package com.conoco.cfe.client.gui.controls;

/**
 * A constraints object that is used by <code>com.conoco.cfe.client.gui.controls.CellLayout</code>.
 * The custom layout manager lays out components in their containers using this constraints object.
 * 
 * @see com.conoco.cfe.client.gui.controls.CellLayout
 */
public class CellConstraints {
  /**
   * Variable for the X-axis position
   * 
   * @serial
   */
  public int xPos      = 1;
  
  /**
   * Variable for the Y-axis position
   */
  public int yPos      = 1;
  
  /**
   * Variable for the X-axis size of the component
   * 
   * @serial
   */
  public int xSize    = 1;
  
  /**
   * Variable for the Y-axis size of the component
   */
  public int ySize    = 1;
  
  /**
   * Variable for the X-axis stretch of the component
   * 
   * @serial
   */
  public boolean xStretch = false;
  
  /**
   * Variable for the Y_axis stretch of the component
   * 
   * @serial
   */
  public boolean yStretch = false;
}