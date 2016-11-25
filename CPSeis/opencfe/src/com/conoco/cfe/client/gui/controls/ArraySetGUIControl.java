// ArraySetGUIControl.java

package com.conoco.cfe.client.gui.controls;

/**
 * Defines the interface of an arrayset GUI control. An arrayset
 * is a container of array components. 
 */
public interface ArraySetGUIControl  extends GUIControl {  
  /**
   * Adds a new column to this array set control
   */
  public ArrayGUIControl addColumn(String keyword, String type, int size, 
     int columnSize, boolean sizeMaxLength, String clabel, boolean editable, boolean sensitive);

  public ArrayGUIControl [] getColumns();  
}