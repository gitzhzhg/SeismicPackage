// ArrayGUIControl.java

package com.conoco.cfe.client.gui.controls;

/**
 * Defines an interface for an array GUI control. An array
 * control has a multiple-data-items model. 
 */
public interface ArrayGUIControl {
      
  /**
   * Modifies the elements of this array component.
   * 
   * @param elements   the array of <code>java.lang.String</code>
   *          that describe the new data values
   * @param start the index of the first element to be modified
   * @param end the index of the last element to be modified
   */
  public void modifyElements(String[] elements, int start, int end);
  
  /**
   * Inserts the elements in this array control.
   * 
   * @param elements   the array of <code>java.lang.String</code>
   *          that describe the new data values
   * @param start the index of the first element to be inserted
   */
  public void insertElements(String[] elements, int start);

  /**
   * Deletes the elements from this array control.
   * 
   * @param start the index of the first element to be deleted
   * @param end the index of the last element to be deleted
   */
  public void deleteElements(int start, int end);   
   
  /**
    * To fill the combo component in the array.
    * 
    * @param elements   values to set in the combo box component
    */
  public void fillArrayCombo(String[] elements);
  
  
  /**
    * To clear cells when they are selected
    * 
    * @param elements   values to set in the combo box component
    */
  public void clearSelection();
  
  /**
   * Returns the elements contained by this array component.
   * 
   * @return   the elements contained by this array component
   *       as an array of <code>java.lang.String</code>
   */
  public String[] getElements();
  
  /**
   * Returns the number of rows in the array control.
   * 
   * @return the number of items in the array control
   */
  public int getRowCount();
  
  /**
   * Returns the index of the array element that has been 
   * modified.
   * 
   * @return the index of the element that has been modified
   */
  public int getChangedIndex();   
  
  /**
   * Returns the column header.
   * 
   * @return the column header string
   */
  public String getColumnName();
}