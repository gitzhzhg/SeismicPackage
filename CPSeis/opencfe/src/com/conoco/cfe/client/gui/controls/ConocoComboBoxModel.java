// ConocoComboBoxModel.java

package com.conoco.cfe.client.gui.controls;

import java.io.Serializable;

import javax.swing.DefaultComboBoxModel;
import javax.swing.AbstractListModel;
import javax.swing.MutableComboBoxModel;

import javax.swing.event.*;

import java.util.Vector;

/**
 * A combo box model created here to get around the default
 * combo box behaviour. 
 */
public class ConocoComboBoxModel 
  extends AbstractListModel implements MutableComboBoxModel, Serializable {
  Vector objects;
  Object selectedObject;

  /**
   * Constructs an empty DefaultComboBoxModel object.
   */
  public ConocoComboBoxModel() {
    objects = new Vector();
  }

  /**
   * Constructs a DefaultComboBoxModel object initialized with
   * an array of objects.
   *
   * @param items  an array of Object objects
   */
  public ConocoComboBoxModel(final Object items[]) {
    objects = new Vector();
    objects.ensureCapacity( items.length );

    int i,c;
    for ( i=0,c=items.length;i<c;i++ )
      objects.addElement(items[i]);
    if ( getSize() > 0 ) {
      selectedObject = getElementAt( 0 );
    }
  }

  /**
   * Constructs a DefaultComboBoxModel object initialized with
   * a vector.
   *
   * @param v  a Vector object ...
   */
  public ConocoComboBoxModel(Vector v) {
    objects = v;
    if ( getSize() > 0 ) {
      selectedObject = getElementAt( 0 );
    }
  }
    
  /**
   * Add a listener to the list that's notified each time a change
   * to the data model occurs.
   * @param l the ListDataListener
   */  
  public void addListDataListener(ListDataListener l) {
    super.addListDataListener(l);  
  }

  // implements javax.swing.ComboBoxModel
  public void setSelectedItem(Object anObject) {
  if ( selectedObject != null ||  selectedObject == null && anObject != null ) {
    selectedObject = anObject;
    fireContentsChanged(this, -1, -1);
    }
  }

  // implements javax.swing.ComboBoxModel
  public Object getSelectedItem() {
    return selectedObject;
  }

  // implements javax.swing.ListModel
  public int getSize() {
    return objects.size();
  }

  // implements javax.swing.ListModel
  public Object getElementAt(int index) {
    if ( index >= 0 && index < objects.size() )
      return objects.elementAt(index);
    else
      return null;
  }

  /**
   * Returns the index-position of the specified object in the list.
   *
   * @param anObject  
   * @return an int representing the index position, where 0 is 
   *         the first position
   */
  public int getIndexOf(Object anObject) {
    return objects.indexOf(anObject);
  }

  // implements javax.swing.MutableComboBoxModel
  public void addElement(Object anObject) {
    objects.addElement(anObject);
    fireIntervalAdded(this,objects.size()-1, objects.size()-1);
    if ( objects.size() == 1 && selectedObject == null && anObject != null ) {
      setSelectedItem( anObject );
    }
  }

  // implements javax.swing.MutableComboBoxModel
  public void insertElementAt(Object anObject,int index) {
    objects.insertElementAt(anObject,index);
    fireIntervalAdded(this, index, index);
  }

  // implements javax.swing.MutableComboBoxModel
  public void removeElementAt(int index) {
    if ( getElementAt( index ) == selectedObject ) {
      if ( index == 0 ) {
        setSelectedItem( getSize() == 1 ? null : getElementAt( index + 1 ) );
      }
      else {
        setSelectedItem( getElementAt( index - 1 ) );
      }
    }
    objects.removeElementAt(index);
    fireIntervalRemoved(this, index, index);
  }

  // implements javax.swing.MutableComboBoxModel
  public void removeElement(Object anObject) {
    int index = objects.indexOf(anObject);
    if ( index != -1 ) {
      removeElementAt(index);
    }
  }

  /**
  * Empties the list.
  */
  public void removeAllElements() {
    if ( objects.size() > 0 ) {
      int firstIndex = 0;
      int lastIndex = objects.size() - 1;
      objects.removeAllElements();
      selectedObject = null;
      fireIntervalRemoved(this, firstIndex, lastIndex);
    }
  }
}