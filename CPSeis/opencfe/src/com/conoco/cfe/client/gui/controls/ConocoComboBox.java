// ConocoComboBox.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import java.awt.*;
import java.awt.event.*;

import java.beans.*;

import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;

import java.util.*;

import javax.accessibility.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.*;
import javax.swing.border.*;

/**
 * A modified combo box created to override some default 
 * combo box behavior, more specifically, this modified 
 * combo box sends a selection event even when an already
 * selected item is reselected.
 */
public class ConocoComboBox extends JComboBox {
  /**
   * Creates a JComboBox that takes its items from an existing ComboBoxModel.
   *
   * @param aModel the ComboBoxModel that provides the displayed list of items
   */
  public ConocoComboBox(ComboBoxModel aModel) {
    super(aModel);
  }

  /** 
   * Creates a JComboBox that contains the elements in the specified array.
   */
  public ConocoComboBox(final Object items[]) {
    super(items);
  }

  /**
   * Creates a JComboBox that contains the elements in the specified Vector.
   */
  public ConocoComboBox(Vector items) {
    super(items);
  }

  /**
   * Creates a JComboBox with a default data model.
   * The default data model is an empty list of objects. 
   * Use <code>addItem</code> to add items.
   */
  public ConocoComboBox() {
    super();
  }
  
  /** 
   * This method is overriden here to modify the default 
   * combo box behaviour. 
   *
   * @see javax.swing.event.ListDataListener
   */
  public void contentsChanged(ListDataEvent e) {
    ComboBoxModel mod = getModel();
    Object newSelectedItem = mod.getSelectedItem();
    if (selectedItemReminder == null) {
      if (newSelectedItem != null) {
        selectedItemChanged();
      }
    }
    else {
      selectedItemChanged();
    }
    if (!isEditable() && newSelectedItem != null ) {
      int i,c;
      boolean shouldResetSelectedItem = true;
      Object o;
      Object selectedItem = mod.getSelectedItem();
      for ( i=0,c=mod.getSize();i<c;i++ ) {
        o = mod.getElementAt(i);
        if ( o.equals(selectedItem)) {
          shouldResetSelectedItem = false;
          break;
        }
      }
      if (shouldResetSelectedItem) {
        if (mod.getSize()>0) {
          setSelectedIndex(0);
        }
        else {
          setSelectedItem(null);
        }
      }
    }
  }
}