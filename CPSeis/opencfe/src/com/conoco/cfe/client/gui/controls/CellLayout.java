// CellLayout.java

package com.conoco.cfe.client.gui.controls;

import java.awt.FontMetrics;
import java.awt.LayoutManager;
import java.awt.LayoutManager2;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;

import java.util.Hashtable;

import javax.swing.JTextField;

/**
 * A custom layout manager that lays out components 
 * in containers according to specific constraints.
 * 
 * @see com.conoco.cfe.client.gui.controls.CellConstraints
 * @see java.awt.LayoutManager2
 */
public class CellLayout implements LayoutManager2 {
  /**
   * Declares a variable for a lookup table that
   * maps a component with its constraints
   * 
   * @serial
   */
  Hashtable _table;
  
  /**
   * Declares a variable for the maximum number of 
   * columns in the container
   *
   * @serial
   */
  int _maxColumns = 0;
  
  /**
   * Declares a variable for the maximum number of
   * rows in the container
   * 
   * @serial
   */
  int _maxRows = 0;
  
  /**
   * Declares a boolean variable taht is set if 
   * layout needs to be updated
   * 
   * @serial
   */
  boolean _invalidateLayout = true;
  
  /**
   * Declares a variable for the horizontal gap 
   *
   * @serial
   */
  int hgap = 0;
  
  /**
   * Declares a variable for the vertical gap
   * 
   * @serial
   */
   int vgap = 4;
  
  /**
   * Declares a variable for the minimumCellWidth
   * 
   * @serial
   */
  int _minimumCellWidth;
   
  /**
   * Declares a variable for the minimumCellWidth
   * 
   * @serial
   */
  int _minimumCellHeight;
   
   
  /**
   * Constructs a new cell layout manager.
   * 
   * @param maxColumns the maximum number of columns 
   * @param maxRows the maximum number of rows
   */
  public CellLayout(int maxColumns, int maxRows, int minCellWidth, int minCellHeight) {
    _table = new Hashtable();
    _maxColumns = maxColumns;
    _maxRows = maxRows;
    _minimumCellWidth = minCellWidth;
    _minimumCellHeight = minCellHeight;
  }

  /**
   * Adds a component to this layout manager.
   * 
   * @param comp the component to be added
   * @param constraints   the constraints object that is to be
   *             used
   */
  public void addLayoutComponent(Component comp, Object constraints) {
    if (constraints == null) {
      constraints = new CellConstraints();
    }
      
    _table.put(comp, constraints);
  }
    
  /**
   */  
  public void addLayoutComponent(String name, Component comp) {
    System.out.println("AddLayoutComponent(String, Component)");
  }
    
  /**
   */
  public void removeLayoutComponent(Component comp) {
    System.out.println("RemoveLayoutComponent(Component)");
  }
     
  /**
   */ 
  public float getLayoutAlignmentX(Container target) {
    System.out.println("GetLayoutAlignmentX(Container)");
    return 0.0f;
  }

  /**
   */
  public float getLayoutAlignmentY(Container target) {
     System.out.println("GetLayoutAlignmentY(Container)");
     return 0.0f;
  }

  /**
   * Invalidates the layout.
   * 
   * @param target   the target container whose layout is to 
   *           updated
   */
  public void invalidateLayout(Container target) {
    _invalidateLayout = true;
  }

  /**
   * Returns the preferred layout size of the container.
   * 
   * @param parent the container whose preferred size is desired
   */
  public Dimension preferredLayoutSize(Container parent) {
    double cell_width = _minimumCellWidth;
    double cell_height = _minimumCellHeight;
      
    for (int i=0; i < parent.getComponentCount(); i++) {
      Component c = parent.getComponent(i);
      CellConstraints cell = (CellConstraints) _table.get(c);
      Dimension preferredSize = c.getPreferredSize();
      if (!cell.xStretch) {
        double w = preferredSize.width / cell.xSize;
        if (w > cell_width) 
          cell_width = w;
      }
      if (!cell.yStretch) {
        double h = preferredSize.height / cell.ySize;
         if (h > cell_height) 
           cell_height = h;
      }
     }
    int maxCellWidth = (int) 2.00*_minimumCellWidth;
    int maxCellHeight = (int) 2.00*_minimumCellHeight;
    if ( cell_width > maxCellWidth ) {
      cell_width = maxCellWidth;
    } 
    if ( cell_height > maxCellHeight ) {
      cell_height = maxCellHeight;
    }   
    Dimension calDim = new Dimension((int) (_maxColumns * cell_width), (int) (_maxRows * cell_height));      
    return calDim;
  }
    
    protected int compareDims(Dimension dim1, Dimension dim2) {
      
      if (   (dim1.width > dim2.width) ||
          (dim1.height > dim2.height) ) {
            return 1;
      } else {
        return -1;
      }
    }

  /**
   * Returns the minimum layout size of the container.
   * 
   * @param parent the container whose minimum size is desired
   * @return the minimum size as a <code>java.awt.Dimension<code>
   */
    public Dimension minimumLayoutSize(Container parent) {
      return new Dimension(_maxColumns * _minimumCellWidth,
                         _maxRows    * _minimumCellHeight);
    }
    
  /**
   * Returns the maximum layout size of the container.
   * 
   * @param parent the container whose maximum size is desired
   * @return the maximum size as a <code>java.awt.Dimension<code>
   */
    public Dimension maximumLayoutSize(Container target) {
      System.out.println("MaximumLayoutSize(Container)");
      return new Dimension(100,100);
    }
  
  /**
   * Lays out the components inside the container.
   * 
   * @param parent the container 
   */
    public void layoutContainer(Container parent) {
    if (_invalidateLayout) {
      _invalidateLayout = false;
      
      Dimension s = parent.getSize();
            
      int cellx = s.width / _maxColumns;
      int celly = s.height / _maxRows;
      
      for (int i = 0; i < parent.getComponentCount(); i++) {
        Component c = parent.getComponent(i);
        CellConstraints cell = (CellConstraints) _table.get(c);
        Dimension d = c.getPreferredSize();
        
        int width = d.width;
        int height = d.height;
        
        if (cell.xStretch) {
          width = cell.xSize * cellx;
        }
        
        if (cell.yStretch) {
          height = cell.ySize * celly;
        }
              
        c.setBounds(cell.xPos * cellx,
              cell.yPos * celly + vgap / 2,
              width, height);
      }
    }
    }
}