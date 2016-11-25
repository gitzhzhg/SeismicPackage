// ScreenViewportAdjuster.java

package com.conoco.cfe.client.gui.controls;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Dimension;

import javax.swing.JScrollPane;
import javax.swing.JViewport;

/**
 * Performs the task of adjusting the screen's scrollpane
 * viewport. The adjuster is created for screens with 
 * a scroll pane. The adjuster has the responsibility of
 * bringing the component into view if the component is 
 * totally or partially out of view and focus is requested
 * by the component.
 */
public class ScreenViewportAdjuster  implements GUIControlListener {
  /**
   * Variable for the screen control
   * 
   * @serial
   */
  protected ScreenControl _sc;
    
  /**
   * Constructs a new adjuster with the specified
   * screen control.
   * 
   * @param sc the screen control that is acted upon by this adjuster
   */
  public ScreenViewportAdjuster(ScreenControl sc) {
    _sc = sc;
  }
  
  /**
   * This is invoked after a component requests focus.
   *
   * @param e the event that is generated when the component gains focus
   */
  public void guiControlChanged(GUIControlEvent e) {
    if (e.getType() == GUIControlEvent.COMPONENT_FOCUS_EVENT) {
      GUIControl c = (GUIControl) e.getSource();
      JScrollPane  scroll = (JScrollPane) _sc.getComponent();
      JViewport vp = scroll.getViewport();
      Dimension extentSize = vp.getExtentSize();
      Point compPosLeft = (c.getComponent()).getLocation();
      Dimension compSize = (c.getComponent()).getSize();
      Point compPosRight = new Point(compPosLeft.x + compSize.width,
                                      compPosLeft.y + compSize.height);
      Point currentVPLeft = vp.getViewPosition();
            
      // return if the component is too big to fit 
      // in the viewport.. nothing to be done
      if ( ( compSize.width >= extentSize.width ) ||
                  (compSize.height >= extentSize.height ) ) {
        return;    
      }
      
      // keep on adjusting the viewport's left hand up corner 
      // till the component comes into view
      while (true) {
        // break as soon as the component comes into view
        if (   (compPosLeft.x >= currentVPLeft.x) &&
                    (compPosRight.x <= (currentVPLeft.x + extentSize.width)) &&
                    (compPosLeft.y >= currentVPLeft.y) &&
                    (compPosRight.y <= (currentVPLeft.y + extentSize.height)) ){
          break;
        }
        // adjust the viewport position depending upon 
        // how the component is hidden from the view
        if ( compPosLeft.x < currentVPLeft.x ) {
          currentVPLeft.x = compPosLeft.x-1;
        } 
        else if (compPosLeft.y < currentVPLeft.y ) {
          currentVPLeft.y = compPosLeft.y-1;
        } 
        else if (compPosRight.x > (currentVPLeft.x + extentSize.width) ) {
          currentVPLeft.x += (compPosRight.x - (currentVPLeft.x + extentSize.width))+1;
        } 
        else if ( compPosRight.y > (currentVPLeft.y + extentSize.height) ) {
          currentVPLeft.y += (compPosRight.y - (currentVPLeft.y + extentSize.height))+1;
        }
      }
      // adjust the viewport position
      vp.setViewPosition(currentVPLeft);
    }
  }
}