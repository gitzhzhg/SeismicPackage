///
/// FocusWatchingThread.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  2.
///  1. 09-05-2002 SMCook   Original version.  Useful for AWT/Swing debugging.
///

/**
 * As of 1.4, it's particularly easy to query which component has the focus.
 * This thread prints this information out (every 2 seconds, for example).
 */

package com.conoco.shared;

import com.conoco.cfe.client.application.Console;

import java.awt.KeyboardFocusManager;

public class FocusWatchingThread extends Thread {

  private int _msecToSleep;

  /**
   * Constructor 1 of 1 - takes sleep milliseconds as argument.
   */
  public FocusWatchingThread(int msecToSleep) {
    _msecToSleep = msecToSleep;
  }

  /**
   * Continually print information about where the focus is.
   */
  public void run() {

    while(true) {

      try {
        KeyboardFocusManager manager =
          KeyboardFocusManager.getCurrentKeyboardFocusManager();

        //Object window = manager.getFocusedWindow();
        Object owner  = manager.getFocusOwner();

        //System.err.println("");
        //System.err.println("focused window = " + window.getClass().getName());
        System.err.println("focus owner    = " + owner.getClass().getName());
      }
      catch(Exception e) {
        System.err.println("focus exception = " + e.getMessage());
      }

      try {
        Thread.sleep(_msecToSleep);
      }
      catch(InterruptedException e) {
      }
    }
  }

}
