///
/// KillAppThread.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  3. 11-03-2003 SMCook   Made exit error message more specific.
///  2. 10-17-2003 SMCook   Eliminated loop and sleep() call within cancel().
///                         Added a setTitle() call to message dialog.
///  1. 10-08-2003 SMCook   Initial version.
///

package com.conoco.shared;

import com.conoco.cfe.client.application.Console;
import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;
import com.conoco.cfe.client.gui.controller.AppGUIController;

/**
 * Kills the application after a certain number of seconds.
 */
public class KillAppThread extends Thread {

  private boolean _showDialog, _breakNow, _runIsFinished;
  private float _warnMinutes, _killMinutes;
  private long _startMsec;

  public KillAppThread(
    float warnMinutes, float killMinutes, boolean showDialog) {

    _warnMinutes = warnMinutes;
    _killMinutes = killMinutes;
    _showDialog  = showDialog;

    if(_warnMinutes <= 0) {
      Console.logMessage(this, "bug: _warnMinutes value = " + _warnMinutes);
      System.exit(1);
    }
    if(_killMinutes <= 0) {
      Console.logMessage(this, "bug: _killMinutes value = " + _killMinutes);
      System.exit(1);
    }
    if(_killMinutes <= _warnMinutes) {
      Console.logMessage(this, "bug: _killMinutes <= _warnMinutes");
      System.exit(1);
    }
  }

  public void start() {
    _startMsec = System.currentTimeMillis();
    super.start();
  }

  public final void run() {
    ErrorInfoWarningDialog dlg = null;

    while(true) {

      if(_breakNow) {
        if(dlg != null) {
          dlg.setVisible(false);
          dlg.dispose();
        }
        break;
      }

      float currentMinutes = (System.currentTimeMillis()-_startMsec) / 60000f;

      if(currentMinutes > _killMinutes) {
        System.out.println(
          "KillAppThread: forcing exit after " + _killMinutes + " minutes total.");
        System.exit(0);
      }

      if(_showDialog && currentMinutes > _warnMinutes && dlg == null) {
        dlg = new ErrorInfoWarningDialog(
          AppGUIController._firstJFrame, false, false);
        dlg.setTitle("Application Response Time Warning");
        dlg.setMessages(
          new String[] {
            "This application is still waiting for information that it",
            "normally receives quickly.  This can happen if, for example,",
            "it is attempting to scan a particularly large file system, or",
            "it is possible that a disk node is down or that there's a",
            "network problem.  If the expected information is not received",
            "within " + (_killMinutes - _warnMinutes) +
            " more minutes, this application will exit.",
            "",
            "You should normally just WAIT rather than attempt to kill it",
            "manually, and you should refrain from firing up new copies of the",
            "application until you understand the nature of the problem." });
        dlg.setVisible(true);
      }

      try {
        sleep(50);
      }
      catch(InterruptedException e) {
      } 
    }

  _runIsFinished = true;
  }

  public final void cancel() {
    _breakNow = true;
  }

}
