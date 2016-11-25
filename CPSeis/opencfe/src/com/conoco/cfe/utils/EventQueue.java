package com.conoco.cfe.utils;

import javax.swing.SwingUtilities;

public class EventQueue {

  public static void invokeLater(Runnable r) {
    // JDK 1.1
    SwingUtilities.invokeLater(r);
    
    // JDK 1.2
    //java.awt.EventQueue.invokeLater(r);
  }

}
