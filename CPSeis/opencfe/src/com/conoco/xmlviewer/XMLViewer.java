package com.conoco.xmlviewer;

///
/// XMLViewer.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  2.
///  1. 09-26-2001 SMCook   Detabbed & removed DOS carriage returns, then
///                          added this file to the code tree, hoping to reduce
///                          maintenance, and enabling easier deployment on CD.
///

import java.awt.Window;
import java.awt.event.*;

import com.conoco.cfe.client.*;
import com.conoco.cfe.client.gui.*;
import com.conoco.cfe.client.gui.builder.*;
import com.conoco.cfe.client.gui.controls.*;

import java.util.*;
import javax.swing.*;

public class XMLViewer {
  
  AppGUIDecoder _decoder;
  AppGUIBuilder _builder;
  
  public XMLViewer(String file) {
    
    _builder = new AppGUIBuilder();
    _decoder = new AppGUIDecoder();
    _decoder.setActionHandlers(_builder.getActionHandlers());
    
    _decoder.setFilename(file);
    
    Hashtable h = _builder.getGUIControls();
    
    Enumeration en = h.elements();
    
    while ( en.hasMoreElements()) {    
      
      GUIControl c = (GUIControl) (en.nextElement());
      if ( c != null ) {
        c.addGUIControlListener(new MyListener());
      }
    }
    
    Window w = (Window) _builder.getTopLevelWindow().getComponent();
    
    JMenu help = new JMenu("DISPLAY");
    JMenuItem helpWin = new JMenuItem("Help Window");
    help.add(helpWin);
    helpWin.addActionListener(
      new HelpWindowAction());
    
    if ( w instanceof JFrame ) {
      JMenuBar mb = ((JFrame) w).getJMenuBar();
      if ( mb != null ) {
        mb.add(help);  
      } else {
        JMenuBar newmb = new JMenuBar();
        ((JFrame) w).setJMenuBar(newmb);
        newmb.add(help);      
      }
    } else if ( w instanceof JDialog ) {
      JMenuBar mb = ((JFrame) w).getJMenuBar();
      if ( mb != null ) {
        mb.add(help);  
      } else {
        JMenuBar newmb = new JMenuBar();
        ((JDialog) w).setJMenuBar(newmb);
        newmb.add(help);      
      }
    }
    
    w.addWindowListener( new Close());
//    w.setSize( w.getPreferredSize());
    w.setSize(1024,768);   //SMCook
    w.setVisible(true);
    
  }
  
  public static void main(String[] args) {
  
    if ( args.length == 0 ) {
      
      System.out.println("Usage: XMLViewer fileName");
      System.exit(1);  
        
    }
    
    Client.init();
    
    new XMLViewer(args[0]);  
    
  }

  class HelpWindowAction implements ActionListener {
    
    public void actionPerformed(ActionEvent e) {
    
      HelpWindowControl.show();  
      
    }  
    
  }
  
  class Close extends WindowAdapter {
  
    public void windowClosing(WindowEvent e) {
      
      System.exit(0);  
      
    }  
    
  }
  
  /**
   * Listens only to the top level window focus events. Updates the 
   * help section object based on the window thats under focus.
   */
  private class MyListener implements GUIControlListener {
              
    /**
     * This method is invoked when a window or a control gains focus.
     * 
     * @param e the event that is generated when a component gains focus
     */
    public void guiControlChanged(GUIControlEvent e) {
      
      int type = e.getType();
      
      if ( (type == GUIControlEvent.COMPONENT_FOCUS_EVENT)
         || (type == GUIControlEvent.MOUSE_ENTERED_EVENT) ) {

          updateHelp((GUIControl) e.getSource());
                
      }
      
    }
    
    /**
     * Updates the help window.
     */
    private void updateHelpWindow(int winId) {
      
      HelpSection hs = _builder.getHelpSection();
      
      String winHelpText = hs.getHelpText(
        _builder.getTopLevelWindow().getKeyword());
      
      HelpWindowControl.setProcessHelp(winHelpText);
      
    }
    
    /**
     * Updates the help panel for displaying the help for the
     * component that is under focus. This method gets invoked
     * when a component gains focus. 
     * 
     * @param c the GUI control that has gained the focus
     */
    private void updateHelp(GUIControl c) {
    
      try {
        String key = c.getKeyword();
      
        JPanel helpPanel = _builder.getTopLevelWindow().getHelpPanel();
        HelpSection currentHelpSection = _builder.getHelpSection();
        String t = currentHelpSection.getHelpTip(key.toUpperCase());                
        String text = currentHelpSection.getHelpText(key.toUpperCase());
        
        JLabel l = (JLabel) helpPanel.getComponents()[0];
        
        if ( t != null ) {
          l.setText(t);
        } else {
          l.setText(" ");
        }
        
        HelpWindowControl.setContextHelp(text);
        
        _builder.getTopLevelWindow().getComponent().invalidate();
        _builder.getTopLevelWindow().getComponent().validate();
        _builder.getTopLevelWindow().getComponent().repaint();  
        
      } catch ( Exception en ) {

        //en.printStackTrace();
        //Console.logMessage("AppGUIController: Error in updating help tip");        
        return;
      }      

    }
        
  }
  
  
}
