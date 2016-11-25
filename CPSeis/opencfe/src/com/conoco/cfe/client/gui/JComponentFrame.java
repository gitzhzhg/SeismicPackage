///
/// JComponentFrame.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  1. 09-23-2003 SMCook   Initial version.
///

package com.conoco.cfe.client.gui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * A JFrame that displays a JComponent passed by the caller.
 */
public class JComponentFrame
  extends JFrame
  implements ActionListener {
  
  protected JPanel _buttonPanel;
  protected JButton _okButton;

  /**
   */
  public JComponentFrame() {
    _buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER,100,5));

    _okButton = new JButton("Ok");
    _okButton.addActionListener(this);

    _buttonPanel.add(_okButton);
  }

  public void setComponent(JComponent comp) {
    getContentPane().removeAll();

    getContentPane().add(comp, BorderLayout.CENTER);
    getContentPane().add(_buttonPanel, BorderLayout.SOUTH);

    pack();
  }

  /**
   * Catch action events from button(s).
   * The dialog is disposed of when the user clicks a button.
   */
  public void actionPerformed(ActionEvent e) {
    Object src=e.getSource();
    if(src == _okButton) {
      dispose();
      return;
    }
  }

}
