///
/// FontJList.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  2.
///  1. 09-05-2002 SMCook   Original version.  Program to view installed fonts.
///

package com.conoco.shared;

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.util.Vector;

import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

public class FontJList extends JList implements ListSelectionListener {

  private static JLabel sampleLabel = new JLabel("[none selected]");

  public FontJList(Object[] o) {
    super(o);
    addListSelectionListener(this);
  }

  public static Font[] getAllFonts() {
    return
      GraphicsEnvironment.
        getLocalGraphicsEnvironment().getAllFonts();
  }

  public static String[] getAvailableFontFamilyNames() {
    return
      GraphicsEnvironment.
        getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
  }

  public void valueChanged(ListSelectionEvent e) {
    if(e.getSource() == this) {
      String name = (String)getSelectedValue();
      Font font = new Font(name, Font.PLAIN, 20);

      sampleLabel.setText(
        "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz012345678");

      sampleLabel.setFont(font);
    }
  }

  public static void main(String [] args) {
    JList THIS = new FontJList(getAvailableFontFamilyNames());

    JFrame frame = new JFrame();

    JScrollPane scrollPane = new JScrollPane();
    scrollPane.getViewport().setView(THIS);

    frame.getContentPane().setLayout(new BorderLayout());
    frame.getContentPane().add("North", sampleLabel);
    frame.getContentPane().add("Center", scrollPane);

    frame.setSize(400, 800);
    frame.setVisible(true);
  }
}

