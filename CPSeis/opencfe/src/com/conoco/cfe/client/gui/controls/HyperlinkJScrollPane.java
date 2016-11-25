///
/// HyperlinkJScrollPane.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  1. 09-23-2003 SMCook   Initial version.
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.gui.JComponentFrame;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Window;

import javax.swing.JEditorPane;
import javax.swing.JScrollPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkEvent.EventType;
import javax.swing.event.HyperlinkListener;

public class HyperlinkJScrollPane
  extends JScrollPane implements HyperlinkListener {

  private JEditorPane _editor;
  private int _width, _height;
  private Window _owner;

  public HyperlinkJScrollPane(Window owner, String url, int width, int height) {

    _owner = owner;
    _width = width;
    _height = height;

    _editor = new JEditorPane();
    _editor.setEditable(false);
    _editor.addHyperlinkListener(this);

    setViewportView(_editor);

    setPage(url);
  }

  public final JEditorPane getEditor() {
    return _editor;
  }

  public void hyperlinkUpdate(HyperlinkEvent e) {
    HyperlinkEvent.EventType type = e.getEventType();
    if(type != type.ACTIVATED) return;

    if(e.getURL() != null) {
      JComponentFrame f = new JComponentFrame();
      Point p = _owner.getLocation();
      p.x += 30;
      p.y += 30;
      HyperlinkJScrollPane newPane = new HyperlinkJScrollPane(
        f, e.getURL().toString(), _width, _height);
      f.setComponent(newPane);
      f.setLocation(p.x, p.y);
      f.setSize(new Dimension(_width, _height));
      f.setVisible(true);
    }
  }

  private void setPage(String url) {
    if(url == null) return;
    try {
      _editor.setPage(url);
      repaint();
    } catch (Exception ex) {
      ex.printStackTrace();
      _editor.setText("Could not load url:\n\t" + url);
    }
  }

}
