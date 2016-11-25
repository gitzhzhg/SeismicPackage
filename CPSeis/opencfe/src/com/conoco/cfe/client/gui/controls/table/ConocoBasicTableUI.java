///
/// ConocoBasicTableUI.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  6.
///  5. 08-02-2002 SMCook   Completely replaced old version, extending from
///                          BasicTableUI instead of TableUI.  The old version
///                          was over 1200 lines long.  It was "low-level",
///                          manually performing mundane tasks like painting
///                          the table's grid lines, etc.  It was not taking
///                          full advantage of standard Java widgets, and was
///                          commonly in need of maintenance.
///  4. 10-08-2001 SMCook   Fixed Windows platform paint bug in preparation for
///                          cutting new CD -- commented out logic causing
///                          isSpecialRedrawCase to be 'true' for Windows
///                          platform.  Change is related to shift to 1.1.8.
///  3. 10-04-2001 SMCook   Reinstated this class in code tree (and used in
///                          class ArrayComponent as before).
///

package com.conoco.cfe.client.gui.controls.table;

import javax.swing.plaf.basic.BasicTableUI;

public class ConocoBasicTableUI extends BasicTableUI {

  /**
   * Constructor 1 of 1.
   */
  public ConocoBasicTableUI() {
  }

}
