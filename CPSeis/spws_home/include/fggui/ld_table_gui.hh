/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/

//------------------------ ld_table_gui.hh ---------------------//
//------------------------ ld_table_gui.hh ---------------------//
//------------------------ ld_table_gui.hh ---------------------//

//              header file for the LdTableGui class
//                derived from the SLDatabox class
//                        subdirectory fggui

 
#ifndef _LD_TABLE_GUI_HH_
#define _LD_TABLE_GUI_HH_

#include "sl/sl_databox.hh"
#include "geom/fg_inform.hh"
//////////#include "geom/fg_constants.hh"


class LdTableGui  :  public SLDatabox, public FgInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:    // also protected _fg in FgInform base class.

//////////  enum { NCOLUMNS = NUM_FIELD_FLAG_VARIABLES + 7 };

  class LdTopGui *_top;
  int             _table_option;
//////////  long            _sw[NCOLUMNS];     // switches for table columns.
  long           *_sw;               // switches for table columns.

  long            _use_which;        // used with QC tables.
  float           _use_this;         // used with QC tables.
  float           _inc_percent;      // used with QC tables.
  float           _cum_percent;      // used with QC tables.
  float           _azim_change;      // used with QC tables.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  LdTableGui (SLDelay *slparent, char *name, class FieldGeometry *fg,
                           class LdTopGui *top, int table_option);
  virtual ~LdTableGui();

  FieldGeometry *getFieldGeometry()  const  { return _fg; }
  LdTopGui      *getLdTopGui     ()  const  { return _top; }

public:    // get and set values.

  long  getUseWhich   ()           const  { return _use_which; }
  float getUseThis    ()           const  { return _use_this; }
  float getIncPercent ()           const  { return _inc_percent; }
  float getCumPercent ()           const  { return _cum_percent; }
  float getAzimChange ()           const  { return _azim_change; }

  void  setUseWhich   (long  value)       { _use_which   = value; }
  void  setUseThis    (float value)       { _use_this    = value; }
  void  setIncPercent (float value)       { _inc_percent = value; }
  void  setCumPercent (float value)       { _cum_percent = value; }
  void  setAzimChange (float value)       { _azim_change = value; }

private:

  static void promptTrap (void *data, long ident, long index,
                          char *value, long nread, char *endkey);

  static long   tableSwitchUpdate(void *data, long ident, long index);
  static long diamondSwitchUpdate(void *data, long ident, long index);
  static long  selectSwitchUpdate(void *data, long ident, long index);
  static long      srSwitchUpdate(void *data, long ident, long index);

  int addQcChoices();
  void makeHelper();

public:   // overrides FgInform

  virtual void postNewActiveFlag(FieldGeometry *fg, long ixl);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
