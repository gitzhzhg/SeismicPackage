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

//------------------------ rp_table_gui.hh ---------------------//
//------------------------ rp_table_gui.hh ---------------------//
//------------------------ rp_table_gui.hh ---------------------//

//              header file for the RpTableGui class
//                derived from the SLDatabox class
//              also derived from the FgInform class
//                        subdirectory fggui

 
#ifndef _RP_TABLE_GUI_HH_
#define _RP_TABLE_GUI_HH_

#include "sl/sl_databox.hh"
#include "geom/fg_constants.hh"
#include "geom/fg_inform.hh"


class RpTableGui  :  public SLDatabox, public FgInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:  // also protected _fg in FgInform

  enum { NCOLUMNS = NUM_RP_VARIABLES + 1 };
 
  long _sw[NCOLUMNS];           // switches for table columns.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  RpTableGui (SLDelay *slparent, char *name, class FieldGeometry *fg);
  virtual ~RpTableGui();

  virtual void postNewActiveRpCard(FieldGeometry *fg);  // overrides FgInform

private:

  static void promptTrap (void *data, long ident, long index,
                          char *value, long nread, char *endkey);

  static long   tableSwitchUpdate (void *data, long ident, long index);
  static long diamondSwitchUpdate (void *data, long ident, long index);

  void makeHelper();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
