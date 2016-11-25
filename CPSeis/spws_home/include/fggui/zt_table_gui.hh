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

//------------------------ zt_table_gui.hh ---------------------//
//------------------------ zt_table_gui.hh ---------------------//
//------------------------ zt_table_gui.hh ---------------------//

//              header file for the ZtTableGui class
//                derived from the SLDatabox class
//              also derived from the FgInform class
//                        subdirectory fggui

 
#ifndef _ZT_TABLE_GUI_HH_
#define _ZT_TABLE_GUI_HH_

#include "sl/sl_databox.hh"
#include "geom/fg_constants.hh"
#include "geom/fg_inform.hh"


class ZtTableGui  :  public SLDatabox, public FgInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:  // also protected _fg in FgInform

  int   _which;         // which ZT card set to show (1,2,3,4).
  int   _num_switches;  // number of switches for table columns.
  int   _first_ident;   // ident of first variable.
  int   _diamond_ident; // ident of last variable (active flag).
  int   _code_ident;    // ident of code variable.
  long *_sw;            // switches for table columns.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  ZtTableGui (SLDelay *slparent, char *name, class FieldGeometry *fg,
                                                 int which);
  virtual ~ZtTableGui();

private:     // override SLDatabox or FgInform.

  virtual void makeHelper();
  virtual void postNewActiveZt1Card(FieldGeometry *fg);
  virtual void postNewActiveZt2Card(FieldGeometry *fg);
  virtual void postNewActiveZt3Card(FieldGeometry *fg);
  virtual void postNewActiveZt4Card(FieldGeometry *fg);

private:     // convenience functions.

  long  numCards           ();
  long  insertCard         (long index);
  long  removeCard         (long index);
  long  appendCard         ();
  long  getCode            (long index);
  void  setCode            (long index, long code);
  long  getLongValue       (int ident, long index);
  void  setLongValue       (int ident, long index, long value);
  float getFloatValue      (int ident, long index);
  void  setFloatValue      (int ident, long index, float value);
  long  getActiveCardIndex ();
  void  setActiveCardIndex (long index);
  int   maybeAppendCard    (long index, long nread, char *endkey);

private:     // static functions.

  static void diamondTrap(void *data, long ident, long index,
                          long  value, long nread, char *endkey);

  static void codeTrap   (void *data, long ident, long index,
                          char *value, long nread, char *endkey);

  static void longTrap   (void *data, long ident, long index,
                          long  value, long nread, char *endkey);

  static void floatTrap  (void *data, long ident, long index,
                          float value, long nread, char *endkey);

  static void promptTrap (void *data, long ident, long index,
                          char *value, long nread, char *endkey);

  static long  diamondUpdate (void *data, long ident, long index);
  static char *codeUpdate    (void *data, long ident, long index);
  static long  nUpdate       (void *data);
  static long  nmaxUpdate    (void *data);
  static long  longUpdate    (void *data, long ident, long index);
  static float floatUpdate   (void *data, long ident, long index);
  static long  switchUpdate  (void *data, long ident, long index);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
