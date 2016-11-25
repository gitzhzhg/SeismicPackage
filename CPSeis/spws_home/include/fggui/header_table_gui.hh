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

//------------------------ header_table_gui.hh ---------------------//
//------------------------ header_table_gui.hh ---------------------//
//------------------------ header_table_gui.hh ---------------------//

//              header file for the HeaderTableGui class
//               derived from the SLMatrixViewBox class
//                also derived from the FgInform class
//                         subdirectory fggui

     // Displays trace headers from FieldGeometry.

 
#ifndef _HEADER_TABLE_GUI_HH_
#define _HEADER_TABLE_GUI_HH_

#include "sl/sl_matrix_view_box.hh"
#include "geom/fg_inform.hh"


class HeaderTableGui  :  public SLMatrixViewBox, public FgInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:    // also protected _fg in FgInform.

  class HeaderTopGui  *_top;

  long *_hold;      // list of user-chosen hold flags.
  int  *_toggled;   // list of true-false header toggles for sorting.
  int  *_order;     // list of current sorted order of headers.
  int   _ascending; // whether current header order is ascending (T/F).

  long _trace;         // original trace number which has been calculated.
  long _current_trace; // corresponding trace number in current gather.
  long _which_source;  // which source at flag to show (1 or more).
  long _show_choice;   // which type of collection of traces to show (enum).
  long _scroll_choice; // which type of card to scroll to (enum).
  long _more_choice;   // calculate all or some headers (enum).
  long _round_choice;  // whether to show rounded values (TRUE or FALSE).

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor.

  HeaderTableGui (SLDelay *slparent, char *name,
                  class FieldGeometry *fg,
                  class HeaderTopGui  *top);
  virtual ~HeaderTableGui();

  long           getWhichSource  ()  const;
  long           getShowChoice   ()  const  { return _show_choice; }
  long           getScrollChoice ()  const  { return _scroll_choice; }
  long           getMore         ()  const  { return _more_choice; }
  long           getRoundChoice  ()  const  { return _round_choice; }
  void           setWhichSource  (long value);
  void           setShowChoice   (long value);
  void           setScrollChoice (long value);
  void           setMoreChoice   (long value);
  void           setRoundChoice  (long value);
  void           sortHeaders     ();

protected:     //  virtual functions overriding SLMatrixViewBox.

  virtual void   newFirstVisibleColumn (long first);
  virtual void   newNumVisibleColumns  (long numvis);

  virtual long   numRowsUpdate         ();
  virtual long   numColumnsUpdate      ();
  virtual void   readyToUpdate         ();

  virtual double matrixUpdate          (long irow, long icol);
  virtual char  *promptUpdate                     (long icol);
  virtual long   integerUpdate         (long irow);
  virtual char  *messageUpdate         (long irow);
  virtual long   integerSwitchUpdate   (long irow);

  virtual void   matrixTrap (long irow, long icol,  double dvar,
                                        long nread, char* endkey);
  virtual void   integerTrap(long irow,             long   ivar,
                                        long nread, char* endkey);

  virtual void   makeHelper();

protected:     //  virtual functions overriding FgInform.

  virtual void postNewActiveTrace  (FieldGeometry *fg);
  virtual void finishedChanges     (FieldGeometry *fg);

private:       //  static functions.

  static void  staticHoldTrap    (void *data, long ident, long index,
                                  long  ivar, long nread, char* endkey);
  static long  staticHoldUpdate  (void *data, long ident, long index);

  static void  staticScrollTrap  (void *data, long ident, long index,
                                  char *cvar, long nread, char* endkey);
  static char *staticScrollMessageUpdate
                                 (void *data, long ident, long index);
  static long  staticScrollSwitchUpdate
                                 (void *data, long ident, long index);

private:

  int  getHeaderWordNumberFromRow      (long irow)           const;
  long getCurrentTraceNumberFromColumn (long icol)           const;
  long getOriginalTraceNumber          (long current_trace)  const;
  long getCurrentTraceNumber           (long trace)          const;
  void calculateHeaders                (long current_trace);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
