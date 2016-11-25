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

//-------------------- sl_table_view.hh -------------------//
//-------------------- sl_table_view.hh -------------------//
//-------------------- sl_table_view.hh -------------------//

//             header file for the SLTableView class
//              derived from the SLSmartForm class
//                        subdirectory sl

     // Has an instance of an SLTableViewBox.
     // Has an instance of an SLRangeSelect.

#ifndef _SL_TABLE_VIEW_HH_
#define _SL_TABLE_VIEW_HH_

#include "sl/sl_smart_form.hh"

class SLTableViewBox;
class SLRangeSelect;
class SLpPush;

typedef void TableViewBoxLocateTrap
                      (void *data, long first, long last, long active);
typedef void TableViewBoxSelectTrap
                      (void *data, long column, Boolean selected);


class SLTableView : public SLSmartForm
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  SLTableViewBox *_databox;
  SLRangeSelect  *_range;
  SLSmartForm    *_sort;
  SLpPush        *_sort1;
  SLpPush        *_sort2;

  TableViewBoxLocateTrap *_locate_trap; // trap to call when columns change.
  void                   *_locate_data; // data to pass to _locate_trap.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  SLTableView (SLDelay *slparent, char *name, HelpCtx hctx = NULL,
               long nchar = 10, long ndec = 10,
               long nrowmax = 30, long ncolmax = 6);
  SLTableView (Widget    wparent, char *name, HelpCtx hctx = NULL,
               long nchar = 10, long ndec = 10,
               long nrowmax = 30, long ncolmax = 6);
  virtual ~SLTableView();

private:

  void constructorHelper (long nchar, long ndec,
                          long nrowmax, long ncolmax);

public:        // methods to set values

  void    newTable   (const float *array, long nrows, long ncolumns,
                      long nstride, long istart, long istop);
  void    emptyTable ();
  Boolean setFirstVisibleColumn (long first);
  Boolean setSelectedColumn     (long column, Boolean selected = TRUE);
  Boolean toggleSelectedColumn  (long column);
  Boolean selectTrueColumns     (long mincol, long maxcol,
                                              Boolean selected = TRUE);

public:       // methods to register traps

  void setLocateTrap (TableViewBoxLocateTrap *locate_trap,
                                        void *locate_data = NULL);
  void setSelectTrap (TableViewBoxSelectTrap *select_trap,
                                        void *select_data = NULL);

public:       // methods to get information

  long    firstDisplayableColumn                 ()  const;
  long    lastDisplayableColumn                  ()  const;
  long    firstVisibleColumn                     ()  const;
  long    lastVisibleColumn                      ()  const;
  long    activeColumn                           ()  const;
  long    numberOfColumns                        ()  const;
  long    numberOfAllSelectedColumns             ()  const;
  long    numberOfDisplayableSelectedColumns     ()  const;
  long    trueColumnFromCurrentColumn (long column)  const;
  long    currentColumnFromTrueColumn (long truecol) const;
  Boolean columnIsSelected            (long column)  const;

private:    // traps

  static void  calledFromDatabox(void *data, long first, long last,
                                                         long active);
  static void  calledFromRange  (void *data, long value);
  static void  sortTrap         (void *data, long ident);
  static char *label1Trap       (void *data);
  static char *label2Trap       (void *data);
  static long  sense1Trap       (void *data);
  static long  sense2Trap       (void *data);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
