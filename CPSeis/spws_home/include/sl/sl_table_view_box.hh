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

//------------------------ sl_table_view_box.hh ---------------------//
//------------------------ sl_table_view_box.hh ---------------------//
//------------------------ sl_table_view_box.hh ---------------------//

//              header file for the SLTableViewBox class
//                derived from the SLDatabox class
//                         subdirectory sl

     // Each column in _array contains _nrow numbers.
     // The first column in _array starts at _array[0].
     // The second column in _array starts at _array[_nstride].
     // Etc.
     // Strides thru _array and _prompt are from one column to the next.
     // Stride thru _array should be >= _nrows.
     // Stride thru _prompt is _nchar + 1.
 
#ifndef _SL_TABLE_VIEW_BOX_HH_
#define _SL_TABLE_VIEW_BOX_HH_

#include "sl/sl_databox.hh"


typedef void TableViewBoxLocateTrap
                        (void *data, long first, long last, long active);
typedef void TableViewBoxSelectTrap
                        (void *data, long column, Boolean selected);

class TableViewData;

class SLTableViewBox  :  public SLDatabox
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  const long _NCHAR;     // number of displayed characters.
  const long _NDEC;      // number of displayed decimals.
  const long _NROWMAX;   // maximum number of displayed rows.
  const long _NCOLMAX;   // maximum number of displayed columns.
  TableViewData *_tvdata;
  long _nrows;

private:    // pointers to registered traps

  TableViewBoxLocateTrap *_locate_trap; // trap to call when columns change.
  void                   *_locate_data; // data to pass to _locate_trap.
  TableViewBoxSelectTrap *_select_trap; // trap to call when column selected.
  void                   *_select_data; // data to pass to _select_trap.

private:    // variables corresponding to displayed columns

  char   *_prompt; // pointer to _NCOLMAX prompts with (_NCHAR+1) chars each.
  float **_value;  // pointer to _NCOLMAX value pointers.
  long   *_psw;    // pointer to _NCOLMAX prompt switches.
  long   *_asw;    // pointer to _NCOLMAX array  switches.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  SLTableViewBox (SLDelay *slparent, char *name, HelpCtx hctx = NULL,
               long nchar = 10, long ndec = 10,
               long nrowmax = 30, long ncolmax = 6);
  SLTableViewBox (Widget    wparent, char *name, HelpCtx hctx = NULL,
               long nchar = 10, long ndec = 10,
               long nrowmax = 30, long ncolmax = 6);
  virtual ~SLTableViewBox();

public:        // methods to set values

  void    newTable   (const float *array, long nrows, long ncolumns,
                      long nstride, long istart, long istop);
  void    emptyTable ();
  Boolean setFirstVisibleColumn   (long first);
  Boolean setSelectedColumn       (long column, Boolean selected = TRUE);
  Boolean toggleSelectedColumn    (long column);
  void    sortAllColumns             ();
  void    alignDisplayableSelections ();

private:        // internal methods to set values

  Boolean setFirstVisibleColumn (long first,  Boolean internal);
  Boolean setSelectedColumn     (long column, Boolean selected,
                                              Boolean internal);
  Boolean toggleSelectedColumn  (long column, Boolean internal);
  void    showFocus();

public:       // methods to register traps

  void setLocateTrap (TableViewBoxLocateTrap *locate_trap,
                                        void *locate_data = NULL);
  void setSelectTrap (TableViewBoxSelectTrap *select_trap,
                                        void *select_data  = NULL); 

public:       // methods to get information

  long    firstDisplayableColumn                 ()  const;
  long    lastDisplayableColumn                  ()  const;
  long    firstVisibleColumn                     ()  const;
  long    lastVisibleColumn                      ()  const;
  long    activeColumn                           ()  const;
  long    numberOfVisibleColumns                 ();
  long    numberOfColumns                        ()  const;
  long    numberOfAllSelectedColumns             ()  const;
  long    numberOfDisplayableSelectedColumns     ()  const;
  long    trueColumnFromCurrentColumn (long column)  const;
  long    currentColumnFromTrueColumn (long truecol) const;
  Boolean columnIsSelected            (long column)  const;

private:

  void constructorHelper    ();
  void move                 ();
  void reregister           ();
  void makeHelper           ();
  void switchColumns        (long lo, long up);
  void selectionHasChanged  (long column, Boolean internal);
  void callLocateTrap       ();
  void callSelectTrap       (long column);
  void tableTrapHelper      (long *ident, long *index, char *endkey);

  static void tableTrap (void *box, long *ident, long *index,
                          char *text, long *nread, char *endkey);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
