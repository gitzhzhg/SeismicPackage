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

//---------------------- sl_table_view.cc -----------------------//
//---------------------- sl_table_view.cc -----------------------//
//---------------------- sl_table_view.cc -----------------------//

//         implementation file for the SLTableView class
//              derived from the SLSmartForm class
//                       subdirectory sl


#include "sl/sl_table_view.hh"
#include "sl/sl_table_view_box.hh"
#include "sl/sl_range_select.hh"
#include "sl/slp_push.hh"
#include "named_constants.h"
#include <stdio.h>
//#include <iostream.h>


//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

SLTableView::SLTableView(SLDelay *slparent, char *name,
                                  HelpCtx hctx,
                                  long nchar, long ndec,
                                  long nrowmax, long ncolmax)
        : SLSmartForm(slparent, name, hctx)
{
  constructorHelper(nchar, ndec, nrowmax, ncolmax);
}


SLTableView::SLTableView(Widget wparent, char *name,
                                  HelpCtx hctx,
                                  long nchar, long ndec,
                                  long nrowmax, long ncolmax)
        : SLSmartForm(wparent, name, hctx)
{
  constructorHelper(nchar, ndec, nrowmax, ncolmax);
}


SLTableView::~SLTableView(void)
{
}


//--------------------- constructor helper ---------------------//
//--------------------- constructor helper ---------------------//
//--------------------- constructor helper ---------------------//

void SLTableView::constructorHelper(long nchar, long ndec,
                                       long nrowmax, long ncolmax)
{
  _databox = new SLTableViewBox(this, "databox", NULL,
                                  nchar, ndec, nrowmax, ncolmax);
  _range   = new SLRangeSelect (this, "range");
  _sort    = new SLSmartForm   (this, "sort");
  _sort1   = new SLpPush       (_sort, "Sort By Columns" , 1);
  _sort2   = new SLpPush       (_sort, "Align Selections", 2);

  _range->setPage(ncolmax);
  _range->setNoValues();
  _range->setRangeSelectTrap(calledFromRange, this);
  _databox->setLocateTrap(calledFromDatabox, this);
  _sort->showEvenSpacing();
  _sort1->setAtrap     (sortTrap  , this);
  _sort2->setAtrap     (sortTrap  , this);
  _sort1->setupLabelFun(label1Trap, this);
  _sort2->setupLabelFun(label2Trap, this);
  _sort1->setupSenseFun(sense1Trap, this);
  _sort2->setupSenseFun(sense2Trap, this);

  attach(_range  , this, this,  this   , NULL,  0,  0,  0, 0);
  attach(_sort   , this, this, _range  , NULL,  0,  0, 10, 0);
  attach(_databox, this, this, _sort   , this,  0,  0, 10, 0);
  if(made()) make();
  updateChildren();
}


//--------------------- set values --------------------------//
//--------------------- set values --------------------------//
//--------------------- set values --------------------------//

void SLTableView::newTable(const float *array, long nrows, long ncolumns,
                           long nstride, long istart, long istop)
{
  _databox->newTable(array, nrows, ncolumns, nstride, istart, istop);
  long first = _databox->firstVisibleColumn();
  istart     = _databox->firstDisplayableColumn();
  istop      = _databox->lastDisplayableColumn();
  _range->setValues(first, istart, istop);
  updateChildren();
}


void SLTableView::emptyTable()
{
  _databox->emptyTable();
  _range->setNoValues();
  updateChildren();
}


Boolean SLTableView::setFirstVisibleColumn(long first)
{
  Boolean changed = _databox->setFirstVisibleColumn(first);
  if(!changed) return FALSE;
  first = _databox->firstVisibleColumn();
  _range->setValue(first);
  return TRUE;
}


Boolean SLTableView::setSelectedColumn(long column, Boolean selected)
{
  return _databox->setSelectedColumn(column, selected);
}


Boolean SLTableView::toggleSelectedColumn(long column)
{
  return _databox->toggleSelectedColumn(column);
}



//------------------- select true columns ----------------------//
//------------------- select true columns ----------------------//
//------------------- select true columns ----------------------//

   // Might also reset first visible column if selected == TRUE.
   // Returns TRUE if first visible column is actually reset.

Boolean SLTableView::selectTrueColumns(long mincol, long maxcol,
                                        Boolean selected)
{
  long first  = firstVisibleColumn();
  long last   = lastVisibleColumn();
  Boolean something_is_visible = FALSE;
  long set_column = 999999;
  if(mincol > maxcol)
     {
     long temp = mincol; mincol = maxcol; maxcol = temp;
     }
  for(long i = mincol; i <= maxcol; i++)
     {
     long column = currentColumnFromTrueColumn(i);
     setSelectedColumn(column, selected);
     if(selected)
        {
        if(column >= first && column <= last) something_is_visible = TRUE;
        set_column = MinimumValue(set_column, column);
        }
     }
  Boolean changed = FALSE;
  if(selected && something_is_visible == FALSE)
     {
     changed = setFirstVisibleColumn(set_column);
     }
  return changed;
}



//------------------- register traps ------------------------//
//------------------- register traps ------------------------//
//------------------- register traps ------------------------//

void SLTableView::setLocateTrap(TableViewBoxLocateTrap *locate_trap,
                                                  void *locate_data)
{
  _locate_trap = locate_trap;
  _locate_data = locate_data;
}


void SLTableView::setSelectTrap(TableViewBoxSelectTrap *select_trap,
                                                  void *select_data)
{
  _databox->setSelectTrap(select_trap, select_data);
}



//------------------- get information -------------------------//
//------------------- get information -------------------------//
//------------------- get information -------------------------//

long    SLTableView::firstDisplayableColumn()      const
  { return _databox->firstDisplayableColumn(); }

long    SLTableView::lastDisplayableColumn()      const
  { return _databox->lastDisplayableColumn(); }

long    SLTableView::firstVisibleColumn()      const
  { return _databox->firstVisibleColumn(); }

long    SLTableView::lastVisibleColumn()      const
  { return _databox->lastVisibleColumn(); }

long    SLTableView::activeColumn()      const
  { return _databox->activeColumn(); }

long    SLTableView::numberOfColumns()          const
  { return _databox->numberOfColumns(); }

long    SLTableView::numberOfAllSelectedColumns()          const
  { return _databox->numberOfAllSelectedColumns(); }

long    SLTableView::numberOfDisplayableSelectedColumns()          const
  { return _databox->numberOfDisplayableSelectedColumns(); }

long    SLTableView::trueColumnFromCurrentColumn(long column)   const
  { return _databox->trueColumnFromCurrentColumn(     column); }

long    SLTableView::currentColumnFromTrueColumn(long truecol)   const
  { return _databox->currentColumnFromTrueColumn(     truecol); }

Boolean SLTableView::columnIsSelected(long column)  const
  { return _databox->columnIsSelected(     column); }



//-------------------------- traps ------------------------------//
//-------------------------- traps ------------------------------//
//-------------------------- traps ------------------------------//

void SLTableView::calledFromDatabox(void *data,
                             long first, long last, long active)
{
  SLTableView *view = (SLTableView*)data;
  view->_range->setValue(first);
  view->_range->setPage(view->_databox->numberOfVisibleColumns());
  if(view->_locate_trap) view->_locate_trap(view->_locate_data,
                                       first, last, active);
}


void SLTableView::calledFromRange(void *data, long first)
{
  SLTableView *view = (SLTableView*)data;
  view->_databox->setFirstVisibleColumn(first);
  first       = view->_databox->firstVisibleColumn();
  long last   = view->_databox->lastVisibleColumn ();
  long active = view->_databox->activeColumn      ();
  if(view->_locate_trap) view->_locate_trap(view->_locate_data,
                                       first, last, active);
}


void SLTableView::sortTrap(void *data, long ident)
{
  SLTableView *view = (SLTableView*)data;
  if     (ident == 1) view->_databox->sortAllColumns            ();
  else if(ident == 2) view->_databox->alignDisplayableSelections();
  if(ident == 2)
      {
      long first = 1;
      view->_range->setValue(first);
      view->_range->setPage(view->_databox->numberOfVisibleColumns());
      view->_databox->setFirstVisibleColumn(first);
      long last   = view->_databox->lastVisibleColumn ();
      long active = view->_databox->activeColumn      ();
      if(view->_locate_trap) view->_locate_trap(view->_locate_data,
                                       first, last, active);
      }
}


char *SLTableView::label1Trap(void *data)
{
  static char label[81];
  SLTableView *view = (SLTableView*)data;
  long istart = view->_databox->firstDisplayableColumn();
  long istop  = view->_databox->lastDisplayableColumn();
  long n = MaximumValue(istop - istart + 1, 0);
  if(n == 1) n = 0;
  sprintf(label, "Sort All  %d  Columns", n);
  return label;
}


char *SLTableView::label2Trap(void *data)
{
  static char label[81];
  SLTableView *view = (SLTableView*)data;
  long nsel = view->_databox->numberOfDisplayableSelectedColumns();
  sprintf(label, "Align  %d  Selections", nsel);
  return label;
}


long SLTableView::sense1Trap(void *data)
{
  SLTableView *view = (SLTableView*)data;
  long ncolumns = view->_databox->numberOfColumns();
  return (ncolumns > 1);
}


long SLTableView::sense2Trap(void *data)
{
  SLTableView *view = (SLTableView*)data;
  long ncolumns = view->_databox->numberOfColumns();
  long nsel     = view->_databox->numberOfDisplayableSelectedColumns();
  return (nsel > 0 && ncolumns > 1);
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
