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

//---------------------- sl_table_view_box.cc -----------------------//
//---------------------- sl_table_view_box.cc -----------------------//
//---------------------- sl_table_view_box.cc -----------------------//

//         implementation file for the SLTableViewBox class
//                derived from the SLDatabox class
//                       subdirectory sl


#include "sl/sl_table_view_box.hh"
#include "sl/sl_prim.hh"
#include "sl/table_view_data.hh"
#include "wbox.h"
#include "cprim.h"
#include "str.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>


#define PROMPT_SWITCH             2
#define MISSING_PROMPT_SWITCH    -2
#define SELECTED_ARRAY_SWITCH     5
#define UNSELECTED_ARRAY_SWITCH  11
#define MISSING_ARRAY_SWITCH      0


//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//


SLTableViewBox::SLTableViewBox(Widget wparent, char *name, HelpCtx hctx,
               long nchar, long ndec, long nrowmax, long ncolmax)
           : SLDatabox(wparent, name, hctx),
                 _NCHAR      (nchar),
                 _NDEC       (ndec),
                 _NROWMAX    (nrowmax),
                 _NCOLMAX    (ncolmax)
{
  constructorHelper();
}


SLTableViewBox::SLTableViewBox(SLDelay *slparent, char *name, HelpCtx hctx,
               long nchar, long ndec, long nrowmax, long ncolmax)
           : SLDatabox(slparent, name, hctx),
                 _NCHAR      (nchar),
                 _NDEC       (ndec),
                 _NROWMAX    (nrowmax),
                 _NCOLMAX    (ncolmax)
{
  constructorHelper();
}


//---------------------- constructor helper --------------------//
//---------------------- constructor helper --------------------//
//---------------------- constructor helper --------------------//

void SLTableViewBox::constructorHelper()
{
  if(_NCHAR <= 0 || _NDEC < 0 || _NROWMAX <= 0 || _NCOLMAX <= 0)
       {
       cout << "in SLTableViewBox constructor:"            << endl;
       cout << " illegal value for NCHAR   = " << _NCHAR   << endl;
       cout << "            and/or NDEC    = " << _NDEC    << endl;
       cout << "            and/or NROWMAX = " << _NROWMAX << endl;
       cout << "            and/or NCOLMAX = " << _NCOLMAX << endl;
       exit(0);
       }
  _tvdata = new TableViewData();
  _tvdata->setNumberOfVisibleColumns(_NCOLMAX);
  _nrows = _tvdata->numberOfRows();
  _locate_trap = NULL;
  _locate_data = NULL;
  _select_trap = NULL;
  _select_data = NULL;
  _prompt      = new char   [(_NCHAR + 1) * _NCOLMAX];
  _value       = new float* [_NCOLMAX];
  _psw         = new long   [_NCOLMAX];
  _asw         = new long   [_NCOLMAX];
  move();
  showFocus();
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

SLTableViewBox::~SLTableViewBox()
{
  if(_prompt   ) delete [] _prompt   ;
  if(_value    ) delete [] _value    ;
  if(_psw      ) delete [] _psw      ;
  if(_asw      ) delete [] _asw      ;
                 delete    _tvdata   ;
}


//----------------------------- new table ---------------------//
//----------------------------- new table ---------------------//
//----------------------------- new table ---------------------//

void SLTableViewBox::newTable(const float *array, long nrows, long ncolumns,
                              long nstride, long istart, long istop)
{
  _tvdata->newArray(array, nrows, ncolumns, nstride, istart, istop);
  _nrows = _tvdata->numberOfRows();
  move();
  reregister();
  showFocus();
  SLPrim::updateEverything();
}


//-------------------------- empty table ------------------------//
//-------------------------- empty table ------------------------//
//-------------------------- empty table ------------------------//

void SLTableViewBox::emptyTable()
{
  _tvdata->emptyArray();
  move();
  reregister();
  showFocus();
  SLPrim::updateEverything();
}


//------------------ methods to set values --------------------------//
//------------------ methods to set values --------------------------//
//------------------ methods to set values --------------------------//


Boolean SLTableViewBox::setFirstVisibleColumn(long first)
{
  Boolean changed = setFirstVisibleColumn(first, FALSE);
  if(changed) SLPrim::updateEverything();
  return changed;
}



Boolean SLTableViewBox::setSelectedColumn(long column, Boolean selected)
{
  Boolean changed = setSelectedColumn(column, selected, FALSE);
  if(changed) SLPrim::updateEverything();
  return changed;
}


Boolean SLTableViewBox::toggleSelectedColumn(long column)
{
  Boolean changed = toggleSelectedColumn(column, FALSE);
  if(changed) SLPrim::updateEverything();
  return changed;
}



//-------------- internal methods to set values ----------------------//
//-------------- internal methods to set values ----------------------//
//-------------- internal methods to set values ----------------------//

  // The argument internal is FALSE when called by user,
  //   and TRUE when called internally from a trap in this class.
  // When internal is TRUE, the appropriate user-registered trap is called.


Boolean SLTableViewBox::setFirstVisibleColumn(long first, Boolean internal)
{
  Boolean changed = _tvdata->setFirstVisibleColumn(first);
  if(!changed) return FALSE;
  move();
  reregister();
  showFocus();
  if(internal) callLocateTrap();
  return TRUE;
}


Boolean SLTableViewBox::setSelectedColumn(long column, Boolean selected,
                                                    Boolean internal)
{
  Boolean changed = _tvdata->setSelectedColumn(column, selected);
  if(changed) selectionHasChanged(column, internal);
  return changed;
}


Boolean SLTableViewBox::toggleSelectedColumn(long column, Boolean internal)
{
  Boolean changed = _tvdata->toggleSelectedColumn(column);
  if(changed) selectionHasChanged(column, internal);
  return changed;
}


void SLTableViewBox::selectionHasChanged(long column, Boolean internal)
{
  long i = column - _tvdata->firstVisibleColumn();
  if(i >= 0 && i < _NCOLMAX)
       {
       if(_tvdata->columnIsSelected(column))
            _asw[i] = SELECTED_ARRAY_SWITCH;
       else _asw[i] = UNSELECTED_ARRAY_SWITCH;
       }
  if(internal) callSelectTrap(column);
}


//------------------------ sort all columns -------------------------//
//------------------------ sort all columns -------------------------//
//------------------------ sort all columns -------------------------//

        // Sort all columns, whether displayable or not.

void SLTableViewBox::sortAllColumns()
{
  _tvdata->sortAllColumns();
  move();
  reregister();
  showFocus();
  callLocateTrap();
}


//-------------------- align selected columns -------------------//
//-------------------- align selected columns -------------------//
//-------------------- align selected columns -------------------//

        // First sort all columns, whether displayable or not.
        // Then align only the selected displayable columns.

void SLTableViewBox::alignDisplayableSelections()
{
  _tvdata->alignDisplayableSelections();
  move();
  reregister();
  showFocus();
  callLocateTrap();
}



//------------------ methods to register traps ------------------//
//------------------ methods to register traps ------------------//
//------------------ methods to register traps ------------------//

void SLTableViewBox::setLocateTrap(TableViewBoxLocateTrap *locate_trap,
                                                     void *locate_data)
{
  _locate_trap = locate_trap;
  _locate_data = locate_data;
}


void SLTableViewBox::setSelectTrap(TableViewBoxSelectTrap *select_trap,
                                                     void *select_data)
{
  _select_trap = select_trap;
  _select_data = select_data;
}



//--------------------- methods to call traps --------------------//
//--------------------- methods to call traps --------------------//
//--------------------- methods to call traps --------------------//

void SLTableViewBox::callLocateTrap()
{
  if(_locate_trap) _locate_trap(_locate_data,
                                _tvdata->firstVisibleColumn(),
                                _tvdata->lastVisibleColumn(),
                                _tvdata->activeColumn());
}


void SLTableViewBox::callSelectTrap(long column)
{
  if(_select_trap) _select_trap(_select_data,
                                 column,
                                _tvdata->columnIsSelected(column));
}



//------------------ methods to get information -----------------//
//------------------ methods to get information -----------------//
//------------------ methods to get information -----------------//

long SLTableViewBox::firstDisplayableColumn()       const
   { return _tvdata->firstDisplayableColumn(); }

long SLTableViewBox::lastDisplayableColumn()       const
   { return _tvdata->lastDisplayableColumn(); }

long SLTableViewBox::firstVisibleColumn()       const
   { return _tvdata->firstVisibleColumn(); }

long SLTableViewBox::lastVisibleColumn()       const
   { return _tvdata->lastVisibleColumn(); }

long SLTableViewBox::activeColumn()       const
   { return _tvdata->activeColumn(); }

long SLTableViewBox::numberOfColumns()       const
   { return _tvdata->numberOfColumns(); }

long SLTableViewBox::numberOfAllSelectedColumns()       const
   { return _tvdata->numberOfAllSelectedColumns(); }

long SLTableViewBox::numberOfDisplayableSelectedColumns()       const
   { return _tvdata->numberOfDisplayableSelectedColumns(); }

long SLTableViewBox::trueColumnFromCurrentColumn(long column)   const
   { return _tvdata->trueColumnFromCurrentColumn(     column); }

long SLTableViewBox::currentColumnFromTrueColumn(long truecol)   const
   { return _tvdata->currentColumnFromTrueColumn(     truecol); }

Boolean SLTableViewBox::columnIsSelected(long column)   const
      { return _tvdata->columnIsSelected(     column); }


//------------------------ move -----------------------------//
//------------------------ move -----------------------------//
//------------------------ move -----------------------------//

void SLTableViewBox::move()
{
  long first    = _tvdata->firstVisibleColumn();
  long istart   = _tvdata->firstDisplayableColumn();
  long istop    = _tvdata->lastDisplayableColumn();
  for(int i = 0; i < _NCOLMAX; i++)
       {
       long column   = first + i;         // _array column number
       _value[i]     = (float*)_tvdata->vector(column);
       long prompt_index = i * (_NCHAR + 1);
       if(column >= istart && column <= istop && _tvdata->array())
           {
           int truecol = (int)_tvdata->trueColumnFromCurrentColumn(column);
           int nchar = (int)_NCHAR;
/*
           convert_ii2ss(&truecol, &_prompt[prompt_index], &nchar);
*/
           str_ii2ss(truecol, &_prompt[prompt_index], nchar);
           _psw  [i] = PROMPT_SWITCH;
           if(_tvdata->columnIsSelected(column))
                _asw[i] = SELECTED_ARRAY_SWITCH;
           else _asw[i] = UNSELECTED_ARRAY_SWITCH;
           }
       else
           {
           strcpy(&_prompt[prompt_index], " ");
           _psw  [i] = MISSING_PROMPT_SWITCH;
           _asw  [i] = MISSING_ARRAY_SWITCH;
           }
       }
}


//--------------------- show focus --------------------------//
//--------------------- show focus --------------------------//
//--------------------- show focus --------------------------//

void SLTableViewBox::showFocus()
{
  long active = _tvdata->activeColumn();
  long first  = _tvdata->firstVisibleColumn();
  for(int i = 0; i < _NCOLMAX; i++)
       {
       long prompt_index = i * (_NCHAR + 1);
       long length = strlen(&_prompt[prompt_index]);
       long last = prompt_index + length - 1;
       if(i == active - first && _prompt[last] != '*' && length <= _NCHAR - 2)
            {
            _prompt[last + 1] = ' ';
            _prompt[last + 2] = '*';
            _prompt[last + 3] = '\0';
            }
       else if(i != active - first && _prompt[last] == '*' && length >= 2)
            {
            _prompt[last - 1] = '\0';
            }
       }
}



//--------------------- reregister --------------------------//
//--------------------- reregister --------------------------//
//--------------------- reregister --------------------------//

void SLTableViewBox::reregister()
{
  for(int i = 0; i < _NCOLMAX; i++)
       {
       long ident = 1 + i;
       fnewreg(ident, _value[i]);
       }
}



//------------------ number of visible columns ---------------//
//------------------ number of visible columns ---------------//
//------------------ number of visible columns ---------------//

long SLTableViewBox::numberOfVisibleColumns()
{
  long j = 0;
  for(int i = 0; i < _NCOLMAX; i++)
       {
       if(isVisible(i+1)) j++;
       }
  _tvdata->setNumberOfVisibleColumns(j);
  return j;
}



//---------------------- table trap -------------------------//
//---------------------- table trap -------------------------//
//---------------------- table trap -------------------------//

void SLTableViewBox::tableTrap(void *box, long *ident, long *index,
                               char * /* text */, long *nread, char *endkey)
{
  if(strings_equal("REDRAW", endkey)) return;   // box not valid.
  if(strings_equal("RESTORED", endkey)) return;
  if(*nread > 0)
                 {
                 strcpy(endkey, "RESTORE");
                 return;
                 }
  SLTableViewBox *tv = (SLTableViewBox*)wbox_get_userdata(box);
  if(!tv) return;
  tv->tableTrapHelper(ident, index, endkey);
}



//----------------- table trap helper -----------------------//
//----------------- table trap helper -----------------------//
//----------------- table trap helper -----------------------//

void SLTableViewBox::tableTrapHelper(long *ident, long *index,
                                                     char *endkey)
{
  long abs_ident = AbsoluteValue(*ident);
  long first    = _tvdata->firstVisibleColumn();
  long numvis   =          numberOfVisibleColumns();
  long istop    = _tvdata->lastDisplayableColumn();
  long last     = _tvdata->lastVisibleColumn();
  long last_first = istop - numvis + 1;
  if(strings_equal("ARRIVED", endkey))
            {
            Boolean changed = _tvdata->setActiveColumn
                                          (first + abs_ident - 1);
            if(changed)
                 {
                 showFocus();
                 callLocateTrap();
                 strcpy(endkey, "REFRESH");
                 }
            }
  else if(strings_equal("INSERT", endkey))
            {
            strcpy(endkey, "junk");
            }
  else if(strings_equal("REMOVE", endkey))
            {
            strcpy(endkey, "junk");
            }
  else if(strings_equal("RETURN", endkey) && *ident < 0)
            {
            toggleSelectedColumn(first - *ident - 1, TRUE);
            }
  else if(strings_equal("LEFT", endkey) && abs_ident == 1)
            {
            setFirstVisibleColumn(first - 1, TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("RIGHT", endkey) && abs_ident == numvis)
            {
            setFirstVisibleColumn
                     (MinimumValue(first + 1, last_first), TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("^LEFT", endkey) && abs_ident == 1)
            {
            setFirstVisibleColumn(first - numvis, TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("^RIGHT", endkey) && abs_ident == numvis)
            {
            setFirstVisibleColumn
                     (MinimumValue(first + numvis, last_first), TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("@LEFT", endkey) && abs_ident == 1)
            {
            setFirstVisibleColumn(1, TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("@RIGHT", endkey) && abs_ident == numvis)
            {
            setFirstVisibleColumn(last_first, TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("UP", endkey) && *ident > 0 && *index == 1)
            {
            *ident = -*ident;
            strcpy(endkey, "junk");
            }
  else if(strings_equal("DOWN", endkey) && *ident < 0)
            {
            *ident = -*ident;
            strcpy(endkey, "junk");
            }
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

void SLTableViewBox::makeHelper()
{
  static long P0  = 0;
  static long M44 = -44; 
  move();

         //    N       NMAX  ROW  MAXROWS
  wbox_rega(&_nrows, &_nrows, 0, (int)_NROWMAX);

         //  TRAP  ID  LABEL SWITCH  VARIABLE SWITCH  COL NCHAR NDEC
  wbox_xrega(NULL,  0,  " ",  &P0,     &P0,    &M44,   0,   6,   0);
  for(int i = 0; i < _NCOLMAX; i++)
       {
       long ident = 1 + i;
       long prompt_index = i * (_NCHAR + 1);
       string_copy(&_prompt[prompt_index], _NCHAR, "123456789", 0);
       wbox_frega(tableTrap, (int)ident, &_prompt[prompt_index], &_psw[i],
                            _value[i], &_asw[i], 0, (int)_NCHAR, (int)_NDEC);
       }
  move();
  showFocus();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
