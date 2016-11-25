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

//---------------------- table_view_data.cc -----------------------//
//---------------------- table_view_data.cc -----------------------//
//---------------------- table_view_data.cc -----------------------//

//         implementation file for the TableViewData class
//                    not derived from any class
//                         subdirectory sl


#include "sl/table_view_data.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>


//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//


TableViewData::TableViewData()
           :
                 _array       (NULL),
                 _nrows       (0),
                 _ncolumns    (1),
                 _nstride     (1),
                 _istart      (1),
                 _istop       (1),
                 _first       (1),
                 _last        (1),
                 _numvis      (1),
                 _active      (1),
                 _nseltot     (0),
                 _nseldisp    (0),
                 _nilvector   (NULL),
                 _selected    (NULL),
                 _truecol     (NULL)
{
  _nilvector  = new float  [MaximumValue(_nrows, 1)]; // will change later
  _selected   = new Boolean[_ncolumns];               // will change later
  _truecol    = new long   [_ncolumns];               // will change later
/*
  get_fnil(&_nilvector[0]);
*/
  _nilvector[0] = FNIL;
  _selected [0] = FALSE;
  _truecol  [0] = 1;
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

TableViewData::~TableViewData()
{
  if(_nilvector) delete [] _nilvector;  // changed at various times
  if(_selected ) delete [] _selected ;  // changed at various times
  if(_truecol  ) delete [] _truecol  ;  // changed at various times
}


//----------------------------- new array ---------------------//
//----------------------------- new array ---------------------//
//----------------------------- new array ---------------------//

      // The selected columns will be retained if ncolumns
      // has not changed.

void TableViewData::newArray(const float *array, long nrows, long ncolumns,
                             long nstride, long istart, long istop)
{
  if(!array || nrows < 1 || ncolumns < 1)
       {
       emptyArray();
       return;
       }
  _array = (float*)array;
  if(nrows != _nrows)
       {
       _nrows = nrows;
       if(_nilvector) delete [] _nilvector;
       _nilvector = new float[_nrows];
       for(int i = 0; i < _nrows; i++)
            {
/*
            get_fnil(&_nilvector[i]);
*/
            _nilvector[i] = FNIL;
            }
       }
  _nstride = nstride;
  istart = ConstrainValue(istart, 1     , ncolumns);
  istop  = ConstrainValue(istop , istart, ncolumns);
  if(ncolumns != _ncolumns)
       {
       _ncolumns = ncolumns;
       _istart   = istart;
       _istop    = istop ;
       if(_selected) delete [] _selected;
       if(_truecol ) delete [] _truecol ;
       _selected = new Boolean[_ncolumns];
       _truecol  = new long   [_ncolumns];
       for(int i = 0; i < _ncolumns; i++)
            {
            _selected[i] = FALSE;
            _truecol [i] = i + 1;
            }
       _nseltot  = 0;
       _nseldisp = 0;
       }
  else if(istart != _istart || istop != _istop)
       {
       sortAllColumns();
       _istart   = istart;
       _istop    = istop ;
       _nseldisp = 0;
       for(long i = _istart - 1; i < _istop; i++)
            {
            if(_selected[i]) _nseldisp++;
            }
       }
  setFirstVisibleColumn(_first);
}


//---------------------------- empty array ----------------------//
//---------------------------- empty array ----------------------//
//---------------------------- empty array ----------------------//

void TableViewData::emptyArray()
{
  _array = NULL;
}


//------------------ methods to set values --------------------------//
//------------------ methods to set values --------------------------//
//------------------ methods to set values --------------------------//


Boolean TableViewData::setFirstVisibleColumn(long first)
{
  first     = ConstrainValue(first, _istart, _istop);
  long last = MinimumValue(first + _numvis - 1, _istop);
  if(first == _first && last == _last) return FALSE;
  long active = _active + first - _first;
  _first = first;
  _last  = last;
  setActiveColumn(active);
  return TRUE;
}


Boolean TableViewData::setNumberOfVisibleColumns(long numvis)
{
  numvis    = MaximumValue(numvis, 1);
  long last = MinimumValue(_first + numvis - 1, _istop);
  if(numvis == _numvis && last == _last) return FALSE;
  _numvis = numvis;
  _last   = last;
  return TRUE;
}


Boolean TableViewData::setActiveColumn(long active)
{
  active = ConstrainValue(active, _first, _last);
  if(active == _active) return FALSE;
  _active = active;
  return TRUE;
}


Boolean TableViewData::setSelectedColumn(long column, Boolean selected)
{
  if(column < 1 || column > _ncolumns) return FALSE;
  if( selected &&  _selected[column - 1]) return FALSE;
  if(!selected && !_selected[column - 1]) return FALSE;
  return toggleSelectedColumn(column);
}


Boolean TableViewData::toggleSelectedColumn(long column)
{
  if(column < 1 || column > _ncolumns) return FALSE;
  if(_selected[column - 1])
         {
         _selected[column - 1] = FALSE;
         _nseltot--;
         if(column >= _istart && column <= _istop) _nseldisp--;
         }
  else   {
         _selected[column - 1] = TRUE ;
         _nseltot++;
         if(column >= _istart && column <= _istop) _nseldisp++;
         }
  return TRUE;
}


void TableViewData::clearAllSelections()
{
  for(int i = 0; i < _ncolumns; i++)
       {
       _selected[i] = FALSE;
       }
  _nseltot  = 0;
  _nseldisp = 0;
}


void TableViewData::clearDisplayableSelections()
{
  for(long i = _istart - 1; i < _istop; i++)
       {
       if(_selected[i]) { _selected[i] = FALSE; _nseltot--; }
       }
  _nseldisp = 0;
}



//---------------------- switch columns ----------------------//
//---------------------- switch columns ----------------------//
//---------------------- switch columns ----------------------//

void TableViewData::switchColumns(long lo2, long up2)
{
  long temp     = _truecol[lo2];
  _truecol[lo2] = _truecol[up2];
  _truecol[up2] = temp;
  Boolean temp2  = _selected[lo2];
  _selected[lo2] = _selected[up2];
  _selected[up2] = temp2;
}



//------------- functions called from generic_sort -------------//
//------------- functions called from generic_sort -------------//
//------------- functions called from generic_sort -------------//

int TableViewData::sortHelper(void *data, long lo, long up)
{
  TableViewData *tv = (TableViewData*)data;
  if(!tv) return FALSE;
  long lo2 = (lo - 1);
  long up2 = (up - 1);
  if(tv->_truecol[lo2] > tv->_truecol[up2])
       {
       tv->switchColumns(lo2, up2);
       return TRUE;
       }
  return FALSE;
}


int TableViewData::alignHelper(void *data, long lo, long up)
{
  TableViewData *tv = (TableViewData*)data;
  if(!tv) return FALSE;
  long lo2 = (lo - 1) + (tv->_istart - 1);
  long up2 = (up - 1) + (tv->_istart - 1);
  if(tv->_selected[lo2] < tv->_selected[up2] ||
     (tv->_selected[lo2] == tv->_selected[up2] &&
      tv->_truecol[lo2] > tv->_truecol[up2]))
       {
       tv->switchColumns(lo2, up2);
       return TRUE;
       }
  return FALSE;
}



//------------------------ sort all columns -------------------------//
//------------------------ sort all columns -------------------------//
//------------------------ sort all columns -------------------------//

        // Sort all columns, whether displayable or not.

void TableViewData::sortAllColumns()
{
  if(_ncolumns <= 1) return;
  generic_sort(_ncolumns, sortHelper, this);
}


//-------------------- align displayable selections -------------//
//-------------------- align displayable selections -------------//
//-------------------- align displayable selections -------------//

        // First sort all columns, whether displayable or not.
        // Then align only the selected displayable columns.

void TableViewData::alignDisplayableSelections()
{
  sortAllColumns();
  long nsort = _istop - _istart + 1;
  if(nsort <= 1) return;
  generic_sort(nsort, alignHelper, this);
}



//------------------ methods to get information -----------------//
//------------------ methods to get information -----------------//
//------------------ methods to get information -----------------//

const float *TableViewData::vector(long column) const
{
  if(column < _istart || column > _istop) return _nilvector;
  long truecol = trueColumnFromCurrentColumn(column);
  if(truecol == 0) return _nilvector;
  return &_array[(truecol - 1) *_nstride];
}

Boolean TableViewData::columnIsSelected(long column) const
{
  if(column >= 1 && column <= _ncolumns && _array)
       return _selected[column - 1];
  return FALSE;
}


long TableViewData::trueColumnFromCurrentColumn(long column) const
{
  if(column >= 1 && column <= _ncolumns && _array)
       return _truecol[column - 1];
  return 0;
}


long TableViewData::currentColumnFromTrueColumn(long truecol) const
{
  if(truecol >= 1 && truecol <= _ncolumns && _array)
       {
       for(int i = 0; i < _ncolumns; i++)
            {
            if(_truecol[i] == truecol) return (i + 1);
            }
       }
  return 0;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
