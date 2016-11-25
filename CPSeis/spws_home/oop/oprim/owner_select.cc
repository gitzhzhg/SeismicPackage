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

//-------------------------- owner_select.cc ---------------------------//
//-------------------------- owner_select.cc ---------------------------//
//-------------------------- owner_select.cc ---------------------------//

//           implementation file for the OwnerSelect class
//                derived from the SimpleSelect class
//                       subdirectory oprim


#include "oprim/owner_select.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//-------------------------- constructor ---------------------------//
//-------------------------- constructor ---------------------------//
//-------------------------- constructor ---------------------------//


OwnerSelect::OwnerSelect()
           :
                _num        (0),
                _sel        (NULL)
{
}



//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//

OwnerSelect::~OwnerSelect()
{
  if(_sel) delete [] _sel;
}


//----------------- update array length if necessary ---------------------//
//----------------- update array length if necessary ---------------------//
//----------------- update array length if necessary ---------------------//

   // public.
   // this forgets and clears all previous selections if the array size changes.

void OwnerSelect::updateArrayLengthIfNecessary(long num)
{
  if(num == _num) return;
  if(_sel) delete [] _sel;
  _sel = NULL;
  _num = num;
  if(_num > 0) _sel = new char [_num];
  for(long index = 0; index < _num; index++) { _sel[index] = ' '; }
  registerSelectArray(_sel);
  updateSelections(_num);
}



//------------------------- remove or insert elements --------------------//
//------------------------- remove or insert elements --------------------//
//------------------------- remove or insert elements --------------------//

   // public.
   // this remembers and adjusts previous selections if the array size changes.
   // index < 0 or index >= num simply extends or truncates the array.
   // otherwise, the upper portion of the array will be moved.

void OwnerSelect::removeOrInsertElements(long num, long index)
{
  if(num == _num) return;
  if(num == 0)
      {
      updateArrayLengthIfNecessary(num);
      return;
      }
  char *newsel = new char [num];
  if(index < 0) index = num;
  for(long index2 = 0; index2 < num; index2++)
      {
      long index3 = index2;
      if(index2 >= index) index3 = index2 + _num - num;
      if(index3 >= 0 && index3 < _num) newsel[index2] = _sel[index3];
      else                             newsel[index2] = ' ';
      }
  if(_sel) delete [] _sel;
  _sel = newsel;
  _num = num;
  registerSelectArray(_sel);
  updateSelections(_num);
}



//--------------------------- copy all elements ------------------------//
//--------------------------- copy all elements ------------------------//
//--------------------------- copy all elements ------------------------//

   // public.

void OwnerSelect::copyAllElements(OwnerSelect *another)
{
  updateArrayLengthIfNecessary(another->_num);
  if(_num == 0) return;
  memcpy(_sel, another->_sel, (int)_num * sizeof(char));
  updateSelections(_num);
}



//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//

