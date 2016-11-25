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

//---------------------- selections_keeper.cc ------------------------//
//---------------------- selections_keeper.cc ------------------------//
//---------------------- selections_keeper.cc ------------------------//

//          implementation file for the SelectionsKeeper class
//                    not derived from any class
//                        subdirectory oprim


#include "oprim/selections_keeper.hh"
#include "cprim.h"
#include "swapio.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#define INT  4
#define LONG 8
#define CHAR 1


//------------------- constructor and destructor ------------------------//
//------------------- constructor and destructor ------------------------//
//------------------- constructor and destructor ------------------------//


SelectionsKeeper::SelectionsKeeper()
           :
                _nelements  (0),
                _array      (NULL),
                _nselect    (0)
{
}



SelectionsKeeper::~SelectionsKeeper()
{
  if(_array) delete [] _array;
}



//----------------------------- get flags -----------------------------------//
//----------------------------- get flags -----------------------------------//
//----------------------------- get flags -----------------------------------//

   // public.

int  SelectionsKeeper::getSelectFlag (long index)  const
{
  assert(index >= 0 && index < _nelements);
  if(_array == NULL) return NO_MAYBE;
  return (int)_array[index];
}


const char *SelectionsKeeper::getSelectString (long index)  const
{
  static char buffer[] = " ";
  buffer[0] = (char)getSelectFlag(index);
  return buffer;
}


int   SelectionsKeeper::isSelected    (long index)  const
{
  assert(index >= 0 && index < _nelements);
  if(_array == NULL) return FALSE;
  switch(_array[index])
      {
      case NO_MAYBE   : return FALSE;
      case YES_MAYBE  : return TRUE;
      case YES_ALWAYS : return TRUE;
      case NO_ALWAYS  : return FALSE;
      case YES_TOP    : return TRUE;
      case YES_BOTTOM : return TRUE;
      default                : break;
      }
  return FALSE;
}



//----------------------- set several flags ---------------------------//
//----------------------- set several flags ---------------------------//
//----------------------- set several flags ---------------------------//

   // public.

void SelectionsKeeper::beforeSettingSeveralSelectFlags()
{
}


void SelectionsKeeper::setOneOfSeveralSelectFlags(long index, int select_flag)
{
  assert(index >= 0 && index < _nelements);
  assert(select_flag == NO_MAYBE   ||
         select_flag == YES_MAYBE  ||
         select_flag == YES_ALWAYS ||
         select_flag == NO_ALWAYS  ||
         select_flag == YES_TOP    ||
         select_flag == YES_BOTTOM);
  allocateArrayIfNecessary();
  _array[index] = (char)select_flag;
}


void SelectionsKeeper::afterSettingSeveralSelectFlags()
{
  updateSelections();
}



//-------------------------- set flags ---------------------------------//
//-------------------------- set flags ---------------------------------//
//-------------------------- set flags ---------------------------------//

   // public.

void SelectionsKeeper::setSelectFlag(long index, int select_flag)
{
  setOneOfSeveralSelectFlags(index, select_flag);
  updateSelections();
}



void SelectionsKeeper::incrementSelectFlag(long index)
{
  assert(index >= 0 && index < _nelements);
  allocateArrayIfNecessary();
  switch(_array[index])
      {
      case NO_MAYBE   : _array[index] = YES_ALWAYS; break;
      case YES_MAYBE  : _array[index] = YES_ALWAYS; break;
      case YES_ALWAYS : _array[index] = NO_ALWAYS;  break;
      case NO_ALWAYS  : _array[index] = YES_TOP;    break;
      case YES_TOP    : _array[index] = YES_BOTTOM; break;
      case YES_BOTTOM : _array[index] = NO_MAYBE;   break;
      default         : assert(FALSE);
      }
  updateSelections();
}



void SelectionsKeeper::toggleAllSelections()
{
  if(_nelements == 0) return;
  if(isSelected(0)) clearSelectFlags();
  else              setAllSelections();
}



void SelectionsKeeper::clearSelectFlags()
{
  if(_array) delete [] _array;
  _array = NULL;
  _nselect = 0;
}


void SelectionsKeeper::setAllSelections()
{
  clearSelectFlags();
  allocateArrayIfNecessary();
  if(_nelements > 0) _array[0] = YES_TOP;
  updateSelections();
}



//--------------------- insert or remove one element ---------------------//
//--------------------- insert or remove one element ---------------------//
//--------------------- insert or remove one element ---------------------//

   // public.

void SelectionsKeeper::setOrAppendValue(long index)
{
  if(index == _nelements) appendElement();
}


void SelectionsKeeper::appendElement ()
{
  insertElement(_nelements);
}



void SelectionsKeeper::insertElement (long index)
{
  assert(index >= 0 && index <= _nelements);
  if(_array == NULL)
      {
      _nelements++;
      return;
      }
  long  new_nelements = _nelements + 1;
  char *new_array     = new char [new_nelements];
  for(long index2 = 0; index2 < new_nelements; index2++)
      {
      if     (index2 < index) new_array[index2] = _array[index2];
      else if(index2 > index) new_array[index2] = _array[index2 - 1];
      else                    new_array[index2] = ' ';
      }
  delete [] _array;
  _array     = new_array;
  _nelements = new_nelements;
  updateSelections();
}



void SelectionsKeeper::removeElement(long index)
{
  assert(index >= 0 && index < _nelements);
  if(_array == NULL)
      {
      _nelements--;
      return;
      }
  if(_nelements == 1)
      {
      deleteAllElements();
      return;
      }
  long  new_nelements = _nelements - 1;
  char *new_array     = NULL;
  new_array = new char [new_nelements];
  for(long index2 = 0; index2 < new_nelements; index2++)
      {
      if(index2 < index) new_array[index2] = _array[index2];
      else               new_array[index2] = _array[index2 + 1];
      }
  delete [] _array;
  _array     = new_array;
  _nelements = new_nelements;
  updateSelections();
}



//----------------------- change number of elements ---------------------//
//----------------------- change number of elements ---------------------//
//----------------------- change number of elements ---------------------//

   // public.

void SelectionsKeeper::deleteAllElements()
{
  if(_array) delete [] _array;
  _array     = NULL;
  _nelements = 0;
  _nselect   = 0;
}



void SelectionsKeeper::resetNumElements(long nelements)
{
  if(nelements == _nelements) return;
  resetNumElementsAndClear(nelements);
}



void SelectionsKeeper::resetNumElementsAndClear(long nelements)
{
  deleteAllElements();
  _nelements = nelements;
}



void SelectionsKeeper::copyAllElements(const SelectionsKeeper *another)
{
  assert(another);
  deleteAllElements();
  _nelements = another->_nelements;
  _nselect   = another->_nselect;
  if(another->_array == NULL) return;
  _array = new char [_nelements];
  for(long index = 0; index < _nelements; index++)
      {
      _array[index] = another->_array[index];
      }
}



//------------------------ static functions ----------------------------//
//------------------------ static functions ----------------------------//
//------------------------ static functions ----------------------------//


static int binr (FILE *stream, void *values, int type, long nobj)
{
  assert(stream);
/*
  int n = fread(values, size, (int)nobj, stream);
*/
  int n;
  switch (type) {
  case INT :
    n = swapio_fread_int ((int *)values, (int)nobj, stream);
    break;
  case LONG :
    n = swapio_fread_long ((long *)values, (int)nobj, stream);
    break;
  case CHAR :
    n = swapio_fread_char ((char *)values, (int)nobj, stream);
    break;
  default:
    assert (0);
    break;
  }
  if(feof(stream) || ferror(stream) || n != nobj) return TRUE;
  return FALSE;
}


static int binw (FILE *stream, const void *values, int type, long nobj)
{
  assert(stream);
/*
  int n = fwrite(values, size, (int)nobj, stream);
*/
  int n;
  switch (type) {
  case INT :
    n = swapio_fwrite_int ((int *)values, (int)nobj, stream);
    break;
  case LONG :
    n = swapio_fwrite_long ((long *)values, (int)nobj, stream);
    break;
  case CHAR :
    n = swapio_fwrite_char ((char *)values, (int)nobj, stream);
    break;
  default:
    assert (0);
    break;
  }
  if(n != nobj) return TRUE;
  return FALSE;
}



//------------------------- binary read ------------------------------//
//------------------------- binary read ------------------------------//
//------------------------- binary read ------------------------------//

   // public.

int SelectionsKeeper::binaryRead(FILE *stream)
{
  deleteAllElements();
  int exists;
  long nselect;
  if(binr(stream, &_nelements, LONG, 1)) return TRUE;
  if(binr(stream, &nselect   , LONG, 1)) return TRUE;
  if(binr(stream, &exists    , INT , 1)) return TRUE;

  if (_nelements < 0 || _nelements > 999999)
      {
      swapio_do_swap_long (&_nelements, 1);
      swapio_do_swap_long (&nselect   , 1);
      swapio_do_swap_int  (&exists    , 1);
      }

  if(exists)
      {
      allocateArrayIfNecessary();
      if(binr(stream, _array, CHAR, _nelements)) return TRUE;
      _nselect = nselect;
      }
  return FALSE;
}



//------------------------- binary write ------------------------------//
//------------------------- binary write ------------------------------//
//------------------------- binary write ------------------------------//

   // public.

int SelectionsKeeper::binaryWrite(FILE *stream)  const
{
  int exists = (_array != NULL);
  if(binw(stream, &_nelements, LONG, 1)) return TRUE;
  if(binw(stream, &_nselect  , LONG, 1)) return TRUE;
  if(binw(stream, &exists    , INT , 1)) return TRUE;
  if(exists)
      {
      if(binw(stream, _array, CHAR, _nelements)) return TRUE;
      }
  return FALSE;
}



//--------------- allocate array if necessary -------------------------//
//--------------- allocate array if necessary -------------------------//
//--------------- allocate array if necessary -------------------------//

   // private.

void SelectionsKeeper::allocateArrayIfNecessary()
{
  if(_array || _nelements == 0) return;
  assert(_nselect == 0);
  _array = new char [_nelements];
  for(long index = 0; index < _nelements; index++)
      {
      _array[index] = NO_MAYBE;
      }
}



//------------------------- update selections -------------------------//
//------------------------- update selections -------------------------//
//------------------------- update selections -------------------------//

   // private.

void SelectionsKeeper::updateSelections()
{
  if(_array == NULL) return;
  _nselect = 0;
  char remember = NO_MAYBE;
  for(long index = 0; index < _nelements; index++)
      {
      switch(_array[index])
          {
          case NO_MAYBE   : _array[index] = remember;  break;
          case YES_MAYBE  : _array[index] = remember;  break;
          case YES_ALWAYS :                            break;
          case NO_ALWAYS  :                            break;
          case YES_TOP    : remember      = YES_MAYBE; break;
          case YES_BOTTOM : remember      = NO_MAYBE;  break;
          default         : assert(FALSE);             break;
          }
      if(isSelected(index)) _nselect++;
      }
}



//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//

