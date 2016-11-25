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

//---------------------- active_index_keeper.cc ------------------------//
//---------------------- active_index_keeper.cc ------------------------//
//---------------------- active_index_keeper.cc ------------------------//

//          implementation file for the ActiveIndexKeeper class
//                    not derived from any class
//                        subdirectory oprim


#include "oprim/active_index_keeper.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>



//------------------- constructor and destructor ------------------------//
//------------------- constructor and destructor ------------------------//
//------------------- constructor and destructor ------------------------//


ActiveIndexKeeper::ActiveIndexKeeper (const Which which)
           :
        _which        (which),
        _nelements    (0),
        _active       (-1)
{
  assert(_which == MOVE_WITH_VALUE || _which == SHOW_LAST_CHANGE);
}



ActiveIndexKeeper::~ActiveIndexKeeper()
{
}



//-------------------------- set one value ------------------------------//
//-------------------------- set one value ------------------------------//
//-------------------------- set one value ------------------------------//


void ActiveIndexKeeper::setActiveIndex(long index)
{
  if(_nelements == 0) _active = -1;
  else                _active = ConstrainValue(index, 0, _nelements - 1);
}



void ActiveIndexKeeper::setValue(long index)
{
  assert(index >= 0 && index < _nelements);
  if(_which == SHOW_LAST_CHANGE) setActiveIndex(index);
}



void ActiveIndexKeeper::setLastValue()
{
  setValue(_nelements - 1);
}



//----------------------- set or append one value -----------------------//
//----------------------- set or append one value -----------------------//
//----------------------- set or append one value -----------------------//


void ActiveIndexKeeper::setOrAppendValue(long index)
{
  if(index == _nelements) appendElement();
  else                    setValue(index);
}



//--------------------- insert or remove one element ---------------------//
//--------------------- insert or remove one element ---------------------//
//--------------------- insert or remove one element ---------------------//


void ActiveIndexKeeper::appendElement ()
{
  insertElement(_nelements);
}



void ActiveIndexKeeper::insertElement (long index)
{
  assert(index >= 0 && index <= _nelements);
  _nelements++;
  if     (_which == SHOW_LAST_CHANGE) setActiveIndex(index);
  else if(_active >= index)           setActiveIndex(_active + 1);
  else                                setActiveIndex(_active);
}



void ActiveIndexKeeper::removeElement(long index)
{
  assert(index >= 0 && index < _nelements);
  _nelements--;
  if     (_which == SHOW_LAST_CHANGE) setActiveIndex(index);
  else if(_active > index)            setActiveIndex(_active - 1);
  else                                setActiveIndex(_active);
}



//----------------------- change number of elements ---------------------//
//----------------------- change number of elements ---------------------//
//----------------------- change number of elements ---------------------//


void ActiveIndexKeeper::deleteAllElements()
{
  _nelements = 0;
  setActiveIndex(0);
}



void ActiveIndexKeeper::resetNumElements(long nelements)
{
  _nelements = nelements;
  setActiveIndex(_active);
}



void ActiveIndexKeeper::resetNumElementsAndClear(long nelements)
{
  _nelements = nelements;
  setActiveIndex(0);
}



void ActiveIndexKeeper::copyAllElements(const ActiveIndexKeeper *object)
{
  assert(object);
  assert(_which == object->_which);
  _nelements = object->_nelements;
  _active    = object->_active;
}



//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//

