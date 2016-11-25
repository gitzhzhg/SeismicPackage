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

//---------------------- object_array.cc -----------------------//
//---------------------- object_array.cc -----------------------//
//---------------------- object_array.cc -----------------------//

//        implementation file for the ObjectArray class
//              derived from the ArrayBase class
//                     subdirectory oprim


#include "oprim/object_array.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


ObjectArray::ObjectArray(long istep, long nstep)
           :    ArrayBase(sizeof(void*), istep, nstep)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

ObjectArray::~ObjectArray()
{
  removeAllElements();
}



//--------------- virtual moving element functions --------------//
//--------------- virtual moving element functions --------------//
//--------------- virtual moving element functions --------------//

          // private virtual functions overriding ArrayBase

void ObjectArray::elementDeletingFromArray(long index)
{
  void *obj = arrayElement(index);
  assert(obj);
  objectWillBeRemoved(index);
  doDeleteObject(obj);
}


void ObjectArray::elementDeletingFromBuffer()
{
  void *obj = *(void**)(bufferElementPointer());
  assert(obj);
  doDeleteObject(obj);
}


void ObjectArray::elementRemovingToBuffer(long index)
{
  void *obj = arrayElement(index);
  assert(obj);
  objectWillBeRemoved(index);
}


void ObjectArray::elementInsertedFromBuffer(long index)
{
  void *obj = arrayElement(index);
  assert(obj);
  objectHasBeenInserted(index);
}


void ObjectArray::elementInsertedFromList(long /*index*/)
{
  assert(FALSE);
}


void ObjectArray::elementCreatedInArray(long index)
{
  void *obj = doCreateObject();
  assert(obj);
  void **element = (void**)arrayElementPointer(index);
  assert(element);
  assert(!*element);
  *element = obj;
  objectHasBeenInserted(index);
}




/****************

       not yet

//--------- virtual functions passing thru to array elements ------//
//--------- virtual functions passing thru to array elements ------//
//--------- virtual functions passing thru to array elements ------//


double ObjectArray::getValue(int ident, long index)  const
{
  ArrayElement *obj = fetchElement(index);
  return obj->getValue(ident);
}


void ObjectArray::setValue(int ident, long index, double value)
{
  ArrayElement *obj = fetchElement(index);
  obj->setValue(ident, value);
}


void ObjectArray::setDependentValue(int ident, long index, double value)
{
  ArrayElement *obj = fetchElement(index);
  obj->setDependentValue(ident, value);
}


int ObjectArray::valueIsDependent(int ident, long index)  const
{
  ArrayElement *obj = fetchElement(index);
  return obj->valueIsDependent(ident);
}

*****************/


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

