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

//----------------------- static_dataset_array.cc -------------------------//
//----------------------- static_dataset_array.cc -------------------------//
//----------------------- static_dataset_array.cc -------------------------//

//        implementation file for the StaticDatasetArray class
//                derived from the SmartArray class
//                         subdirectory stat


#include "stat/static_dataset_array.hh"
#include "stat/static_dataset.hh"
#include "str.h"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <math.h>

#define STEP 5

//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


StaticDatasetArray::StaticDatasetArray
                     (const char *progname, StaticInformer *informer)
           : SmartArray(STEP),
                _progname     (str_newstr(progname)),
                _informer     (informer)
{
  assert(_progname);
  assert(_informer);
  appendNewDataset();
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

StaticDatasetArray::~StaticDatasetArray()
{
  removeAllElements();   // required in derived class destructor.
  free(_progname);
}



//---------------- set active or reference dataset index -----------------//
//---------------- set active or reference dataset index -----------------//
//---------------- set active or reference dataset index -----------------//

    // public.
    // these functions must be used instead of the setActiveIndex
    //   and setReferenceIndex functions.

void StaticDatasetArray::setActiveDatasetIndex(int index)
{
  setActiveIndex(index);
  privateResetParameters();
}


void StaticDatasetArray::setReferenceDatasetIndex(int index)
{
  setReferenceIndex(index);
  privateResetParameters();
}



//----------------------- append and remove dataset ------------------//
//----------------------- append and remove dataset ------------------//
//----------------------- append and remove dataset ------------------//

    // public.
    // these are the only insert/remove functions which should be used.

StaticDataset *StaticDatasetArray::appendNewDataset()
{
  appendNullElement();
  privateResetParameters();
  return dataset(numElements() - 1);
}


StaticDataset *StaticDatasetArray::appendNewActiveDataset()
{
  appendNullElement();
  setActiveIndex(numElements() - 1);
  privateResetParameters();
  return dataset(numElements() - 1);
}


StaticDataset *StaticDatasetArray::appendNewReferenceDataset()
{
  appendNullElement();
  setReferenceIndex(numElements() - 1);
  privateResetParameters();
  return dataset(numElements() - 1);
}


void StaticDatasetArray::removeDataset(int index)
{
  assert(index >= 0);
  assert(index < numElements());
  assert(numElements() >= 2);
  removeElement(index);
  privateResetParameters();
}



//------------------ private reset parameters -----------------------//
//------------------ private reset parameters -----------------------//
//------------------ private reset parameters -----------------------//

      // private.

void StaticDatasetArray::privateResetParameters()
{
  int number = numElements();
  for(int index = 0; index < number; index++)
      {
      int active    = (index == getActiveIndex());
      int reference = (index == getReferenceIndex());
      dataset(index)->setActiveFlagByDatasetArrayOnly    (active);
      dataset(index)->setReferenceFlagByDatasetArrayOnly (reference);
      dataset(index)->setIndexByDatasetArrayOnly         (index);
      }
}



//------------------- overriding virtual functions ------------------//
//------------------- overriding virtual functions ------------------//
//------------------- overriding virtual functions ------------------//

       // private.

void *StaticDatasetArray::doCreateObject()
{
  return new StaticDataset(_progname, _informer);
}


void  StaticDatasetArray::doDeleteObject (void *object)
{
  delete (StaticDataset*)object;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

