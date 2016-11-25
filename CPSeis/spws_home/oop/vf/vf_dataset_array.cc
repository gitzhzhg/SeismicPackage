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

//----------------------- vf_dataset_array.cc -------------------------//
//----------------------- vf_dataset_array.cc -------------------------//
//----------------------- vf_dataset_array.cc -------------------------//

//        implementation file for the VfDatasetArray class
//                derived from the SmartArray class
//                         subdirectory vf


#include "vf/vf_dataset_array.hh"
#include "vf/vf_dataset.hh"
#include "named_constants.h"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>

#define STEP 5

//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfDatasetArray::VfDatasetArray(long number, long numedit, const char *progname,
                               VfInformer *informer, VfUtilities *utilities)
           : SmartArray(STEP),
/*
                _numedit      (MaximumValue(numedit, 0)),
*/
                _numedit      (numedit),
                _progname     (str_newstr(progname)),
                _editable     (FALSE),
                _informer     (informer),
                _utilities    (utilities)
{
  assert(_informer);
  assert(_utilities);
  number = MaximumValue(number, _numedit);
  allocateSpace(number);
  for(long index = 0; index < number; index++)
      {
      _editable = (index < _numedit);      // needed by doCreateObject.
      if(_numedit < 0) _editable = VfDataset::EDIT_BUT_SUPPRESS_BACKUPS;
      appendNullElement();
      }
  privateResetParameters();
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

VfDatasetArray::~VfDatasetArray()
{
  removeAllElements();   // required in derived class destructor.
}



//-------------- set active or reference dataset index ---------------//
//-------------- set active or reference dataset index ---------------//
//-------------- set active or reference dataset index ---------------//

     // public.
     // this function must be used instead of the setActiveIndex function.
     // this function must be used instead of the setReferenceIndex function.

void VfDatasetArray::setActiveDatasetIndex(long index)
{
  setActiveIndex(index);
  privateResetParameters();
}


void VfDatasetArray::setReferenceDatasetIndex(long index)
{
  setReferenceIndex(index);
  privateResetParameters();
}



//----------------------- append and remove dataset ------------------//
//----------------------- append and remove dataset ------------------//
//----------------------- append and remove dataset ------------------//

       // public.
       // these are the only insert/remove functions which should be used.

void VfDatasetArray::appendDataset()
{
  _editable = FALSE;      // needed by doCreateObject.
  if(_numedit < 0) _editable = VfDataset::EDIT_BUT_SUPPRESS_BACKUPS;
  appendNullElement();
  privateResetParameters();
}



void VfDatasetArray::removeDataset(long index)
{
/*
  assert(index >= _numedit);
*/
  assert(index >= _numedit || _numedit < 0);
  removeElement(index);
  privateResetParameters();
}



//------------------ private reset parameters -----------------------//
//------------------ private reset parameters -----------------------//
//------------------ private reset parameters -----------------------//

      // private.

void VfDatasetArray::privateResetParameters()
{
  long number = numElements();
  for(long index = 0; index < number; index++)
      {
      int active    = (index == getActiveIndex   ());
      int reference = (index == getReferenceIndex());
      dataset(index)->setActiveFlagByVfDatasetArrayOnly   (active);
      dataset(index)->setReferenceFlagByVfDatasetArrayOnly(reference);
      dataset(index)->setIndexByVfDatasetArrayOnly        (index);
      }
}



//------------------- overriding virtual functions ------------------//
//------------------- overriding virtual functions ------------------//
//------------------- overriding virtual functions ------------------//

       // private.

void *VfDatasetArray::doCreateObject()
{
  return new VfDataset(_editable, _informer, _utilities, _progname);
}


void  VfDatasetArray::doDeleteObject (void *object)
{
  delete (VfDataset*)object;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

