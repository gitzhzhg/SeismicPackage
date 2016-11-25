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

//-------------------------- vf_offsets.cc --------------------------//
//-------------------------- vf_offsets.cc --------------------------//
//-------------------------- vf_offsets.cc --------------------------//

//            implementation file for the VfOffsets class
//                    not derived from any class
//                         subdirectory vf



#include "vf/vf_offsets.hh"
#include "oprim/simple_float_array.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


#define FEET_PER_METER  3.28084


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfOffsets::VfOffsets()
           :
              _muteflag  (MUTEFLAG_MATCH),
              _offmin    (0.0),
              _offmax    (10000.0),
              _omute     (NULL),
              _tmute     (NULL),
              _error     (FALSE)
{
  _omute  = new SimpleFloatArray();
  _tmute  = new SimpleFloatArray();
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

VfOffsets::~VfOffsets()
{
  delete _omute;
  delete _tmute;
}



//----------------------- get values -----------------------------//
//----------------------- get values -----------------------------//
//----------------------- get values -----------------------------//


long VfOffsets::numMuteTimes()  const
{
  return _omute->numElements();
}


float VfOffsets::getMuteOffset(long imute)  const
{
  return _omute->getValue(imute);
}


float VfOffsets::getMuteTime(long imute)  const
{
  return _tmute->getValue(imute);
}


const float *VfOffsets::getMuteOffsetPointer()  const
{
  return _omute->getArrayPointer();
}


const float *VfOffsets::getMuteTimePointer()  const
{
  return _tmute->getArrayPointer();
}



//----------------------- set values ----------------------------------//
//----------------------- set values ----------------------------------//
//----------------------- set values ----------------------------------//


void VfOffsets::setMuteFlag(int muteflag)
{
  assert(muteflag == MUTEFLAG_NONE  ||
         muteflag == MUTEFLAG_MATCH ||
         muteflag == MUTEFLAG_ARRAY);
  _muteflag = muteflag;
}


void VfOffsets::setMinimumOffset(float offmin)
{
  _offmin = MaximumValue(0.0, offmin);
  _offmax = MaximumValue(_offmin, _offmax);
}


void VfOffsets::setMaximumOffset(float offmax)
{
  _offmax = MaximumValue(_offmin, offmax);
}


void VfOffsets::setMuteOffset(long imute, float offset)
{
  _omute->setValue(imute, offset);
}


void VfOffsets::setMuteTime(long imute, float time)
{
  _tmute->setValue(imute, time);
}



//---------------- insert or remove mute offset/time pairs -------------//
//---------------- insert or remove mute offset/time pairs -------------//
//---------------- insert or remove mute offset/time pairs -------------//

          // public.

void VfOffsets::appendMutePair()
{
  _omute->appendNilElement();
  _tmute->appendNilElement();
  privateValidate();
}


void VfOffsets::insertMutePair (long imute)
{
  _omute->insertNilElement(imute);
  _tmute->insertNilElement(imute);
  privateValidate();
}


void VfOffsets::insertMutePairFromBuffer (long imute)
{
  _omute->insertElementFromBuffer(imute);
  _tmute->insertElementFromBuffer(imute);
  privateValidate();
}


void VfOffsets::removeMutePair (long imute)
{
  _omute->removeElement(imute);
  _tmute->removeElement(imute);
  privateValidate();
}


void VfOffsets::removeMutePairToBuffer (long imute)
{
  _omute->removeElementToBuffer(imute);
  _tmute->removeElementToBuffer(imute);
  privateValidate();
}



//------------------- convert between feet and meters ------------------//
//------------------- convert between feet and meters ------------------//
//------------------- convert between feet and meters ------------------//

       // public.

void VfOffsets::convertOffsetFeetToMeters()
{
  privateMultiply(1.0 / FEET_PER_METER);
}


void VfOffsets::convertOffsetMetersToFeet()
{
  privateMultiply(FEET_PER_METER);
}



//--------------------- private functions ---------------------------//
//--------------------- private functions ---------------------------//
//--------------------- private functions ---------------------------//


void VfOffsets::privateValidate()
{
  long imute1 = _omute->validateAscending();
  long imute2 = _tmute->validateAscending();
  long nmutes = _omute->numElements();
  _error = (imute1 < nmutes || imute2 < nmutes);
}


void VfOffsets::privateMultiply(float constant)
{
  if(_offmin != FNIL) _offmin *= constant;
  if(_offmax != FNIL) _offmax *= constant;
  _omute->multiplyByConstant(constant);
}




//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

