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

//-------------------------- vf_edit_multiply.cc ---------------------------//
//-------------------------- vf_edit_multiply.cc ---------------------------//
//-------------------------- vf_edit_multiply.cc ---------------------------//

//            implementation file for the VfEditMultiply class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_multiply.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_function.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_utilities.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//


VfEditMultiply::VfEditMultiply()
           : VfEditBase(INFORM_TOTAL, "multiply"),
             _type                (VTNM),
             _constant            (1.0),
             _min_time            (0.0),
             _max_time            (9999999.0),
             _min_depth           (0.0),
             _max_depth           (9999999.0)
{
}



VfEditMultiply::~VfEditMultiply()
{
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditMultiply::setType   (int   value)
{
  assert(value == VTNM ||
         value == VTRM ||
         value == VTAV ||
         value == VTIN ||
         value == VTDP ||
         value == VZRM ||
         value == VZAV ||
         value == VZIN);
  _type = value;
}


void   VfEditMultiply::setConstant   (float value)
{
  if(value != 0.0) _constant = value;
}


void   VfEditMultiply::setMinTime   (float value)
{
  _min_time = MaximumValue(value, 0.0);
}


void   VfEditMultiply::setMaxTime   (float value)
{
  _max_time = MaximumValue(value, 0.0);
}


void   VfEditMultiply::setMinDepth  (float value)
{
  _min_depth = MaximumValue(value, 0.0);
}


void   VfEditMultiply::setMaxDepth  (float value)
{
  _max_depth = MaximumValue(value, 0.0);
}



//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditMultiply::virtualCheck (VfKernal *kernal, char *msg)
{
  if(kernal->utilities()->abscissaIsTime(_type))
      {
      if(_max_time < _min_time)
          {
          strcpy(msg, "maximum time is less than minimum time");
          return TRUE;
          }
      }
  else   // if(kernal->utilities()->abscissaIsDepth(_type))
      {
      if(_max_depth < _min_depth)
          {
          strcpy(msg, "maximum depth is less than minimum depth");
          return TRUE;
          }
      }
  strcpy(msg, "multiplying velocities by a constant...");
  return FALSE;
}



//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditMultiply::virtualEdit (VfKernal *kernal, char *msg)
{
  float minimum, maximum;
  if(kernal->utilities()->abscissaIsTime(_type))
      {
      minimum = _min_time;
      maximum = _max_time;
      }
  else   // if(kernal->utilities()->abscissaIsDepth(_type))
      {
      minimum = _min_depth;
      maximum = _max_depth;
      }
  long nfun = kernal->numVelocityFunctions();
  long kount  = 0;
  long kounte = 0;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(reject(kernal, ifun, "editing function")) continue;
      kount++;
      VfFunction *velfun = kernal->velfun(ifun);
      long npicks = velfun->numPicks();
      float abscissae[MAXPICKS];
      float ordinates[MAXPICKS];
      velfun->getAbscissaArray(abscissae, _type);
      velfun->getOrdinateArray(ordinates, _type);
      for(long ipick = 0; ipick < npicks; ipick++)
          {
          if(abscissae[ipick] >= minimum &&
             abscissae[ipick] <= maximum &&
             abscissae[ipick] != FNIL  &&
             ordinates[ipick] != FNIL) ordinates[ipick] *= _constant;
          }
      velfun->replaceAllPicks(abscissae, ordinates, _type);
      if(velfun->getErrorFlag() != ERROR_NONE) kounte++;
      }
  sprintf(msg, "processed %d functions with %d errors", kount, kounte);
  return (kounte > 0);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

