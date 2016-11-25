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

//-------------------------- vf_edit_misc.cc ---------------------------//
//-------------------------- vf_edit_misc.cc ---------------------------//
//-------------------------- vf_edit_misc.cc ---------------------------//

//            implementation file for the VfEditMisc class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_misc.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_function.hh"
#include "vf/vf_constants.hh"
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


VfEditMisc::VfEditMisc()
           : VfEditBase(INFORM_TOTAL),
             _what                (WHAT_FEET2METERS),
             _vwater              (5000)
{
}



VfEditMisc::~VfEditMisc()
{
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditMisc::setWhat   (int   value)
{
  assert(value == WHAT_FEET2METERS ||
         value == WHAT_METERS2FEET ||
         value == WHAT_WATER);
  _what = value;
}


void   VfEditMisc::setWaterVelocity   (float value)
{
  _vwater = ConstrainValue(value, 99.0, 9999.0);
}



//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditMisc::virtualCheck (VfKernal* /*kernal*/, char *msg)
{
  switch(_what)
     {
     case WHAT_FEET2METERS: strcpy(msg, "converting feet to meters..."); break;
     case WHAT_METERS2FEET: strcpy(msg, "converting meters to feet..."); break;
     case WHAT_WATER      : strcpy(msg, "removing water velocity..."  ); break;
     default: assert(FALSE);
     }
  return FALSE;
}



//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditMisc::virtualEdit (VfKernal *kernal, char *msg)
{
  long nfun = kernal->numVelocityFunctions();
  long kount  = 0;
  long kounte = 0;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(reject(kernal, ifun, "editing function")) continue;
      VfFunction *velfun = kernal->velfun(ifun);
      switch(_what)
          {
          case WHAT_FEET2METERS: velfun->convertFeetToMeters(); break;
          case WHAT_METERS2FEET: velfun->convertMetersToFeet(); break;
          case WHAT_WATER      : velfun->removeWaterVelocity(_vwater); break;
          default: assert(FALSE);
          }
      kount++;
      if(velfun->getErrorFlag() != ERROR_NONE) kounte++;
      }
  if(kounte > 0)
      {
      sprintf(msg, "%d functions edited with %d errors", kount, kounte);
      return TRUE;
      }
  sprintf(msg, "%d functions successfully edited", kount);
  return FALSE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

