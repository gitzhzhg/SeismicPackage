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

//-------------------------- vf_edit_types.cc ---------------------------//
//-------------------------- vf_edit_types.cc ---------------------------//
//-------------------------- vf_edit_types.cc ---------------------------//

//            implementation file for the VfEditTypes class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_types.hh"
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


VfEditTypes::VfEditTypes()
           : VfEditBase(INFORM_TYPE, "edit types"),
             _type                (VTNM)
{
}



VfEditTypes::~VfEditTypes()
{
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditTypes::setType   (int   value)
{
  assert(value >= FIRSTTYPE && value <= LASTTYPE);
  _type = value;
}



//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditTypes::virtualCheck (VfKernal* /*kernal*/, char *msg)
{
  strcpy(msg, "editing velocity function types...");
  return FALSE;
}



//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditTypes::virtualEdit (VfKernal *kernal, char *msg)
{
  long nfun = kernal->numVelocityFunctions();
  long kount = 0;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(reject(kernal, ifun, NULL)) continue;
      kernal->velfun(ifun)->setDefaultType(_type);
      kount++;
      }
  sprintf(msg, "types reset on %d velocity functions", kount);
  return FALSE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

