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

//-------------------------- vf_edit_names.cc ---------------------------//
//-------------------------- vf_edit_names.cc ---------------------------//
//-------------------------- vf_edit_names.cc ---------------------------//

//            implementation file for the VfEditNames class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_names.hh"
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


VfEditNames::VfEditNames()
           : VfEditBase(INFORM_STRINGS, "edit names"),
             _how                 (HOW_COUNTER)
{
  strcpy(_prefix, "VEL");
}



VfEditNames::~VfEditNames()
{
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditNames::setHow (int   value)
{
  assert(value == HOW_COUNTER ||
         value == HOW_XLOC    ||
         value == HOW_YXLOC);
  _how = value;
}



void   VfEditNames::setPrefix (const char *value)
{
  strncpy(_prefix, value, 7);
  _prefix[3] = '\0';
}



//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditNames::virtualCheck (VfKernal* /*kernal*/, char *msg)
{
  strcpy(msg, "editing velocity function names...");
  return FALSE;
}



//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditNames::virtualEdit (VfKernal *kernal, char *msg)
{
  long nfun = kernal->numVelocityFunctions();
  long kount = 0;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(reject(kernal, ifun, NULL)) continue;
      VfFunction *velfun = kernal->velfun(ifun);
      long xloc = NearestInteger(velfun->getXloc());
      long yloc = NearestInteger(velfun->getYloc());
      char vfid[22];
      switch(_how)
          {
          case HOW_COUNTER: sprintf(vfid, "%3s%5d", _prefix, ifun+1); break;
          case HOW_XLOC   : sprintf(vfid, "%3s%5d", _prefix,   xloc); break;
          case HOW_YXLOC  : sprintf(vfid, "%4d%4d",    yloc,   xloc); break;
          default: assert(FALSE);
          }
      for(int j = 0; j < 8; j++)
          {
          if(vfid[j] == ' ') vfid[j] = '-';
          }
      velfun->setVfid(vfid);
      kount++;
      }
  sprintf(msg, "names reset on %d velocity functions", kount);
  return FALSE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

