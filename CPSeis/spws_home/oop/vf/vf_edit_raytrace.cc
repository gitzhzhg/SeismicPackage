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

//-------------------------- vf_edit_raytrace.cc ---------------------------//
//-------------------------- vf_edit_raytrace.cc ---------------------------//
//-------------------------- vf_edit_raytrace.cc ---------------------------//

//            implementation file for the VfEditRaytrace class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_raytrace.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_function.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_informer.hh"
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


VfEditRaytrace::VfEditRaytrace()
           : VfEditBase(INFORM_TOTAL, "raytrace"),
             _how                 (HOW_FORWARD)
{
}



VfEditRaytrace::~VfEditRaytrace()
{
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditRaytrace::setHow   (int   value)
{
  assert(value == HOW_FORWARD   ||
         value == HOW_INVERSE   ||
         value == HOW_RESET_NMO ||
         value == HOW_RESET_RMS);
  _how = value;
}



//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditRaytrace::virtualCheck (VfKernal* /*kernal*/, char *msg)
{
  static char *msg1 = "raytracing velocity functions...";
  static char *msg2 = "inverse raytracing velocity functions...";
  static char *msg3 = "resetting NMO velocities to RMS velocities...";
  static char *msg4 = "resetting RMS velocities to NMO velocities...";
  switch(_how)
      {
      case HOW_FORWARD  :  strcpy(msg, msg1); break;
      case HOW_INVERSE  :  strcpy(msg, msg2); break;
      case HOW_RESET_NMO:  strcpy(msg, msg3); break;
      case HOW_RESET_RMS:  strcpy(msg, msg4); break;
      default: assert(FALSE);
      }
  return FALSE;
}



//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditRaytrace::virtualEdit (VfKernal *kernal, char *msg)
{
  char gerund[60];
  switch(_how)
      {
      case HOW_FORWARD  : strcpy(gerund, "raytracing function"        ); break;
      case HOW_INVERSE  : strcpy(gerund, "inverse raytracing function"); break;
      case HOW_RESET_NMO: strcpy(gerund, "resetting function"         ); break;
      case HOW_RESET_RMS: strcpy(gerund, "resetting function"         ); break;
      default: assert(FALSE);
      }
  long nfun = kernal->numVelocityFunctions();
  long kount = 0;
  long kounte = 0;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(reject(kernal, ifun, gerund)) continue;
      VfFunction *velfun = kernal->velfun(ifun);
      switch(_how)
          {
          case HOW_FORWARD  : velfun->invokeRayTracing(VTRM); break;
          case HOW_INVERSE  : velfun->invokeRayTracing(VTNM); break;
          case HOW_RESET_NMO: velfun->cancelRayTracing(VTRM); break;
          case HOW_RESET_RMS: velfun->cancelRayTracing(VTNM); break;
          default: assert(FALSE);
          }
      kount++;
      if(velfun->getErrorFlag() != ERROR_NONE) kounte++;
      }
  char phrase[33];
  switch(_how)
      {
      case HOW_FORWARD  : strcpy(phrase, "raytraced"         ); break;
      case HOW_INVERSE  : strcpy(phrase, "inverse raytraced" ); break;
      case HOW_RESET_NMO: strcpy(phrase, "reset with new NMO"); break;
      case HOW_RESET_RMS: strcpy(phrase, "reset with new RMS"); break;
      default: assert(FALSE);
      }
  if(kounte > 0)
      {
      sprintf(msg, "%d functions %s with %d errors", kount, phrase, kounte);
      return TRUE;
      }
  sprintf(msg, "%d functions successfully %s", kount, phrase);
  return FALSE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

