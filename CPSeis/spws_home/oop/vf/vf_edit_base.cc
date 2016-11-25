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

//---------------------- vf_edit_base.cc ------------------------//
//---------------------- vf_edit_base.cc ------------------------//
//---------------------- vf_edit_base.cc ------------------------//

//          implementation file for the VfEditBase class
//                   not derived from any class
//                         subdirectory vf


#include "vf/vf_edit_base.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_function.hh"
#include "vf/vf_informer.hh"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


VfEditBase::VfEditBase(int style, const char *verb)
       :
         _style   (style),
         _choice  (CHOICE_ALL),
         _verb    (str_newstr(verb))
{
  assert(_style == INFORM_TOTAL   ||
         _style == INFORM_STRINGS ||
         _style == INFORM_TYPE);
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


VfEditBase::~VfEditBase()
{
  free(_verb);
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditBase::setChoice (int   value)
{
  assert(value == CHOICE_SELECTED ||
         value == CHOICE_ALL      ||
         value == CHOICE_ACTIVE   ||
         value == CHOICE_BLANK    ||
         value == CHOICE_EMPTY);
  _choice = value;
}



//------------------------ reject --------------------------------//
//------------------------ reject --------------------------------//
//------------------------ reject --------------------------------//

     // protected.

int VfEditBase::reject(VfKernal *kernal, long ifun, const char *gerund)
{
  if(gerund != NULL)
      {
      long nfun = kernal->numVelocityFunctions();
      kernal->informer()->showWorkingMessage(gerund, ifun, nfun);
      }
  if(_choice == CHOICE_SELECTED &&
       !kernal->velocityFunctionIsSelected(ifun)) return TRUE;
  if(_choice == CHOICE_ACTIVE   &&
        kernal->getActiveVelocityFunction() != ifun) return TRUE;
  if(_choice == CHOICE_BLANK    &&
       !kernal->velfun(ifun)->vfidIsBlank()) return TRUE;
/*
       strncmp(kernal->velfun(ifun)->getVfid(), "none", 4) != 0 &&
       strcmp (kernal->velfun(ifun)->getVfid(), ""       ) != 0 &&
       strncmp(kernal->velfun(ifun)->getVfid(), " "   , 1) != 0) return TRUE;
*/
  return FALSE;
}



//---------------------- check for errors ------------------------//
//---------------------- check for errors ------------------------//
//---------------------- check for errors ------------------------//

      // public.

int VfEditBase::checkForErrors (class VfKernal *kernal, char *msg)
{
  assert(kernal);
  assert(msg);
  long nfun   = kernal->numVelocityFunctions();
  long nsel   = kernal->numSelectedVelocityFunctions();
  long nblank = kernal->numVelocityFunctionsWithBlankNames();
  if(nfun == 0)
      {
      strcpy(msg, "no velocity functions to ");
      strcat(msg, _verb);
      return TRUE;
      }
  if(_choice == CHOICE_SELECTED && nsel == 0)
      {
      strcpy(msg, "no selected velocity functions to ");
      strcat(msg, _verb);
      return TRUE;
      }
  if(_choice == CHOICE_BLANK && nblank == 0)
      {
      strcpy(msg, "no velocity functions with blank names to ");
      strcat(msg, _verb);
      return TRUE;
      }
/*
  if(_choice == CHOICE_BLANK)
      {
      long kount = 0;
      for(long ifun = 0; ifun < nfun; ifun++)
          {
          if(reject(kernal, ifun, NULL)) continue;
          kount++;
          }
      if(kount == 0)
          {
          strcpy(msg, "no velocity functions with blank names to ");
          strcat(msg, _verb);
          return TRUE;
          }
      }
*/
  strcpy(msg, "");
  int error = virtualCheck(kernal, msg);
  if(strcmp(msg, "") == 0)
      {
      if(error) strcpy(msg, "error - editing not done");
      else      strcpy(msg, "working...");
      }
  return error;
}



//------------------------- edit kernal ----------------------------//
//------------------------- edit kernal ----------------------------//
//------------------------- edit kernal ----------------------------//

      // public.

int VfEditBase::editKernal (class VfKernal *kernal, char *msg)
{
  assert(kernal);
  assert(msg);
  strcpy(msg, "");
  int error = virtualEdit(kernal, msg);
  if(strcmp(msg, "") == 0)
      {
      if(error) strcpy(msg, "editing completed with errors");
      else      strcpy(msg, "editing successfully completed");
      }
  return error;
}



//------------------ virtual functions to override ------------------//
//------------------ virtual functions to override ------------------//
//------------------ virtual functions to override ------------------//

      // protected.

int VfEditBase::virtualCheck (class VfKernal* /*kernal*/, char *msg)
{
  strcpy(msg, "working...");
  return FALSE;
}


int VfEditBase::virtualEdit (class VfKernal* /*kernal*/, char *msg)
{
  strcpy(msg, "this edit option is not yet implemented");
  return TRUE;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
