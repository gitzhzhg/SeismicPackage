
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
//---------------------- fg_groups.cc -----------------------//
//---------------------- fg_groups.cc -----------------------//
//---------------------- fg_groups.cc -----------------------//

//            implementation file for the FgGroups class
//                    not derived from any class
//                        subdirectory geom


#include "geom/fg_groups.hh"
#include "geom/fg_group.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


FgGroups::FgGroups()
           :
             _grps      (NULL),
             _ngroups   (0)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

FgGroups::~FgGroups()
{
  clearGroups();
}



//--------------------- get group pointer -----------------------//
//--------------------- get group pointer -----------------------//
//--------------------- get group pointer -----------------------//


FgGroup *FgGroups::getGroupPointer(long group)  const
{
  assert(group >= 1 && group <= _ngroups && _grps);
  return &_grps[group - 1];
}



//----------------------- clear groups ---------------------------//
//----------------------- clear groups ---------------------------//
//----------------------- clear groups ---------------------------//

     // public.

void FgGroups::clearGroups()
{
  if(_ngroups == 0) return;
  assert(_grps);
  delete [] _grps;
  _ngroups  = 0;
  _grps     = NULL;
}



//------------------ create groups -------------------------//
//------------------ create groups -------------------------//
//------------------ create groups -------------------------//

     // public.

void FgGroups::createGroups(long ngroups)
{
  clearGroups();
  _ngroups = ngroups;
  if(_ngroups == 0) return;
  _grps = new FgGroup [_ngroups];
}



//------------------- get values -----------------------------//
//------------------- get values -----------------------------//
//------------------- get values -----------------------------//

      // public.

#define FUNCTION(getIxpp)                                   \
long FgGroups::getIxpp(long group)  const                   \
{                                                           \
  if(group < 1 || group > _ngroups || !_grps) return -1;    \
  return _grps[group - 1].getIxpp();                        \
}


FUNCTION(getIxpp)
FUNCTION(getIxlSource)
FUNCTION(getIxfSource)
FUNCTION(getIxlChan1)
FUNCTION(getIxfChan1)


long FgGroups::numUnplacedTraces(long group)  const
{                                                 
  if(group < 1 || group > _ngroups || !_grps) return INIL;
  return _grps[group - 1].numUnplacedTraces();             
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

