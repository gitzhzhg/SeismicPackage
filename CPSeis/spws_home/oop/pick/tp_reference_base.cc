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
//************************ COPYRIGHT NOTICE ******************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        ***
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         ***
//************************ COPYRIGHT NOTICE ******************************



//************************************************************************
//***             Base class for deriving reference overlay files.     ***
//***             Author:Michael L. Sherrill 01/2002                   ***
//************************************************************************


#include "pick/tp_reference_base.hh"
#include "cprim.h"
#include <iostream.h>


//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//

TpReferenceBase::TpReferenceBase(SLDelay    *slparent,
                     char       *name,
                     int         io,
                     FileBase   *file,
                     const char *label,
                     HelpCtx     hctx)
  : SLFileChoice(slparent, name, io, file, label, hctx, True, True, True, False)

{
  _is_mute_file = 0;
}



//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//

TpReferenceBase::~TpReferenceBase()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
