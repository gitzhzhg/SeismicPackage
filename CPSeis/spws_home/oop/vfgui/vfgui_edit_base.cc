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

//---------------------- vfgui_edit_base.cc ------------------------//
//---------------------- vfgui_edit_base.cc ------------------------//
//---------------------- vfgui_edit_base.cc ------------------------//

//         implementation file for the VfguiEditBase class
//               derived from the SLSmartForm class
//                       subdirectory vfgui


#include "vfgui/vfgui_edit_base.hh"
#include "vf/vf_edit_base.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiEditBase::VfguiEditBase(SLDelay *slparent)
       : SLSmartForm(slparent, "vfgui_edit_base", NULL, FALSE),
               _edit         (NULL)
{
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiEditBase::~VfguiEditBase()
{
  assert(_edit);
  delete _edit;
}



//------------------------ supply edit object --------------------------//
//------------------------ supply edit object --------------------------//
//------------------------ supply edit object --------------------------//

     // protected.

void VfguiEditBase::supplyEditObject(VfEditBase *edit)
{
  assert(edit);
  _edit = edit;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

