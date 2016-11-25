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

//------------------- vfpop_transform.cc -----------------------//
//------------------- vfpop_transform.cc -----------------------//
//------------------- vfpop_transform.cc -----------------------//

//       implementation file for the VfpopTransform class
//                   derived from the SLDialog class
//                        subdirectory vfgui


#include "vfgui/vfpop_transform.hh"
#include "vfgui/vfgui_transform.hh"
#include "vf/vf_horizons.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include <stream.h>
#include <assert.h>



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


VfpopTransform::VfpopTransform(SLDelay *slparent, char *name,
                               VfHorizons *horizons)
            : SLDialog(slparent, name, NULL, TRUE)
{
  assert(horizons);
  SLSmartForm    *work = workArea();
  GridTransform  *transform = horizons->transform();
  VfInformer     *informer  = horizons->informer();
  VfguiTransform *gui  = new VfguiTransform(work, transform, informer);

  work->attach(gui, work, work, work, work);

                 addBottomRemove();
                 addBottomHelp("TRANSFORM_OVERVIEW");
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopTransform::~VfpopTransform()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
