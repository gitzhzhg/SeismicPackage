
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
//---------------------- pp_top_gui.cc ------------------------//
//---------------------- pp_top_gui.cc ------------------------//
//---------------------- pp_top_gui.cc ------------------------//

//           implementation file for the PpTopGui class
//               derived from the SLSmartForm class
//                        subdirectory fggui


#include "fggui/pp_top_gui.hh"
#include "fggui/fg_active_choice.hh"
#include "geom/field_geometry.hh"
#include "sl/slp_toggle.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void show_trap(void *data, long /*ident*/, long /*oldvar*/,
                                                         long newvar)
{
  PpTopGui *top = (PpTopGui*)data;
  top->setShowDependentValues((Boolean)newvar);
}



//------------- update functions ----------------------------//
//------------- update functions ----------------------------//
//------------- update functions ----------------------------//


static long show_update(void *data)
{
  PpTopGui *top = (PpTopGui*)data;
  return top->showDependentValues();
}
 


//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


PpTopGui::PpTopGui(SLDelay *slparent, char *name, FieldGeometry *fg)
       : SLSmartForm(slparent, name, NULL, TRUE),
               _fg                     (fg),
               _show_dependent_values  (TRUE)
{
  assert(slparent && fg);

  SLpToggle *show  = new SLpToggle (this, "show", 0,
                                          "show dependent values");

  FgActiveChoice *group = new FgActiveChoice(this, "group", fg,
            FgActiveChoice::GROUP_PLUS_PP, "set and find active group");

  FgActiveChoice *trace = new FgActiveChoice(this, "trace", fg,
            FgActiveChoice::TRACE_PLUS_PP, "set and find active trace");

//                left   right    top      bottom

  attach(show   , this , NULL   , this   , NULL);
  attach(group  , NULL , this   , this   , NULL);
  attach(trace  , NULL , this   , group  , NULL);

  show   ->setItrap      (show_trap         , this);
  show   ->setupIvarFun  (show_update       , this);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


PpTopGui::~PpTopGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

