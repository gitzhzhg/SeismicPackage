
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
//---------------------- transform_pop.cc ------------------------------//
//---------------------- transform_pop.cc ------------------------------//
//---------------------- transform_pop.cc ------------------------------//

//           implementation file for the TransformPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to edit the grid tansformation
      // within field geometry data.


#include "fggui/transform_pop.hh"
#include "fggui/transform_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>



//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//


static void apply_trap(void *data, long /*ident*/)
{
  TransformPop        *pop = (TransformPop*)data;
  FieldGeometry        *fg = pop->getFieldGeometry();
  fg->setGridTransformValues();
}


static void ok_trap(void *data, long ident)
{
  TransformPop *pop = (TransformPop*)data;
  apply_trap(data, ident);
  pop->unmanage();
}


static void reset_trap(void *data, long /*ident*/)
{
  TransformPop        *pop = (TransformPop*)data;
  FieldGeometry        *fg = pop->getFieldGeometry();
  fg->resetTestingGridTransformValues();
}


static void cancel_trap(void *data, long ident)
{
  TransformPop *pop = (TransformPop*)data;
  reset_trap(data, ident);
  pop->unmanage();
}



//----------------- static sense update functions --------------//
//----------------- static sense update functions --------------//
//----------------- static sense update functions --------------//


static long ok_sense_upfun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->allowModifyingGridTransform();
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


TransformPop::TransformPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, TRUE),
                    _fg         (fg)
{
  assert(_fg && dcp);
  setTitle("Grid Transformation");
  TransformGui *gui = new TransformGui(this, "transform_gui",
                                                    _fg, clist);
  setWorkArea(gui);

  SLpPush *push_ok      = addBottomOK     (0, ok_trap    , this);
  SLpPush *push_apply   = addBottomApply  (0, apply_trap , this);
                          addBottomReset  (0, reset_trap , this);
                          addBottomCancel (0, cancel_trap, this);
  SLpPush *push_control = addBottomPush("Data Control...");
  addBottomHelp   ("TRANSFORM_POP");

  push_control->manageShellWhenPressed((SLShellContainer*)dcp);
  push_ok     ->setupSenseFun(ok_sense_upfun, _fg);
  push_apply  ->setupSenseFun(ok_sense_upfun, _fg);
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


TransformPop::~TransformPop()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
