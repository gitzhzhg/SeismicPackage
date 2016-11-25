
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
//---------------------- midpoints_top_gui.cc ------------------------//
//---------------------- midpoints_top_gui.cc ------------------------//
//---------------------- midpoints_top_gui.cc ------------------------//

//        implementation file for the MidpointsTopGui class
//               derived from the SLSmartForm class
//                        subdirectory fggui


#include "fggui/midpoints_top_gui.hh"
#include "fggui/fg_user_abort_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl2_scale.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include "sl/slp_label.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void active_trap
               (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ncmps = fg->numCmpGathers();
  if(newvar < 1 || newvar > ncmps) { fg->ringBell(); return; }
  fg->setActiveCmpIndex(newvar-1);
}


static void upfold_trap(void *data, long /*ident*/)
{
  MidpointsTopGui *gui = (MidpointsTopGui*)data;
  FieldGeometry   *fg  = gui->getFieldGeometry();
  FgUserAbortGui  *uag = new FgUserAbortGui(fg, gui->topWidget());
  fg->updateLiveFold();
  delete uag;
}



//------------- update functions ----------------------------//
//------------- update functions ----------------------------//
//------------- update functions ----------------------------//


static long active_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getActiveCmpIndex() + 1;
}


static long max_ncmps_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numCmpGathers();
}


static long min_ncmps_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numCmpGathers() == 0) return 0;
  return 1;
}


static long nsel_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numSelectedCmpGathers();
}


static long ntr_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numTraces();
}


static long ntr2_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numUnplacedTraces();
}



//------------- sense update functions ----------------------------//
//------------- sense update functions ----------------------------//
//------------- sense update functions ----------------------------//


static long active_sense_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numCmpGathers() == 0) return FALSE;
  return TRUE;
}


static long upfold_sense_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return ( fg->numTraces() > 0 &&
          !fg->dependentUpdatesFrozen() &&
          !fg->midpointGathersOutOfDate() &&
           fg->liveFoldOutOfDate());
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


MidpointsTopGui::MidpointsTopGui(SLDelay *slparent, char *name,
                               FieldGeometry *fg)
       : SLSmartForm(slparent, name, NULL, TRUE),
                       _fg       (fg)
{
  assert(slparent && fg);

  SL2Scale    *scale = new SL2Scale     (this, "scale", 0, "",
                                            SLpScale::_HORIZONTAL, FALSE);
  SLSmartForm *row1  = new SLSmartForm  (this, "row1");
  SLSmartForm *row2  = new SLSmartForm  (this, "row2");

  SL2Text   *ncmps  = new SL2Text (row1, "ncmps" , 0, "#CMPs:",
                                                    SLpText::_LONG, 6);
  SL2Text   *active = new SL2Text (row1, "active", 0, "ACTIVE CMP:",
                                                    SLpText::_LONG, 6);
  SL2Text   *nsel   = new SL2Text (row1, "nsel"  , 0, "#selected CMPs:",
                                                    SLpText::_LONG, 6);
  SL2Text   *ntr    = new SL2Text (row2, "ntr"   , 0, "#traces:",
                                                    SLpText::_LONG, 8);
  SLpPush   *upfold = new SLpPush (row2, "update live fold");
  SL2Text   *ntr2   = new SL2Text (row2, "ntr2"  , 0, "#unplaced traces:",
                                                    SLpText::_LONG, 8);

  row1  ->showEvenSpacing();
  row2  ->showEvenSpacing();
  ncmps ->showLabelAppearance();
  nsel  ->showLabelAppearance();
  ntr   ->showLabelAppearance();
  ntr2  ->showLabelAppearance();

//               left    right   top     bottom    L   R   T   B

  attach(scale , this  , this  , this  , NULL);
  attach(row1  , this  , this  , scale , NULL,     0,  0,  5,  0);
  attach(row2  , this  , this  , row1  , this,     0,  0,  5,  0);

  scale ->setItrap      (active_trap        , fg);
  active->setItrap      (active_trap        , fg);
  upfold->setAtrap      (upfold_trap        , this);

  scale ->setupIvarFun  (active_update      , fg);
  scale ->setupIminFun  (min_ncmps_update   , fg);
  scale ->setupImaxFun  (max_ncmps_update   , fg);
  ncmps ->setupIvarFun  (max_ncmps_update   , fg);
  active->setupIvarFun  (active_update      , fg);
  nsel  ->setupIvarFun  (nsel_update        , fg);
  ntr   ->setupIvarFun  (ntr_update         , fg);
  ntr2  ->setupIvarFun  (ntr2_update        , fg);

  active->setupSenseFun (active_sense_update, fg);
  upfold->setupSenseFun (upfold_sense_update, fg);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


MidpointsTopGui::~MidpointsTopGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

