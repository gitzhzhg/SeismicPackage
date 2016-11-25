
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
//---------------------- groups_top_gui.cc ------------------------//
//---------------------- groups_top_gui.cc ------------------------//
//---------------------- groups_top_gui.cc ------------------------//

//        implementation file for the GroupsTopGui class
//               derived from the SLSmartForm class
//                        subdirectory fggui


#include "fggui/groups_top_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl2_scale.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include "sl/slp_label.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include "named_constants.h"
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
  long ngroups = fg->numGroups();
  if(newvar < 1 || newvar > ngroups) { fg->ringBell(); return; }
  fg->setActiveGroupNumberPlus(newvar);
}



//------------- update functions ----------------------------//
//------------- update functions ----------------------------//
//------------- update functions ----------------------------//


static long active_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getActiveGroupNumber();
}


static long max_ngrps_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numGroups();
}


static long min_ngrps_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numGroups() == 0) return 0;
  return 1;
}


/****
static long nsel_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numSelectedGroups();    // not yet implemented
}
****/



static long unplaced1_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->sourceGathersOutOfDate()) return INIL;
  return fg->numUnplacedSources();
}



static long unplaced2_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->receiverGathersOutOfDate()) return INIL;
  return fg->numUnplacedTraces();
}





//------------- sense update functions ----------------------------//
//------------- sense update functions ----------------------------//
//------------- sense update functions ----------------------------//


static long active_sense_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numGroups() == 0) return FALSE;
  return TRUE;
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


GroupsTopGui::GroupsTopGui(SLDelay *slparent, char *name,
                               FieldGeometry *fg)
       : SLSmartForm(slparent, name, NULL, TRUE),
                       _fg       (fg)
{
  assert(slparent && fg);

  SL2Scale    *scale = new SL2Scale     (this, "scale", 0, "",
                                            SLpScale::_HORIZONTAL, FALSE);
  SLSmartForm *row1  = new SLSmartForm  (this, "row1");
  SLSmartForm *row2  = new SLSmartForm  (this, "row2");

  SL2Text    *ngrps  = new SL2Text (row1, "ngrps" , 0, "#groups:",
                                                    SLpText::_LONG, 6);
  SL2Text    *active = new SL2Text (row1, "active", 0, "ACTIVE GROUP:",
                                                    SLpText::_LONG, 6);
//SL2Text    *nsel   = new SL2Text (row1, "nsel"  , 0, "#selected groups:",
//                                                  SLpText::_LONG, 6);
  SL2Text *unplaced1 = new SL2Text (row2, "unplaced1", 0, "#unplaced groups:",
                                                    SLpText::_LONG, 8);
  SL2Text *unplaced2 = new SL2Text (row2, "unplaced2", 0, "#unplaced traces:",
                                                    SLpText::_LONG, 8);


  row1     ->showEvenSpacing();
  row2     ->showEvenSpacing();
  ngrps    ->showLabelAppearance();
//nsel     ->showLabelAppearance();
  unplaced1->showLabelAppearance();
  unplaced2->showLabelAppearance();

//               left    right   top     bottom    L   R   T   B

  attach(scale , this  , this  , this  , NULL);
  attach(row1  , this  , this  , scale , NULL,     0,  0,  5,  0);
  attach(row2  , this  , this  , row1  , this,     0,  0,  5,  0);

  scale   ->setItrap      (active_trap        , fg);
  active  ->setItrap      (active_trap        , fg);

  scale    ->setupIvarFun  (active_update      , fg);
  scale    ->setupIminFun  (min_ngrps_update   , fg);
  scale    ->setupImaxFun  (max_ngrps_update   , fg);
  ngrps    ->setupIvarFun  (max_ngrps_update   , fg);
  active   ->setupIvarFun  (active_update      , fg);
//nsel     ->setupIvarFun  (nsel_update        , fg);
  unplaced1->setupIvarFun  (unplaced1_update   , fg);
  unplaced2->setupIvarFun  (unplaced2_update   , fg);

  active->setupSenseFun (active_sense_update, fg);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


GroupsTopGui::~GroupsTopGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

