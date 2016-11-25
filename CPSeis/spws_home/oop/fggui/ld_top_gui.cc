
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
//---------------------- ld_top_gui.cc ------------------------//
//---------------------- ld_top_gui.cc ------------------------//
//---------------------- ld_top_gui.cc ------------------------//

//           implementation file for the LdTopGui class
//               derived from the SLSmartForm class
//                        subdirectory fggui


#include "fggui/ld_top_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/slp_label.hh"
#include "sl/sl2_text.hh"
#include "sl/sl2_scale.hh"
#include "sl/slp_option.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_radio.hh"
#include "sl/radio_list.hh"
#include "sl/sl_column.hh"
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
  if(newvar == INIL) return;
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->preMultipleOperations();
  fg->setActiveLineNumber(newvar);
  if(fg->getActiveLineNumber() != newvar)
      {
      fg->placeNewLine(newvar);
      fg->setActiveLineNumber(newvar);
      }
  fg->postMultipleOperations();
}


static void scale_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->setActiveLineNumber(newvar);
}


static void chain_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  LdTopGui *top = (LdTopGui*)data;
  FieldGeometry *fg = top->getFieldGeometry();
  fg->setChaining((int)newvar);
}


static void fixdist_trap(void *data, long /*ident*/,
                 float /*oldvar*/, float newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->setFixdist(newvar);
}


static void show_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  LdTopGui *top = (LdTopGui*)data;
  top->setShowDependentValues((Boolean)newvar);
}



//------------- update functions ----------------------------//
//------------- update functions ----------------------------//
//------------- update functions ----------------------------//


static long active_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getActiveLineNumber();
}


static long scale_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long value = fg->getActiveLineNumber();
  if(value == INIL) return 0;
  return value;
}


static long scale_min_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long value = fg->getSmallestLineNumber();
  if(value == INIL) return 0;
  return value;
}


static long scale_max_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long value = fg->getLargestLineNumber();
  if(value == INIL) return 0;
  return value;
}


static long chain_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getChaining();
}
 

static float fixdist_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getFixdist();
}


static long chain_sense_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->allowChangeChaining();
}
 

static long show_update(void *data)
{
  LdTopGui *top = (LdTopGui*)data;
  return top->showDependentValues();
}
 

static long nflags_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return INIL;
  return fg->numFlagsOnLine(ixl);
}
 
static long nsours_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return INIL;
  if(fg->sourceGathersOutOfDate()) return INIL;
  return fg->numSourcesOnLine(ixl);
}
 
static long nrecs_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return INIL;
  if(fg->receiverGathersOutOfDate()) return INIL;
  return fg->numReceiversOnLine(ixl);
}
 
static long nsel_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return INIL;
  return fg->numSelectedFlagsOnLine(ixl);
}
 
 
static char *sort_update(void *data)
{
  static char blank[] = " ";
  static char msg  [] = "SHOTPOINTS ARE DUPLICATED OR NOT IN ORDER";
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return blank;
  if(fg->shotpointsAreDuplicatedOrNotSorted(ixl)) return msg;
  return blank;
}


static float min_inc_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return FNIL;
  return fg->getMinShotpointIncrOnLine(ixl);
}


static float max_inc_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixl = fg->getActiveLineIndex();
  if(ixl == -1) return FNIL;
  return fg->getMaxShotpointIncrOnLine(ixl);
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


LdTopGui::LdTopGui(SLDelay *slparent, char *name, FieldGeometry *fg)
       : SLSmartForm(slparent, name, NULL, TRUE),
               _fg                     (fg),
               _show_dependent_values  (TRUE)
{
  assert(slparent && fg);

  SLSmartForm *row1 = new SLSmartForm(this, "row1");
  SLSmartForm *row2 = new SLSmartForm(this, "row2");

  SL2Text   *active = new SL2Text (row1, "active", 0, "ACTIVE LINE:",
                                                    SLpText::_LONG, 6);
  SL2Scale  *scale  = new SL2Scale(row1, "scale", 0, "",
                                            SLpScale::_HORIZONTAL, FALSE);
  SL2Text   *nflags = new SL2Text (this, "nflags", 0, "#flags:",
                                           SLpText::_LONG, 6);
  SL2Text   *nsours = new SL2Text (this, "nsours", 0, "#sources:",
                                           SLpText::_LONG, 6);
  SL2Text   *nrecs  = new SL2Text (this, "nrecs", 0, "#receivers:",
                                           SLpText::_LONG, 6);
  SL2Text   *nsel   = new SL2Text (this, "nsel", 0, "#selected flags:",
                                           SLpText::_LONG, 6);
  SLpToggle *show   = new SLpToggle (row2, "show", 0,
                                          "show dependent values");
  SLpOption *chain  = new SLpOption (row2, "chaining", 0, "");
  SL2Text   *fixdist= new SL2Text (row2, "fixdist", 0, "Fixdist:",
                                                    SLpText::_FLOAT, 6);
  SLpText   *sort   = new SLpText (this, "sort");
  SLpLabel  *label  = new SLpLabel(this, "label", 0,
                                          "min/max shotpoint increments:");
  SLpText   *inc1   = new SLpText (this, "inc1", 0, SLpText::_FLOAT, 6);
  SLpText   *inc2   = new SLpText (this, "inc2", 0, SLpText::_FLOAT, 6);

  row2  ->showEvenSpacing();
  nflags->showLabelAppearance();
  nsours->showLabelAppearance();
  nrecs ->showLabelAppearance();
  nsel  ->showLabelAppearance();
  sort  ->showLabelAppearance();
  inc1  ->showLabelAppearance();
  inc2  ->showLabelAppearance();

  chain ->addOption("horizontal chaining", HORIZONTAL_CHAINING);
  chain ->addOption("slope chaining"     , SLOPE_CHAINING);
  chain ->addOption("no chaining"        , NO_CHAINING);

//               left    right   top     bottom

  attach(active, row1  , NULL  , NULL  , row1);
  attach(scale , active, row1  , row1  , row1);

  attach(row1  , this  , nrecs , this  , NULL);
  attach(row2  , this  , nsel  , row1  , NULL);
//attach(sort  , this  , nsel  , row2  , NULL);
  attach(sort  , this  , label , row2  , this);

  attach(nflags, NULL  , this  , this  , NULL);
  attach(nsours, NULL  , this  , nflags, NULL);
  attach(nrecs , NULL  , this  , nsours, NULL);
  attach(nsel  , NULL  , this  , nrecs , NULL);
  attach(label , NULL  , inc1  , nsel  , this);
  attach(inc1  , NULL  , inc2  , nsel  , this);
  attach(inc2  , NULL  , this  , nsel  , this);

  active ->setItrap      (active_trap       , fg);
  scale  ->setItrap      (scale_trap        , fg);
  show   ->setItrap      (show_trap         , this);
  chain  ->setItrap      (chain_trap        , this);
  fixdist->setFtrap      (fixdist_trap      , fg);

  active ->setupIvarFun  (active_update     , fg);
  scale  ->setupIvarFun  (scale_update      , fg);
  scale  ->setupIminFun  (scale_min_update  , fg);
  scale  ->setupImaxFun  (scale_max_update  , fg);
  nflags ->setupIvarFun  (nflags_update     , fg);
  nsours ->setupIvarFun  (nsours_update     , fg);
  nrecs  ->setupIvarFun  (nrecs_update      , fg);
  nsel   ->setupIvarFun  (nsel_update       , fg);
  show   ->setupIvarFun  (show_update       , this);
  chain  ->setupIvarFun  (chain_update      , fg);
  fixdist->setupFvarFun  (fixdist_update    , fg);
  chain  ->setupSenseFun (chain_sense_update, fg);
  sort   ->setupCvarFun  (sort_update       , fg);
  inc1   ->setupFvarFun  (min_inc_update    , fg);
  inc2   ->setupFvarFun  (max_inc_update    , fg);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


LdTopGui::~LdTopGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

