
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
//---------------------- linelist_top_gui.cc ------------------------//
//---------------------- linelist_top_gui.cc ------------------------//
//---------------------- linelist_top_gui.cc ------------------------//

//        implementation file for the LinelistTopGui class
//               derived from the SLSmartForm class
//                        subdirectory fggui


#include "fggui/linelist_top_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl2_scale.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include "sl/slp_label.hh"
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


static void scale_trap
               (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->setActiveLineNumber(newvar);
}


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


static void fixdist_trap(void *data, long /*ident*/,
                 float /*oldvar*/, float newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->setFixdist(newvar);
}



//------------- update functions ----------------------------//
//------------- update functions ----------------------------//
//------------- update functions ----------------------------//


static long scale_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long value = fg->getActiveLineNumber();
  if(value == INIL) return 0;
  return value;
}


static long active_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getActiveLineNumber();
}


static float fixdist_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getFixdist();
}


static long nlines_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numLines();
}


static long text_min_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getSmallestLineNumber();
}


static long text_max_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getLargestLineNumber();
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
 

static long min_inc_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getMinLineIncrement();
}
 
 
static long max_inc_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getMaxLineIncrement();
}
 

static char *sort_update(void *data)
{
  static char blank[] = " ";
  static char msg  [] = "LINES ARE DUPLICATED OR NOT SORTED";
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->linesAreDuplicatedOrNotSorted()) return msg;
  return blank;
}


static long select_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numSelectedLines();
}
 
 

//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


LinelistTopGui::LinelistTopGui(SLDelay *slparent, char *name,
                               FieldGeometry *fg)
       : SLSmartForm(slparent, name, NULL, TRUE),
                       _fg       (fg)
{
  assert(slparent && fg);

  SL2Scale  *scale  = new SL2Scale(this, "scale", 0, "",
                                            SLpScale::_HORIZONTAL, FALSE);
  SLpLabel  *lab1   = new SLpLabel(this, "lab1", 0, "first line");
  SLpLabel  *lab2   = new SLpLabel(this, "lab2", 0, "last line");
  SLpText   *itext1 = new SLpText (this, "itext1", 0, SLpText::_LONG, 6);
  SLpText   *itext2 = new SLpText (this, "itext2", 0, SLpText::_LONG, 6);

  SL2Text   *nlines = new SL2Text (this, "nlines", 0, "#lines:",
                                                    SLpText::_LONG, 5);
  SL2Text   *active = new SL2Text (this, "active", 0, "ACTIVE LINE:",
                                                    SLpText::_LONG, 6);
  SLpLabel  *label  = new SLpLabel(this, "label", 0,
                                               "min/max line increments:");
  SLpText   *inc1   = new SLpText (this, "inc1", 0, SLpText::_LONG, 5);
  SLpText   *inc2   = new SLpText (this, "inc2", 0, SLpText::_LONG, 5);

  SLpText   *sort   = new SLpText (this, "sort");
  SL2Text  *fixdist = new SL2Text (this, "fixdist", 0, "Fixdist:",
                                                    SLpText::_FLOAT, 6);
  SL2Text   *sel    = new SL2Text (this, "sel", 0, "#selected lines:",
                                                    SLpText::_LONG, 5);

  itext1->showFramedLabelAppearance();
  itext2->showFramedLabelAppearance();
  nlines->showLabelAppearance();
  inc1  ->showLabelAppearance();
  inc2  ->showLabelAppearance();
  sort  ->showLabelAppearance();
  sel   ->showLabelAppearance();

//                left     right   top     bottom    L   R   T   B

  attach(lab1   , this   , NULL  , this  , NULL,     5,  0,  5,  0);
  attach(itext1 , lab1   , NULL  , this  , NULL,     0,  0,  5,  0);
  attach(scale  , itext1 , itext2, this  , NULL,     0,  0,  0,  0);
  attach(itext2 , NULL   , lab2  , this  , NULL,     0,  0,  5,  0);
  attach(lab2   , NULL   , this  , this  , NULL,     0,  5,  5,  0);

  attach(nlines, this  , NULL  , scale , NULL,    15,  0,  5,  0);
  attach(active, nlines, NULL  , scale , NULL,    15,  0,  5,  0);
  attach(label , NULL  , inc1  , scale , NULL,     0,  5,  5,  0);
  attach(inc1  , NULL  , inc2  , scale , NULL,     0,  0,  5,  0);
  attach(inc2  , NULL  , this  , scale , NULL,     0,  0,  5,  0);

  attach(sort   , this , fixdist, active, this,    10, 10,  5,  5);
  attach(fixdist, NULL , sel    , active, this,     0, 50,  5,  5);
  attach(sel    , NULL , this   , active, this,     0, 10,  5,  5);

  scale  ->setItrap      (scale_trap        , fg);
  active ->setItrap      (active_trap       , fg);
  fixdist->setFtrap      (fixdist_trap      , fg);

  scale  ->setupIvarFun  (scale_update      , fg);
  active ->setupIvarFun  (active_update     , fg);
  nlines ->setupIvarFun  (nlines_update     , fg);
  itext1 ->setupIvarFun  (text_min_update   , fg);
  itext2 ->setupIvarFun  (text_max_update   , fg);
  scale  ->setupIminFun  (scale_min_update  , fg);
  scale  ->setupImaxFun  (scale_max_update  , fg);
  inc1   ->setupIvarFun  (min_inc_update    , fg);
  inc2   ->setupIvarFun  (max_inc_update    , fg);
  sort   ->setupCvarFun  (sort_update       , fg);
  fixdist->setupFvarFun  (fixdist_update    , fg);
  sel    ->setupIvarFun  (select_update     , fg);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


LinelistTopGui::~LinelistTopGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

