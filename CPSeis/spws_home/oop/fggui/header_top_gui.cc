
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
//---------------------- header_top_gui.cc ------------------------//
//---------------------- header_top_gui.cc ------------------------//
//---------------------- header_top_gui.cc ------------------------//

//           implementation file for the HeaderTopGui class
//               derived from the SLSmartForm class
//                        subdirectory fggui


#include "fggui/header_top_gui.hh"
#include "fggui/fg_active_choice.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_range_select.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_option.hh"
#include "sl/slp_arrow.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_push.hh"
#include "sl/sl2_arrows.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>


//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void ve_trap(void *data, long /*ident*/,
                 float /*oldvar*/, float newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(newvar <= 0)
      {
      fg->ringBell();
      return;
      }
  fg->setVe(newvar);
}


static void ref_trap(void *data, long /*ident*/,
                 float /*oldvar*/, float newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->setRef(newvar);
}


static void fixdist_trap(void *data, long /*ident*/,
                 float /*oldvar*/, float newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->setFixdist(newvar);
}


/*
static void ndpt_trap(void *data, long / *ident* /,
                 long / *oldvar* /, long newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(newvar <= 0)
      {
      fg->ringBell();
      return;
      }
  fg->setNdpt(newvar);
}
*/



//------------------- update functions ------------------------//
//------------------- update functions ------------------------//
//------------------- update functions ------------------------//


static float ve_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getVe();
}


static float ref_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getRef();
}


static float fixdist_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getFixdist();
}


/*
static long ndpt_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getNdpt();
}
*/



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


HeaderTopGui::HeaderTopGui(SLDelay *slparent, char *name, FieldGeometry *fg)
       : SLSmartForm(slparent, name, NULL, TRUE),
            _range    (NULL),     // reset below
            _show     (NULL),     // reset below
            _scroll   (NULL),     // reset below
            _more     (NULL),     // reset below
            _round    (NULL),     // reset below
            _sort     (NULL),     // reset below
            _source   (NULL),     // reset below
            _gather   (NULL)      // reset below
{
  assert(slparent && fg);

  _range            = new SLRangeSelect (this, "range");
  SLSmartForm *row  = new SLSmartForm   (this, "row");
  _source           = new SL2Arrows     (this, "source", "source", 2);
  _gather           = new FgActiveChoice(this, "gather", fg);

  SL2Text *ve      = new SL2Text
                          (row, "ve"     , 0, "Ve:"     , SLpText::_FLOAT, 6);
  SL2Text *ref     = new SL2Text
                          (row, "ref"    , 0, "Ref:"    , SLpText::_FLOAT, 6);
  SL2Text *fixdist = new SL2Text
                          (row, "fixdist", 0, "Fixdist:", SLpText::_FLOAT, 6);
/*
  SL2Text *ndpt    = new SL2Text
                          (row, "ndpt"   , 0, "Ndpt:"   , SLpText::_LONG , 6);
*/
  _more    = new SLpOption(row, "more"   , 0, "");

  _show    = new SLpOption(this, "show"   , 0, "");
  _scroll  = new SLpOption(this, "scroll" , 0, "");
  _round   = new SLpToggle(this, "round"  , 0, "show rounded header values");
  _sort    = new SLpPush  (this, "sort"   , 0, "sort headers");

  row     ->showEvenSpacing();

//                 left      right   top     bottom

  attach(_range  , this    , this  , this  , NULL);
  attach(row     , this    , this  , _range, NULL);
  attach(_show   , this    , NULL  , row   , _sort,  0,  0,  10);
  attach(_source , _show   , NULL  , NULL  , _sort, 10,  0,   0,  4);
  attach(_scroll , _source , this  , row   , _sort, 10,  0,  10);
  attach(_round  , this    , NULL  , NULL  , this , 20);
  attach(_gather , _round  , NULL  , NULL  , this , 40);
  attach(_sort   , NULL    , this  , NULL  , this ,  0, 20,  10);

  ve     ->setFtrap     (     ve_trap, fg);
  ref    ->setFtrap     (    ref_trap, fg);
  fixdist->setFtrap     (fixdist_trap, fg);
/*
  ndpt   ->setItrap     (   ndpt_trap, fg);
*/

  ve     ->setupFvarFun (     ve_update, fg);
  ref    ->setupFvarFun (    ref_update, fg);
  fixdist->setupFvarFun (fixdist_update, fg);
/*
  ndpt   ->setupIvarFun (   ndpt_update, fg);
*/

  // option choices, traps, and update functions are added to
  //   most of these GUIs in HeaderTableGui.
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


HeaderTopGui::~HeaderTopGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

