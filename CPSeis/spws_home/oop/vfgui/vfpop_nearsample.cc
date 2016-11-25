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

//------------------------- vfpop_nearsample.cc ---------------------------//
//------------------------- vfpop_nearsample.cc ---------------------------//
//------------------------- vfpop_nearsample.cc ---------------------------//

//          implementation file for the VfpopNearsample class
//               derived from the VfpopEditBase class
//                       subdirectory vfgui


#include "vfgui/vfpop_nearsample.hh"
#include "vf/vf_edit_nearsample.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_kernal.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_sep.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include "sl/slp_push.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include "sl/slp_toggle.hh"
#include <stream.h>
#include <assert.h>


#define EDIT     VfEditNearsample *edit    = (VfEditNearsample*)data;
#define MANAGER  VfManager        *manager = (VfManager      *)data;

#define WAO 3


//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


#define TRAP(long2, type_trap, setResampleType)           \
static void type_trap(void *data, long /*ident*/,         \
                        long2 /*oldvar*/, long2 newvar)   \
{                                                         \
  EDIT                                                    \
  edit->setResampleType;                                  \
}


TRAP (long ,     type_trap, setResampleType   ((int)newvar-1))
TRAP (long ,    which_trap, setWhichAbscissae ((int)newvar-WAO))
TRAP (float,       x1_trap, setX1                  (newvar))
TRAP (float,       y1_trap, setY1                  (newvar))
TRAP (float,     xinc_trap, setXinc                (newvar))
TRAP (float,     yinc_trap, setYinc                (newvar))
TRAP (long ,       nx_trap, setNx                  (newvar))
TRAP (long ,       ny_trap, setNy                  (newvar))
TRAP (float,     xend_trap, setXend                (newvar))
TRAP (float,     yend_trap, setYend                (newvar))



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


#define UPDATE(long2, nfun_upfun, numVelocityFunctions)       \
static long2 nfun_upfun(void *data)                           \
{                                                             \
  MANAGER                                                     \
  return manager->activeDataset()->numVelocityFunctions;      \
}



UPDATE (long, nfun_upfun, numVelocityFunctions           ())
UPDATE (long, nerr_upfun, numVelocityFunctionsWithErrors ())
UPDATE (long, nray_upfun, numRaytracedVelocityFunctions  ())


#define UPFUN(long2, type_upfun, getResampleType)      \
static long2 type_upfun(void *data)                    \
{                                                      \
  EDIT                                                 \
  return edit->getResampleType;                        \
}


UPFUN (long ,     type_upfun, getResampleType   ()+1)
UPFUN (long ,    which_upfun, getWhichAbscissae ()+WAO)
UPFUN (float,       x1_upfun, getX1             ())
UPFUN (float,       y1_upfun, getY1             ())
UPFUN (float,     xinc_upfun, getXinc           ())
UPFUN (float,     yinc_upfun, getYinc           ())
UPFUN (long ,       nx_upfun, getNx             ())
UPFUN (long ,       ny_upfun, getNy             ())
UPFUN (float,     xend_upfun, getXend           ())
UPFUN (float,     yend_upfun, getYend           ())



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopNearsample::VfpopNearsample(SLDelay *slparent, char *name,
                   VfManager *manager, ContainerList *clist)
       : VfpopEditBase(slparent, name, manager, clist, "NEARSAMPLE_OVERVIEW")
{
  _edit = new VfEditNearsample(manager->utilities());

  SLSmartForm *row1 = new SLSmartForm(_editor, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(_editor, "row2", NULL);
  SLSmartForm *row3 = new SLSmartForm(_editor, "row3", NULL);
  SLSmartForm *row4 = new SLSmartForm(_editor, "row4", NULL);
  SLSep       *sep1 = new SLSep      (_editor, "sep1");
  SLSep       *sep2 = new SLSep      (_editor, "sep2");
  SLSep       *sep3 = new SLSep      (_editor, "sep3");

//                       LEFT     RIGHT    TOP      BOTTOM
  _editor->attach(row1, _editor, _editor, _editor,  NULL  ,  0, 0, 5);
  _editor->attach(sep1, _editor, _editor,  row1  ,  NULL  ,  0, 0, 5);
  _editor->attach(row2, _editor, _editor,  sep1  ,  NULL  ,  0, 0, 5);
  _editor->attach(sep2, _editor, _editor,  row2  ,  NULL  ,  0, 0, 5);
  _editor->attach(row3, _editor, _editor,  sep2  ,  NULL  ,  0, 0, 5);
  _editor->attach(sep3, _editor, _editor,  row3  ,  NULL  ,  0, 0, 5);
  _editor->attach(row4, _editor, _editor,  sep3  , _editor,  0, 0, 5, 5);

////////// row1:

  SL2Text *nfun  = new SL2Text (row1, "nfun", 0,
                          " #functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nerr  = new SL2Text (row1, "nerr", 0,
                          " #with errors:", SLpText::_LONG, 6);
  SL2Text *nray  = new SL2Text (row1, "nray", 0,
                          " #raytraced:", SLpText::_LONG, 6);

  nfun->showLabelAppearance();
  nerr->showLabelAppearance();
  nray->showLabelAppearance();

  nfun  ->setupIvarFun (  nfun_upfun, manager);
  nerr  ->setupIvarFun (  nerr_upfun, manager);
  nray  ->setupIvarFun (  nray_upfun, manager);

  row1->showEvenSpacing();

////////// row2:

  SLpOption *type = new SLpOption(row2, "typexxx", 0,
                                            "Type of function to resample:");
  for(int i = FIRSTTYPE; i <= LASTTYPE; i++)
      {
      if(manager->utilities()->abscissaIsThickness(i)) continue;
      type->addOption("typexxx", i+1,
                          (char*)_manager->utilities()->typeDescription(i));
      }

  type ->setItrap      (   type_trap       ,   _edit);
  type ->setupIvarFun  (   type_upfun      ,   _edit);

//                   LEFT  RIGHT  TOP  BOTTOM
  row2->attach(type, row2, NULL, row2, row2);

////////// row3:

  SL2Text *x1    = new SL2Text (row3, "3", 0,
                        "first X:", SLpText::_FLOAT, 10, 3);
  SL2Text *y1    = new SL2Text (row3, "3", 0,
                        "first Y:", SLpText::_FLOAT, 10, 3);
  SL2Text *xinc  = new SL2Text (row3, "3", 0,
                        "X incr:", SLpText::_FLOAT, 5, 3);
  SL2Text *yinc  = new SL2Text (row3, "3", 0,
                        "Y incr:", SLpText::_FLOAT, 5, 3);
  SL2Text *xend  = new SL2Text (row3, "3", 0,
                        "last X:", SLpText::_FLOAT, 10, 3);
  SL2Text *yend  = new SL2Text (row3, "3", 0,
                        "last Y:", SLpText::_FLOAT, 10, 3);
  SL2Text *nx    = new SL2Text (row3, "3", 0,
                        "#X locations:", SLpText::_LONG, 5);
  SL2Text *ny    = new SL2Text (row3, "3", 0,
                        "#Y locations:", SLpText::_LONG, 5);

  x1      ->setFtrap     (      x1_trap, _edit);
  y1      ->setFtrap     (      y1_trap, _edit);
  xinc    ->setFtrap     (    xinc_trap, _edit);
  yinc    ->setFtrap     (    yinc_trap, _edit);
  xend    ->setFtrap     (    xend_trap, _edit);
  yend    ->setFtrap     (    yend_trap, _edit);
  nx      ->setItrap     (      nx_trap, _edit);
  ny      ->setItrap     (      ny_trap, _edit);

  x1      ->setupFvarFun (      x1_upfun, _edit);
  y1      ->setupFvarFun (      y1_upfun, _edit);
  xinc    ->setupFvarFun (    xinc_upfun, _edit);
  yinc    ->setupFvarFun (    yinc_upfun, _edit);
  xend    ->setupFvarFun (    xend_upfun, _edit);
  yend    ->setupFvarFun (    yend_upfun, _edit);
  nx      ->setupIvarFun (      nx_upfun, _edit);
  ny      ->setupIvarFun (      ny_upfun, _edit);

//                       LEFT     RIGHT  TOP      BOTTOM
  row3->attach(x1      , row3   , NULL,  row3    , NULL, 30, 0, 0, 0);
  row3->attach(y1      , row3   , NULL,  x1      , row3, 30, 0, 0, 0);
  row3->attach(xinc    , x1     , NULL,  row3    , NULL, 10, 0, 0, 0);
  row3->attach(yinc    , y1     , NULL,  x1      , row3, 10, 0, 0, 0);
  row3->attach(xend    , xinc   , NULL,  row3    , NULL, 10, 0, 0, 0);
  row3->attach(yend    , yinc   , NULL,  x1      , row3, 10, 0, 0, 0);
  row3->attach(nx      , xend   , NULL,  row3    , NULL, 10, 0, 0, 0);
  row3->attach(ny      , yend   , NULL,  x1      , row3, 10, 0, 0, 0);

////////// row4:

  row4->showEvenSpacing();

  SLpOption *which = new SLpOption(row4, "whichxxx", 0, "");
  which->addOption("whichxxx", VfKernal::NEAREST_ABSCISSAE + WAO,
                   "get times/depths from single nearest velocity function");
  which->addOption("whichxxx", VfKernal::RESTRICTED_ABSCISSAE + WAO,
                   "get times/depths from all neighboring velocity functions");

  which->setItrap      (which_trap       ,   _edit);
  which->setupIvarFun  (which_upfun      ,   _edit);

//                    LEFT  RIGHT  TOP  BOTTOM
  row4->attach(which, row4, NULL, row4, row4);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopNearsample::~VfpopNearsample()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
