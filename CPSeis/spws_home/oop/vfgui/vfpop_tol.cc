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

//------------------------- vfpop_tol.cc ---------------------------//
//------------------------- vfpop_tol.cc ---------------------------//
//------------------------- vfpop_tol.cc ---------------------------//

//          implementation file for the VfpopTol class
//               derived from the SLDialog class
//                       subdirectory vfgui


#include "vfgui/vfpop_tol.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include "sl/slp_push.hh"
#include "sl/slp_label.hh"
#include <stream.h>
#include <assert.h>


#define MANAGER  VfManager *manager = (VfManager*)data;

//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


static void mm_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  MANAGER
  manager->utilities()->setPickingTolerance(newvar);
}


static void xcenter_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  MANAGER
  manager->setXcenter(newvar);
}


static void xwidth_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  MANAGER
  manager->setXwidth(newvar);
}


static void ycenter_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  MANAGER
  manager->setYcenter(newvar);
}


static void ywidth_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  MANAGER
  manager->setYwidth(newvar);
}



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static float mm_upfun(void *data)
{
  MANAGER
  return manager->utilities()->getPickingTolerance();
}


static float xcenter_upfun(void *data)
{
  MANAGER
  return manager->utilities()->getXcenter();
}


static float xwidth_upfun(void *data)
{
  MANAGER
  return manager->utilities()->getXwidth();
}


static float ycenter_upfun(void *data)
{
  MANAGER
  return manager->utilities()->getYcenter();
}


static float ywidth_upfun(void *data)
{
  MANAGER
  return manager->utilities()->getYwidth();
}



//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopTol::VfpopTol(SLDelay *slparent, char *name, VfManager *manager)
       : SLDialog(slparent, name, NULL, FALSE),
             _manager   (manager)
{
  assert(_manager);
  SLSmartForm *work   = workArea();
  SLpLabel    *lab1   = new SLpLabel   (work, "lab1", 0, "Picking Tolerance");
  SLpLabel    *lab2   = new SLpLabel   (work, "lab2", 0,
             "Xbin and Ybin Matching Tolerances");
  SLSmartForm *row1   = new SLSmartForm(work, "row1", NULL, TRUE);
  SLSmartForm *row2   = new SLSmartForm(work, "row2", NULL, TRUE);

  SL2Text  *mm      = new SL2Text (row1, "mm", 0,
                           "tolerance", SLpText::_FLOAT, 8);
  SLpText  *mm2     = new SLpText (row1, "mm2");

  SLpLabel *lab3    = new SLpLabel (row2, "lab3", 0,
                                    "any bin center             bin width");
  SL2Text  *xcenter = new SL2Text  (row2, "xcenter" , 0,
                           "xcenter:", SLpText::_FLOAT, 8);
  SL2Text  *ycenter = new SL2Text  (row2, "ycenter" , 0,
                           "ycenter:", SLpText::_FLOAT, 8);
  SL2Text  *xwidth  = new SL2Text  (row2, "xwidth" , 0,
                           "xwidth:", SLpText::_FLOAT, 8);
  SL2Text  *ywidth  = new SL2Text  (row2, "ywidth" , 0,
                           "ywidth:", SLpText::_FLOAT, 8);

  mm2 ->showLabelAppearance();

  mm     ->setFtrap     (     mm_trap , manager);
  mm     ->setupFvarFun (     mm_upfun, manager);
  mm2    ->setCvar      ("millimeters");
  xcenter->setFtrap     (xcenter_trap , manager);
  xcenter->setupFvarFun (xcenter_upfun, manager);
  xwidth ->setFtrap     ( xwidth_trap , manager);
  xwidth ->setupFvarFun ( xwidth_upfun, manager);
  ycenter->setFtrap     (ycenter_trap , manager);
  ycenter->setupFvarFun (ycenter_upfun, manager);
  ywidth ->setFtrap     ( ywidth_trap , manager);
  ywidth ->setupFvarFun ( ywidth_upfun, manager);

//                      LEFT       RIGHT    TOP    BOTTOM
  work->attach(lab1   , work   ,   work,    work,   NULL,  0,  0,  5);
  work->attach(row1   , work   ,   work,    lab1,   NULL,  0,  0,  0);
  work->attach(lab2   , work   ,   work,    row1,   NULL,  0,  0, 15);
  work->attach(row2   , work   ,   work,    lab2,   work,  0,  0,  0);

  work->attach(mm     , row1   ,   NULL,    row1,   row1,  3,  3,  3,  3);
  work->attach(mm2    , mm     ,   NULL,    row1,   row1,  3,  3,  3,  3);

  work->attach(lab3   , row2   ,   row2,    row2,   NULL,  3,  3,  3);
  work->attach(xcenter, row2   ,   NULL,    lab3,   NULL,  3,  3,  3);
  work->attach(xwidth , xcenter,   row2,    lab3,   NULL,  3,  3,  3);
  work->attach(ycenter, row2   ,   NULL, xcenter,   row2,  3,  3,  3,  3);
  work->attach(ywidth , ycenter,   row2, xcenter,   row2,  3,  3,  3,  3);

                   addBottomRemove();
                   addBottomHelp("TOL_OVERVIEW");
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopTol::~VfpopTol()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
