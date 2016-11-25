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

//------------------------- vfpop_resample.cc ---------------------------//
//------------------------- vfpop_resample.cc ---------------------------//
//------------------------- vfpop_resample.cc ---------------------------//

//          implementation file for the VfpopResample class
//               derived from the VfpopEditBase class
//                       subdirectory vfgui


#include "vfgui/vfpop_resample.hh"
#include "vf/vf_edit_resample.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
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


#define EDIT     VfEditResample *edit    = (VfEditResample*)data;
#define MANAGER  VfManager      *manager = (VfManager     *)data;


//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


#define TRAP(long2, choice_trap, setChoice)               \
static void choice_trap(void *data, long /*ident*/,       \
                        long2 /*oldvar*/, long2 newvar)   \
{                                                         \
  EDIT                                                    \
  edit->setChoice;                                        \
}


TRAP (long ,   choice_trap, setChoice         ((int)newvar))
TRAP (long ,     type_trap, setResampleType   ((int)newvar-1))
TRAP (long , stepflag_trap, setStepflag       ((int)newvar))
TRAP (long , terpflag_trap, setTerpflag       ((int)newvar))
TRAP (long ,      rtd_trap, setResetTimedepth ((int)newvar))
TRAP (long ,     rvel_trap, setResetVelocity  ((int)newvar))
TRAP (float,    btime_trap, setBottomTime          (newvar))
TRAP (float,   bdepth_trap, setBottomDepth         (newvar))
TRAP (float,     bvel_trap, setBottomVelocity      (newvar))
TRAP (float,     tvel_trap, setTopVelocity         (newvar))
TRAP (long ,    nwant_trap, setNwant               (newvar))
TRAP (long ,    ncoef_trap, setNcoef               (newvar))
TRAP (long ,     nrun_trap, setNrun                (newvar))
TRAP (float,      dx1_trap, setDx1                 (newvar))
TRAP (float,      dx2_trap, setDx2                 (newvar))
TRAP (float,      dt1_trap, setDt1                 (newvar))
TRAP (float,      dt2_trap, setDt2                 (newvar))
TRAP (float,       dv_trap, setDv                  (newvar))



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
UPDATE (long, nsel_upfun, numSelectedVelocityFunctions   ())
UPDATE (long,  act_upfun, getActiveVelocityFunction      ()+1)
UPDATE (long, nerr_upfun, numVelocityFunctionsWithErrors ())
UPDATE (long, nray_upfun, numRaytracedVelocityFunctions  ())


#define UPFUN(long2, choice_upfun, getChoice)   \
static long2 choice_upfun(void *data)           \
{                                               \
  EDIT                                          \
  return edit->getChoice;                       \
}


UPFUN (long ,   choice_upfun, getChoice         ())
UPFUN (long ,     type_upfun, getResampleType   ()+1)
UPFUN (long , stepflag_upfun, getStepflag       ())
UPFUN (long , terpflag_upfun, getTerpflag       ())
UPFUN (long ,      rtd_upfun, getResetTimedepth ())
UPFUN (long ,     rvel_upfun, getResetVelocity  ())
UPFUN (float,    btime_upfun, getBottomTime     ())
UPFUN (float,   bdepth_upfun, getBottomDepth    ())
UPFUN (float,     bvel_upfun, getBottomVelocity ())
UPFUN (float,     tvel_upfun, getTopVelocity    ())
UPFUN (long ,    nwant_upfun, getNwant          ())
UPFUN (long ,    ncoef_upfun, getNcoef          ())
UPFUN (long ,     nrun_upfun, getNrun           ())
UPFUN (float,      dx1_upfun, getDx1            ())
UPFUN (float,      dx2_upfun, getDx2            ())
UPFUN (float,      dt1_upfun, getDt1            ())
UPFUN (float,      dt2_upfun, getDt2            ())
UPFUN (float,       dv_upfun, getDv             ())



//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//


#define ALLOW(type_sense_upfun, allowType, VTNM)    \
static long type_sense_upfun(void *data)            \
{                                                   \
  EDIT                                              \
  return edit->allowType(VTNM);                     \
}


ALLOW (     type_sense_VTNM  , allowType    , VTNM)
ALLOW (     type_sense_VTRM  , allowType    , VTRM)
ALLOW (     type_sense_VZRM  , allowType    , VZRM)
ALLOW (     type_sense_VTAV  , allowType    , VTAV)
ALLOW (     type_sense_VZAV  , allowType    , VZAV)
ALLOW (     type_sense_VTIN  , allowType    , VTIN)
ALLOW (     type_sense_VZIN  , allowType    , VZIN)
ALLOW (     type_sense_VTDP  , allowType    , VTDP)
ALLOW ( stepflag_sense_DELTA , allowStepflag, VfEditResample::STEPFLAG_DELTA)
ALLOW ( stepflag_sense_NUMBER, allowStepflag, VfEditResample::STEPFLAG_NUMBER)
ALLOW ( stepflag_sense_RETAIN, allowStepflag, VfEditResample::STEPFLAG_RETAIN)
ALLOW ( terpflag_sense_LINEAR, allowTerpflag, VfEditResample::TERPFLAG_LINEAR)
ALLOW ( terpflag_sense_CUBIC , allowTerpflag, VfEditResample::TERPFLAG_CUBIC )
ALLOW ( terpflag_sense_SPLINE, allowTerpflag, VfEditResample::TERPFLAG_SPLINE)
ALLOW ( terpflag_sense_POLY  , allowTerpflag, VfEditResample::TERPFLAG_POLY  )
ALLOW ( terpflag_sense_RUN   , allowTerpflag, VfEditResample::TERPFLAG_RUN   )
ALLOW ( terpflag_sense_EARTH , allowTerpflag, VfEditResample::TERPFLAG_EARTH )
ALLOW ( terpflag_sense_TOP   , allowTerpflag, VfEditResample::TERPFLAG_TOP   )
ALLOW ( terpflag_sense_BOTTOM, allowTerpflag, VfEditResample::TERPFLAG_BOTTOM)



#define NEED(stepflag_sense_upfun, needStepflag)    \
static long stepflag_sense_upfun(void *data)        \
{                                                   \
  EDIT                                              \
  return edit->needStepflag();                      \
}


NEED ( stepflag_sense_upfun, needStepflag       )
NEED (    rtime_sense_upfun, needResetTime      )
NEED (   rdepth_sense_upfun, needResetDepth     )
NEED (     rvel_sense_upfun, needResetVelocity  )
NEED (    btime_sense_upfun, needBottomTime     )
NEED (   bdepth_sense_upfun, needBottomDepth    )
NEED (     bvel_sense_upfun, needBottomVelocity )
NEED (     tvel_sense_upfun, needTopVelocity    )
NEED (    nwant_sense_upfun, needNwant          )
NEED (    ncoef_sense_upfun, needNcoef          )
NEED (     nrun_sense_upfun, needNrun           )
NEED (       dx_sense_upfun, needDx             )
NEED (       dt_sense_upfun, needDt             )
NEED (       dv_sense_upfun, needDv             )



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopResample::VfpopResample(SLDelay *slparent, char *name,
                   VfManager *manager, ContainerList *clist)
       : VfpopEditBase(slparent, name, manager, clist, "RESAMPLE_OVERVIEW")
{
  _edit = new VfEditResample(manager->utilities());

  SLSmartForm *row1 = new SLSmartForm(_editor, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(_editor, "row2", NULL);
  SLSmartForm *row3 = new SLSmartForm(_editor, "row3", NULL);
  SLSmartForm *row4 = new SLSmartForm(_editor, "row4", NULL);
  SLSmartForm *row5 = new SLSmartForm(_editor, "row5", NULL);
  SLSmartForm *row6 = new SLSmartForm(_editor, "row6", NULL);
  SLSep       *sep1 = new SLSep      (_editor, "sep1");
  SLSep       *sep2 = new SLSep      (_editor, "sep2");
  SLSep       *sep3 = new SLSep      (_editor, "sep3");
  SLSep       *sep4 = new SLSep      (_editor, "sep4");

//                       LEFT     RIGHT    TOP      BOTTOM
  _editor->attach(row1, _editor, _editor, _editor,  NULL  ,  0, 0, 5);
  _editor->attach(row2, _editor, _editor,  row1  ,  NULL  ,  0, 0, 5);
  _editor->attach(sep1, _editor, _editor,  row2  ,  NULL  ,  0, 0, 5);
  _editor->attach(row3, _editor, _editor,  sep1  ,  NULL  ,  0, 0, 5);
  _editor->attach(sep2, _editor, _editor,  row3  ,  NULL  ,  0, 0, 5);
  _editor->attach(row4, _editor, _editor,  sep2  ,  NULL  ,  0, 0, 5);
  _editor->attach(sep3, _editor, _editor,  row4  ,  NULL  ,  0, 0, 5);
  _editor->attach(row5, _editor, _editor,  sep3  ,  NULL  ,  0, 0, 5);
  _editor->attach(sep4, _editor, _editor,  row5  ,  NULL  ,  0, 0, 5);
  _editor->attach(row6, _editor, _editor,  sep4  , _editor,  0, 0, 5, 5);

////////// row1:

  SL2Text *nfun  = new SL2Text (row1, "nfun", 0,
                          " #functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nsel  = new SL2Text (row1, "nsel", 0,
                                              "#selected:", SLpText::_LONG, 6);
  SL2Text *act   = new SL2Text (row1, "act" , 0,
                                        "active function:", SLpText::_LONG, 6);

  nfun->showLabelAppearance();
  nsel->showLabelAppearance();
  act ->showLabelAppearance();

  nfun  ->setupIvarFun (  nfun_upfun, manager);
  nsel  ->setupIvarFun (  nsel_upfun, manager);
  act   ->setupIvarFun (   act_upfun, manager);

  row1->showEvenSpacing();

////////// row2:

  SL2Text *nerr  = new SL2Text (row2, "nerr", 0,
                          " #functions with errors:", SLpText::_LONG, 6);
  SL2Text *nray  = new SL2Text (row2, "nray", 0,
                          " #functions raytraced:", SLpText::_LONG, 6);

  nerr->showLabelAppearance();
  nray->showLabelAppearance();

  nerr  ->setupIvarFun (  nerr_upfun, manager);
  nray  ->setupIvarFun (  nray_upfun, manager);

  row2->showEvenSpacing();

////////// row3:

  SLpOption *choice = new SLpOption(row3, "choice", 0, "");
  choice->addOption("choice", VfEditResample::CHOICE_SELECTED,
                    "resample SELECTED velocity functions in active dataset");
  choice->addOption("choice", VfEditResample::CHOICE_ALL,
                    "resample ALL velocity functions in active dataset");
  choice->addOption("choice", VfEditResample::CHOICE_ACTIVE,
                    "resample ACTIVE velocity function in active dataset");

  choice->setItrap     (choice_trap ,   _edit);
  choice->setupIvarFun (choice_upfun,   _edit);

  row3->showEvenSpacing();

////////// row4:

  SLpOption *type = new SLpOption(row4, "typexxx", 0,
                                            "Type of function to resample:");
  for(int i = FIRSTTYPE; i <= LASTTYPE; i++)
      {
      if(manager->utilities()->abscissaIsThickness(i)) continue;
      type->addOption("typexxx", i+1,
                          (char*)_manager->utilities()->typeDescription(i));
      }

  type  ->setItrap      (  type_trap       ,   _edit);
  type  ->setupIvarFun  (  type_upfun      ,   _edit);

  type->setupOptionSenseFun (VTNM+1,    type_sense_VTNM  , _edit);
  type->setupOptionSenseFun (VTRM+1,    type_sense_VTRM  , _edit);
  type->setupOptionSenseFun (VZRM+1,    type_sense_VZRM  , _edit);
  type->setupOptionSenseFun (VTAV+1,    type_sense_VTAV  , _edit);
  type->setupOptionSenseFun (VZAV+1,    type_sense_VZAV  , _edit);
  type->setupOptionSenseFun (VTIN+1,    type_sense_VTIN  , _edit);
  type->setupOptionSenseFun (VZIN+1,    type_sense_VZIN  , _edit);
  type->setupOptionSenseFun (VTDP+1,    type_sense_VTDP  , _edit);

//                   LEFT  RIGHT  TOP  BOTTOM
  row4->attach(type, row4, NULL, row4, row4);

////////// row5:

  SLpOption *stepflag = new SLpOption(row5, "stepflag", 0,
                                                     "Resampling method:");
  stepflag->addOption("stepflag", VfEditResample::STEPFLAG_DELTA,
                    "use specified time or depth increments");
  stepflag->addOption("stepflag", VfEditResample::STEPFLAG_NUMBER,
                    "use specified number of picks");
  stepflag->addOption("stepflag", VfEditResample::STEPFLAG_RETAIN,
                    "retain the current picks");

  SL2Text *dt1   = new SL2Text (row5, "5", 0,
                    "time increment (top/bottom):", SLpText::_FLOAT, 5, 3);
  SL2Text *dx1   = new SL2Text (row5, "5", 0,
                    "depth increment (top/bottom):", SLpText::_FLOAT, 4, 0);
  SL2Text *nwant = new SL2Text (row5, "5", 0,
                    "desired number of picks:", SLpText::_LONG, 3);
  SLpText *dt2   = new SLpText (row5, "5", 0, SLpText::_FLOAT, 5, 3);
  SLpText *dx2   = new SLpText (row5, "5", 0, SLpText::_FLOAT, 4, 0);
  SLpText *dt3   = new SLpText (row5, "5");
  SLpText *dx3   = new SLpText (row5, "5");

  dt3->showLabelAppearance();
  dx3->showLabelAppearance();
  dt3->setCvar("seconds");
  dx3->setCvar("feet or meters");

  stepflag->setItrap     (stepflag_trap, _edit);
  dx1     ->setFtrap     (     dx1_trap, _edit);
  dx2     ->setFtrap     (     dx2_trap, _edit);
  dt1     ->setFtrap     (     dt1_trap, _edit);
  dt2     ->setFtrap     (     dt2_trap, _edit);
  nwant   ->setItrap     (   nwant_trap, _edit);

  stepflag->setupIvarFun (stepflag_upfun, _edit);
  dx1     ->setupFvarFun (     dx1_upfun, _edit);
  dx2     ->setupFvarFun (     dx2_upfun, _edit);
  dt1     ->setupFvarFun (     dt1_upfun, _edit);
  dt2     ->setupFvarFun (     dt2_upfun, _edit);
  nwant   ->setupIvarFun (   nwant_upfun, _edit);

  stepflag->setupSenseFun (stepflag_sense_upfun, _edit);
  dx1     ->setupSenseFun (      dx_sense_upfun, _edit);
  dx2     ->setupSenseFun (      dx_sense_upfun, _edit);
  dt1     ->setupSenseFun (      dt_sense_upfun, _edit);
  dt2     ->setupSenseFun (      dt_sense_upfun, _edit);
  nwant   ->setupSenseFun (   nwant_sense_upfun, _edit);
  dx3     ->setupSenseFun (      dx_sense_upfun, _edit);
  dt3     ->setupSenseFun (      dt_sense_upfun, _edit);

  stepflag->setupOptionSenseFun (VfEditResample::STEPFLAG_DELTA ,
                                           stepflag_sense_DELTA , _edit);
  stepflag->setupOptionSenseFun (VfEditResample::STEPFLAG_NUMBER,
                                           stepflag_sense_NUMBER, _edit);
  stepflag->setupOptionSenseFun (VfEditResample::STEPFLAG_RETAIN,
                                           stepflag_sense_RETAIN, _edit);

//                       LEFT  RIGHT  TOP      BOTTOM
  row5->attach(stepflag, row5, NULL,  row5    , NULL,  0, 0, 0, 0);
  row5->attach(dt1     , row5, NULL,  stepflag, NULL, 30, 0, 0, 0);
  row5->attach(dt2     , dt1 , NULL,  stepflag, NULL,  0, 0, 0, 0);
  row5->attach(dt3     , dt2 , row5,  stepflag, NULL,  0, 0, 0, 0);
  row5->attach(dx1     , row5, NULL,  dt1     , NULL, 30, 0, 0, 0);
  row5->attach(dx2     , dx1 , NULL,  dt1     , NULL,  0, 0, 0, 0);
  row5->attach(dx3     , dx2 , row5,  dt1     , NULL,  0, 0, 0, 0);
  row5->attach(nwant   , row5, NULL,  dx1     , row5, 30, 0, 0, 0);

////////// row6:

  SLpOption *terpflag = new SLpOption(row6, "terpflag", 0,
                                       "Interpolation / smoothing method:");
  terpflag->addOption("terpflag", VfEditResample::TERPFLAG_LINEAR,
                    "resample with linear interpolation");
  terpflag->addOption("terpflag", VfEditResample::TERPFLAG_CUBIC,
                    "resample with 4-point cubic interpolation");
  terpflag->addOption("terpflag", VfEditResample::TERPFLAG_SPLINE,
                    "resample with spline interpolation");
  terpflag->addOption("terpflag", VfEditResample::TERPFLAG_POLY,
                    "resample with polynomial fit");
  terpflag->addOption("terpflag", VfEditResample::TERPFLAG_RUN,
                    "use linear interpolation plus running average");
  terpflag->addOption("terpflag", VfEditResample::TERPFLAG_EARTH,
                    "resample without changing earth model");
  terpflag->addOption("terpflag", VfEditResample::TERPFLAG_TOP,
                    "reset top pick");
  terpflag->addOption("terpflag", VfEditResample::TERPFLAG_BOTTOM,
                    "reset bottom pick");

  SL2Text *dv    = new SL2Text (row6, "6", 0,
          "average spline velocity error to accept:", SLpText::_FLOAT, 4, 0);
  SL2Text *ncoef  = new SL2Text (row6, "6", 0,
          "number of coefficients in polynomial fit:", SLpText::_LONG, 2);
  SL2Text *nrun  = new SL2Text (row6, "6", 0,
          "number of points in running average:", SLpText::_LONG, 2);

  SLpToggle *rtime  = new SLpToggle (row6, "6", 0,
                                               "reset bottom time");
  SLpToggle *rdepth = new SLpToggle (row6, "6", 0,
                                               "reset bottom depth");
  SLpToggle *rvel   = new SLpToggle (row6, "6", 0,
                                               "reset bottom velocity");
  SL2Text *btime   = new SL2Text (row6, "6", 0, "to", SLpText::_FLOAT, 6, 3);
  SL2Text *bdepth  = new SL2Text (row6, "6", 0, "to", SLpText::_FLOAT, 6, 0);
  SL2Text *bvel    = new SL2Text (row6, "6", 0, "to", SLpText::_FLOAT, 6, 0);
  SL2Text *tvel    = new SL2Text (row6, "6", 0, "reset top velocity to",
                                                      SLpText::_FLOAT, 6, 0);
  SLpText *secs    = new SLpText (row6, "6");
  SLpText *feet    = new SLpText (row6, "6");

  secs->showLabelAppearance();
  secs->setCvar("seconds");
  feet->showLabelAppearance();
  feet->setCvar("feet or meters");

  terpflag->setItrap     (terpflag_trap, _edit);
  rtime   ->setItrap     (     rtd_trap, _edit);
  rdepth  ->setItrap     (     rtd_trap, _edit);
  rvel    ->setItrap     (    rvel_trap, _edit);
  btime   ->setFtrap     (   btime_trap, _edit);
  bdepth  ->setFtrap     (  bdepth_trap, _edit);
  bvel    ->setFtrap     (    bvel_trap, _edit);
  tvel    ->setFtrap     (    tvel_trap, _edit);
  ncoef   ->setItrap     (   ncoef_trap, _edit);
  nrun    ->setItrap     (    nrun_trap, _edit);
  dv      ->setFtrap     (      dv_trap, _edit);

  terpflag->setupIvarFun (terpflag_upfun, _edit);
  rtime   ->setupIvarFun (     rtd_upfun, _edit);
  rdepth  ->setupIvarFun (     rtd_upfun, _edit);
  rvel    ->setupIvarFun (    rvel_upfun, _edit);
  btime   ->setupFvarFun (   btime_upfun, _edit);
  bdepth  ->setupFvarFun (  bdepth_upfun, _edit);
  bvel    ->setupFvarFun (    bvel_upfun, _edit);
  tvel    ->setupFvarFun (    tvel_upfun, _edit);
  ncoef   ->setupIvarFun (   ncoef_upfun, _edit);
  nrun    ->setupIvarFun (    nrun_upfun, _edit);
  dv      ->setupFvarFun (      dv_upfun, _edit);

  rtime   ->setupSenseFun (   rtime_sense_upfun, _edit);
  rdepth  ->setupSenseFun (  rdepth_sense_upfun, _edit);
  rvel    ->setupSenseFun (    rvel_sense_upfun, _edit);
  btime   ->setupSenseFun (   btime_sense_upfun, _edit);
  bdepth  ->setupSenseFun (  bdepth_sense_upfun, _edit);
  bvel    ->setupSenseFun (    bvel_sense_upfun, _edit);
  tvel    ->setupSenseFun (    tvel_sense_upfun, _edit);
  ncoef   ->setupSenseFun (   ncoef_sense_upfun, _edit);
  nrun    ->setupSenseFun (    nrun_sense_upfun, _edit);
  dv      ->setupSenseFun (      dv_sense_upfun, _edit);
  secs    ->setupSenseFun (   btime_sense_upfun, _edit);
  feet    ->setupSenseFun (  bdepth_sense_upfun, _edit);

  terpflag->setupOptionSenseFun (VfEditResample::TERPFLAG_LINEAR,
                                           terpflag_sense_LINEAR, _edit);
  terpflag->setupOptionSenseFun (VfEditResample::TERPFLAG_CUBIC ,
                                           terpflag_sense_CUBIC , _edit);
  terpflag->setupOptionSenseFun (VfEditResample::TERPFLAG_SPLINE,
                                           terpflag_sense_SPLINE, _edit);
  terpflag->setupOptionSenseFun (VfEditResample::TERPFLAG_POLY  ,
                                           terpflag_sense_POLY  , _edit);
  terpflag->setupOptionSenseFun (VfEditResample::TERPFLAG_RUN   ,
                                           terpflag_sense_RUN   , _edit);
  terpflag->setupOptionSenseFun (VfEditResample::TERPFLAG_EARTH ,
                                           terpflag_sense_EARTH , _edit);
  terpflag->setupOptionSenseFun (VfEditResample::TERPFLAG_TOP   ,
                                           terpflag_sense_TOP   , _edit);
  terpflag->setupOptionSenseFun (VfEditResample::TERPFLAG_BOTTOM,
                                           terpflag_sense_BOTTOM, _edit);

//                       LEFT     RIGHT  TOP      BOTTOM
  row6->attach(terpflag, row6   , NULL,  row6    , NULL,  0, 0, 0, 0);
  row6->attach(dv      , row6   , NULL,  terpflag, NULL, 30, 0, 0, 0);
  row6->attach(ncoef   , row6   , NULL,  dv      , NULL, 30, 0, 0, 0);
  row6->attach(nrun    , row6   , NULL,  ncoef   , NULL, 30, 0, 0, 0);
  row6->attach(tvel    , row6   , NULL,  nrun    , NULL, 30, 0, 0, 0);
  row6->attach(rtime   , row6   , NULL,  tvel    , NULL, 30, 0, 0, 0);
  row6->attach(rdepth  , row6   , NULL,  rtime   , NULL, 30, 0, 0, 0);
  row6->attach(rvel    , row6   , NULL,  rdepth  , row6, 30, 0, 0, 0);
  row6->attach(btime   , rtime  , NULL,  tvel    , NULL,  0, 0, 0, 0);
  row6->attach(bdepth  , rdepth , NULL,  rtime   , NULL,  0, 0, 0, 0);
  row6->attach(bvel    , rvel   , NULL,  rdepth  , row6,  0, 0, 0, 0);
  row6->attach(secs    , btime  , NULL,  tvel    , NULL,  0, 0, 0, 0);
  row6->attach(feet    , bdepth , NULL,  rtime   , NULL,  0, 0, 0, 0);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopResample::~VfpopResample()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
