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

//------------------------- vfpop_latsample.cc ---------------------------//
//------------------------- vfpop_latsample.cc ---------------------------//
//------------------------- vfpop_latsample.cc ---------------------------//

//          implementation file for the VfpopLatsample class
//               derived from the VfpopEditBase class
//                       subdirectory vfgui


#include "vfgui/vfpop_latsample.hh"
#include "vf/vf_edit_latsample.hh"
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


#define EDIT     VfEditLatsample *edit    = (VfEditLatsample*)data;
#define MANAGER  VfManager       *manager = (VfManager      *)data;


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
TRAP (long , terpflag_trap, setTerpflag       ((int)newvar))
TRAP (long , stepflag_trap, setStepflag       ((int)newvar))
TRAP (long ,  endflag_trap, setEndflag        ((int)newvar))
TRAP (long ,  dirflag_trap, setDirflag        ((int)newvar))
TRAP (float,       x1_trap, setX1                  (newvar))
TRAP (float,       y1_trap, setY1                  (newvar))
TRAP (float,     xinc_trap, setXinc                (newvar))
TRAP (float,     yinc_trap, setYinc                (newvar))
TRAP (long ,       nx_trap, setNx                  (newvar))
TRAP (long ,       ny_trap, setNy                  (newvar))
TRAP (float,     xend_trap, setXend                (newvar))
TRAP (float,     yend_trap, setYend                (newvar))
TRAP (long ,   smooth_trap, setSmooth         ((int)newvar))
TRAP (float,  tsmooth_trap, setTsmooth             (newvar))
TRAP (float,  dsmooth_trap, setDsmooth             (newvar))
TRAP (float,       dv_trap, setDv                  (newvar))
TRAP (long ,    ncoef_trap, setNcoef               (newvar))
TRAP (long ,     nrun_trap, setNrun                (newvar))



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
UPFUN (long , terpflag_upfun, getTerpflag       ())
UPFUN (long , stepflag_upfun, getStepflag       ())
UPFUN (long ,  endflag_upfun, getEndflag        ())
UPFUN (long ,  dirflag_upfun, getDirflag        ())
UPFUN (float,       x1_upfun, getX1             ())
UPFUN (float,       y1_upfun, getY1             ())
UPFUN (float,     xinc_upfun, getXinc           ())
UPFUN (float,     yinc_upfun, getYinc           ())
UPFUN (long ,       nx_upfun, getNx             ())
UPFUN (long ,       ny_upfun, getNy             ())
UPFUN (float,     xend_upfun, getXend           ())
UPFUN (float,     yend_upfun, getYend           ())
UPFUN (long ,   smooth_upfun, getSmooth         ())
UPFUN (float,  tsmooth_upfun, getTsmooth        ())
UPFUN (float,  dsmooth_upfun, getDsmooth        ())
UPFUN (float,       dv_upfun, getDv             ())
UPFUN (long ,    ncoef_upfun, getNcoef          ())
UPFUN (long ,     nrun_upfun, getNrun           ())



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
ALLOW ( stepflag_sense_CHANGE, allowStepflag, VfEditLatsample::STEPFLAG_CHANGE)
ALLOW ( stepflag_sense_RETAIN, allowStepflag, VfEditLatsample::STEPFLAG_RETAIN)
ALLOW ( terpflag_sense_LINEAR, allowTerpflag, VfEditLatsample::TERPFLAG_LINEAR)
ALLOW ( terpflag_sense_CUBIC , allowTerpflag, VfEditLatsample::TERPFLAG_CUBIC )
ALLOW ( terpflag_sense_SPLINE, allowTerpflag, VfEditLatsample::TERPFLAG_SPLINE)
ALLOW ( terpflag_sense_POLY  , allowTerpflag, VfEditLatsample::TERPFLAG_POLY  )
ALLOW ( terpflag_sense_RUN   , allowTerpflag, VfEditLatsample::TERPFLAG_RUN   )



#define NEED(stepflag_sense_upfun, needStepflag)    \
static long stepflag_sense_upfun(void *data)        \
{                                                   \
  EDIT                                              \
  return edit->needStepflag();                      \
}


NEED (  stepflag_sense_upfun, needStepflag       )
NEED (   endflag_sense_upfun, needEndflag        )
NEED (xlocations_sense_upfun, needXlocations     )
NEED (ylocations_sense_upfun, needYlocations     )
NEED (    smooth_sense_upfun, needSmooth         )
NEED (   tsmooth_sense_upfun, needTsmooth        )
NEED (   dsmooth_sense_upfun, needDsmooth        )
NEED (        dv_sense_upfun, needDv             )
NEED (     ncoef_sense_upfun, needNcoef          )
NEED (      nrun_sense_upfun, needNrun           )



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopLatsample::VfpopLatsample(SLDelay *slparent, char *name,
                   VfManager *manager, ContainerList *clist)
       : VfpopEditBase(slparent, name, manager, clist, "LATSAMPLE_OVERVIEW")
{
  _edit = new VfEditLatsample(manager->utilities());

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

  SLpOption *dir = new SLpOption(row2, "dir", 0, "Resampling direction:");
  dir->addOption("dir", VfEditLatsample::DIRFLAG_X_ONLY, "X direction only");
  dir->addOption("dir", VfEditLatsample::DIRFLAG_Y_ONLY, "Y direction only");
  dir->addOption("dir", VfEditLatsample::DIRFLAG_X_THEN_Y,
                                                       "X then Y direction");
  dir->addOption("dir", VfEditLatsample::DIRFLAG_Y_THEN_X,
                                                       "Y then X direction");

  type ->setItrap      (   type_trap       ,   _edit);
  dir  ->setItrap      (dirflag_trap       ,   _edit);

  type ->setupIvarFun  (   type_upfun      ,   _edit);
  dir  ->setupIvarFun  (dirflag_upfun      ,   _edit);

  type->setupOptionSenseFun (VTNM+1,    type_sense_VTNM  , _edit);
  type->setupOptionSenseFun (VTRM+1,    type_sense_VTRM  , _edit);
  type->setupOptionSenseFun (VZRM+1,    type_sense_VZRM  , _edit);
  type->setupOptionSenseFun (VTAV+1,    type_sense_VTAV  , _edit);
  type->setupOptionSenseFun (VZAV+1,    type_sense_VZAV  , _edit);
  type->setupOptionSenseFun (VTIN+1,    type_sense_VTIN  , _edit);
  type->setupOptionSenseFun (VZIN+1,    type_sense_VZIN  , _edit);
  type->setupOptionSenseFun (VTDP+1,    type_sense_VTDP  , _edit);

//                   LEFT  RIGHT  TOP  BOTTOM
  row2->attach(type, row2, NULL, row2, NULL);
  row2->attach(dir , row2, NULL, type, row2);

////////// row3:

  SLpOption *stepflag = new SLpOption(row3, "stepflag", 0,
                                                     "Resampling method:");
  stepflag->addOption("stepflag", VfEditLatsample::STEPFLAG_CHANGE,
                    "change the X and/or Y function locations");
  stepflag->addOption("stepflag", VfEditLatsample::STEPFLAG_RETAIN,
                    "retain the current function locations");

  SL2Text *x1    = new SL2Text (row3, "3", 0,
                        "first X:", SLpText::_FLOAT, 6, 3);
  SL2Text *y1    = new SL2Text (row3, "3", 0,
                        "first Y:", SLpText::_FLOAT, 6, 3);
  SL2Text *xinc  = new SL2Text (row3, "3", 0,
                        "X incr:", SLpText::_FLOAT, 5, 3);
  SL2Text *yinc  = new SL2Text (row3, "3", 0,
                        "Y incr:", SLpText::_FLOAT, 5, 3);
  SL2Text *xend  = new SL2Text (row3, "3", 0,
                        "last X:", SLpText::_FLOAT, 6, 3);
  SL2Text *yend  = new SL2Text (row3, "3", 0,
                        "last Y:", SLpText::_FLOAT, 6, 3);
  SL2Text *nx    = new SL2Text (row3, "3", 0,
                        "#X locations:", SLpText::_LONG, 5);
  SL2Text *ny    = new SL2Text (row3, "3", 0,
                        "#Y locations:", SLpText::_LONG, 5);

  SLpToggle *smooth  = new SLpToggle (row3, "3", 0, "smooth only below");
  SL2Text   *tsmooth = new SL2Text   (row3, "3", 0,
                                       "time", SLpText::_FLOAT, 6, 3);
  SL2Text   *dsmooth = new SL2Text   (row3, "3", 0,
                                       "or depth", SLpText::_FLOAT, 6, 0);

  stepflag->setItrap     (stepflag_trap, _edit);
  x1      ->setFtrap     (      x1_trap, _edit);
  y1      ->setFtrap     (      y1_trap, _edit);
  xinc    ->setFtrap     (    xinc_trap, _edit);
  yinc    ->setFtrap     (    yinc_trap, _edit);
  xend    ->setFtrap     (    xend_trap, _edit);
  yend    ->setFtrap     (    yend_trap, _edit);
  nx      ->setItrap     (      nx_trap, _edit);
  ny      ->setItrap     (      ny_trap, _edit);
  smooth  ->setItrap     (  smooth_trap, _edit);
  tsmooth ->setFtrap     ( tsmooth_trap, _edit);
  dsmooth ->setFtrap     ( dsmooth_trap, _edit);

  stepflag->setupIvarFun (stepflag_upfun, _edit);
  x1      ->setupFvarFun (      x1_upfun, _edit);
  y1      ->setupFvarFun (      y1_upfun, _edit);
  xinc    ->setupFvarFun (    xinc_upfun, _edit);
  yinc    ->setupFvarFun (    yinc_upfun, _edit);
  xend    ->setupFvarFun (    xend_upfun, _edit);
  yend    ->setupFvarFun (    yend_upfun, _edit);
  nx      ->setupIvarFun (      nx_upfun, _edit);
  ny      ->setupIvarFun (      ny_upfun, _edit);
  smooth  ->setupIvarFun (  smooth_upfun, _edit);
  tsmooth ->setupFvarFun ( tsmooth_upfun, _edit);
  dsmooth ->setupFvarFun ( dsmooth_upfun, _edit);

  stepflag->setupSenseFun (  stepflag_sense_upfun, _edit);
  x1      ->setupSenseFun (xlocations_sense_upfun, _edit);
  y1      ->setupSenseFun (ylocations_sense_upfun, _edit);
  xinc    ->setupSenseFun (xlocations_sense_upfun, _edit);
  yinc    ->setupSenseFun (ylocations_sense_upfun, _edit);
  xend    ->setupSenseFun (xlocations_sense_upfun, _edit);
  yend    ->setupSenseFun (ylocations_sense_upfun, _edit);
  nx      ->setupSenseFun (xlocations_sense_upfun, _edit);
  ny      ->setupSenseFun (ylocations_sense_upfun, _edit);
  smooth  ->setupSenseFun (    smooth_sense_upfun, _edit);
  tsmooth ->setupSenseFun (   tsmooth_sense_upfun, _edit);
  dsmooth ->setupSenseFun (   dsmooth_sense_upfun, _edit);

  stepflag->setupOptionSenseFun (VfEditLatsample::STEPFLAG_CHANGE,
                                           stepflag_sense_CHANGE, _edit);
  stepflag->setupOptionSenseFun (VfEditLatsample::STEPFLAG_RETAIN,
                                           stepflag_sense_RETAIN, _edit);

//                       LEFT     RIGHT  TOP      BOTTOM
  row3->attach(stepflag, row3   , NULL,  row3    , NULL,  0, 0, 0, 0);
  row3->attach(x1      , row3   , NULL,  stepflag, NULL, 30, 0, 0, 0);
  row3->attach(y1      , row3   , NULL,  x1      , NULL, 30, 0, 0, 0);
  row3->attach(xinc    , x1     , NULL,  stepflag, NULL, 10, 0, 0, 0);
  row3->attach(yinc    , y1     , NULL,  x1      , NULL, 10, 0, 0, 0);
  row3->attach(xend    , xinc   , NULL,  stepflag, NULL, 10, 0, 0, 0);
  row3->attach(yend    , yinc   , NULL,  x1      , NULL, 10, 0, 0, 0);
  row3->attach(nx      , xend   , NULL,  stepflag, NULL, 10, 0, 0, 0);
  row3->attach(ny      , yend   , NULL,  x1      , NULL, 10, 0, 0, 0);
  row3->attach(smooth  , row3   , NULL,  y1      , row3, 30, 0, 5, 0);
  row3->attach(tsmooth , smooth , NULL,  y1      , row3,  0, 0, 5, 0);
  row3->attach(dsmooth , tsmooth, NULL,  y1      , row3,  0, 0, 5, 0);

////////// row4:

  SLpOption *terpflag = new SLpOption(row4, "terpflag", 0,
                                       "Interpolation / smoothing method:");
  terpflag->addOption("terpflag", VfEditLatsample::TERPFLAG_LINEAR,
                    "use linear interpolation");
  terpflag->addOption("terpflag", VfEditLatsample::TERPFLAG_CUBIC,
                    "use 4-point cubic interpolation");
  terpflag->addOption("terpflag", VfEditLatsample::TERPFLAG_SPLINE,
                    "use spline interpolation");
  terpflag->addOption("terpflag", VfEditLatsample::TERPFLAG_POLY,
                    "use least-squares polynomial fit");
  terpflag->addOption("terpflag", VfEditLatsample::TERPFLAG_RUN,
                    "use running average smoothing");

  SL2Text *dv    = new SL2Text (row4, "4", 0,
          "average spline velocity error to accept:", SLpText::_FLOAT, 4, 0);
  SL2Text *ncoef  = new SL2Text (row4, "4", 0,
          "number of coefficients in polynomial fit:", SLpText::_LONG, 2);
  SL2Text *nrun  = new SL2Text (row4, "4", 0,
          "number of points in running average:", SLpText::_LONG, 2);

  SLpOption *endflag = new SLpOption(row4, "endflag", 0, "");
  endflag->addOption("endflag", VfEditLatsample::ENDFLAG_TRUNCATED,
                            "use truncated running average end option");
  endflag->addOption("endflag", VfEditLatsample::ENDFLAG_SHIFTED,
                            "use shifted running average end option");
  endflag->addOption("endflag", VfEditLatsample::ENDFLAG_EXTENDED,
                            "use extended running average end option");
  endflag->addOption("endflag", VfEditLatsample::ENDFLAG_NARROWED,
                            "use narrowed end option (graded ends)");

  terpflag->setItrap     (terpflag_trap, _edit);
  dv      ->setFtrap     (      dv_trap, _edit);
  ncoef   ->setItrap     (   ncoef_trap, _edit);
  nrun    ->setItrap     (    nrun_trap, _edit);
  endflag ->setItrap     ( endflag_trap, _edit);

  terpflag->setupIvarFun (terpflag_upfun, _edit);
  dv      ->setupFvarFun (      dv_upfun, _edit);
  ncoef   ->setupIvarFun (   ncoef_upfun, _edit);
  nrun    ->setupIvarFun (    nrun_upfun, _edit);
  endflag ->setupIvarFun ( endflag_upfun, _edit);

  dv      ->setupSenseFun (      dv_sense_upfun, _edit);
  ncoef   ->setupSenseFun (   ncoef_sense_upfun, _edit);
  nrun    ->setupSenseFun (    nrun_sense_upfun, _edit);
  endflag ->setupSenseFun ( endflag_sense_upfun, _edit);

  terpflag->setupOptionSenseFun (VfEditLatsample::TERPFLAG_LINEAR,
                                           terpflag_sense_LINEAR, _edit);
  terpflag->setupOptionSenseFun (VfEditLatsample::TERPFLAG_CUBIC ,
                                           terpflag_sense_CUBIC , _edit);
  terpflag->setupOptionSenseFun (VfEditLatsample::TERPFLAG_SPLINE,
                                           terpflag_sense_SPLINE, _edit);
  terpflag->setupOptionSenseFun (VfEditLatsample::TERPFLAG_POLY  ,
                                           terpflag_sense_POLY  , _edit);
  terpflag->setupOptionSenseFun (VfEditLatsample::TERPFLAG_RUN   ,
                                           terpflag_sense_RUN   , _edit);

//                       LEFT     RIGHT  TOP      BOTTOM
  row4->attach(terpflag, row4   , NULL,  row4    , NULL,  0, 0, 0, 0);
  row4->attach(dv      , row4   , NULL,  terpflag, NULL, 30, 0, 0, 0);
  row4->attach(ncoef   , row4   , NULL,  dv      , NULL, 30, 0, 0, 0);
  row4->attach(nrun    , row4   , NULL,  ncoef   , NULL, 30, 0, 0, 0);
  row4->attach(endflag , row4   , NULL,  nrun    , row4, 20, 0, 0, 0);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopLatsample::~VfpopLatsample()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
