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

//---------------------- vfgui_transform.cc ------------------------//
//---------------------- vfgui_transform.cc ------------------------//
//---------------------- vfgui_transform.cc ------------------------//

//         implementation file for the VfguiTransform class
//               derived from the SLSmartForm class
//                        subdirectory vfgui


#include "vfgui/vfgui_transform.hh"
#include "vf/vf_informer.hh"
#include "oprim/grid_transform.hh"
#include "sl/sl_sep.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_option.hh"
#include "sl/sl_column.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { HAND_RIGHT, HAND_LEFT };

/****
static double xloc_coord  = 0.0;
static double yloc_coord  = 0.0;
static double xgrid_coord = 0.0;
static double ygrid_coord = 0.0;
static long   which_coord =   1;
****/


#define GUI        VfguiTransform *gui       = (VfguiTransform*)data;
#define TRANSFORM  GridTransform  *transform = gui->transform();
#define INFORMER   VfInformer     *informer  = gui->informer();


//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


#define TRAP(xorigin_trap, setXorigin)                  \
static void xorigin_trap(void *data, long /*ident*/,    \
                 double /*oldvar*/, double newvar)      \
{                                                       \
  GUI                                                   \
  TRANSFORM                                             \
  INFORMER                                              \
  if(informer) informer->preNewHorizonTransform();      \
  transform->setXorigin(newvar);                        \
  if(informer) informer->postNewHorizonTransform();     \
}


TRAP(xorigin_trap, setXorigin)
TRAP(yorigin_trap, setYorigin)
TRAP(angle_trap  , setRotationAngle)
TRAP(xwidth_trap , setXgridWidth)
TRAP(ywidth_trap , setYgridWidth)
TRAP(dx11_trap   , setDx11)
TRAP(dx21_trap   , setDx21)
TRAP(dx12_trap   , setDx12)
TRAP(dx22_trap   , setDx22)
TRAP(dn11_trap   , setDn11)
TRAP(dn21_trap   , setDn21)
TRAP(dn12_trap   , setDn12)
TRAP(dn22_trap   , setDn22)


static void hand_trap(void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  GUI
  TRANSFORM
  INFORMER
  if(informer) informer->preNewHorizonTransform();
  switch(newvar)
     {
     case HAND_RIGHT: transform->setRightHanded(); break;
     case HAND_LEFT : transform->setLeftHanded (); break;
     default: assert(FALSE);
     }
  if(informer) informer->postNewHorizonTransform();
}



//------------------- update functions ----------------------------//
//------------------- update functions ----------------------------//
//------------------- update functions ----------------------------//


#define UPDATE(xorigin_update, getXorigin)   \
static double xorigin_update(void *data)     \
{                                            \
  GUI                                        \
  TRANSFORM                                  \
  return transform->getXorigin();            \
}


UPDATE(xorigin_update, getXorigin)
UPDATE(yorigin_update, getYorigin)
UPDATE(angle_update  , getRotationAngle)
UPDATE(xwidth_update , getXgridWidth)
UPDATE(ywidth_update , getYgridWidth)
UPDATE(dx11_update   , getDx11)
UPDATE(dx21_update   , getDx21)
UPDATE(dx12_update   , getDx12)
UPDATE(dx22_update   , getDx22)
UPDATE(dn11_update   , getDn11)
UPDATE(dn21_update   , getDn21)
UPDATE(dn12_update   , getDn12)
UPDATE(dn22_update   , getDn22)


static long hand_update(void *data)
{
  GUI
  TRANSFORM
  if(transform->isRightHanded()) return HAND_RIGHT;
  if(transform->isLeftHanded ()) return HAND_LEFT;
  assert(FALSE);
  return 0;
}
 


//------------ convenience area traps and updates --------------------//
//------------ convenience area traps and updates --------------------//
//------------ convenience area traps and updates --------------------//


/****
static void xloc_trap(void *, long, double, double newvar)
{
  if(newvar == DNIL) return;
  xloc_coord = newvar;
  which_coord = 1;
}


static void yloc_trap(void *, long, double, double newvar)
{
  if(newvar == DNIL) return;
  yloc_coord = newvar;
  which_coord = 1;
}


static void xgrid_trap(void *, long, double, double newvar)
{
  if(newvar == DNIL) return;
  xgrid_coord = newvar;
  which_coord = 2;
}


static void ygrid_trap(void *, long, double, double newvar)
{
  if(newvar == DNIL) return;
  ygrid_coord = newvar;
  which_coord = 2;
}


static void which_trap(void *, long, long, long newvar)
{
  which_coord = newvar;
}



static double xloc_update(void *data)
{
  GUI
  TRANSFORM
  if(which_coord == 1) return xloc_coord;
  xloc_coord = transform->getXlocCoord(xgrid_coord, ygrid_coord);
  return xloc_coord;
}


static double yloc_update(void *data)
{
  GUI
  TRANSFORM
  if(which_coord == 1) return yloc_coord;
  yloc_coord = transform->getYlocCoord(xgrid_coord, ygrid_coord);
  return yloc_coord;
}


static double xgrid_update(void *data)
{
  GUI
  TRANSFORM
  if(which_coord == 2) return xgrid_coord;
  xgrid_coord = transform->getXgridCoord(xloc_coord, yloc_coord);
  return xgrid_coord;
}


static double ygrid_update(void *data)
{
  GUI
  TRANSFORM
  if(which_coord == 2) return ygrid_coord;
  ygrid_coord = transform->getYgridCoord(xloc_coord, yloc_coord);
  return ygrid_coord;
}


static long which_update(void *)
{
  return which_coord;
}
****/



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiTransform::VfguiTransform (SLDelay *slparent, GridTransform *transform,
                                                   VfInformer    *informer)
       : SLSmartForm(slparent, "vfgui_transform", NULL, FALSE),
                  _transform   (transform),
                  _informer    (informer)
{
  assert(_transform);
  SL2Text   *xorigin = new SL2Text (this, "xorigin", 0, "X origin:",
                                           SLpText::_DOUBLE, 13);
  SL2Text   *yorigin = new SL2Text (this, "yorigin", 0, "Y origin:",
                                           SLpText::_DOUBLE, 13);
  SL2Text   *angle   = new SL2Text (this, "angle", 0,
                                          "rotation angle (degrees):",
                                           SLpText::_DOUBLE, 9);
  SL2Text   *xwidth  = new SL2Text (this, "xwidth", 0, "X grid width:",
                                           SLpText::_DOUBLE, 10);
  SL2Text   *ywidth  = new SL2Text (this, "ywidth", 0, "Y grid width:",
                                           SLpText::_DOUBLE, 10);
  SLpOption *hand    = new SLpOption (this, "hand", 0, "");

  SL2Text   *dx11    = new SL2Text (this, "dx11", 0, "DX(1,1):",
                                           SLpText::_DOUBLE, 13);
  SL2Text   *dx21    = new SL2Text (this, "dx21", 0, "DX(2,1):",
                                           SLpText::_DOUBLE, 13);
  SL2Text   *dx12    = new SL2Text (this, "dx12", 0, "DX(1,2):",
                                           SLpText::_DOUBLE, 13);
  SL2Text   *dx22    = new SL2Text (this, "dx22", 0, "DX(2,2):",
                                           SLpText::_DOUBLE, 13);

  SL2Text   *dn11    = new SL2Text (this, "dn11", 0, "DN(1,1):",
                                           SLpText::_DOUBLE, 13);
  SL2Text   *dn21    = new SL2Text (this, "dn21", 0, "DN(2,1):",
                                           SLpText::_DOUBLE, 13);
  SL2Text   *dn12    = new SL2Text (this, "dn12", 0, "DN(1,2):",
                                           SLpText::_DOUBLE, 13);
  SL2Text   *dn22    = new SL2Text (this, "dn22", 0, "DN(2,2):",
                                           SLpText::_DOUBLE, 13);

  hand ->addOption("right handed coordinate system", HAND_RIGHT);
  hand ->addOption("left handed coordinate system" , HAND_LEFT);

  SLSep    *sep1 = new SLSep(this, "sep1");
  SLSep    *sep2 = new SLSep(this, "sep2");
/****
  SLSep    *sep3 = new SLSep(this, "sep3");
****/
  SLpLabel *lab1 = new SLpLabel(this, "Rotation Matrix");
  SLpLabel *lab2 = new SLpLabel(this, "Inverse Rotation Matrix");
/****
  SLpLabel *lab3 = new SLpLabel(this,
                                 "Transform Calculations Convenience Area");

  SL2Text *xloc  = new SL2Text(this, "xloc", 0, "X surveyed coord:",
                                           SLpText::_DOUBLE, 13);
  SL2Text *yloc  = new SL2Text(this, "yloc", 0, "Y surveyed coord:",
                                           SLpText::_DOUBLE, 13);
  SL2Text *xgrid = new SL2Text(this, "xgrid", 0, "X grid coord:",
                                           SLpText::_DOUBLE, 13);
  SL2Text *ygrid = new SL2Text(this, "ygrid", 0, "Y grid coord:",
                                           SLpText::_DOUBLE, 13);
  SLpOption *which   = new SLpOption (this, "which", 0, "");
  which->addOption("calculate grid coordinates from surveyed coordinates", 1);
  which->addOption("calculate surveyed coordinates from grid coordinates", 2);
****/

//                  left   right   top      bottom

  attach(xorigin  , this , NULL  , this    , NULL, 5, 0, 5);
  attach(yorigin  , this , NULL  , xorigin , NULL, 5);
  attach(angle    , this , this  , yorigin , NULL, 5, 5);
  attach(xwidth   , this , NULL  , angle   , NULL, 5);
  attach(ywidth   , this , NULL  , xwidth  , NULL, 5);
  attach(hand     , this , this  , ywidth  , NULL, 5, 5);

  attach(sep1     , this , this  , hand    , NULL, 0, 0, 5);
  attach(lab1     , this , this  , sep1    , NULL, 0, 0, 5);

  attach(dx11     , this , NULL  , lab1    , NULL, 5);
  attach(dx21     , this , NULL  , dx11    , NULL, 5);
  attach(dx12     , this , NULL  , dx21    , NULL, 5);
  attach(dx22     , this , NULL  , dx12    , NULL, 5);

  attach(sep2     , this , this  , dx22    , NULL, 0, 0, 5);
  attach(lab2     , this , this  , sep2    , NULL, 0, 0, 5);

  attach(dn11     , this , NULL  , lab2    , NULL, 5);
  attach(dn21     , this , NULL  , dn11    , NULL, 5);
  attach(dn12     , this , NULL  , dn21    , NULL, 5);
/****
  attach(dn22     , this , NULL  , dn12    , NULL);

  attach(sep3     , this , this  , dn22    , NULL, 0, 0, 5);
  attach(lab3     , this , this  , sep3    , NULL, 0, 0, 5);

  attach(xloc     , this , NULL  , lab3    , NULL);
  attach(xgrid    , xloc , NULL  , lab3    , NULL, 20);
  attach(yloc     , this , NULL  , xloc    , NULL);
  attach(ygrid    , yloc , NULL  , xloc    , NULL, 20);
  attach(which    , this , NULL  , yloc    , this);
****/
  attach(dn22     , this , NULL  , dn12    , this, 5, 0, 0, 5);

  xorigin->setDtrap    (xorigin_trap   , this);
  yorigin->setDtrap    (yorigin_trap   , this);
  angle  ->setDtrap    (angle_trap     , this);
  xwidth ->setDtrap    (xwidth_trap    , this);
  ywidth ->setDtrap    (ywidth_trap    , this);
  hand   ->setItrap    (hand_trap      , this);

  dx11   ->setDtrap    (dx11_trap      , this);
  dx21   ->setDtrap    (dx21_trap      , this);
  dx12   ->setDtrap    (dx12_trap      , this);
  dx22   ->setDtrap    (dx22_trap      , this);

  dn11   ->setDtrap    (dn11_trap      , this);
  dn21   ->setDtrap    (dn21_trap      , this);
  dn12   ->setDtrap    (dn12_trap      , this);
  dn22   ->setDtrap    (dn22_trap      , this);

  xorigin->setupDvarFun  (xorigin_update  , this);
  yorigin->setupDvarFun  (yorigin_update  , this);
  angle  ->setupDvarFun  (angle_update    , this);
  xwidth ->setupDvarFun  (xwidth_update   , this);
  ywidth ->setupDvarFun  (ywidth_update   , this);
  hand   ->setupIvarFun  (hand_update     , this);

  dx11    ->setupDvarFun  (dx11_update   , this);
  dx21    ->setupDvarFun  (dx21_update   , this);
  dx12    ->setupDvarFun  (dx12_update   , this);
  dx22    ->setupDvarFun  (dx22_update   , this);

  dn11    ->setupDvarFun  (dn11_update   , this);
  dn21    ->setupDvarFun  (dn21_update   , this);
  dn12    ->setupDvarFun  (dn12_update   , this);
  dn22    ->setupDvarFun  (dn22_update   , this);

/****
  xloc    ->setDtrap      (xloc_trap   , this);
  yloc    ->setDtrap      (yloc_trap   , this);
  xgrid   ->setDtrap      (xgrid_trap  , this);
  ygrid   ->setDtrap      (ygrid_trap  , this);
  which   ->setItrap      (which_trap  , this);

  xloc    ->setupDvarFun  (xloc_update   , this);
  yloc    ->setupDvarFun  (yloc_update   , this);
  xgrid   ->setupDvarFun  (xgrid_update  , this);
  ygrid   ->setupDvarFun  (ygrid_update  , this);
  which   ->setupIvarFun  (which_update  , this);
****/
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiTransform::~VfguiTransform()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

