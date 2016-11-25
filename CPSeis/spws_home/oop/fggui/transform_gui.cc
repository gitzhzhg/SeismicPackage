
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
//---------------------- transform_gui.cc ------------------------//
//---------------------- transform_gui.cc ------------------------//
//---------------------- transform_gui.cc ------------------------//

//         implementation file for the TransformGui class
//               derived from the SLSmartForm class
//                derived from the FgInform class
//                        subdirectory fggui


#include "fggui/transform_gui.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_sep.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_option.hh"
#include "sl/sl_column.hh"
#include "cprim.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>


enum { HAND_RIGHT, HAND_LEFT };

static double xloc_coord  = 0.0;
static double yloc_coord  = 0.0;
static double xgrid_coord = 0.0;
static double ygrid_coord = 0.0;
static long   which_coord =   1;


//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


#define TRAP(xorigin_trap, setTestingXorigin)       \
static void xorigin_trap(void *data, long      ,    \
                 double /*oldvar*/, double newvar)  \
{                                                   \
  FieldGeometry *fg = (FieldGeometry*)data;         \
  fg->setTestingXorigin(newvar);                    \
}


TRAP(xorigin_trap, setTestingXorigin)
TRAP(yorigin_trap, setTestingYorigin)
TRAP(angle_trap  , setTestingRotationAngle)
TRAP(xwidth_trap , setTestingXgridWidth)
TRAP(ywidth_trap , setTestingYgridWidth)
TRAP(dx11_trap   , setTestingDx11)
TRAP(dx21_trap   , setTestingDx21)
TRAP(dx12_trap   , setTestingDx12)
TRAP(dx22_trap   , setTestingDx22)
TRAP(dn11_trap   , setTestingDn11)
TRAP(dn21_trap   , setTestingDn21)
TRAP(dn12_trap   , setTestingDn12)
TRAP(dn22_trap   , setTestingDn22)


static void hand_trap(void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  switch(newvar)
     {
     case HAND_RIGHT: fg->setTestingRightHanded(); break;
     case HAND_LEFT : fg->setTestingLeftHanded (); break;
     default: assert(FALSE);
     }
}



//------------------- update functions ----------------------------//
//------------------- update functions ----------------------------//
//------------------- update functions ----------------------------//

static char *lock_update(void *data)
{                                  
  static char *yes_locked = "GRID TRANSFORM IS LOCKED";
  static char *no_locked  = " ";
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->allowModifyingGridTransform()) return no_locked;                
  return yes_locked;
}

#define UPDATE(xorigin_update, getTestingXorigin)   \
static double xorigin_update(void *data)            \
{                                                   \
  FieldGeometry *fg = (FieldGeometry*)data;         \
  return fg->getTestingXorigin();                   \
}


UPDATE(xorigin_update, getTestingXorigin)
UPDATE(yorigin_update, getTestingYorigin)
UPDATE(angle_update  , getTestingRotationAngle)
UPDATE(xwidth_update , getTestingXgridWidth)
UPDATE(ywidth_update , getTestingYgridWidth)
UPDATE(dx11_update   , getTestingDx11)
UPDATE(dx21_update   , getTestingDx21)
UPDATE(dx12_update   , getTestingDx12)
UPDATE(dx22_update   , getTestingDx22)
UPDATE(dn11_update   , getTestingDn11)
UPDATE(dn21_update   , getTestingDn21)
UPDATE(dn12_update   , getTestingDn12)
UPDATE(dn22_update   , getTestingDn22)


static long hand_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->isTestingRightHanded()) return HAND_RIGHT;
  if(fg->isTestingLeftHanded ()) return HAND_LEFT;
  assert(FALSE);
  return 0;
}
 


#define UPDATE2(xorigin_update2, getXorigin)   \
static double xorigin_update2(void *data)      \
{                                              \
  FieldGeometry *fg = (FieldGeometry*)data;    \
  return fg->getXorigin();                     \
}


UPDATE2(xorigin_update2, getXorigin)
UPDATE2(yorigin_update2, getYorigin)
UPDATE2(angle_update2  , getRotationAngle)
UPDATE2(xwidth_update2 , getXgridWidth)
UPDATE2(ywidth_update2 , getYgridWidth)
UPDATE2(dx11_update2   , getDx11)
UPDATE2(dx21_update2   , getDx21)
UPDATE2(dx12_update2   , getDx12)
UPDATE2(dx22_update2   , getDx22)
UPDATE2(dn11_update2   , getDn11)
UPDATE2(dn21_update2   , getDn21)
UPDATE2(dn12_update2   , getDn12)
UPDATE2(dn22_update2   , getDn22)


static char *hand_update2(void *data)
{
  static char buffer1[] = "right";
  static char buffer2[] = "left";
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->isRightHanded()) return buffer1;
  if(fg->isLeftHanded ()) return buffer2;
  assert(FALSE);
  return NULL;
}
 


//------------ convenience area traps and updates --------------------//
//------------ convenience area traps and updates --------------------//
//------------ convenience area traps and updates --------------------//


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
  if(which_coord == 1) return xloc_coord;
  TransformGui *gui = (TransformGui*)data;
  FieldGeometry *fg = gui->getFieldGeometry();
  xloc_coord = fg->getTestingXlocCoord(xgrid_coord, ygrid_coord);
  return xloc_coord;
}


static double yloc_update(void *data)
{
  if(which_coord == 1) return yloc_coord;
  TransformGui *gui = (TransformGui*)data;
  FieldGeometry *fg = gui->getFieldGeometry();
  yloc_coord = fg->getTestingYlocCoord(xgrid_coord, ygrid_coord);
  return yloc_coord;
}


static double xgrid_update(void *data)
{
  if(which_coord == 2) return xgrid_coord;
  TransformGui *gui = (TransformGui*)data;
  FieldGeometry *fg = gui->getFieldGeometry();
  xgrid_coord = fg->getTestingXgridCoord(xloc_coord, yloc_coord);
  return xgrid_coord;
}


static double ygrid_update(void *data)
{
  if(which_coord == 2) return ygrid_coord;
  TransformGui *gui = (TransformGui*)data;
  FieldGeometry *fg = gui->getFieldGeometry();
  ygrid_coord = fg->getTestingYgridCoord(xloc_coord, yloc_coord);
  return ygrid_coord;
}


static long which_update(void *)
{
  return which_coord;
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TransformGui::TransformGui(SLDelay *slparent, char *name,
                     FieldGeometry *fg,
                     ContainerList *clist)
       : SLSmartForm(slparent, name, NULL, FALSE),
         FgInform(fg)
{
  SLpText   *lock    = new SLpText      (this, "lock");
  SLDelay   *msg     = new FgMessageGui (this, "trans_msg" , _fg);
  SLDelay   *stat    = new FgStatusGui  (this, "trans_stat", _fg, clist);

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


  SLpText   *xorigin2 = new SLpText (this, "xorigin2", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *yorigin2 = new SLpText (this, "yorigin2", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *angle2   = new SLpText (this, "angle2", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *xwidth2  = new SLpText (this, "xwidth2", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *ywidth2  = new SLpText (this, "ywidth2", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *hand2    = new SLpText (this, "hand2", 0,
                                           SLpText::_CHAR, 13);

  SLpText   *dx11a   = new SLpText (this, "dx11a", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *dx21a   = new SLpText (this, "dx21a", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *dx12a   = new SLpText (this, "dx12a", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *dx22a   = new SLpText (this, "dx22a", 0,
                                           SLpText::_DOUBLE, 13);

  SLpText   *dn11a   = new SLpText (this, "dn11a", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *dn21a   = new SLpText (this, "dn21a", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *dn12a   = new SLpText (this, "dn12a", 0,
                                           SLpText::_DOUBLE, 13);
  SLpText   *dn22a   = new SLpText (this, "dn22a", 0,
                                           SLpText::_DOUBLE, 13);

  lock    ->showLabelAppearance();
  xorigin2->showLabelAppearance();
  yorigin2->showLabelAppearance();
  angle2  ->showLabelAppearance();
  xwidth2 ->showLabelAppearance();
  ywidth2 ->showLabelAppearance();
  hand2   ->showLabelAppearance();

  dx11a   ->showLabelAppearance();
  dx21a   ->showLabelAppearance();
  dx12a   ->showLabelAppearance();
  dx22a   ->showLabelAppearance();

  dn11a   ->showLabelAppearance();
  dn21a   ->showLabelAppearance();
  dn12a   ->showLabelAppearance();
  dn22a   ->showLabelAppearance();

  hand ->addOption("right handed coordinate system", HAND_RIGHT);
  hand ->addOption("left handed coordinate system" , HAND_LEFT);

  SLSep    *sep0 = new SLSep(this, "sep0");
  SLSep    *sep1 = new SLSep(this, "sep1");
  SLSep    *sep2 = new SLSep(this, "sep2");
  SLSep    *sep3 = new SLSep(this, "sep3");
  SLpLabel *lab1 = new SLpLabel(this, "Rotation Matrix");
  SLpLabel *lab2 = new SLpLabel(this, "Inverse Rotation Matrix");
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

//                  left   right   top      bottom

  attach(lock     , this , this  , this    , NULL);
  attach(msg      , this , this  , lock    , NULL);
  attach(stat     , this , this  , msg     , NULL);
  attach(sep0     , this , this  , stat    , NULL, 0, 0, 5);

  attach(xorigin  , this , NULL  , sep0    , NULL, 0, 0, 5);
  attach(yorigin  , this , NULL  , xorigin , NULL);
  attach(angle    , this , NULL  , yorigin , NULL);
  attach(xwidth   , this , NULL  , angle   , NULL);
  attach(ywidth   , this , NULL  , xwidth  , NULL);
  attach(hand     , this , hand2 , ywidth  , NULL);

  attach(xorigin2 , NULL , this  , sep0    , NULL);
  attach(yorigin2 , NULL , this  , xorigin , NULL);
  attach(angle2   , NULL , this  , yorigin , NULL);
  attach(xwidth2  , NULL , this  , angle   , NULL);
  attach(ywidth2  , NULL , this  , xwidth  , NULL);
  attach(hand2    , NULL , this  , ywidth  , NULL);

  attach(sep1     , this , this  , hand    , NULL, 0, 0, 5);
  attach(lab1     , this , this  , sep1    , NULL, 0, 0, 5);

  attach(dx11     , this , NULL  , lab1    , NULL);
  attach(dx21     , this , NULL  , dx11    , NULL);
  attach(dx12     , this , NULL  , dx21    , NULL);
  attach(dx22     , this , NULL  , dx12    , NULL);

  attach(dx11a    , NULL , this  , lab1    , NULL);
  attach(dx21a    , NULL , this  , dx11    , NULL);
  attach(dx12a    , NULL , this  , dx21    , NULL);
  attach(dx22a    , NULL , this  , dx12    , NULL);

  attach(sep2     , this , this  , dx22    , NULL, 0, 0, 5);
  attach(lab2     , this , this  , sep2    , NULL, 0, 0, 5);

  attach(dn11     , this , NULL  , lab2    , NULL);
  attach(dn21     , this , NULL  , dn11    , NULL);
  attach(dn12     , this , NULL  , dn21    , NULL);
  attach(dn22     , this , NULL  , dn12    , NULL);

  attach(dn11a    , NULL , this  , lab2    , NULL);
  attach(dn21a    , NULL , this  , dn11    , NULL);
  attach(dn12a    , NULL , this  , dn21    , NULL);
  attach(dn22a    , NULL , this  , dn12    , NULL);

  attach(sep3     , this , this  , dn22    , NULL, 0, 0, 5);
  attach(lab3     , this , this  , sep3    , NULL, 0, 0, 5);

  attach(xloc     , this , NULL  , lab3    , NULL);
  attach(xgrid    , xloc , NULL  , lab3    , NULL, 20);
  attach(yloc     , this , NULL  , xloc    , NULL);
  attach(ygrid    , yloc , NULL  , xloc    , NULL, 20);
  attach(which    , this , NULL  , yloc    , this);

  xorigin->setDtrap    (xorigin_trap   , fg);
  yorigin->setDtrap    (yorigin_trap   , fg);
  angle  ->setDtrap    (angle_trap     , fg);
  xwidth ->setDtrap    (xwidth_trap    , fg);
  ywidth ->setDtrap    (ywidth_trap    , fg);
  hand   ->setItrap    (hand_trap      , fg);

  dx11   ->setDtrap    (dx11_trap      , fg);
  dx21   ->setDtrap    (dx21_trap      , fg);
  dx12   ->setDtrap    (dx12_trap      , fg);
  dx22   ->setDtrap    (dx22_trap      , fg);

  dn11   ->setDtrap    (dn11_trap      , fg);
  dn21   ->setDtrap    (dn21_trap      , fg);
  dn12   ->setDtrap    (dn12_trap      , fg);
  dn22   ->setDtrap    (dn22_trap      , fg);

  lock   ->setupCvarFun  (lock_update     , fg);

  xorigin->setupDvarFun  (xorigin_update  , fg);
  yorigin->setupDvarFun  (yorigin_update  , fg);
  angle  ->setupDvarFun  (angle_update    , fg);
  xwidth ->setupDvarFun  (xwidth_update   , fg);
  ywidth ->setupDvarFun  (ywidth_update   , fg);
  hand   ->setupIvarFun  (hand_update     , fg);

  dx11    ->setupDvarFun  (dx11_update   , fg);
  dx21    ->setupDvarFun  (dx21_update   , fg);
  dx12    ->setupDvarFun  (dx12_update   , fg);
  dx22    ->setupDvarFun  (dx22_update   , fg);

  dn11    ->setupDvarFun  (dn11_update   , fg);
  dn21    ->setupDvarFun  (dn21_update   , fg);
  dn12    ->setupDvarFun  (dn12_update   , fg);
  dn22    ->setupDvarFun  (dn22_update   , fg);

  xorigin2->setupDvarFun  (xorigin_update2  , fg);
  yorigin2->setupDvarFun  (yorigin_update2  , fg);
  angle2  ->setupDvarFun  (angle_update2    , fg);
  xwidth2 ->setupDvarFun  (xwidth_update2   , fg);
  ywidth2 ->setupDvarFun  (ywidth_update2   , fg);
  hand2   ->setupCvarFun  (hand_update2     , fg);

  dx11a   ->setupDvarFun  (dx11_update2  , fg);
  dx21a   ->setupDvarFun  (dx21_update2  , fg);
  dx12a   ->setupDvarFun  (dx12_update2  , fg);
  dx22a   ->setupDvarFun  (dx22_update2  , fg);

  dn11a   ->setupDvarFun  (dn11_update2  , fg);
  dn21a   ->setupDvarFun  (dn21_update2  , fg);
  dn12a   ->setupDvarFun  (dn12_update2  , fg);
  dn22a   ->setupDvarFun  (dn22_update2  , fg);

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
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TransformGui::~TransformGui()
{
}



//--------------- post new testing grid transform -----------------------//
//--------------- post new testing grid transform -----------------------//
//--------------- post new testing grid transform -----------------------//

       // overrides FgInform

void TransformGui::postNewTestingGridTransform(FieldGeometry*)
{
  update();
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

