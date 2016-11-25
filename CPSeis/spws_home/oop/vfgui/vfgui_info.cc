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

//---------------------- vfgui_info.cc ------------------------//
//---------------------- vfgui_info.cc ------------------------//
//---------------------- vfgui_info.cc ------------------------//

//         implementation file for the VfguiInfo class
//               derived from the SLSmartForm class
//                       subdirectory vfgui


#include "vfgui/vfgui_info.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_informer.hh"
#include "sl/sl2_arrows.hh"
#include "sl/slp_push.hh"
/*
#include "sl/slp_option.hh"
#include "sl/sl2_text.hh"
*/
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>


#define MANAGER  VfManager *manager = (VfManager*)data;

#define LABEL_VEL "velocity functions selected:"
#define LABEL_SEM \
"sembl panels (marked with *) have NO matching velocity function"
#define LABEL_CMP \
" cmp  panels (marked with *) have NO matching velocity function"





/***********************
static void info_topreg(VelStruct *vel, Widget parent)
{
  Widget w1, w2, w3, w4, w5, w6;
  Widget row = make_row(parent, "row");
  Widget row2 = make_form(parent, "row2");
  Widget row3 = make_form(parent, "row3");

  VbStruct *vb;

  get_vbstruct_pointer(&vb);
  make_itext2(row   ,"nselect"  , LABEL_VEL    ,IIHHDD,NULL,&vb->nselect ,6);
  make_itext2(row   ,"nerrmsg"  ,"with errors:",IIHHDD,NULL,&vb->nerrmsg ,6);
  make_itext2(row   ,"nraymsg"  ,"raytraced:"  ,IIHHDD,NULL,&vb->nraymsg ,6);
  make_itext2(row   ,"nhx"      ,"NHX:" ,IIHHDD,NULL,&vel->vd->nhx,2);
  make_itext2(row   ,"nhy"      ,"NHY:" ,IIHHDD,NULL,&vel->vd->nhy,2);
  w1=make_itext3(row2  ,"sembmatch", LABEL_SEM ,IIHHDD,NULL,&sembmatch   ,6);
  w2=make_itext3(row3  ,"cmpmatch" , LABEL_CMP    ,IIHHDD,NULL,&cmpmatch ,6);
  w3=make_ftext2(row2,"xcenter" ,"xcenter:",IIHHDD,NULL,&vel->vd->xcenter,7,3);
  w4=make_ftext2(row3,"ycenter" ,"ycenter:",IIHHDD,NULL,&vel->vd->ycenter,7,3);
  w5=make_ftext2(row2,"xwidth", "  xwidth:",IIHHDD,NULL,&vel->vd->xwidth ,5,3);
  w6=make_ftext2(row3,"ywidth", "  ywidth:",IIHHDD,NULL,&vel->vd->ywidth ,5,3);
  attach_widget(w1, row2,NULL ,row2,row2, 0,0,0,0);
  attach_widget(w3, NULL,w5   ,row2,row2, 0,0,0,0);
  attach_widget(w5, NULL,row2 ,row2,row2, 0,0,0,0);
  attach_widget(w2, row3,NULL ,row3,row3, 0,0,0,0);
  attach_widget(w4, NULL,w6   ,row3,row3, 0,0,0,0);
  attach_widget(w6, NULL,row3 ,row3,row3, 0,0,0,0);
}
***********************/





//------------------- build static functions -----------------------//
//------------------- build static functions -----------------------//
//------------------- build static functions -----------------------//


static void build_trap(void *data, long /*ident*/)
{
  MANAGER
  VfDataset *dataset = manager->activeDataset();
  dataset->informer()->beforeChanges();
  dataset->appendVelocityFunction();
  long nfun = dataset->numVelocityFunctions();
  dataset->setActiveVelocityFunction(nfun - 1);
  dataset->informer()->afterChanges();
}



static long build_sense_update(void *data)
{
  MANAGER
  VfDataset *dataset = manager->activeDataset();
  if(dataset->notEditable() || dataset->isLocked()) return FALSE;
  long ifun = dataset->getActiveVelocityFunction();
  long nfun = dataset->numVelocityFunctions();
  if(nfun == 0 || ifun == -1) return TRUE;
  if(ifun == nfun - 1 && dataset->numPicks(ifun) > 0) return TRUE;
  return FALSE;
}



//---------------------- arrows static functions ---------------------//
//---------------------- arrows static functions ---------------------//
//---------------------- arrows static functions ---------------------//


static void arrows_num1_trap(void *data, long newvar)
{
  MANAGER
  VfDataset *dataset = manager->activeDataset();
  long nfun = dataset->numVelocityFunctions();
  if(nfun == 0)
      {
      dataset->setActiveVelocityFunction(-1);
      }
  else
      {
      newvar = ConstrainValue(newvar, 1, nfun);
      dataset->setActiveVelocityFunction(newvar - 1);
      }
}


static long arrows_num1_update(void *data)
{
  MANAGER
  VfDataset *dataset = manager->activeDataset();
  long ifun = dataset->getActiveVelocityFunction();
  long nfun = dataset->numVelocityFunctions();
  if(ifun == -1) return 0;
  return (ifun + 1);
}


static long arrows_num2_update(void *data)
{
  MANAGER
  VfDataset *dataset = manager->activeDataset();
  long nfun = dataset->numVelocityFunctions();
  return nfun;
}


static long arrows_sense_update(void *data)
{
  MANAGER
  VfDataset *dataset = manager->activeDataset();
  long nfun = dataset->numVelocityFunctions();
  return (nfun > 0 && dataset->isEditable());
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiInfo::VfguiInfo(SLDelay *slparent, char *name, VfManager *manager)
       : SLSmartForm(slparent, name, NULL, FALSE),
               _manager      (manager)
{
  assert(_manager);
  showEvenSpacing();

  SL2Arrows *arrows = new SL2Arrows(this, "vfgui_pick_arrows",
                                "active velocity function", 6);

  arrows->registerNum1Trap    (arrows_num1_trap   , manager);
  arrows->registerNum1Update  (arrows_num1_update , manager);
  arrows->registerNum2Update  (arrows_num2_update , manager);
  arrows->registerSenseUpdate (arrows_sense_update, manager);

  SLpPush *build = new SLpPush (this, "build", 0,
                                   " build new velocity function ");

  build->setAtrap      (build_trap        , manager);
  build->setupSenseFun (build_sense_update, manager);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiInfo::~VfguiInfo()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

