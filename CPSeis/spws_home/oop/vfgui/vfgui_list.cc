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

//---------------------- vfgui_list.cc ------------------------//
//---------------------- vfgui_list.cc ------------------------//
//---------------------- vfgui_list.cc ------------------------//

//         implementation file for the VfguiList class
//               derived from the SLSmartForm class
//                       subdirectory vfgui


#include "vfgui/vfgui_list.hh"
#include "vfgui/vfgui_regulate.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_sep.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include "sl/slp_push.hh"
#include "sl/slp_label.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiList::VfguiList(SLDelay *slparent, char *name, VfManager *manager)
       : SLSmartForm(slparent, name, NULL, FALSE),
               _manager              (manager),
               _regulate             (NULL)
{
  assert(manager);
  _regulate = new VfguiRegulate(manager);
  if(manager->numEditableDatasets() > 1) severalEditableDatasets();
  else                                   oneEditableDataset();
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiList::~VfguiList()
{
  delete _regulate;
}



//----------------------- one editable dataset -----------------------//
//----------------------- one editable dataset -----------------------//
//----------------------- one editable dataset -----------------------//

       // private.

void VfguiList::oneEditableDataset()
{
  SLSmartForm *form = summaryForm (this);
//SLpPush *compare1 = new SLpPush (this, "compare1");
  SLpPush *compare2 = new SLpPush (this, "compare2");
  SLpPush *backup3  = new SLpPush (this, "backup3");

  _regulate->regulateCompare (NULL, compare2);
//_regulate->regulateCompare (compare1, compare2);
  _regulate->regulateBackup  (backup3);

    //              LEFT     RIGHT   TOP      BOTTOM

  attach(form     , this   , this  , this    , NULL,  5,  5,  5,  5);
/*
  attach(compare1 , NULL   , this  , form    , NULL, 25, 25, 15,  5);
  attach(compare2 , NULL   , this  , compare1, this, 25, 25,  5,  5);
  attach(backup3  , this   , NULL  , form    , NULL, 25, 25, 15,  5);
*/
  attach(backup3  , this   , NULL  , form    , this, 25, 25, 15,  5);
  attach(compare2 , backup3, this  , form    , this, 25, 25, 15,  5);
}



//-------------------- several editable datasets --------------------//
//-------------------- several editable datasets --------------------//
//-------------------- several editable datasets --------------------//

       // private.

void VfguiList::severalEditableDatasets()
{
  SLSmartForm *form = summaryForm (this);
//SLpPush *compare1 = new SLpPush (this, "compare1");
  SLpPush *compare2 = new SLpPush (this, "compare2");
  SLpPush *backup1  = new SLpPush (this, "backup1");
  SLpPush *backup2  = new SLpPush (this, "backup2");

  _regulate->regulateCompare (NULL, compare2);
//_regulate->regulateCompare (compare1, compare2);
  _regulate->regulateBackup  (backup1, backup2);

    //              LEFT   RIGHT   TOP      BOTTOM

  attach(form     , this , this  , this    , NULL,  5,  5,  5,  5);
/*
  attach(compare1 , NULL , this  , form    , NULL, 25, 25, 15,  5);
  attach(compare2 , NULL , this  , compare1, this, 25, 25,  5,  5);
  attach(backup1  , this , NULL  , form    , NULL, 25, 25, 15,  5);
  attach(backup2  , this , NULL  , backup1 , this, 25, 25,  5,  5);
*/
  attach(backup1  , this , NULL  , form    , NULL, 25, 25, 15,  5);
  attach(backup2  , this , NULL  , backup1 , this, 25, 25,  5,  5);
  attach(compare2 , NULL , this  , form    , NULL, 25, 25, 15,  5);
}



//-------------------------- static functions ----------------------------//
//-------------------------- static functions ----------------------------//
//-------------------------- static functions ----------------------------//

static void nhx_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  VfguiList *gui = (VfguiList*)data;
  gui->manager()->activeDataset()->setNhx((int)newvar);
}


static void nhy_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  VfguiList *gui = (VfguiList*)data;
  gui->manager()->activeDataset()->setNhy((int)newvar);
}


static void order_trap(void *data, long /*ident*/,
                       long /*oldvar*/, long newvar)
{
  VfguiList *gui = (VfguiList*)data;
  gui->manager()->activeDataset()->setMoveoutOrder((int)newvar);
}



#define UPFUN(long2, nfun_upfun, numVelocityFunctions)             \
                                                                   \
static long2 nfun_upfun(void *data)                                \
{                                                                  \
  VfguiList *gui = (VfguiList*)data;                               \
  return gui->manager()->activeDataset()->numVelocityFunctions;    \
}


UPFUN (long ,        nfun_upfun, numVelocityFunctions()  )
UPFUN (long ,        nsel_upfun, numSelectedVelocityFunctions()  )
UPFUN (long ,         act_upfun, getActiveVelocityFunction()+1  )
UPFUN (long ,         ref_upfun, getReferenceVelocityFunction()+1  )
UPFUN (long ,        nerr_upfun, numVelocityFunctionsWithErrors()  )
UPFUN (long ,        nray_upfun, numRaytracedVelocityFunctions()  )
UPFUN (long ,         nhx_upfun, getNhx()  )
UPFUN (long ,         nhy_upfun, getNhy()  )
UPFUN (long ,       order_upfun, getMoveoutOrder()  )
UPFUN (float,     nhosign_upfun, getNhosign()  )
UPFUN (float,      nhoexp_upfun, getNhoexp()  )
UPFUN (float,  min_xcoord_upfun, minimumXloc()  )
UPFUN (float,  max_xcoord_upfun, maximumXloc()  )
UPFUN (float,  min_ycoord_upfun, minimumYloc()  )
UPFUN (float,  max_ycoord_upfun, maximumYloc()  )
UPFUN (float,    min_time_upfun, minimumTime()  )
UPFUN (float,    max_time_upfun, maximumTime()  )
UPFUN (float,   min_depth_upfun, minimumDepth()  )
UPFUN (float,   max_depth_upfun, maximumDepth()  )
UPFUN (float,    min_vnmo_upfun, minimumVnmo()  )
UPFUN (float,    max_vnmo_upfun, maximumVnmo()  )



static char *row6_upfun(void *data)
{
  VfguiList *gui = (VfguiList*)data;
  return (char*)gui->manager()->activeDataset()->lastFileRead();
}

static char *row7_upfun(void *data)
{
  VfguiList *gui = (VfguiList*)data;
  return (char*)gui->manager()->activeDataset()->lastFileSaved();
}

static char *row8_upfun(void *data)
{
  VfguiList *gui = (VfguiList*)data;
  return (char*)gui->manager()->activeDataset()->lastBackupFileSaved();
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        "*summary_form.borderWidth:        2",
        "*summary_form*background:         wheat",
        "*summary_form*fontList:           fixed",
        "*summary_form.sep.separatorType:  SINGLE_LINE",
        "*summary_form.sep.height:         6",
            NULL };



//------------------------ summary form ------------------------------//
//------------------------ summary form ------------------------------//
//------------------------ summary form ------------------------------//

      // private.

SLSmartForm *VfguiList::summaryForm(SLDelay *parent)
{
//setFallbackResources((const char**)defres);
// the above also works if defres is char *defres[].
  setFallbackResources(defres);
  SLSmartForm *form = new SLSmartForm(parent, "summary_form");

  SLSep       *sep1 = new SLSep       (form, "sep");
  SLSep       *sep2 = new SLSep       (form, "sep");

  SLSmartForm *row1  = new SLSmartForm (form, "row1" , NULL);
  SLSmartForm *row1a = new SLSmartForm (form, "row1a", NULL);
  SLSmartForm *row2  = new SLSmartForm (form, "row2" , NULL);
  SLSmartForm *row3  = new SLSmartForm (form, "row3" , NULL);
  SLSmartForm *row4  = new SLSmartForm (form, "row4" , NULL);
  SLSmartForm *row5  = new SLSmartForm (form, "row5" , NULL);
  SL2Text     *row6  = new SL2Text (form, "row6", 0, "last file read........:");
  SL2Text     *row7  = new SL2Text (form, "row7", 0, "last file saved.......:");
  SL2Text     *row8  = new SL2Text (form, "row8", 0, "last backup file saved:");

  row1 ->showEvenSpacing();
  row1a->showEvenSpacing();
  row2 ->showEvenSpacing();
  row3 ->showEvenSpacing();
  row4 ->showEvenSpacing();
  row5 ->showEvenSpacing();
  row6 ->showLabelAppearance();
  row7 ->showLabelAppearance();
  row8 ->showLabelAppearance();

  SLpLabel *lab  = new SLpLabel (row1, "lab" , 0, "ACTIVE DATASET");
  SL2Text *nfun  = new SL2Text (row1, "nfun" , 0,
                           "#functions:", SLpText::_LONG, 6);
  SL2Text *nsel  = new SL2Text (row1, "nsel" , 0,
                           "#selected:", SLpText::_LONG, 6);
  SL2Text *act   = new SL2Text (row1, "act" , 0,
                           "active function:", SLpText::_LONG, 6);

  SL2Text *nerr  = new SL2Text (row1a, "nerr" , 0,
                           "#functions with errors:", SLpText::_LONG, 6);
  SL2Text *nray  = new SL2Text (row1a, "nray" , 0,
                           "#raytraced functions:", SLpText::_LONG, 6);
  SL2Text *ref   = new SL2Text (row1a, "ref" , 0,
                           "reference function:", SLpText::_LONG, 6);

  SL2Text *nhx   = new SL2Text (row2, "nn", 0, "nhx:"  , SLpText::_LONG, 2);
  SL2Text *nhy   = new SL2Text (row2, "nn", 0, "nhy:"  , SLpText::_LONG, 2);
  SL2Text *order = new SL2Text (row2, "nn", 0, "order:", SLpText::_LONG, 2);
  SL2Text *nhos  = new SL2Text (row2, "nn", 0, "nhosign:", SLpText::_FLOAT, 4);
  SL2Text *nhoe  = new SL2Text (row2, "nn", 0, "nhoexp:", SLpText::_FLOAT, 4);

  SLpLabel  *left1 = new SLpLabel (row3, "   ");
  SLpLabel  *left2 = new SLpLabel (row4, "left", 0, "minimum:");
  SLpLabel  *left3 = new SLpLabel (row5, "left", 0, "maximum:");

  SLpLabel *lab1 = new SLpLabel (row3, "lab", 0, "X coord");
  SLpLabel *lab2 = new SLpLabel (row3, "lab", 0, "Y coord");
  SLpLabel *lab3 = new SLpLabel (row3, "lab", 0, "time");
  SLpLabel *lab4 = new SLpLabel (row3, "lab", 0, "depth");
  SLpLabel *lab5 = new SLpLabel (row3, "lab", 0, "NMO velocity");

  SLpText *minx = new SLpText (row4, "min", 0, SLpText::_FLOAT, 8,4);
  SLpText *miny = new SLpText (row4, "min", 0, SLpText::_FLOAT, 8,4);
  SLpText *mint = new SLpText (row4, "min", 0, SLpText::_FLOAT, 8,4);
  SLpText *mind = new SLpText (row4, "min", 0, SLpText::_FLOAT, 8,0);
  SLpText *minv = new SLpText (row4, "min", 0, SLpText::_FLOAT, 8,0);

  SLpText *maxx = new SLpText (row5, "max", 0, SLpText::_FLOAT, 8,4);
  SLpText *maxy = new SLpText (row5, "max", 0, SLpText::_FLOAT, 8,4);
  SLpText *maxt = new SLpText (row5, "max", 0, SLpText::_FLOAT, 8,4);
  SLpText *maxd = new SLpText (row5, "max", 0, SLpText::_FLOAT, 8,0);
  SLpText *maxv = new SLpText (row5, "max", 0, SLpText::_FLOAT, 8,0);

  nfun->showLabelAppearance();
  nsel->showLabelAppearance();
  act ->showLabelAppearance();
  ref ->showLabelAppearance();
  nerr->showLabelAppearance();
  nray->showLabelAppearance();
  nhos->showLabelAppearance();
  nhoe->showLabelAppearance();

  minx->showLabelAppearance();
  miny->showLabelAppearance();
  mint->showLabelAppearance();
  mind->showLabelAppearance();
  minv->showLabelAppearance();

  maxx->showLabelAppearance();
  maxy->showLabelAppearance();
  maxt->showLabelAppearance();
  maxd->showLabelAppearance();
  maxv->showLabelAppearance();

  nhx  ->setItrap (  nhx_trap, this);
  nhy  ->setItrap (  nhy_trap, this);
  order->setItrap (order_trap, this);

  nfun ->setupIvarFun ( nfun_upfun, this);
  nsel ->setupIvarFun ( nsel_upfun, this);
  act  ->setupIvarFun (  act_upfun, this);
  ref  ->setupIvarFun (  ref_upfun, this);
  nerr ->setupIvarFun ( nerr_upfun, this);
  nray ->setupIvarFun ( nray_upfun, this);

  nhx  ->setupIvarFun (    nhx_upfun, this);
  nhy  ->setupIvarFun (    nhy_upfun, this);
  order->setupIvarFun (  order_upfun, this);
  nhos ->setupFvarFun (nhosign_upfun, this);
  nhoe ->setupFvarFun ( nhoexp_upfun, this);

  minx ->setupFvarFun ( min_xcoord_upfun, this);
  miny ->setupFvarFun ( min_ycoord_upfun, this);
  mint ->setupFvarFun (   min_time_upfun, this);
  mind ->setupFvarFun (  min_depth_upfun, this);
  minv ->setupFvarFun (   min_vnmo_upfun, this);

  maxx ->setupFvarFun ( max_xcoord_upfun, this);
  maxy ->setupFvarFun ( max_ycoord_upfun, this);
  maxt ->setupFvarFun (   max_time_upfun, this);
  maxd ->setupFvarFun (  max_depth_upfun, this);
  maxv ->setupFvarFun (   max_vnmo_upfun, this);

  row6 ->setupCvarFun (   row6_upfun, this);
  row7 ->setupCvarFun (   row7_upfun, this);
  row8 ->setupCvarFun (   row8_upfun, this);

//                        LEFT   RIGHT   TOP      BOTTOM
  form->attach(row1     , form , form  , form    , NULL,  0,  0, 5);
  form->attach(row1a    , form , form  , row1    , NULL,  0,  0, 0);
  form->attach(row2     , form , form  , row1a   , NULL,  0,  0, 0);
  form->attach(sep1     , form , form  , row2    , NULL,  0,  0, 0);
  form->attach(row3     , form , form  , sep1    , NULL,  0,  0, 0);
  form->attach(row4     , form , form  , row3    , NULL,  0,  0, 0);
  form->attach(row5     , form , form  , row4    , NULL,  0,  0, 0);
  form->attach(sep2     , form , form  , row5    , NULL,  0,  0, 0);
  form->attach(row6     , form , form  , sep2    , NULL,  5,  0, 0);
  form->attach(row7     , form , form  , row6    , NULL,  5,  0, 0);
  form->attach(row8     , form , form  , row7    , form,  5,  0, 0, 5);

  return form;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

