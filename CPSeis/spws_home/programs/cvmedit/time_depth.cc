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
//------------------- coord_edit.cc ----------------------//
//         implementation file for the CoordEdit class
//                 derived from the SLDatabox class
#include <assert.h>
#include "wproc.h"
#include "cprim.h"
#include "sl/slp_push.hh"
#include "sl_cvm_app.hh"
#include "transform.h"
#include "mlimits.h"
#include "vect/ll_vector.hh"
#include "model_desc.hh"
#include "time_depth.hh"

class SeisPlot;

ModelDesc *zot_time_to_depth(char *czo, ModelDesc *modi);

#if ( ultrix || sun || __sgi)
#define cellcalc_ cell_
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

#ifdef __cplusplus
}                   // for C++
#endif


TimeDepth::TimeDepth (SLDelay *slparent, char *name,CvmApp *cvm,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now)
         : SLSmartForm(slparent,name,Hctx,doframe,False,manage_now)
{ _cvmapp =  cvm;
  assert(_cvmapp);
  if(slparent->made() && make_if_can) make();
  setup();
}

TimeDepth::TimeDepth (Widget parent, char *name,CvmApp *cvm,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_now, Boolean   manage_now)
         : SLSmartForm(parent,name,Hctx,doframe,False,manage_now)
{ _cvmapp = cvm;
  assert(_cvmapp);
  if(make_now) make();
  setup();
}

void TimeDepth::setup()
{// Initialize & retain starting information.
  assert(_cvmapp);
  _mod=NULL;
}


TimeDepth::~TimeDepth(void)
{
 _cvmapp = NULL;
}

Widget TimeDepth::make(Widget/* parent*/)
{
 if(!made())
  {
   Widget wid = SLSmartForm::make();
    _form = this;
  }
 makeChildren();
 return topWidget();
}


//------------------------ traps -------------------------//


void TimeDepth::compTrap(void *data,long )
{ TimeDepth         *td = (TimeDepth *) data;
  CvmApp           *cvmapp=NULL;
  SeisPlot         *sp=NULL;
  ModelDesc        *mod=NULL;
  ModLimits        *mlim;
  float            xmin,xmax,ymin,ymax,zmin,zmax;
  ErsTransform     *tx=NULL,*ty=NULL,*tz=NULL;
  char              *czi,czo[16],msg[120];
  float ptl;

  if(!data) return;

// Get the structural data.
  cvmapp = (CvmApp *) td->_cvmapp;
  if(!cvmapp) return;
  mod = cvmapp->getModelDesc();
  if(!mod) return;

// Identify coord sys
  cvmapp->cvm_gettrans(&tx,&ty,&tz);
  czi = transform_getname(tz);
  strcpy(czo,"NONE");
  if(strcmp(czi,"TIME")==0) strcpy(czo,"DEPTH");
  if(strcmp(czi,"DEPTH")==0) strcpy(czo,"TIME");
  if(strcmp(czi,"NONE")==0) return;

// Compute the new converted model.
 cvmapp->setMessage("Wait: time depth conversion");
 td->_mod = zot_time_to_depth(czo,mod);
 cvmapp->setMessage("");
 if(!td->_mod )
  {
    sprintf(msg,"compTrap: conversion failed\n");
    printf("%s",msg);
    return;
  }

 CvmAppBase *appbase;
 appbase = new CvmAppBase(XtDisplay(XtParent(td->_form->W())),
           "CVMEDIT", "CvmEdit");
 if(!appbase) return;
 appbase->setup();
 sp = (SeisPlot *) appbase->cvm_get_seisplot(appbase);
 mlim = td->_mod->MLimits();
// gdata= mod->GridData();
// gdata->getGridVals(&n1,&o1,&d1,&n2,&o2,&d2,&n3,&o3,&d3);
 mlimits_get(mlim,&xmin,&xmax,&zmin,&zmax,&ymin,&ymax,&tx,&ty,&tz );
 sprintf(msg,"CVMEdit: Converted Model(%s)",transform_getname(tz));
 appbase->setTitle(msg,True);
 sp->setGridXYS(xmin,xmax,zmin,zmax);
 sp->setTmin(zmin);
 sp->setTmax(zmax);
 cvmapp->cvmSPTransform(sp,tx,ty,tz,NULL);
 ptl = (zmax-zmin)/5.0;
 sp->setTimingLines((double) ptl,(double) ptl);
 appbase->setModelDesc(td->_mod);
 appbase->setOwnsModel(1);
 sp->plot();

}


//--------------------- end --------------------//


//------------------- time_depth_pop.cc -------------------//
//       implementation file for the TimeDepthPop class
//              derived from the SLDialog class
//        Creates a TimeDepth class inside a dialogue 

//------------- constructors and destructor --------------//

TimeDepthPop::TimeDepthPop (SLDelay *slparent, char *name, HelpCtx Hctx,
         CvmApp *cvm)
        : SLDialog(slparent, name, Hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *cc;
  SLpPush     *rem = addBottomRemove(REMOVE);
                     addBottomHelp();
//  remove->unmanageShellWhenPressed(this);
  setTitle("Time-Depth Conversion");
  cc = new SLpPush( work, "tdpb",0,"Time<->Depth");
  work->attach(cc, work, work, NULL, work,4,4,0,4);
  _timdep = new TimeDepth(work,"zotcl", cvm, Hctx, False, True, True );
  work->attach(_timdep, work, work, work, cc,4,4,0,4);
  cc->setAtrap(_timdep->compTrap, _timdep); //static member function
  _slform = work;
}

TimeDepthPop::TimeDepthPop (Widget parent, char *name, HelpCtx Hctx,
         CvmApp *cvm)
        : SLDialog(parent, name, Hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *cc;
  SLpPush     *rem = addBottomRemove(REMOVE);
                    addBottomHelp();
  setTitle("Time-Depth Conversion");
  cc = new SLpPush( work, "tdpb",0,"Time<->Depth");
  work->attach(cc, work, work, NULL, work,4,4,0,4);
  _timdep = new TimeDepth(work,"zotcl", cvm, Hctx, False, True, True);
  cc->setAtrap(_timdep->compTrap, _timdep); 
  work->attach(_timdep, work, work, work, cc,4,4,0,4);
  _slform = work;
}


TimeDepthPop::~TimeDepthPop(void)
{// TimeDepth is deleted automatically by its parent
 //if(_timdep != NULL) delete _timdep;
}

Boolean TimeDepthPop::removeNotify()
{
 if(_timdep)
  {
    // _timdep->table_to_data();
   return True;
  }
  else return False;
}

Widget  TimeDepthPop::Form()
{if(_slform) return _slform->W();
 return NULL;
}
//--------------- end ----------------------//
