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
#include <string.h>
#include <stdio.h>

#include "new_cvm.h"
#include "sl/sl_dialog.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_row_column.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_push.hh"
#include "sl_cvm_app.hh"
#include "model_desc.hh"
//#include "model.h"

enum { NOK, NCAN };

/* PROTOTYPES FOR PRIVATE METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void   CVMMsgBox(Widget , char *);

static void KillCB(Widget W, new_cvm*, caddr_t b);
static void NControlTrap(void *data, long ident);

#ifdef __cplusplus
}                   // for C++
#endif


SLDialog *NewGUI(Widget parent, CvmApp *cvmapp, HelpCtx Hctx)
{/* Dialog to control primary header used for picking *
  * When starting a new model                        */
 ModelDesc   *mod=NULL;
 SLDialog    *dial;
 SLSmartForm *work;
 SLRowColumn *rc;
 SLpPush     *ok, *cancel;
 new_cvm *data;


 
/***************************************************
 * Do some sanity checking                        */
 if(!parent) return NULL;
 if(!cvmapp) return NULL;

 data = (new_cvm *) calloc(1,sizeof(new_cvm));
 data->cvmapp = (CvmApp *) cvmapp;
/*************************************************
 * Create a SLDialog and SLSmartForm            */
  dial = new SLDialog(parent,"newshell", Hctx,True);
  work = dial->workArea();
  ok   = dial->addBottomOK(NOK,NControlTrap,
                    (void *) data);
  cancel = dial->addBottomCancel(NCAN,NControlTrap,
                    (void *) data);
  dial->addBottomHelp();
  data->shell = dial;
  data->work  = work;
  dial->make();

  dial->setTitle("Start A New CVM Model");
  XtAddCallback(work->W(),XmNdestroyCallback,(XtCallbackProc) KillCB,data);

  rc = new SLRowColumn(work,"rc_axes",NULL,True, True, True);
  work->attach(rc,work,NULL,work,NULL,10,10,10,0);

  data->xtext = new SL2Text(rc, "xaxis",0,"XCOORDINATE:", SLpText::_CHAR,16);
  data->ytext = new SL2Text(rc, "yaxis",0,"YCOORDINATE:", SLpText::_CHAR,16);
  data->ztext = new SL2Text(rc, "zaxis",0,"ZCOORDINATE:", SLpText::_CHAR,16);

  data->mtext = new SL2Text(work, "mname",0,"Model Name:", SLpText::_CHAR,0);
  work->attach(data->mtext,work,work,rc,NULL,10,10,10,0);

  data->xtext->setupCvarPoint(data->xname,16);
  data->ytext->setupCvarPoint(data->yname,16);
  data->ztext->setupCvarPoint(data->zname,16);
  data->mtext->setupCvarPoint(data->mname,72);
  strcpy(data->xname,"XBASEMENT");
  strcpy(data->yname,"YBASEMENT");
  strcpy(data->zname,"KILOMETER");
  mod = cvmapp->getModelDesc();
  if(mod->Name())
   strcpy(data->mname,mod->Name());
  else
   strcpy(data->mname,"NONE");

 dial->makeAndManage();
 return dial;
}

static void NControlTrap(void *data, long ident)
{ /* OK = NOK, CANCEL = NCAN  */
 new_cvm *ndata = (new_cvm *) data;
 CvmApp   *cvmapp;
 ModLimits     *mlimits;
 ErsTransforms *tdata;
 ErsTransform  *tx,*ty,*tz;
 char msg[240];
 if(!ndata) return;
 cvmapp = ndata->cvmapp;
 if(!cvmapp) return;
 

 if(ident == NCAN)  // CANCEL
  { 
    goto jump;
  }

 if(ident == NOK ) // OK
  { 
   tdata = cvmapp->cvmGetTransforms();
   tx    = ErsTransGetTran(tdata,ndata->xname);
   ty    = ErsTransGetTran(tdata,ndata->yname);
   tz    = ErsTransGetTran(tdata,ndata->zname);
   if(!tx)
    { strcpy(msg,"xaxes value is invalid"); goto error; }
   if(!ty)
    { strcpy(msg,"yaxes value is invalid"); goto error; }
   if(!tz)
    { strcpy(msg,"zaxes value is invalid"); goto error; }
   mlimits = cvmapp->cvmGetMLimits();
   mlimits_set_trans(mlimits, tx,ty,tz);

   cvmapp->startNewModel(ndata->mname);

  }
 jump:
  ndata->shell->unmanage();
  return;
 error:
  CVMMsgBox(ndata->work->W(),msg);
}

static void KillCB(Widget , new_cvm *dat, caddr_t )
{ /* Close down everything */
 if(dat == NULL) return;
 free(dat);
 return;
}
