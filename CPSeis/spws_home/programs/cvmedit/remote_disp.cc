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

#include "remote_disp.h"
#include "sl/sl_dialog.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_push.hh"
#include "sl/radio_list.hh"
#include "sl_cvm_app.hh"


enum { ROK, RCAN };

/* PROTOTYPES FOR PRIVATE METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void   CVMMsgBox(Widget , char *);

static void KillCB(Widget W, remote_disp *, caddr_t b);
static void ControlTrap(void *data, long ident);

#ifdef __cplusplus
}                   // for C++
#endif


SLDialog *RemoteGUI(Widget parent, CvmApp *cvmapp, HelpCtx Hctx)
{/* Dialog to control remote displays  */
 SLDialog    *dial;
 SLSmartForm *work;
 SLpPush     *ok, *cancel;
 SL2Text     *t1;
 SLpText     *ptext;
 remote_disp *data;
 static char lab[24];


 
/***************************************************
 * Do some sanity checking                        */
 if(!parent) return NULL;
 if(!cvmapp) return NULL;

 data = (remote_disp *) calloc(1,sizeof(remote_disp));
 strcpy(data->display,"NONE");
 data->cvmapp = (CvmApp *) cvmapp;
/*************************************************
 * Create a SLDialog and SLSmartForm            */
  dial = new SLDialog(parent,"Remote", Hctx,True);
  work = dial->workArea();
  ok   = dial->addBottomOK(ROK,ControlTrap,
                    (void *) data);
  cancel = dial->addBottomCancel(RCAN,ControlTrap,
                    (void *) data);
  dial->addBottomHelp();
  data->shell = dial;
  data->work  = work;
  dial->make();

  dial->setTitle("Remote Display Selection");
  XtAddCallback(work->W(),XmNdestroyCallback,(XtCallbackProc) KillCB,data);

  strcpy(lab," Display Name:");
  t1 = new SL2Text(work, "disp",0,lab,
                    SLpText::_CHAR,31);
  work->attach(t1,work,work,work,NULL,10,10,10,0);
  data->text = t1;

  ptext = t1->getSLpText();
  t1->setupCvarPoint(data->display,31);


 dial->makeAndManage();

 return dial;
}

static void ControlTrap(void *data, long ident)
{ /* OK = ROK, CANCEL = RCAN  */
 remote_disp *disp = (remote_disp *) data;
 CvmApp   *cvmapp;
 char msg[240];
 if(!disp) return;
 cvmapp = disp->cvmapp;
 if(!cvmapp) return;

 if(ident == RCAN)  // CANCEL
  { 
    goto jump;
  }

 if(ident == ROK ) // OK
  { char *dname;
    void *ptr;

    dname = disp->display;
    if(strlen(dname)==0) goto jump;
    if(strcmp(dname,"NONE")==0) goto jump;
    cvmapp->setMessage("Wait: creating remote display");
    ptr = (void *) cvmapp->addDisp(dname);
    if(!ptr)
     { sprintf(msg,"Could not open %s",dname);
       goto error;
     }
    cvmapp->setMessage("");

  }
 jump:
  disp->shell->unmanage();
  return;
 error:
  CVMMsgBox(disp->work->W(),msg);
}

static void KillCB(Widget , remote_disp *dat, caddr_t )
{ /* Close down everything */
 if(dat == NULL) return;
 dat->cvmapp->setRemote(NULL);
 free(dat);
 return;
}
