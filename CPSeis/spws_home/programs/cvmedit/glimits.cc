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

#include "glimits1.h"
#include "sl/sl_dialog.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_push.hh"
#include "sl/radio_list.hh"
#include "sl/sl_row_column.hh"
#include "sl_cvm_app.hh"


enum { LOK, LCAN };

/* PROTOTYPES FOR PRIVATE METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void   CVMMsgBox(Widget , char *);

static void KillCB(Widget W, Glimits *, caddr_t b);
static void ControlTrap(void *data, long ident);

#ifdef __cplusplus
}                   // for C++
#endif

Glimits *glimitsGui(SLDelay *slparent, int opt, CvmApp *cvmapp, HelpCtx Hctx)
{/* Dialog to control model limits  */
 Glimits      *data;
 SLDialog     *dial;
 SLSmartForm  *work;
 SLpPush      *ok, *cancel;


 
/***************************************************
 * Do some sanity checking                        */
 if(!slparent) return NULL;
 if(!cvmapp) return NULL;

 data = (Glimits *) calloc(1,sizeof(Glimits));
 data->cvmapp = (CvmApp *) cvmapp;
/*************************************************
 * Create a SLDialog and SLSmartForm            */
  dial = NULL;
  work = NULL;
  if(opt == 0)
   {
    dial = new SLDialog(slparent,"glimshell", Hctx,True);
    work = dial->workArea();
    ok   = dial->addBottomOK(LOK,ControlTrap,
                      (void *) data);
    cancel = dial->addBottomCancel(LCAN,ControlTrap,
                      (void *) data);
    dial->addBottomHelp();
    data->shell = dial;
    dial->setTitle("Grid Limits Selection");
    dial->make();
   }
  else
   { work = new SLSmartForm(slparent,"work",Hctx,False,True,True);
   }
  data->form  = work;
  data->Hctx  = Hctx;
  data->cvmapp= (CvmApp *) cvmapp;


  SLRowColumn *rc1;
  rc1 = new SLRowColumn(work,"limits",NULL,True, True, True);
  data->x1 = new SL2Text(rc1, "nxg",0," NX:", SLpText::_LONG,8);
  data->x1->setupIvarPoint((long *) &data->nxg);
  data->x2 = new SL2Text(rc1, "xorg",0," X-Origin:", SLpText::_FLOAT,12);
  data->x2->setupFvarPoint(&data->xorg);
  data->x3 = new SL2Text(rc1, "xinc",0," Delta-X:", SLpText::_FLOAT,12);
  data->x3->setupFvarPoint(&data->dx);

  data->y1 = new SL2Text(rc1, "nyg",0," NY:", SLpText::_LONG,8);
  data->y1->setupIvarPoint((long *) &data->nyg);
  data->y2 = new SL2Text(rc1, "yorg",0," Y-Origin:", SLpText::_FLOAT,12);
  data->y2->setupFvarPoint(&data->yorg);
  data->y3 = new SL2Text(rc1, "yinc",0," Delta-Y:", SLpText::_FLOAT,12);
  data->y3->setupFvarPoint(&data->dy);

  data->z1 = new SL2Text(rc1, "nzg",0," NZ:", SLpText::_LONG,8);
  data->z1->setupIvarPoint((long *) &data->nzg);
  data->z2 = new SL2Text(rc1, "zorg",0," Z-Origin:", SLpText::_FLOAT,12);
  data->z2->setupFvarPoint(&data->zorg);
  data->z3 = new SL2Text(rc1, "zinc",0," Delta-Z:", SLpText::_FLOAT,12);
  data->z3->setupFvarPoint(&data->dz);

  work->attach(rc1,work,work,work,work,4,4,4,4);


 if(dial)
  dial->makeAndManage();
 else
  work->makeAndManage();
 XtAddCallback(work->W(),XmNdestroyCallback,(XtCallbackProc) KillCB,data);
 glimitsInit((void *) data);

 return data;
}

static void ControlTrap(void *data, long ident)
{ /* OK = LOK, CANCEL = LCAN  */
 Glimits *udat = (Glimits *) data;


 if(!udat) return;

 if(ident == LCAN)  // CANCEL
  { 
    goto jump;
  }

 if(ident == LOK ) // OK
  {
    glimitsTrap(NULL, udat, NULL);
  }
 jump:
  if(udat->shell) udat->shell->unmanage();
  return;


}

void glimitsDelete(Glimits *dat)
{
 if(!dat) return;
 if(dat->shell) delete dat->shell;
 else delete dat->form;
}

static void KillCB(Widget , Glimits *dat, caddr_t )
{ /* Close down everything */
 if(dat == NULL) return;
 free(dat);
 return;
}

void glimitsGuiUpdate(void *data, GridLimits *globj)
{// Refresh GUI with values from GridLimits
 Glimits *udat = (Glimits *) data;
 ErsTransform *t1,*t2,*t3;
 if(!udat) return;

 if(globj) glimits_get(globj,
    &udat->nzg, &udat->zorg, &udat->dz,
    &udat->nxg, &udat->xorg, &udat->dx,
    &udat->nyg, &udat->yorg, &udat->dy,
    &t1, &t2, &t3 );

 udat->x1->update();
 udat->x2->update();
 udat->x3->update();
 udat->y1->update();
 udat->y2->update();
 udat->y3->update();
 udat->z1->update();
 udat->z2->update();
 udat->z3->update();

}


void glimitsTrap(Widget , Glimits *udat, caddr_t )
{
 CvmApp       *cvmapp;
 GridLimits   *glimits;
 ErsTransform *t1,*t2,*t3;

 if(!udat) return;
 cvmapp = (CvmApp *) udat->cvmapp;
 if(!cvmapp) return;
 glimits = cvmapp->cvmGetGLimits();
 if(!glimits) return;
 glimits_get_trans(glimits,&t1,&t2,&t3);

// Save GUI variables to the GridLimits structure.
 glimits_set(glimits,
     &udat->nzg, &udat->zorg, &udat->dz,
     &udat->nxg, &udat->xorg, &udat->dx,
     &udat->nyg, &udat->yorg, &udat->dy,
     t1, t2, t3 );

 return;
}


void glimitsInit(void *opaque)
{/* Make variable consistent with ModelDesc structure */
 Glimits *udat = (Glimits *) opaque;
 CvmApp       *cvmapp;
 ModLimits    *mlimits;
 GridLimits   *globj;
 float         dx=1.0,dy=1.0,dz=1.0,ox=0.0,oy=0.0,oz=0.0;
 int           nx=2,ny=2,nz=2;
 ErsTransform *t1,*t2,*t3;
 float         XMAX,YMAX,ZMAX;

 if(!udat) return;
 cvmapp  = (CvmApp *) udat->cvmapp;
 mlimits = cvmapp->cvmGetMLimits();
 globj   = cvmapp->cvmGetGLimits();
 if(mlimits)
   { mlimits_get(mlimits,&ox,&XMAX,
    &oz, &ZMAX, &oy, &YMAX, &t2, &t3, &t1 );
    dx = (XMAX-ox)/(nx-1);
    dy = (YMAX-oy)/(ny-1);
    dz = (ZMAX-oz)/(nz-1);
   }
 if(globj)
  {glimits_get(globj,
   &nz, &oz, &dz, &nx, &ox, &dx, &ny, &oy, &dy, &t1, &t2, &t3 );
  }
 if(nx < 1) nx = 1;
 if(ny < 1) ny = 1;
 if(nz < 1) nz = 1;
 if(dx == 0.0) dx = 1.0;
 if(dy == 0.0) dy = 1.0;
 if(dz == 0.0) dz = 1.0;

 glimitsSvals(udat,nx,dx,ox,ny,dy,oy,nz,dz,oz);
}

void glimitsGvals(Glimits *gdat, int  *nx, float *dx, float *ox,
                  int  *ny, float *dy, float *oy,
                  int  *nz, float *dz, float *oz)
{if(!gdat) return;
 *nx = gdat->nxg;
 *ny = gdat->nyg;
 *nz = gdat->nzg;
 *dx = gdat->dx;
 *dy = gdat->dy;
 *dz = gdat->dz;
 *ox = gdat->xorg;
 *oy = gdat->yorg;
 *oz = gdat->zorg;
}

void glimitsSvals(Glimits *gdat, int  nx, float dx, float ox,
                  int  ny, float dy, float oy,
                  int  nz, float dz, float oz)
{if(!gdat) return;
 gdat->nxg = nx;
 gdat->nyg = ny;
 gdat->nzg = nz;
 gdat->dx  = dx;
 gdat->dy  = dy;
 gdat->dz  = dz;
 gdat->xorg = ox;
 gdat->yorg = oy;
 gdat->zorg = oz;
 gdat->x1->update();
 gdat->x2->update();
 gdat->x3->update();
 gdat->y1->update();
 gdat->y2->update();
 gdat->y3->update();
 gdat->z1->update();
 gdat->z2->update();
 gdat->z3->update();
}


SLDialog *glimitsDial(void *data)
{ Glimits *dat = (Glimits *) data;
 if(dat == NULL) return NULL;
 return dat->shell;
}

SLSmartForm *glimitsForm(void *data)
{ Glimits *dat = (Glimits *) data;
 if(dat == NULL) return NULL;
 return dat->form;
}
