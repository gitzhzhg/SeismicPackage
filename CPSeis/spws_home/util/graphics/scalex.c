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
#include "scale.h"
 
#define _NONE    -1
 
static void Kill_scaleCB(Widget W, struct UD_scale *dat, caddr_t b);

/*
 *  Start of new Utility Function 
 *  Window number = 1      */
Widget scale1(int opt, Widget parent,struct UD_scale *udat, long *sid)
{
   Arg    arglist[4];
   Widget form,widget;
   int    i, nobj, oid, otyp;
   extern void EZpop();
   void (*func)();
   UCB    CBpointer[MaxOPerW];
   struct EZwin *Cwin;
   char   Shname[24],str[24];
   long   nmax,nlink, *nent;
   long   atyp[10],  apos[10], *aswc[10];
   long   adec[10],  asiz[10];
   static long zero= 0;
   static long one = 1;
   void   *avar[10], (*(aet[10]))();
   char   *apr[10];
 
   if(udat == NULL) return;
   Cwin = NULL;
   ez_create_win_(&Cwin, sid);
   udat->Cwin = Cwin;
/*
 * Store the window trap address */
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno2",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno3",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno4",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 5   ;  otyp =  TYP_FLOAT;
 nobj=ezregobj_("EZresno5",&oid,&otyp,NULL,&udat->XN1,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno6",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 7   ;  otyp =  TYP_FLOAT;
 nobj=ezregobj_("EZresno7",&oid,&otyp,NULL,&udat->XU1,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno8",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 9   ;  otyp = -TYP_CHAR;
 nobj=ezregobj_("EZresno9",&oid,&otyp,NULL,udat->XNAME,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno10",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 11  ;  otyp =  TYP_FLOAT;
 nobj=ezregobj_("EZresno11",&oid,&otyp,NULL,&udat->XN2,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno12",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 13  ;  otyp =  TYP_FLOAT;
 nobj=ezregobj_("EZresno13",&oid,&otyp,NULL,&udat->XU2,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno14",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 15  ;  otyp =  TYP_FLOAT;
 nobj=ezregobj_("EZresno15",&oid,&otyp,NULL,&udat->YN1,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno16",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 17  ;  otyp =  TYP_FLOAT;
 nobj=ezregobj_("EZresno17",&oid,&otyp,NULL,&udat->YU1,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno18",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 19  ;  otyp = -TYP_CHAR;
 nobj=ezregobj_("EZresno19",&oid,&otyp,NULL,udat->YNAME,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno20",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 21  ;  otyp =  TYP_FLOAT;
 nobj=ezregobj_("EZresno21",&oid,&otyp,NULL,&udat->YN2,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno22",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 23  ;  otyp =  TYP_FLOAT;
 nobj=ezregobj_("EZresno23",&oid,&otyp,NULL,&udat->YU2,NULL,&Cwin);
 
 oid = _NONE;  otyp = TYP_LABEL;
 nobj=ezregobj_("EZresno24",&oid,&otyp,NULL,NULL,NULL,&Cwin);
 
 oid = 25  ;  otyp =  TYP_INT;
 nobj=ezregobj_("EZresno25",&oid,&otyp,NULL,&udat->NTRAN,NULL,&Cwin);
 i=getobjno(&nobj, sid);
 ezadd_eztrap(scale_ntran_,1,sid,Cwin->winobj[i]->eztrap);
 
/*
 * Add a Session Exit & Help pushbuttons if we use a dialog  */
 if(opt == 0)
  { oid = _NONE;  otyp = TYP_PBSE;
    nobj=ezregobj_("SESE",&oid,&otyp,NULL,NULL,NULL,&Cwin);
    i=getobjno(&nobj, sid);
    ezadd_cb(scale_trap,sizeof(struct UD_scale),(void *) udat,
    XmNactivateCallback,&CBpointer[0]);
    Cwin->winobj[i]->Ucb = CBpointer[0];
  }
 if(opt == 0)
  { oid = _NONE;  otyp = TYP_PBHLP;
    nobj=ezregobj_("HELP",&oid,&otyp,NULL,NULL,NULL,&Cwin);
  }

/*
 * Create the EZmotif window      */
   strcpy(Shname,"scale1");
   ezwinbld2_( opt, parent, Cwin,&form,Shname,&udat->Hctx );
   udat->shell = NULL;
   if(opt == 0) udat->shell = XtParent(form);

   return form;
}
 
 
/*************************************
 * Create a new EZX session        ***
 ************************************/
Widget *scale(int opt, Widget P ,char help_file[],
       char help_title[], struct UD_scale *udat)
{ Gwindow *gwin;
  ErsCoordInf *coords;
  extern void scale_init();
  Widget BB[1];
  struct EZwin *CW[1];
  struct EZwlist *SWL;
  struct EZolist *SOL;
  void   *addr[200];
  long   lv;
  long   numses,sid;

 if(udat == NULL) return NULL;
 gwin = udat->gwin;
 if(gwin == NULL) return NULL;
/*
 * Save the session init and the *
 * session end trap addresses    */
 ez_create_ses(&sid,&numses,&SOL,&SWL, help_file,help_title);
 if(SWL==NULL || SOL==NULL)
   { printf("ez_create_ses failed\n");
     return BB;}
 EZsav_sesi(scale_init,&sid);
/*
 * Create the popup windows */
 BB[0] = scale1(opt, P , udat, &sid);
 udat->form = BB[0];
 XtAddCallback(BB[0],XmNdestroyCallback,Kill_scaleCB,udat);
 set_actives(&sid);
 gwin->win.scale = (void *) SWL->Awin[0];
/*
 * Invoke any session init traps */
 scale_init(udat);
 return BB;
}

static void Kill_scaleCB(Widget W, struct UD_scale *dat, caddr_t b)
{ /* Close down everything */
 Gwindow *gwin;
 WinStuff *win;;
 if(dat == NULL) return;
 gwin = dat->gwin;
 win = &gwin->win;
 win->scale =  NULL;
 free(dat);
 return;
}

Widget get_scale_parm_(Widget parent,int opt, struct HELPCTX *Hctx,
       void **data, Gwindow *gwin)
{long i,n;
 Widget wpop;
 Widget *xx ;
 char help_title[40];
 char help_file[120];
 Widget *scale();
 struct UD_scale *udat;

 udat = (struct UD_scale *) calloc(1,sizeof(struct UD_scale));
 *data = (void *) udat;
 udat->gwin = gwin;
 udat->Hctx = Hctx;

 wpop = NULL;
 strcpy(help_file,"Scale_help");
 strcpy(help_title,"HELP FOR: SCALE");
 xx = scale(opt, parent,help_file,help_title,udat);
 wpop = xx[0];
 if(wpop != NULL)
  {if(opt == 0) XtManageChild(wpop);
   return wpop;
  }
 else return NULL;
}
 
void scale_trap(Widget W, struct UD_scale *udat, caddr_t b)
{ErsCoordInf *coords;
 Gwindow *gwin;
 int n;
 long   ntran,ierr,wkid;
 float xn[4],xu[4];
 static XNAME[16],YNAME[16];

 if(udat == NULL) return;
 gwin = udat->gwin;
 if(gwin == NULL) return;
 wkid = gwin->wkstn;
 if(wkid <=0) return;
 gsactive(&wkid);
 gqcntn(&ierr,&ntran);
 coords = &gwin->wincoords[ntran-1];
 if(coords == NULL) return;

 EZget_win(NULL,udat->Cwin,NULL);
 if( !scale_uc(udat) ) return;
 if( !scale_nc(udat) ) return;

 xn[0] = udat->XN1;
 xu[0] = udat->XU1;
 xn[1] = udat->XN2;
 xu[1] = udat->XU2;
 xn[2] = udat->YN1;
 xu[2] = udat->YU1;
 xn[3] = udat->YN2;
 xu[3] = udat->YU2;
 Set_nc(coords,xn);
 Set_uc(coords,xu);

/* Destroy widget only if it is in a dialog shell */
/* It is part of another interface if it is not   */
 if(udat->shell != NULL) XtDestroyWidget(udat->shell);
 return;
}

void scale_init(struct UD_scale *udat)
{ErsCoordInf *coords;
 Gwindow *gwin;
 int n;
 long   ntran,ierr,wkid;
 float xn[4],xu[4];
 static XNAME[16],YNAME[16];

 if(udat == NULL) return;
 gwin = udat->gwin;
 if(gwin == NULL) return;
 wkid = gwin->wkstn;
 if(wkid <=0) return;
 gsactive(&wkid);
 gqcntn(&ierr,&ntran);
 coords = &gwin->wincoords[ntran-1];
 Get_nc(coords,xn);
 Get_uc(coords,xu);

 udat->XN1 = xn[0];
 udat->XU1 = xu[0];
 udat->XN2 = xn[1];
 udat->XU2 = xu[1];
 udat->YN1 = xn[2];
 udat->YU1 = xu[2];
 udat->YN2 = xn[3];
 udat->YU2 = xu[3];
 udat->NTRAN = coords->ntran;
 strcpy(udat->XNAME,"UNKNOWN");
 strcpy(udat->YNAME,"UNKNOWN");
 return;
}

void scale_update(Gwindow *gwin)
{ErsCoordInf *coords;
 struct EZwin *Cwin;
 struct UD_scale udat;
 float  xn[4],xu[4],*fptr;
 long   wkid,ierr,ntran;
 if(gwin == NULL) return;
 Cwin = (struct EZwin *) gwin->win.scale;
 if(Cwin == NULL) return;

 wkid = gwin->wkstn;
 if(wkid <=0) return;
 gqcntn(&ierr,&ntran);
 coords = &gwin->wincoords[ntran-1];
 Get_nc(coords,xn);
 Get_uc(coords,xu);
 udat.XN1 = xn[0];
 udat.XU1 = xu[0];
 udat.XN2 = xn[1];
 udat.XU2 = xu[1];
 udat.YN1 = xn[2];
 udat.YU1 = xu[2];
 udat.YN2 = xn[3];
 udat.YU2 = xu[3];
 fptr =  (float *) Cwin->winobj[3]->udata;
 *fptr = xn[0];
 fptr = (float *) Cwin->winobj[5]->udata;
 *fptr = xu[0];
 fptr = (float *) Cwin->winobj[9]->udata;
 *fptr = xn[1];
 fptr = (float *) Cwin->winobj[11]->udata;
 *fptr = xu[1];
 fptr = (float *) Cwin->winobj[13]->udata;
 *fptr = xn[2];
 fptr = (float *) Cwin->winobj[15]->udata;
 *fptr = xu[2];
 fptr = (float *) Cwin->winobj[19]->udata;
 *fptr = xn[3];
 fptr = (float *) Cwin->winobj[21]->udata;
 *fptr = xu[3];

 EZset_win(NULL, Cwin , NULL);
 return;
}

Boolean scale_uc(struct UD_scale *udat)
{char msg[96];
 if(udat == NULL) return True;
 if(udat->XU1 == udat->XU2)
  { strcpy(msg,"Equal user  X-values is illegal");
    EZwinmsg(udat->Cwin,msg);
    return False; }
 if(udat->YU1 == udat->YU2)
  { strcpy(msg,"Equal user Y-values is illegal");
    EZwinmsg(udat->Cwin,msg);
    return False; }

 return True;
}

Boolean scale_nc(struct UD_scale *udat)
{ Boolean valid;
  char    msg[96];
#define maxv(a,b) ((a) > (b)) ? (a) : (b)
#define minv(a,b) ((a) > (b)) ? (b) : (a)
 valid = True;
 if(udat == NULL) return valid;
 udat->XN1 = minv(udat->XN1,1.0);
 udat->XN2 = minv(udat->XN2,1.0);
 udat->YN1 = minv(udat->YN1,1.0);
 udat->YN2 = minv(udat->YN2,1.0);
 udat->XN1 = maxv(udat->XN1,0.0);
 udat->XN2 = maxv(udat->XN2,0.0);
 udat->YN1 = maxv(udat->YN1,0.0);
 udat->YN2 = maxv(udat->YN2,0.0);
 if(udat->XN1 > udat->XN2) /* xn1 = left, xn2 = right */
  { float temp;
    temp = udat->XN1;
    udat->XN1 = udat->XN2;
    udat->XN2 = temp;
  }
 if(udat->YN1 < udat->YN2) /* yn1 = top, yn2 = bottom */
  { float temp;
    temp = udat->YN1;
    udat->YN1 = udat->YN2;
    udat->YN2 = temp;
  }
 if(udat->XN1 == udat->XN2)
  { strcpy(msg,"Equal viewport X-values is illegal");
    EZwinmsg(udat->Cwin,msg);
    valid =  False; }
 if(udat->YN1 == udat->YN2)
  { strcpy(msg,"Equal viewport Y-values is illegal");
    EZwinmsg(udat->Cwin,msg);
    valid =  False; }

 return valid; 
}

void scale_ntran_(struct UD_scale *udat)
{
 if(udat == NULL) return;
 if(udat->NTRAN < 1) udat->NTRAN = 1;
 if(udat->NTRAN > 9) udat->NTRAN = 9;
}

