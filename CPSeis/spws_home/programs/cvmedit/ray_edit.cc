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
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wproc.h"
#include "cprim.h"
#include "vect/ll_vector.hh"
#include "vect/ll_seis_vect.hh"
#include "vl_data.hh"
#include "vec_list_util.hh"
#include "oprim/ll_base_data.hh"
#include "oprim/modbase_data.hh"
#include "sl/slp_push.hh"
#include "sl/sl2_text.hh"
#include "sl/sl_row_column.hh"
#include "sl/radio_list.hh"
#include "gridcvm.h"
#include "sp/seis_plot.hh"
#include "sl_cvm_app.hh"
#include "pick_point.hh"
#include "model_desc.hh"
#include "ray_edit.hh"


#ifdef CRAY
#define cellcalc_   CELL
#define cellpnt_w_  CELLPNT_W
#define cellwher_w_ CELLWHER_W
#define cellpoly_w_ CELLPOLY_W
#endif
#if (VMS || _AIX || hpux)
#define cellcalc_   cell
#define cellpnt_w_  cellpnt_w
#define cellwher_w_ cellwher_w
#define cellpoly_w_ cellpoly_w
#endif
#if ( ultrix || sun || __sgi)
#define cellcalc_ cell_
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
extern void cellcalc_(float *,float *,float *,float *,int  *,int  *,
       int  *,float *,float *, int  *, int  *, int  *, int  *,int  *,
       float *,float *,int  *, float *, int  *);
extern void cellwher_w_(float *, float *, int  *,int  *,float *,
       float *, float *,int  *, int  *,int  *,float *, float *,int  *);
extern void cellpnt_w_(float *, float *, int  *, float *, float *,
       int  *,int  *);
extern void cellpoly_w_(float *, float *, int  *, float *, float *,
       int  *,int  *);

int    ray_trace_(int *mt_ray,int *nt_ray,float *dt_ray,float *a_ray,
        float *xray,float *y_ray,float *zray,float *t_ray,float *x_min,
        float *x_max,float *z_min,float *z_max,int *is,
        int *nx_vel,float *x0_vel,float *dx_vel,int *nz_vel,
        float *z0_vel,float *dz_vel,float *vel);
int    ray_dvdx_(float *v0,float *dvdx,float *dvdz,float *x,float *z,
        int *nx_vel,float *x0_vel,float *dx_vel,int *nz_vel,
        float *z0_vel, float *dz_vel, float *vel);

#ifdef __cplusplus
}                   // for C++
#endif

ModelDesc *zot_time_to_depth(char *czo, ModelDesc *modi);


RayEdit::RayEdit (SLDelay *slparent, char *name,void *cvm,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now, long ident)
         : SLSmartForm(slparent,name,Hctx,doframe,False,manage_now),
     _ident(ident)
{ _cvmapp = (CvmApp *) cvm;
  assert(cvm);
  _rays = 0;
  if(slparent->made() && make_if_can) make();
  setup();
}

RayEdit::RayEdit (Widget parent, char *name,void *cvm,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_now, Boolean   manage_now, long ident)
         : SLSmartForm(parent,name,Hctx,doframe,False,manage_now),
     _ident(ident)
{ _cvmapp = (CvmApp *) cvm;
  assert(cvm);
  _rays = 0;
  _slow = 0;
  _mode = FanMode;
  if(make_now) make();
  setup();
}

RayEdit::~RayEdit(void)
{SeisPlot *sp;
 sp = (SeisPlot *) _cvmapp->cvm_get_seisplot(_cvmapp);
 if(_rays) KillVectorList((void *) _rays, sp);
 _rays = 0;
 _slow = 0;
  _mode = FanMode;
 delete _pp;
}

void RayEdit::setup()
{// Initialize & retain starting information.
 SeisPlot *sp;
 GridLimits *glimits;
 ModLimits  *mlimits;
 ErsTransform *t1,*t2,*t3;
 ErsTransform *tx,*ty,*tz;
 float       xmin,xmax,zmin,zmax,ymin,ymax;
 float       d1,d2,d3,o1,o2,o3;
 int         n1,n2,n3;
 assert(_cvmapp);

 sp = (SeisPlot *) _cvmapp->cvm_get_seisplot(_cvmapp);
 glimits = _cvmapp->cvmGetGLimits();
 glimits_get(glimits,
     &n1, &o1, &d1, &n2, &o2, &d2, &n3, &o3, &d3,
     &t1, &t2, &t3 );
 mlimits = _cvmapp->cvmGetMLimits();
 mlimits_get(mlimits,&xmin,&xmax,&zmin,&zmax,&ymin,&ymax,&tx,&ty,&tz);

/* Starting point of ray(s) */
 _xs = 0.5*(xmin+xmax);
 _ys = 0.5*(ymin+ymax);
 _zs = o1;

/* Initial specification of angles */
 _angini = 90.0;
 _numang = 1;
 _delang = 1.0;
 _pp = new PickPoint(sp,&_zs,&_xs,&_ys);

}


Widget RayEdit::make(Widget/* parent*/)
{SL2Text *a0,*na,*da;
 SL2Text *x1,*y1,*z1;
 if(!made())
  {
   Widget wid = SLSmartForm::make();
   /******************************************
    * Create RowColumns here               */
    _form = this;
    SLRowColumn *rc1,*rc2;
    rc1 = new SLRowColumn(_form,"alimits",NULL,True, True, True);
    a0 = new SL2Text(rc1, "angini",0,"Start Angle:", SLpText::_FLOAT,12);
    na = new SL2Text(rc1, "numang",0,"Number Rays:", SLpText::_LONG,6);
    da = new SL2Text(rc1, "angdel",0,"Angle delta:", SLpText::_FLOAT,12);
    x1 = new SL2Text(rc1, "xstart",0," Starting X:", SLpText::_FLOAT,12);
    y1 = new SL2Text(rc1, "ystart",0," Starting Y:", SLpText::_FLOAT,12);
    z1 = new SL2Text(rc1, "zstart",0," Starting Z:", SLpText::_FLOAT,12);
    a0->setupFvarPoint(&_angini);
    na->setupIvarPoint(&_numang);
    da->setupFvarPoint(&_delang);
    x1->setupFvarPoint(&_xs);
    y1->setupFvarPoint(&_ys);
    z1->setupFvarPoint(&_zs);

    rc2 = new SLRowColumn(_form,"rymode",NULL,True, True, True);
    _radio1 = new RadioList();
    _radio1->addRadio(rc2, "rad1", FanMode,"Fan Shooting");
    _radio1->addRadio(rc2, "rad2", HorizonMode,"Horizon Shooting");
    _radio1->setIvar(FanMode);
    _radio1->setItrap(radioTrap, (void *) this);
 

    _form->attach(rc1,this,this,this,0,4,4,4,4);
    _form->attach(rc2,this,this,rc1,this,4,4,4,4);

  }
 makeChildren();
 return topWidget();
}


Vector *RayEdit::cell_n_vector(VectorLinkedList *list, int n)
{// return n'th vector where n starts counting at 1.
 Vector *v=NULL;
 void *p;
 int m=1;
 if(list==NULL) return v;
 v = list->top(&p);
 while(v != NULL )
  { if(m == n) return v;
    m++;
    v = list->next(&p);
  }
 return NULL;
}

Vector *RayEdit::closest(float x1, float x2, float /*x3*/)
{ SeisPlot *sp;
  SeisVectLinkedList *vl;
  int p1,p2,p3=0;

  if(!_cvmapp) return 0;
  sp = (SeisPlot *) _cvmapp->cvm_get_seisplot(_cvmapp);
  vl    = (SeisVectLinkedList *) _cvmapp->cvm_get_vll(CvmAppBase::Structure);
  if(!sp || !vl) return 0;
  p2 = (int) sp->xPixel(x2);
  p1 = (int) sp->yPixel(x1);
  return vl->closest(p2,p1,sp);
}



//------------------------ traps -------------------------//


void RayEdit::compTrap(void *data,long )
{ RayEdit      *redit = (RayEdit *) data;
  CvmApp       *cvmapp=NULL;
  SeisPlot     *splot;
  SeisVectLinkedList *vl=0;
  gridcvm      *gpop;
  gridData     *gdpntr;

  char          msg[120];
  int           istat;
 int       dbgtes=0;

  if(!redit) return;
// Check status of available data.
  cvmapp = (CvmApp *) redit->_cvmapp;
  if(!cvmapp) return;
  splot = (SeisPlot *) cvmapp->cvm_get_seisplot(cvmapp);
  vl    = (SeisVectLinkedList *) cvmapp->cvm_get_vll(CvmAppBase::Structure);
  if(splot == NULL || cvmapp->getModelDesc() == NULL) 
   {sprintf(msg,"RayEdit::compTrap: Seisplot or ModelDesc is null\n");
    goto jump;
   }

// Get the gridData to see if we can trace rays.
 gpop = (gridcvm *) cvmapp->get_gpop();
 gdpntr = gridDataPntr(gpop);
 if(gdpntr==0)
  {sprintf(msg,"RayEdit::compTrap: gridData is null\n");
   goto jump;
  }
 if(dbgtes)
   {
    ModelDesc *out;
    out = zot_time_to_depth("DEPTH",cvmapp->getModelDesc());
   }

// Compute the new ray paths.
 cvmapp->setMessage("Wait: computing ray paths");
 if(redit->_mode==RayEdit::FanMode)
  istat = redit->compute(gdpntr);
 else if(redit->_mode==RayEdit::HorizonMode)
  istat = redit->shootHorizon(gdpntr);
 cvmapp->setMessage("");
 if(istat == 0)
  {sprintf(msg,"compTrap: call to compute failed, istat=%d\n",istat);
   goto jump;
  }


 return;
jump:
 printf("%s",msg);
}

// n1,n2,n3 should correspond to z,x,y respectively
int RayEdit::compute(gridData *gdpntr)
{
 ErsTransforms *tdata;
 ModLimits    *mlimits;
 SeisPlot     *sp;
 ModBaseData  *dobj;
 Vector       *vector=NULL;
 ErsTransform *tx,*ty,*tz;
 ErsTransform *txbase, *tybase, *tdepth;
 
 float     xs,ys,zs;
 int       count;
 int       i,is=0,mtr=1000;
 float    *xray,*yray,*zray,tray;
 int       phd,shd,thd,zeit;

// Obtain the physical grid description
// temporarily invert velocity to obtain slowness.
 if(physicalGrid(gdpntr)==0) return 0;

// Get the model coordinate description.
 tdata   = _cvmapp->cvmGetTransforms();
 mlimits = _cvmapp->cvmGetMLimits();



 mlimits_get_trans(mlimits,&tx,&ty,&tz);
 phd = (int) transform_gethdr(tx);
 shd = (int) transform_gethdr(ty);
 zeit= (int) transform_gethdr(tz);
 thd = NILKEY;

// convert start point to physical units
// i.e. XBASEMENT, YBASEMENT, DEPTH units
// velocity is assumed to be in these units!!
 xs = _xs;
 ys = _ys;
 zs = _zs;
 txbase = ErsTransGetTran(tdata,"XBASEMENT");
 tybase = ErsTransGetTran(tdata,"YBASEMENT");
 tdepth = ErsTransGetTran(tdata,"DEPTH");
 float sc,io,oo;
 transform_coeff(tx,txbase,&sc,&io,&oo);
 xs = oo + sc*(xs -io);
 transform_coeff(ty,tybase,&sc,&io,&oo);
 ys = oo + sc*(ys -io);
 transform_coeff(tz,tdepth,&sc,&io,&oo);
 zs = oo + sc*(zs -io);

// allocate arrays to hold ray coordinates.
 xray = new float[mtr];
 yray = new float[mtr];
 zray = new float[mtr];
 for(i=0;i<mtr;i++) yray[i] = _ymin;

 sp = (SeisPlot *) _cvmapp->cvm_get_seisplot(_cvmapp);
 if(_rays) KillVectorList(_rays,sp); // destroy old list
 _rays = new SeisVectLinkedList();

// Shoot rays from a point source
// compute rays for all requested takeoff angles.
  char rlab[16];
 for(int n=0;n<_numang;n++)
  { float angle;
    angle = _angini + n*_delang;      /* in degrees */
    angle = (angle/180.0) *3.1415926; /* now in radians */
    count = 0;
    xray[0] = xs;
    yray[0] = ys;
    zray[0] = zs;
    i = ray_trace_( &mtr,&count,&_taustep,&angle,
        xray,yray,zray,&tray,&_xmin,
        &_xmax,&_zmin,&_zmax,&is,
        (int *) &_n2,&_o2,&_d2,(int *) &_n1,&_o1,&_d1,_slow);
    transform_mcnvrt(txbase,xray,count,tx);
//    transform_mcnvrt(tybase,yray,count,tx);
    transform_mcnvrt(tdepth,zray,count,tz);
    dobj = new ModBaseData(count,phd,xray,shd,yray,thd,0,zeit,zray,0);
    dobj->setid(n);
    sprintf(rlab,"ray%d",n);
    vector = _rays->add(rlab,dobj,"red",1,False,
          Vector::SolidLine, Vector::NoMarker,9,1);
    vector->makeVisible();

  }

 _rays->addPlot(sp);

// reset the velocity which was inverted for ray shooting
 if(_slow)
  for(i=0;i<_n1*_n2;i++) _slow[i] =1.0/_slow[i];
 _slow=0;
 delete []xray;
 delete []yray;
 delete []zray;
 return 1;
}

int RayEdit::physicalGrid(gridData *gdpntr)
{
 ErsTransforms *tdata;
 ErsTransform *txbase, *tybase, *tdepth;
 ErsTransform *t1,*t2,*t3;
 int      i;
 float    t, *vel;

// Get the grid data and coordinate description.
 if(gdpntr==0) return 0;
 vel = (float *) gdpntr->getGridData();
 if(vel==0 || strcmp(gdpntr->getWord(),"BYTE") == 0) return 0;
 gdpntr->getGridVals(&_n1,&_o1,&_d1,
               &_n2,&_o2,&_d2,&_n3,&_o3,&_d3);
 gdpntr->getGridCoords(&t1,&t2,&t3);

 _xmin = _o2; _xmax = _o2 + (_n2-1)*_d2;
 if(_xmax < _xmin)
  {t = _xmin; _xmin = _xmax; _xmax = t;}
 _ymin = _o3; _ymax = _o3 + (_n3-1)*_d3;
 if(_ymax < _ymin)
  {t = _ymin; _ymin = _ymax; _ymax = t;}
 _zmin = _o1; _zmax = _o1 + (_n1-1)*_d1;
 if(_zmax < _zmin)
  {t = _zmin; _zmin = _zmax; _zmax = t;}

 tdata   = _cvmapp->cvmGetTransforms();
 

// convert grid limits and grid data to physical units.
// i.e. XBASEMENT, YBASEMENT, DEPTH units
// velocity is assumed to be in these units!!
 txbase = ErsTransGetTran(tdata,"XBASEMENT");
 tybase = ErsTransGetTran(tdata,"YBASEMENT");
 tdepth = ErsTransGetTran(tdata,"DEPTH");
 float sc,io,oo;

 transform_coeff(t2,txbase,&sc,&io,&oo);
 _xmax = oo + sc*(_xmax -io);
 _xmin = oo + sc*(_xmin -io);
 _o2 = oo + sc*(_o2 -io);
 _d2 = sc*_d2;
 if(_xmax < _xmin)
  {t = _xmin; _xmin = _xmax; _xmax = t;}

 transform_coeff(t3,tybase,&sc,&io,&oo);
 _ymax = oo + sc*(_ymax -io);
 _ymin = oo + sc*(_ymin -io);
 _o3 = oo + sc*(_o3 -io);
 _d3 = sc*_d3;
 if(_ymax < _ymin)
  {t = _ymin; _ymin = _ymax; _ymax = t;}

 transform_coeff(t1,tdepth,&sc,&io,&oo);
 _zmax = oo + sc*(_zmax -io);
 _zmin = oo + sc*(_zmin -io);
 _o1 = oo + sc*(_o1 -io);
 _d1 = sc*_d1;
 if(_zmax < _zmin)
  {t = _zmin; _zmin = _zmax; _zmax = t;}

 _slow = vel;
 for(i=0;i<_n1*_n2;i++)
  {if(vel[i]==0.0) vel[i]= 1500;
   _slow[i]=1.0/vel[i];
  }
 _taustep = 0.2*_d1*_slow[0];

return 1;
}


int RayEdit::shootHorizon(gridData *gdpntr)
{
 SeisPlot     *sp;
 ErsTransforms *tdata;
 ModLimits    *mlimits;
 Vector       *vc;
 ModBaseData  *dobj;
 ErsTransform *t1,*t2,*t3;
 ErsTransform *tx,*ty,*tz;
 ErsTransform *txbase, *tybase, *tdepth;
 int       i,n,is=0,mtr=1000;
 float    *xray,*yray,*zray,tray;
 float    *hc1=0,*hc2=0,*hc3=0;
 int       N,phd,shd,thd,zeit;
 int       count;
 char      rlab[16],x1name[16],x2name[16],x3name[16];

// Obtain closest horizon and get coordinates
 vc = closest(_zs,_xs,_ys);
 if(!vc || !gdpntr) return 0;
 dobj = (ModBaseData *) vc->getData();
 phd = (int) dobj->getphdr();
 shd = (int) dobj->getshdr();
 thd = (int) dobj->getthdr();
 zeit= (int) dobj->getzeit();
 N   = dobj->getNumPts();
// Allocate new horizon coordinate arrays.
 hc1 = new float[N];
 hc2 = new float[N];
 hc3 = new float[N];
 dobj->getNx(0,N,hc2);
 dobj->getNs(0,N,hc3);
 dobj->getNy(0,N,hc1);
// Transform horizon coordinates to physical scale
 tdata   = _cvmapp->cvmGetTransforms();
 txbase = ErsTransGetTran(tdata,"XBASEMENT");
 tybase = ErsTransGetTran(tdata,"YBASEMENT");
 tdepth = ErsTransGetTran(tdata,"DEPTH");
 ErsTransName(zeit,x1name);
 ErsTransName(phd,x2name);
 ErsTransName(shd,x3name);
 t1 = ErsTransGetTran(tdata,x1name); 
 t2 = ErsTransGetTran(tdata,x2name);
 t3 = ErsTransGetTran(tdata,x3name);
 transform_mcnvrt(t1,hc1,N,tdepth);
 transform_mcnvrt(t2,hc2,N,txbase);
 transform_mcnvrt(t3,hc3,N,tybase);

 mlimits = _cvmapp->cvmGetMLimits();
 mlimits_get_trans(mlimits,&tx,&ty,&tz);
 phd = (int) transform_gethdr(tx);
 shd = (int) transform_gethdr(ty);
 zeit= (int) transform_gethdr(tz);
 thd = NILKEY;

// Allocate arrays to hold 1 ray
 xray = new float[mtr];
 yray = new float[mtr];
 zray = new float[mtr];
 for(i=0;i<mtr;i++) yray[i]=0.;

// temporarily invert velocity to obtain slowness.
 physicalGrid(gdpntr);

 sp = (SeisPlot *) _cvmapp->cvm_get_seisplot(_cvmapp);
 if(_rays) KillVectorList(_rays,sp); // destroy old list
 _rays = new SeisVectLinkedList();

 float theta,xm,ym,tm,sl;
 for(n=1;n<N;n++)
   {
    xm = 0.5*(hc2[n]+hc2[n-1]);
    ym = 0.5*(hc3[n]+hc3[n-1]);
    tm = 0.5*(hc1[n]+hc1[n-1]);
    if(hc2[n]==hc2[n-1])
     theta = -1.5707963;
    else
     {
      sl = (hc1[n]-hc1[n-1])/(hc2[n]-hc2[n-1]);
      theta = atan(sl)-1.5707963 ;
     }
    xray[0] = xm;
    yray[0] = ym;
    zray[0] = tm;
    count = 0;
    i = ray_trace_( &mtr,&count,&_taustep,&theta,
        xray,yray,zray,&tray,&_xmin,
        &_xmax,&_zmin,&_zmax,&is,
        (int *) &_n2,&_o2,&_d2,(int *) &_n1,&_o1,&_d1,_slow);
    transform_mcnvrt(txbase,xray,count,tx);
//    transform_mcnvrt(tybase,yray,count,tx);
    transform_mcnvrt(tdepth,zray,count,tz);
    dobj = new ModBaseData(count,phd,xray,shd,yray,thd,0,zeit,zray,0);
    dobj->setid(n);
    sprintf(rlab,"ray%d",n);
    vc = _rays->add(rlab,dobj,"red",1,False,
          Vector::SolidLine, Vector::NoMarker,9,1);
    vc->makeVisible();
   }

 _rays->addPlot(sp);

/*
 */
 
// reset the velocity which was inverted for ray shooting
 if(_slow)
  for(i=0;i<_n1*_n2;i++) _slow[i] =1.0/_slow[i];
 _slow = 0;
 delete []hc1;
 delete []hc2;
 delete []hc3;
 delete []xray;
 delete []yray;
 delete []zray;
 return 1;
}

// data, ident, oldv, newv
void RayEdit::radioTrap(void *data,long ,long /*oldv */,long newv)
{ RayEdit *redit = (RayEdit *) data;

  if(redit==NULL) return;
  redit->_radio1->setIvar(newv);
  if(newv == RayEdit::FanMode)
   {redit->_mode = RayEdit::FanMode; }
  else if(newv ==RayEdit::HorizonMode)
   {redit->_mode = RayEdit::HorizonMode; }
  else redit->_mode = 0;

}
//--------------------- end --------------------//


//------------------- ray_edit.cc -------------------//
//       implementation file for the RayEditPop class
//              derived from the SLDialog class

//------------- constructors and destructor --------------//

RayEditPop::RayEditPop (SLDelay *slparent, char *name, HelpCtx Hctx,
         void *cvm)
        : SLDialog(slparent, name, Hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *cc;
  SLpPush     *rem = addBottomRemove(REMOVE);
                    addBottomHelp();
//  remove->unmanageShellWhenPressed(this);
  setTitle("Ray Trace Control");
  cc = new SLpPush( work, "raypb",0,"Compute Rays");
  work->attach(cc, work, work, NULL, work,4,4,0,4);
  _redit = new RayEdit(work,"raydb", cvm, Hctx, False, True, True );
  work->attach(_redit, work, work, work, cc,4,4,0,4);
  cc->setAtrap(_redit->compTrap, _redit); //static member function
  _slform = work;
}

RayEditPop::RayEditPop (Widget parent, char *name, HelpCtx Hctx,
         void *cvm)
        : SLDialog(parent, name, Hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *cc;
  SLpPush     *rem = addBottomRemove(REMOVE);
                    addBottomHelp();
  setTitle("Ray Trace Control");
  cc = new SLpPush( work, "raypb",0,"Compute Rays");
  work->attach(cc, work, work, NULL, work,4,4,0,4);
  _redit = new RayEdit(work,"raydb", cvm, Hctx, False, True, True);
  cc->setAtrap(_redit->compTrap, _redit); 
  work->attach(_redit, work, work, work, cc,4,4,0,4);
  _slform = work;
}


RayEditPop::~RayEditPop(void)
{// RayEdit is deleted automatically by its parent
 //if(_redit != NULL) delete _redit;
}

Boolean RayEditPop::removeNotify()
{SeisPlot *sp = 0;
 if(_redit)
  {
   sp = (SeisPlot *) _redit->_cvmapp->cvm_get_seisplot(_redit->_cvmapp);
   if(_redit->_rays) KillVectorList((void *) _redit->_rays, sp);
   _redit->_rays=0;
   return True;
  }
  else return False;
}

Widget  RayEditPop::Form()
{if(_slform) return _slform->W();
 return NULL;
}
//--------------- end ----------------------//



#define Min(a,b) ((a) < (b) ? (a) : (b))
#define Max(a,b) ((a) > (b) ? (a) : (b))

int ray_trace_(int *mt_ray,int *nt_ray,float *dt_ray,float *a_ray, 
	float *x_ray,float *y_ray,float *z_ray,float *t_ray,float *x_min,
        float *x_max,float *z_min,float *z_max,int *is, 
	int *nx_vel,float *x0_vel,float *dx_vel,int *nz_vel,
        float *z0_vel,float *dz_vel,float *vel)
{
    /* System generated locals */
    int  i__1;

    /* Local variables */
    static float dvdx, dvdz, scale, r1, r2;
    static float v0, x1, z1;
    static int  it;
    static float dx, dz, px, pz, rx, rz;

/*******************************************************************
  trace a ray mt_ray time steps through a gridded slowness model 
  inputs 
  mt_ray = maximum number of time steps to ray trace - dimension of x_ray
,z_ray
  nt_ray = number of time steps in ray 
 dt_ray = ray time step - choose dt_ray so you get ~ 5 steps in a veloci
ty cell
  a_ray  = initial starting angle in radians 0 = + x axis pi/2 = + zaxis
 
  x_ray  = array for ray x locations - dimensioned (mt_ray) 
  y_ray  = array for ray x locations - dimensioned (mt_ray) 
  z_ray  = array for ray z locations - dimensioned (mt_ray) 
  t_ray  = final ray travel time 
  x_min  = minimum box x value to ray trace through 
  x_max  = maximum box x value to ray trace through 
  z_min  = minimum box z value to ray trace through 
  z_max  = maximum box z value to ray trace through 
  is     = flag to indicate which box edge a ray exits through 
  nx_vel = number of velocity grid nodes in the x direction 
  x0_vel = minimum velocity grid node x value 
  dx_vel = velocity grid node x value spacing 
  nz_vel = number of velocity grid nodes in the z direction 
  z0_vel = minimum velocity grid node z value 
  dz_vel = velocity grid node z value spacing 
  vel    = slowness array - dimensioned (nz_vel,nx_vel) 

  Notes
 

  velocity is interpolated between nodes via bilinear interpolation 

  ray tracing is stopped when the ray leaves a box defined by 
  x_min,x_max,z_min,z_max 

      if ray x end is < x_min is = 1 
  elseif ray x end is < z_min is = 2 
  elseif ray x end is > x_max is = 3 
  elseif ray z end is > z_max is = 4 
  elseif ray ends inside box  is = 0 

  dt_ray may not be a multiple of dt_ray if the ray intersects a model 

  boundary 

  x_ray(1), z_ray(1) should be set to the ray starting point 

   pi/2 
 ^  | 
 |  |<- 
 z  |   \ 
    |    |                px = cos(angle) pz = sin(angle) 
    ------------ 0 
       x --> 
  compute velocity gradients at the ray starting location
******************************************************************
*/

    /* Function Body */
    ray_dvdx_(&v0, &dvdx, &dvdz, x_ray, z_ray, nx_vel, 
	    x0_vel, dx_vel, nz_vel, z0_vel, dz_vel, vel);
/*  compute the x,z ray parameters */
    px = cos(*a_ray) * v0;
    pz = sin(*a_ray) * v0;
/*  intiialize box exit flag */
    *is = 0;
/*  cycle over allowed ray steps */
    i__1 = *mt_ray-1;
    for (it = 1; it < i__1; ++it) {
/*  set the number of ray steps */
	*nt_ray = it+1;
/*  compute new x,z coords and new ray parameter using local velocity 
*/
	r2 = v0*v0;
	x_ray[it] = x_ray[it - 1] + px * *dt_ray / (r2);
	z_ray[it] = z_ray[it - 1] + pz * *dt_ray / (r2);
/*  compute new velocity and gradient_ray at this x,z location */
	ray_dvdx_(&v0, &dvdx, &dvdz, &x_ray[it], &z_ray[it], nx_vel, 
		x0_vel, dx_vel, nz_vel, z0_vel, dz_vel, vel);
/*  update the x,z ray parameters */
	px += dvdx * *dt_ray / v0;
	pz += dvdz * *dt_ray / v0;
/*  check to see if the ray has left the box */
	if (x_ray[it] < *x_min) {
	    *is = 1;
	} else if (z_ray[it] < *z_min) {
	    *is = 2;
	} else if (x_ray[it] > *x_max) {
	    *is = 3;
	} else if (z_ray[it] > *z_max) {
	    *is = 4;
	}
/*  if it has jump out of loop */
	if (*is != 0) {
	    goto L1;
	}
    }
/* do it = 2 , mt_ray */
L1:
/*  if ray has left box determine the x,z,t values the exit point */
    dx = x_ray[*nt_ray-1] - x_ray[*nt_ray - 2];
    dz = z_ray[*nt_ray-1] - z_ray[*nt_ray - 2];
    r2 = sqrt(dx*dx + dz*dz);
    x1 = Max(*x_min,Min(*x_max,x_ray[*nt_ray-1]));
    z1 = Max(*z_min,Min(*z_max,z_ray[*nt_ray-1]));
    if (dx != (float)0.) {
	rx = r2 * (x1 - x_ray[*nt_ray - 2]) / dx;
    } else {
	rx = r2;
    }
    if (dz != (float)0.) {
	rz = r2 * (z1 - z_ray[*nt_ray - 2]) / dz;
    } else {
	rz = r2;
    }
    r1 = Min(rx,rz);
    if (r2 == (float)0.) {
	scale = (float)1.;
    } else {
	scale = r1 / r2;
    }
/*  determine the x,z,t locations at the exit point */
    x_ray[*nt_ray-1] = x_ray[*nt_ray - 2] + dx * scale;
    z_ray[*nt_ray-1] = z_ray[*nt_ray - 2] + dz * scale;
    *t_ray = (*nt_ray - 2) * *dt_ray + scale * *dt_ray;
    return 0;
}

 int ray_dvdx_(float *v0,float *dvdx,float *dvdz,float *x,float *z,
        int *nx_vel,float *x0_vel,float *dx_vel,int *nz_vel,
        float *z0_vel, float *dz_vel, float *vel)
{
    /* System generated locals */
    float r__4;
    int  dim1;

    /* Local variables */
    static float xf1, xf2, zf1;
    static int  ix1, ix2, iz1, iz2;
    static float zf2;


/*determine velocity and gradient at point x,z
  assume bilinear interpolation 
  get the x,z grid nodes at this location 
 */

    ix1 = (int ) ((*x - *x0_vel) / *dx_vel);
    ix2 = Max(0,Min(*nx_vel-1,ix1+1));
    ix1 = Max(0,Min(*nx_vel-1,ix1));


    r__4 = (*x - (ix1 * *dx_vel + *x0_vel)) / *dx_vel;
    xf2 = Max(0.0,Min(1.0,r__4));
    xf1 = (float)1. - xf2;

    iz1 = (int ) ((*z - *z0_vel) / *dz_vel);
    iz2 = Max(0,Min(*nz_vel-1,iz1+1));
    iz1 = Max(0,Min(*nz_vel-1,iz1));

    r__4 = (*z - (iz1 * *dz_vel + *z0_vel)) / *dz_vel;
    zf2 = Max(0.0,Min(1.0,r__4));
    zf1 = (float)1. - zf2;

    dim1 = *nz_vel;
    *v0 = xf1 * zf1 * vel[iz1 + ix1 * dim1] 
          + xf2 * zf1 * vel[iz1 + ix2 * dim1]
          + xf1 * zf2 * vel[iz2 + ix1 * dim1]
          + xf2 * zf2 * vel[iz2 + ix2 * dim1];
    *dvdx = (zf1 * (vel[iz1 + ix2 * dim1] - vel[iz1 + ix1 * dim1]) + 
	    zf2 * (vel[iz2 + ix2 * dim1] - vel[iz2 + ix1 * dim1])) / *
	    dx_vel;
    *dvdz = (xf1 * (vel[iz2 + ix1 * dim1] - vel[iz1 + ix1 * dim1]) + 
	    xf2 * (vel[iz2 + ix2 * dim1] - vel[iz1 + ix2 * dim1])) / *
	    dz_vel;
    return 0;
}

