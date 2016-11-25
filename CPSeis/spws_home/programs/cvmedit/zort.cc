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
#include <stdlib.h>
#include "transform.h"
#include "vec_list_util.hh"
#include "vl_data.hh"
#include "grid_data.hh"
#include "mlimits.h"
#include "model_desc.hh"
#include "c2f_interface.h"

ModelDesc *zot_time_to_depth(char *czo, ModelDesc *modi);
int        zot_grid_to_depth(gridData *gdata, ErsTransforms *tdata,
           char *xname, char *yname);
int        zot_vll_trans(int dir, gridData *gd, VectorLinkedList *vll,
            ErsTransforms *tdata, int reppt);

//#if(VMS || _AIX || __hpux)
#ifdef NEED_UNDERSCORE
#define ztot_w ztot_w_
#define ztot_convert_velocity_nw ztot_convert_velocity_nw_
#define ztot_time_to_depth_2d_p ztot_time_to_depth_2d_p_
#endif
#ifdef NEED_CAPITALS
#define ztot_w ZTOT_W
#define ztot_convert_velocity_nw ZTOT_CONVERT_VELOCITY_NW
#define ztot_time_to_depth_2d_p ZTOT_TIME_TO_DEPTH_2D_P
#endif

/* Function prototypes defined in model_io.c*/
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

 int ztot_w(char *czo,int *ncoord,float *xcoord,char *coord,
     float *zmin,float *zmax,float *zdatum,
     int *ns, int *spntr, int *scntr,float *xs,float *zs,
     int *nv, int *mpntr, int *mcntr,float *xv,float *zv,
     int *np, float *xp, float *zp,
     int *n1l,float *o1l,float *d1l,
     int *n2, float *o2,float *d2,int *n1,float *o1,float *d1,float *vg);
 void ztot_convert_velocity_nw(char *czo,int *cord,float *xcord,char *zcord,
     float *zdatum ,int *nxg,int *nzg,float *zgmin,float *zginc,float *vg,
     int *mwork,float *work,int *istat);
 void ztot_time_to_depth_2d_p(int *,float *,int *,int *,int *,
     float *xb,float *zb,int *nxg,float *xgmin,float *xginc,
     int *nzg,float *zgmin,float *zginc,float *vg);

#ifdef __cplusplus
}                   // for C++
#endif

/*
 * Convert the layered model from its current
 * domain to the requested TIME or DEPTH domain.
 * The gridded velocity model defines the transformation.
 * Z(T) = ZDATUM + Integral(0:T){ dt/V(t) }
 * This depth conversion is strictly one-dimensional.
 */
ModelDesc *zot_time_to_depth(char *czo, ModelDesc *modi)
{ ModelDesc     *modo=NULL;
  ErsTransforms *tdata;
  ErsTransform  *tx=NULL,*ty=NULL,*tz=NULL;
  ErsTransform  *gt1,*gt2,*gt3;
  ModLimits     *mlim;
  GridLimits    *glim;
  gridData      *gd;
  char          *czi,*czi_g;
  int   i_dir;
  float zdatum = 0;
  float *vg;
  float d1,d2,d3, o1,o2,o3;
  int   n1,n2,n3;

  if(!modi || !czo) return modo;
  if(strcmp(czo,"TIME")==0)
    i_dir=1;   // DEPTH to TIME
  else if(strcmp(czo,"DEPTH")==0)
   i_dir=-1;   // TIME to DEPTH
  else
   return modo;
  gd = modi->GridData();
  if(!gd) return modo;
  if(!gd->getGridData()) return modo;
  gd->getGridCoords( &gt1, &gt2, &gt3);
  czi_g=transform_getname(gt1);
  if( (strcmp(czi_g,"TIME")!=0) && (strcmp(czi_g,"DEPTH")!=0) ) return modo;

  // Make a duplicate of the model
  modo = modi->copy();
  if(!modo) return modo;

  gd    = modo->GridData();
  tdata = modo->transforms();
  mlim  = modo->MLimits();
  glim  = modo->GLimits();

  // Get the layered model coord. system.
  mlimits_get_trans(mlim,&tx,&ty,&tz);
  czi=transform_getname(tz);
  if(strcmp(czo,czi)==0) return modo;


  // Convert the grid model to the same x&y coordinate
  // system as the layered model via a linear transform.
  // and convert to depth if needed
  if(zot_grid_to_depth(gd, tdata, transform_getname(tx),
     transform_getname(ty)) == 0)
   { delete modo;
     modo=NULL;
     return modo;
   }

  vg = (float *) gd->getGridData();
  gd->getGridVals( &n1,&o1,&d1,&n2,&o2,&d2,&n3,&o3,&d3);
  int i;
  for(i=0;i<n1*n2*n3;i++)
    {if(vg[i]!=0) vg[i]=1.0/vg[i];}

// convert the layer boundaries
   zot_vll_trans(i_dir, gd, modo->VLLComponent(MOD_STRUC), tdata, 0);
   zot_vll_trans(i_dir, gd, modo->VLLComponent(MOD_MDATA), tdata, 0);
   zot_vll_trans(i_dir, gd, modo->VLLComponent(MOD_CELLB), tdata, 1);
   zot_vll_trans(i_dir, gd, modo->VLLComponent(MOD_CELLP), tdata, 1);

  for(i=0;i<n1*n2*n3;i++)
    {if(vg[i]!=0) vg[i]=1.0/vg[i];}

  glimits_trans(glim,tdata,transform_getname(tx),transform_getname(ty),czo);
  mlimits_trans(mlim,tdata,transform_getname(tx),transform_getname(ty),czo);

  return modo;
}


int zot_grid_to_depth(gridData *gdata, ErsTransforms *tdata,
    char *xname, char *yname)
{// Make sure the grid is in depth,
 // and in the targeted x&y coordinate system.
  ErsTransform  *gt1=NULL,*gt2=NULL,*gt3=NULL,*tdepth;
  char          *czi_g, czo_g[16];
  float         *work=NULL;
  float          d1,d2,d3, o1,o2,o3, *vg;
  int            n1,n2,n3;
  int            i, mwork, istat=1;
  float          zdatum = 0;

  if(!gdata || !tdata) return 0;
  // Check acceptability of the model.
  gdata->getGridCoords( &gt1, &gt2, &gt3);
  czi_g=transform_getname(gt1);
  if( (strcmp(czi_g,"TIME")!=0) && (strcmp(czi_g,"DEPTH")!=0) ) return 0;
  vg = (float *) gdata->getGridData();
  if(!vg) return 0;
  if(strcmp(gdata->getWord(),"FLOAT")!=0) return 0;

  // Reformat the transform data for fortran access.
  int   ncoord, mcoord=16;
  char  coord[256];
  float xcoord[32];
  ncoord = ErsTransAccess(tdata,mcoord,xcoord,coord);
  strcpy(czo_g,"DEPTH");
  tdepth = ErsTransGetTran(tdata,czo_g);

  // Convert the grid model to the target x&y coordinates
  gdata->gridDataTrans(tdata, transform_getname(gt1), xname, yname);
  if(strcmp(czi_g,"DEPTH")==0) return 1;

  // get grid limits information.
  gdata->getGridVals( &n1,&o1,&d1,&n2,&o2,&d2,&n3,&o3,&d3);

  // Convert velocity to slowness.
  for(i=0;i<n1*n2*n3;i++)
    {if(vg[i]!=0) vg[i]=1.0/vg[i];}

  if(strcmp(czi_g,"TIME")==0) {// Convert to DEPTH
    mwork = 2.5*n1;
    work = (float *) malloc(mwork *sizeof(float));
    if(!work) goto jump1;
   // ztot_convert_velocity_nw(czi_g,&ncoord,xcoord,coord,
   //   &zdatum , &n2, &n1,&o1,&d1,vg, &mwork,work,&istat);
    free(work);
  }

 jump1:
  // Convert slowness back to velocity.
  for(i=0;i<n1*n2*n3;i++)
    {if(vg[i]!=0) vg[i]=1.0/vg[i];}

  // make sure the axis-3 label is set properly
 if(istat==0)
  {
   gdata->getGridCoords( &gt1, &gt2, &gt3);
   gdata->setGridCoords(tdepth, gt2,  gt3);
   gdata->setGridIncs( d1,d2,d3);
   return 1;
  }
 else
   return 0;
}

int zot_vll_trans(int dir, gridData *gd, VectorLinkedList *vll,
     ErsTransforms *tdata, int reppt)
{// Convert list from time to depth
 ModBaseData *dobj;
 ErsTransform *tdepth=NULL;
 float zdatum=0.0;
 float          d1,d2,d3, o1,o2,o3, *vg;
 int            n1,n2,n3;
 int            zeit=DPTHHDR_;
 int nb=1,idir=dir;
 int ixb=0,nxb;
 float xo,zo,xb[1000],zb[1000];
 Vector *v;
 void *p;
 if(!gd || !vll || !tdata) return 0;

 vg = (float *) gd->getGridData();
 gd->getGridVals( &n1,&o1,&d1,&n2,&o2,&d2,&n3,&o3,&d3);
 if(dir==1) tdepth = ErsTransGetTran(tdata,"TIME");
 if(dir==-1) tdepth = ErsTransGetTran(tdata,"DEPTH");
 if(tdepth) zeit = (int) transform_gethdr(tdepth);

 v = vll->top(&p);
 while(v != NULL)
  {dobj = (ModBaseData *) v->getData();
   if(dobj)
    {nxb = dobj->getNumPts();
     dobj->getNx(0,nxb,xb);
     dobj->getNy(0,nxb,zb);
     ztot_time_to_depth_2d_p(&idir,&zdatum,&nb,&ixb,&nxb,xb,zb,
       &n2,&o2,&d2,&n1,&o1,&d1,vg);
     dobj->replace(0,nxb,xb,zb);
     if(reppt)
      {dobj->getpt(&xo, &zo);
       nxb=1;
       ztot_time_to_depth_2d_p(&idir,&zdatum,&nb,&ixb,&nxb,&xo,&zo,
         &n2,&o2,&d2,&n1,&o1,&d1,vg);
       dobj->setpt(xo, zo);
      }
     dobj->setzeit(zeit);
    }

   v = vll->next(&p);
  }

 return 1;
}
