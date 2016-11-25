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
#include "mlimits.h"

/*********************************************************
 * Access or Set members of the ModLimits structure.   ***
 ********************************************************/
ModLimits *new_mlimits()
{ModLimits *mlimits;
 mlimits = (ModLimits *) calloc(1,sizeof(ModLimits));
 return mlimits;
}

void destroy_mlimits(ModLimits *m)
{
 if(m != NULL) free(m);
}

void mlimits_set(ModLimits *mlimits, float *xmin, float *xmax,
     float *zmin, float *zmax, float *ymin, float *ymax,
     ErsTransform *tx, ErsTransform *ty, ErsTransform *tz )
{
  if(mlimits != NULL)
  { mlimits->xmin   = *xmin;
    mlimits->xmax   = *xmax;
    if( *xmin > *xmax)
     { mlimits->xmin = *xmax; mlimits->xmax = *xmin; }
    mlimits->ymin   = *ymin;
    mlimits->ymax   = *ymax;
    if( *ymin > *ymax)
     { mlimits->ymin = *ymax; mlimits->ymax = *ymin; }
    mlimits->zmin   = *zmin;
    mlimits->zmax   = *zmax;
    if( *zmin > *zmax)
     { mlimits->zmin = *zmax; mlimits->zmax = *zmin; }
    mlimits->transx = tx;
    mlimits->transy = ty;
    mlimits->transz = tz;
  }

 return;
}

void mlimits_get_trans(ModLimits *mlimits,
     ErsTransform **tx,ErsTransform **ty,ErsTransform **tz)
{ *tx = NULL;
  *ty = NULL;
  *tz = NULL;
  if(mlimits == NULL) return;
  *tx = mlimits->transx;
  *ty = mlimits->transy;
  *tz = mlimits->transz;
  return;
}

void mlimits_set_trans(ModLimits *mlimits,
     ErsTransform *tx,ErsTransform *ty,ErsTransform *tz)
{
  if(mlimits == NULL) return;
  mlimits->transx = tx;
  mlimits->transy = ty;
  mlimits->transz = tz;
  return;
}

void mlimits_get(ModLimits *mlimits, float *xmin, float *xmax,
     float *zmin, float *zmax, float *ymin, float *ymax,
     ErsTransform **tx, ErsTransform **ty, ErsTransform **tz )
{
  if(mlimits != NULL)
  { *xmin = mlimits->xmin;
    *xmax = mlimits->xmax;
    *ymin = mlimits->ymin;
    *ymax = mlimits->ymax;
    *zmin = mlimits->zmin;
    *zmax = mlimits->zmax;
    *tx = mlimits->transx;
    *ty = mlimits->transy;
    *tz = mlimits->transz;
  }

 return;
}

void mlimits_setdef(ModLimits *mlimits, ErsTransforms *tdata)
{float x1, x2, z1, z2, y1, y2;
 ErsTransform *t1,*t2,*t3;
 if(mlimits==NULL || tdata == NULL) return;
 t1 = ErsTransGetTran(tdata,"XBASEMENT");
 t2 = ErsTransGetTran(tdata,"YBASEMENT");
 t3 = ErsTransGetTran(tdata,"KILOMETER");
 transform_getx(t1,&x1,&x2);
 transform_getx(t2,&y1,&y2);
 transform_getx(t3,&z1,&z2);
 mlimits_set(mlimits, &x1, &x2,
     &z1, &z2, &y1, &y2, t1, t2, t3 );
}

/******************************************************
 * Method to transform ModLimits object             ***
 *****************************************************/
int mlimits_trans(ModLimits *mlim, ErsTransforms *tdata,
 char *xname, char *yname, char *zname)
{ErsTransform *to[3],*ti[3];
 int   i,ierr,istat,one=1;

 if(mlim==NULL || tdata==NULL) return 1;
 to[0] = ErsTransGetTran(tdata,xname);
 to[1] = ErsTransGetTran(tdata,yname);
 to[2] = ErsTransGetTran(tdata,zname);
 ti[0] = mlim->transx;
 ti[1] = mlim->transy;
 ti[2] = mlim->transz;
 istat = 0;
 ierr  = transform_mcnvrt(ti[0],&mlim->xmin,one,to[0]);
 ierr += transform_mcnvrt(ti[0],&mlim->xmax,one,to[0]);
 if(ierr == 0) mlim->transx = to[0]; istat += ierr;
 ierr  = transform_mcnvrt(ti[1],&mlim->ymin,one,to[1]);
 ierr += transform_mcnvrt(ti[1],&mlim->ymax,one,to[1]);
 if(ierr == 0) mlim->transy = to[1]; istat += ierr;
 ierr  = transform_mcnvrt(ti[2],&mlim->zmin,one,to[2]);
 ierr += transform_mcnvrt(ti[2],&mlim->zmax,one,to[2]);
 if(ierr == 0) mlim->transz = to[2]; istat += ierr;
 return istat;
}

void mlimits_prt(ModLimits *mlim)
{ErsTransform *t;
 if(mlim == NULL) return;
 t = mlim->transx;
 printf(" mlimits_prt:  Name=%s, min=%f, max=%f\n",
        transform_getname(t),mlim->xmin,mlim->xmax);
 t = mlim->transy;
 printf(" mlimits_prt:  Name=%s, min=%f, max=%f\n",
        transform_getname(t),mlim->ymin,mlim->ymax);

 t = mlim->transz;
 printf(" mlimits_prt:  Name=%s, min=%f, max=%f\n",
        transform_getname(t),mlim->zmin,mlim->zmax);

}

