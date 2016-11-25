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
#include "mod_access.h"
/*******************
 * Routines to extract data in model subcomponents
 *******************/

void access_cdat_(ErsCells *cdata, int *ncell, int *ipntr,
     int *nxcell, float *xcell, float *zcell)
{
 ErsCell       *cell;
 int   i,k,m,cell_id;
 float xi,zi;
 /* Data from *CELL section of file */
 *ncell = 0;
 if(cdata == NULL) return;
 if(xcell==NULL || zcell==NULL) return;
 if(ipntr==NULL || nxcell==NULL) return;
 *ncell = cells_count(cdata);
 m=0;
 k=0;
 for(i=0;i<*ncell;i++)
  { cell = cells_get_nth(cdata,i);
    ipntr[i] = k;
    cell_get_bnd(cell,(int *) &nxcell[i],&xcell[k],&zcell[k]);
    k += nxcell[i];
  }
 
 return;
}
int access_cells(ErsCells *cdata,int *ncell,int **id,int **ipntr,
     int **nxcell,float **xcell,float **zcell)
{
 ErsCell       *cell;
 int   i,k=0,m=0,cell_id;
 float xi,zi;
 /* Data from *CELL section of file */
 *ncell = 0;
 if(cdata == NULL) return 0;
 if(xcell==NULL || zcell==NULL) return 0;
 if(ipntr==NULL || nxcell==NULL) return 0;
 *ncell =  cells_count(cdata);
 for(i=0;i<*ncell;i++)
  { cell = cells_get_nth(cdata,i);
    m += cell_getnum(cell);
  }
 *ipntr = (int *)  malloc(*ncell*sizeof(int));
 *nxcell= (int *) malloc(*ncell*sizeof(int));
 *id    = (int *) malloc(*ncell*sizeof(int));
 *xcell = (float *) malloc(m*sizeof(float));
 *zcell = (float *) malloc(m*sizeof(float));

 for(i=0;i<*ncell;i++)
  { cell = cells_get_nth(cdata,i);
    (*id)[i] = cell_get_id(cell);
    (*ipntr)[i] = k;
    cell_get_bnd(cell,*nxcell+i,*xcell+k,*zcell+k);
    k += (*nxcell)[i];
  }

 return *ncell;
}


void  access_mdat_(ErsModel *model, int *rdof, int *nvcrd,
      int *nmats, int *rid, int *matpntr, int *matcntr,
      int *hflag, float *xv, float *zv, float *pv)
{
 ErsMaterials  *mats;
 ErsMaterial   *mat;
 float         *x,*z,*v;
 int  ndof,*stype,mid;
 int  i,j,k,l,m,count = 0;
 /* Data from *VELOCITY section of file */
 *rdof  = 0;
 *nvcrd = 0;
 *nmats  = 0;
 if(model == NULL) { return; }
 mats = model_getmdata(model);
 if(mats == NULL) { return; }
 mat  = mats_get_nth(mats,0);
 *rdof= material_getdof(mat);
 *nmats = materials_count(mats);
 for(i=0;i<*nmats;i++)
   { mat= mats_get_nth(mats,i);
     count += material_getnpts(mat);
   }
 *nvcrd=count;
 m=0;
 for(i=0;i<*nmats;i++)
   { int npts;
     mat  = mats_get_nth(mats,i);
     material_get(mat, &ndof, &mid, &npts,&stype,&x,&z,&v);
     matpntr[i]= m;
     matcntr[i]= npts;
     rid[i]    = mid;
     for(j=0;j<npts;j++)
       { l = m + j;
         xv[l] = x[j];
         zv[l] = z[j];
         hflag[l]= stype[j];
         for(k=0;k<ndof;k++)
           { pv[k*count + l]=v[j*ndof + k]; }
       }
     m += npts;
   }

 return;
}
int access_mats(ErsModel *model, int *rdof, int *nvcrd,
      int *nmats, int **rid, int **mpntr, int **mcntr,
      int **hflag, float **xv, float **zv, float **pv)
{/* place material data in linear arrays */
 ErsMaterials  *mats;
 ErsMaterial   *mat;
 float         *x,*z,*v;
 int  ndof,*stype,mid;
 int  i,j,k=0,l,m=0,count=0;
 /* Data from *VELOCITY section of file */
 *rdof  = 0;
 *nvcrd = 0;
 *nmats = 0;
 if(model == NULL) { return 0; }
 mats = model_getmdata(model);
 if(mats == NULL) { return 0; }
 mat  = mats_get_nth(mats,0);
 *nmats = materials_count(mats);
 for(i=0;i<*nmats;i++)
   { mat= mats_get_nth(mats,i);
     count += material_getnpts(mat);
   }
 *nvcrd = count;
 ndof= material_getdof(mat);
 *rdof  = ndof;
 *rid   = (int *) malloc(*nmats*sizeof(int));
 *mpntr = (int *) malloc(*nmats*sizeof(int));
 *mcntr = (int *) malloc(*nmats*sizeof(int));
 *hflag = (int *) malloc(count*sizeof(int));
 *xv    = (float *) malloc(count*sizeof(float));
 *zv    = (float *) malloc(count*sizeof(float));
 *pv    = (float *) malloc(count*ndof*sizeof(float));
 m=0;
 for(i=0;i<*nmats;i++)
   { int npts;
     mat  = mats_get_nth(mats,i);
     material_get(mat, &ndof, &mid, &npts,&stype,&x,&z,&v);
     (*mpntr)[i]= m;
     (*mcntr)[i]= (int) npts;
     (*rid)[i]  = mid;
     for(j=0;j<npts;j++)
      { l = m+j;
        (*xv)[l] = x[j];
        (*zv)[l] = z[j];
        (*hflag)[l]= stype[j];
        for(k=0;k<ndof;k++)
         { (*pv)[k*count + l]=v[j*ndof + k]; }
      }
     m += npts;
   }

 return count;
}


void  access_pdat_(ErsModel *model,
      int *npik,int *pdof,float *x1,float *z1,float *a1,
      int *nseg,int *segpntr,int *segptcnt)
{PR_ *pikrec;
 ErsHorizon    *hlist[199],*slist[24],*thorizon;
 ErsPoint      *point;
 int   hnumber,snumber;
 int   Phdr, Shdr, Thdr;
 int   i,j,m;

 /* Data from *PICK section of file */
 *npik = 0;
 *pdof = 2;
 if(model == NULL) return;
 pikrec = (PR_ *) model_getpdata(model);
 if(pikrec == NULL) return;
 if(pikrec->Shdr > 0) *pdof = 3;
 if(pikrec->Thdr > 0) *pdof = 4;
 PRCounts(pikrec,(long *) nseg,(long *) npik);
 m = 0;
 for (i=0; i<pikrec->horizon_count; i++)
  {int count;
   thorizon = pikrec->horizon_list[i];
   hnumber  = thorizon->hid;
   ErsHorizonMemberList(pikrec,thorizon,&count,&snumber,hlist);
   if(snumber == -1) snumber = 1;
   point = pikrec->horizon_list[i]->first_point;
   segpntr[i]=m;
   while (point != NULL)
    {for (j=0; j<=point->npicks; j++)
      {
       if (j == point->npicks)
        {x1[m] = point->pkey;
         z1[m] = point->time;
         a1[*pdof*m]=hnumber;
         a1[*pdof*m+1]=snumber;
         if(pikrec->Shdr > 0) a1[*pdof*m+2] = point->skey;
         if(pikrec->Thdr > 0) a1[*pdof*m+3] = point->tkey;
         m++;
        }
       else
        {x1[m] = point->pkey;
         z1[m] = point->time;
         a1[*pdof*m]=hnumber;
         a1[*pdof*m+1]=snumber;
         if(pikrec->Shdr > 0) a1[*pdof*m+2] = point->skey;
         if(pikrec->Thdr > 0) a1[*pdof*m+3] = point->tkey;
         m++;
        }
      }
     point = point->next;
    }
   segptcnt[i]= m - segpntr[i];
  }
 *npik = m;

 return;
}

void access_mlim_(ErsModel *model,
     float *xmin, float *xmax, float *zmin, float *zmax,
     float *ymin, float *ymax)
{
 ModLimits  *mlimits;
 ErsTransform *tx,*ty,*tz;
 *xmin = 0.0;
 *xmax = 0.0;
 *ymin = 0.0;
 *ymax = 0.0;
 *zmin = 0.0;
 *zmax = 0.0;
 if(model == NULL) { return; }
 /* Break data out of structures for fortran access */
 /* Model limits */
 mlimits = model_getmlim(model);
 mlimits_get(mlimits,xmin,xmax,zmin,zmax,ymin,ymax,
             &tx, &ty, &tz );

 return;
}

void access_glim_(ErsModel *model,
                      int  *n1, float *o1, float *d1,
                      int  *n2, float *o2, float *d2,
                      int  *n3, float *o3, float *d3)
{
 GridLimits *glimits;
 ErsTransform *t1,*t2,*t3;
 *n1= 0;   *n2= 0;   *n3= 0;
 *o1= 0.0; *o2= 0.0; *o3= 0.0;
 *d1= 0.0; *d2= 0.0; *d3= 0.0;
 if(model == NULL) { return; }
 /* Grid limits */
 glimits = model_getglim(model);
 glimits_set(glimits, n1,o1,d1, n2,o2,d2, n3,o3,d3,
             t1, t2, t3 );
 return;
}

void  access_class_(PR_ *pikrec,
      int *nclass,int *clid,int *nsubcl,char *cllab)
{
 ErsHorizon       *hlist[199];
 int  i;

 *nclass = 0;
 cllab[0]='\0';
 if(pikrec == NULL) return;
 ErsHorizonGetHList(pikrec,(int *) nclass, hlist);
 for(i=0;i<*nclass;i++)
   { clid[i] = hlist[i]->hid;
     nsubcl[i] = ErsHorizonGetSegmentCount(pikrec,hlist[i]);
     strcat(cllab,hlist[i]->horizon_name);
     strcat(cllab,",");
   }

 return;
}

