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
#include "model_mat.h"


/*********************************************************
 * Methods that act upon an ErsMaterial structure.     ***
 ********************************************************/
ErsMaterial *new_material()
{ ErsMaterial *m;
 m = (ErsMaterial *) calloc(1,sizeof(ErsMaterial));
 m->mid=1;
 m->x = NULL;
 m->z = NULL;
 m->y = NULL;
 m->pv = NULL;
 m->ndim=2;
 strcpy(m->color,"black");
 return m;
}

void destroy_material(ErsMaterial *mat)
{

 if(mat == NULL) return;
 if(mat->x ) free(mat->x);
 if(mat->y ) free(mat->y);
 if(mat->z ) free(mat->z);
 if(mat->pv ) free(mat->pv);
 if(mat->stype ) free(mat->stype);
 if(mat->segid ) free(mat->segid);
 free(mat);
 return;
}

void material_setdim(ErsMaterial *mat, int ndim)
{ if(mat) mat->ndim = ndim;}
int material_getdim(ErsMaterial *mat)
{ if(!mat) return 0;
  return mat->ndim;
}


int material_getnpts(ErsMaterial *mat)
{
 if(mat == NULL) return 0;
 return mat->npts;
}

int material_segcnt(ErsMaterial *mat)
{ int i,count,oldval,oldseg;
  count = 0;
  if( mat == NULL) return count;
  if( mat->npts < 1) return count;
  count  = 1;
  oldval = mat->stype[0];
  oldseg = mat->segid[0];
  for(i=0;i<mat->npts;i++)
   {if(mat->stype[i] != oldval || mat->segid[i] != oldseg)
     { count++;
       oldval = mat->stype[i];
       oldseg = mat->segid[i];
     }
   }
 return count;
}

int material_getid(ErsMaterial *mat)
{
 if(mat == NULL) return -1;
 return mat->mid;
}
void material_setid(ErsMaterial *mat, int mid)
{
 if(mat == NULL) return;
 mat->mid = mid;
}

int material_getdof(ErsMaterial *mat)
{
 if(mat == NULL) return 0;
 return mat->ndof;
}
void material_setdof(ErsMaterial *mat, int dof)
{
 if(mat == NULL) return;
 mat->ndof = dof;
}

char *material_getcolor(ErsMaterial *mat)
{
 if(mat == NULL) return NULL;
 return mat->color;
}
void material_setcolor(ErsMaterial *mat, char *color)
{
 if(mat == NULL) return;
 if(color == NULL)
  { strcpy(mat->color,"green"); return; }
 if(strlen(color) == 0)
  { strcpy(mat->color,"green"); return; }
 else 
  { strcpy(mat->color,color); return; }
}

char *material_getname(ErsMaterial *mat)
{
 if(mat == NULL) return NULL;
 return mat->name;
}
void material_setname(ErsMaterial *mat, char *name)
{
 if(mat == NULL) return;
 if(name == NULL)
  { mat->name[0] = '\0'; return; }
 if(strlen(name) == 0)
  { mat->name[0] = '\0'; return; }
 else
  { strcpy(mat->name,name); return; }
}

int *material_gettyp(ErsMaterial *mat)
{
 if(mat == NULL) return NULL;
 return mat->stype;
}
void *materialGetVectA(ErsMaterial *mat)
{ if(mat == NULL) return NULL;
  return mat->vectarr;
}
void materialSetVectA(ErsMaterial *mat,void *dat)
{ if(mat == NULL) return;
  mat->vectarr = dat;
}


void material_set(ErsMaterial *mat, int ndof, int mid, int N,
                  int *stype, float *x, float *z, float *pv)
{int i,j,replace;

 if(mat == NULL) return;
 if(N < 1 || ndof < 1) return;
 if(N <= mat->npts && ndof == mat->ndof)
  {/* Replace old information with new */
   replace = 1;
  }
 else
  {/* release any old data */
   replace = 0;
   if(mat->x ) free(mat->x);
   if(mat->z ) free(mat->z);
   if(mat->y ) free(mat->y);
   if(mat->stype ) free(mat->stype);
   if(mat->segid ) free(mat->segid);
   if(mat->pv) free(mat->pv);
   /* allocate storage and save data */
   mat->stype = (int *) calloc(1,N*sizeof(int));
   mat->segid = (int *) calloc(1,N*sizeof(int));
   mat->x = (float *) calloc(1,N*sizeof(float));
   mat->y = (float *) calloc(1,N*sizeof(float));
   mat->z = (float *) calloc(1,N*sizeof(float));
   mat->pv= (float *) calloc(1,N*ndof*sizeof(float));
  }
 mat->npts = N;
 mat->mid  = mid;
 mat->ndof = ndof;
 for(i=0;i<N;i++)
  {mat->stype[i] = stype[i];
   mat->segid[i] = 1;
   mat->x[i] = x[i];
   mat->y[i] = 0.0;
   mat->z[i] = z[i];
   for(j=0;j<ndof;j++)
    { mat->pv[i*(ndof) + j] = pv[i*(ndof) + j]; }
  }
 
 return;
}

void material_set3(ErsMaterial *mat, int ndof, int mid, int N,
                  int *stype, int *segid,
                  float *x, float *y, float *z, float *pv)
{int i,j,replace;

 if(mat == NULL) return;
 if(N < 1 || ndof < 1) return;
 if(N <= mat->npts && ndof == mat->ndof)
  {/* Replace old information with new */
   replace = 1;
  }
 else
  {/* release any old data */
   replace = 0;
   if(mat->x ) free(mat->x);
   if(mat->z ) free(mat->z);
   if(mat->y ) free(mat->y);
   if(mat->stype ) free(mat->stype);
   if(mat->segid ) free(mat->segid);
   if(mat->pv) free(mat->pv);
   /* allocate storage and save data */
   mat->stype = (int *) calloc(1,N*sizeof(int));
   mat->segid = (int *) calloc(1,N*sizeof(int));
   mat->x = (float *) calloc(1,N*sizeof(float));
   mat->y = (float *) calloc(1,N*sizeof(float));
   mat->z = (float *) calloc(1,N*sizeof(float));
   mat->pv= (float *) calloc(1,N*ndof*sizeof(float));
  }
 mat->npts = N;
 mat->mid  = mid;
 mat->ndof = ndof;
 mat->ndim = 3;
 for(i=0;i<N;i++)
  {mat->stype[i] = stype[i];
   mat->segid[i] = segid[i];
   mat->x[i] = x[i];
   mat->y[i] = y[i];
   mat->z[i] = z[i];
   for(j=0;j<ndof;j++)
    { mat->pv[i*(ndof) + j] = pv[i*(ndof) + j]; }
  }

 return;
}

void material_get(ErsMaterial *mat, int *ndof, int *mid, int *N,
                 int **stype, float **x, float **z, float **pv)
{
 *N = 0;
 if(mat == NULL) return;
 /* Return values and pointers to the data */
 *N   = mat->npts;
 *mid = mat->mid;
 *ndof= mat->ndof;
 *stype=mat->stype;
 *x = mat->x;
 *z = mat->z;
 *pv = mat->pv;
 return;
}
void material_get3(ErsMaterial *mat, int *ndof, int *mid, int *N,
                 int **stype,int **segid,
                 float **x, float **y, float **z, float **pv)
{
 *N = 0;
 if(mat == NULL) return;
 /* Return values and pointers to the data */
 *N   = mat->npts;
 *mid = mat->mid;
 *ndof= mat->ndof;
 *stype=mat->stype;
 *segid=mat->segid;
 *x = mat->x;
 *y = mat->y;
 *z = mat->z;
 *pv = mat->pv;
 return;
}

/*********************************************************
 * Methods that act upon an ErsMaterials structure.    ***
 ********************************************************/
ErsMaterials *new_materials()
{ ErsMaterials *m;
 m = (ErsMaterials *) calloc(1,sizeof(ErsMaterials));
 m->ndim = 2;
 return m;
}

void destroy_materials(ErsMaterials *mats)
{ 
  int i, n;

 if(mats == NULL) return;
 n = mats->nmat;
 for(i=0;i<mats->nmat;i++)
  { destroy_material(mats->mdata[i]);
  }
 free(mats);
 return;
}

void materials_setdim(ErsMaterials *mats, int ndim)
{int i;
 if(!mats) return;
 mats->ndim = ndim;
 for(i=0;i<mats->nmat;i++)
  material_setdim(mats->mdata[i],ndim);
}
int materials_getdim(ErsMaterials *mats)
{ if(!mats) return 0;
  return mats->ndim;
}

void destroy_mat_member(ErsMaterials *mats, ErsMaterial *mat)
{int i, n,num;

 if(mats == NULL || mat == NULL) return;
 num = mats->nmat;
 if(num == 0) return;
 for(i=0;i<mats->nmat;i++)
  {if(mat == mats->mdata[i])
    { destroy_material(mats->mdata[i]); break;}
  }
 if(i < mats->nmat)
  {for(n=i;n<mats->nmat-1;n++)
     { mats->mdata[n] = mats->mdata[n+1]; }
  }
 else return;
 mats->nmat -= 1;
 if(mats->nmat < 0) mats->nmat = 0;
 return;
}


int materials_count(ErsMaterials *mats)
{
 if(mats == NULL) return 0;
 return mats->nmat;
}

ErsTransform *mats_get_transx(ErsMaterials *mats)
{ if(mats == NULL) return NULL;
 return mats->transx; }
void mats_set_transx(ErsMaterials *mats, ErsTransform *tx)
{ if(mats == NULL) return;
  mats->transx = tx;
 return; }

ErsTransform *mats_get_transz(ErsMaterials *mats)
{ if(mats == NULL) return NULL;
 return mats->transz; }
void mats_set_transz(ErsMaterials *mats, ErsTransform *tz)
{ if(mats == NULL) return;
  mats->transz = tz;
 return; }

ErsTransform *mats_get_transy(ErsMaterials *mats)
{ if(mats == NULL) return NULL;
 return mats->transy; }
void mats_set_transy(ErsMaterials *mats, ErsTransform *ty)
{ if(mats == NULL) return;
  mats->transy = ty;
 return; }


ErsMaterial *mats_get_nth(ErsMaterials *mats,int n)
{
 if(mats == NULL) return NULL;
 if(n < 0) return NULL;
 if(n > mats->nmat-1) return NULL;

 return (ErsMaterial *) mats->mdata[n];
}

ErsMaterial *mats_getbyid(ErsMaterials *mats, int mid)
{ErsMaterial *mat;
 int m,num;
 if(mats == NULL) return NULL;
 if(mats->nmat<1) return NULL;

 /* return pointers to first match*/
 num = mats->nmat;
 mat = NULL;
 for(m=0;m<num;m++)
  { mat  = mats->mdata[m];
    if(mid == material_getid(mat)) return mat;
  }
 return NULL;
}

int materials_add(ErsMaterials *mats,ErsMaterial *mat)
{
 if(mats == NULL) return 0;
 if(mat  == NULL) return 0;
 
 mats->mdata[mats->nmat] = mat;
 mats->nmat++;
 material_setdim(mat,mats->ndim);
 return mats->nmat;
}
