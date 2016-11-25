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
#include "model_cell.h"


/*********************************************************
 * Methods that act upon the ErsCell structure.        ***
 ********************************************************/
void cell_set_bnd(ErsCell *cell, int npts, float *xb, float *zb)
{
 if(cell == NULL || npts <= 0) return;
 if(xb==NULL || zb==NULL) return;
 if(npts > 10000)
  { printf("cell_set_bnd: npts > 10000 , Get real guy!\n");
    return;
  }
 cell->ndim = 2;
 cell->nbndpt = npts;
 if(cell->xc ) free(cell->xc);
 if(cell->zc ) free(cell->zc);
 if(cell->yc ) free(cell->yc);
 cell->xc = (float *) calloc(1,npts*sizeof(float));
 cell->zc = (float *) calloc(1,npts*sizeof(float));
 cell->yc = (float *) calloc(1,npts*sizeof(float));
 memcpy(cell->xc,xb,npts*sizeof(float));
 memcpy(cell->zc,zb,npts*sizeof(float));
 return;
}
void cell_set_bnd3(ErsCell *cell, int npts, float *xb, float *zb,
     float *yb)
{
 if(cell == NULL || npts <= 0) return;
 if(xb==NULL || zb==NULL) return;
 if(npts > 10000)
  { printf("cell_set_bnd: npts > 10000 , Get real guy!\n");
    return;
  }
 cell->ndim = 3;
 cell->nbndpt = npts;
 if(cell->xc) free(cell->xc);
 if(cell->zc) free(cell->zc);
 if(cell->yc) free(cell->yc);
 cell->xc = (float *) calloc(1,npts*sizeof(float));
 cell->zc = (float *) calloc(1,npts*sizeof(float));
 cell->yc = (float *) calloc(1,npts*sizeof(float));
 memcpy(cell->xc,xb,npts*sizeof(float));
 memcpy(cell->zc,zb,npts*sizeof(float));
 memcpy(cell->yc,yb,npts*sizeof(float));
 return;
}


void cell_get_bnd(ErsCell *cell, int *npts, float *xb, float *zb)
{
 if(cell == NULL || npts == NULL) return;
 *npts = cell->nbndpt;
 if(*npts <= 0 ) return;
 if(xb) memcpy(xb, cell->xc,*npts*sizeof(float));
 if(zb) memcpy(zb, cell->zc,*npts*sizeof(float));
 return;
}
void cell_get_bnd3(ErsCell *cell, int *npts, float *xb, float *zb,
     float *yb)
{
 if(cell == NULL || npts == NULL) return;
 *npts = cell->nbndpt;
 if(*npts <= 0 ) return;
 if(xb) memcpy(xb, cell->xc,*npts*sizeof(float));
 if(zb) memcpy(zb, cell->zc,*npts*sizeof(float));
 if(yb) memcpy(yb, cell->yc,*npts*sizeof(float));
 return;
}

void cell_get_bndptr(ErsCell *cell, int *npts, float **xb, float **zb)
{if(cell == NULL || npts == NULL) return;
 if(xb==NULL || zb==NULL) return;
 *npts = cell->nbndpt;
 if(*npts <= 0 ) return;
 *xb = cell->xc;
 *zb = cell->zc;
 return;
}
void cell_get_bnd3ptr(ErsCell *cell, int *npts,
     float **xb, float **zb,float **yb)
{if(cell == NULL || npts == NULL) return;
 if(xb==NULL || zb==NULL) return;
 *npts = cell->nbndpt;
 if(*npts <= 0 ) return;
 *xb = cell->xc;
 *zb = cell->zc;
 *yb = cell->yc;
 return;
}

int cell_getnum(ErsCell *cell)
{if(cell == NULL ) return 0;
 return (int) cell->nbndpt;
} 

 
void cell_set( ErsCell *cell, float *xi, float *zi, int *cell_id)
{
 if(cell == NULL) return;
 cell->ndim= 2;
 cell->xin = *xi;
 cell->zin = *zi;
 cell->cell_id = *cell_id;
 return;
}
void cell_set3( ErsCell *cell, float *xi, float *zi,
     float *yi, int *cell_id)
{
 if(cell == NULL) return;
 cell->ndim= 3;
 cell->xin = *xi;
 cell->zin = *zi;
 cell->yin = *yi;
 cell->cell_id = *cell_id;
 return;
}
 

void cell_get( ErsCell *cell, float *xi, float *zi, int *cell_id)
{
 if(cell == NULL) return;
 *xi = cell->xin;
 *zi = cell->zin;
 *cell_id = cell->cell_id;
 return;
}
void cell_get3( ErsCell *cell, float *xi, float *zi, 
     float *yi, int *cell_id)
{
 if(cell == NULL) return;
 *xi = cell->xin;
 *zi = cell->zin;
 *yi = cell->yin;
 *cell_id = cell->cell_id;
 return;
}


int cell_get_id(ErsCell *cell)
{
 if(cell == NULL) return -1;
 return cell->cell_id;
}
void  cell_set_id(ErsCell *cell, int id)
{
 if(cell == NULL) return;
 cell->cell_id = id;
}

ErsCell *new_cell()
{ErsCell *cell;
 cell = (ErsCell *) calloc(1,sizeof(ErsCell));
 cell->cell_id = -1;
 cell->ndim=2;
 strcpy(cell->color,"green");
 return cell;
}

void cell_setdim(ErsCell *cell, int ndim)
{ if(cell) cell->ndim = ndim;}
int cell_getdim(ErsCell *cell)
{ if(!cell) return 0;
  return cell->ndim;
}

void cell_setname(ErsCell *cell, char *name)
{ if(cell==NULL) return;
  if(name == NULL ) return;
  strcpy(cell->name,name);
}
char *cell_getname(ErsCell *cell)
{ if(cell==NULL) return NULL;
  return cell->name;
}

void cell_setcolor(ErsCell *cell, char *color)
{ if(cell==NULL) return;
  if(color == NULL ) return;
  strcpy(cell->color,color);
}
char *cell_getcolor(ErsCell *cell)
{ if(cell==NULL) return NULL;
  return cell->color;
}


void destroy_cell(ErsCell *cell)
{
 if(cell == NULL) return;
 if(cell->xc ) free(cell->xc);
 if(cell->zc ) free(cell->zc);
 if(cell->yc ) free(cell->yc);
/*
 if(cell->vector != NULL)
  destroy_vector(cell->vector);
*/
 free(cell);
 return;
}

void cell_print(ErsCell *cell)
{
 if(cell == NULL) return;
 printf("cell data: *** cell id value=%d ***\n",cell->cell_id);
 printf("cell data:  xin=%g  zin=%g yin=%g\n",cell->xin,cell->zin,
         cell->yin);
 printf("cell data:  nbndpt =%g\n",cell->nbndpt);
}

void cell_scalex(ErsCell *cell,ErsTransform *txi,ErsTransform *txo)
{float x1i,x2i,x1o,x2o;
 int i,j;

 if(cell == NULL) return;
 if(txi == NULL || txo == NULL) return;
 if(txi == txo) return;
 transform_getx(txi, &x1i, &x2i);
 transform_getx(txo, &x1o, &x2o);
 if(x2i==x1i) return;

 cell->xin = x1o + x2o*((cell->xin- x1i)/(x2i-x1i));

 for(i=0;i<cell->nbndpt;i++)
  {cell->xc[i] = x1o + x2o*((cell->xc[i]- x1i)/(x2i-x1i));}

 return;
}
void cell_scalez(ErsCell *cell,ErsTransform *tzi,ErsTransform
*tzo)
{float x1i,x2i,x1o,x2o;
 int i,j;

 if(cell == NULL) return;
 if(tzi == NULL || tzo == NULL) return;
 if(tzi == tzo) return;
 transform_getx(tzi, &x1i, &x2i);
 transform_getx(tzo, &x1o, &x2o);
 if(x2i==x1i) return;

 cell->zin = x1o + x2o*((cell->zin- x1i)/(x2i-x1i));

 for(i=0;i<cell->nbndpt;i++)
  {cell->zc[i] = x1o + x2o*((cell->zc[i]- x1i)/(x2i-x1i));}

 return;
}
void cell_scaley(ErsCell *cell,ErsTransform *tyi,ErsTransform
*tyo)
{float x1i,x2i,x1o,x2o;
 int i,j;

 if(cell == NULL) return;
 if(tyi == NULL || tyo == NULL) return;
 if(tyi == tyo) return;
 transform_getx(tyi, &x1i, &x2i);
 transform_getx(tyo, &x1o, &x2o);
 if(x2i==x1i) return;

 cell->yin = x1o + x2o*((cell->yin- x1i)/(x2i-x1i));

 for(i=0;i<cell->nbndpt;i++)
  {cell->yc[i] = x1o + x2o*((cell->yc[i]- x1i)/(x2i-x1i));}

 return;
}

void *cellGetVect(ErsCell *cell)
{ if(cell == NULL) return NULL;
  return cell->vector;
}
void cellSetVect(ErsCell *cell,void *dat)
{ if(cell == NULL) return;
  cell->vector = dat;
}

/*********************************************************
 * Methods that act upon an ErsCells structure.        ***
 ********************************************************/
ErsCells *new_cells()
{ ErsCells *cdata;
 cdata = (ErsCells *) calloc(1,sizeof(ErsCells));
 cdata->ndim=2;
 return cdata;
}

void destroy_cells(ErsCells *cdata)
{ ErsCell *cell;
  int i, n;

 if(cdata == NULL) return;
 n = cells_count(cdata);
 for(i=0;i<n;i++)
  { destroy_cell(cdata->cells[i]);
  }
 free(cdata);
 return;
}

void cells_setdim(ErsCells *cdata, int ndim)
{ int i;
  if(cdata) cdata->ndim = ndim;
  for(i=0;i<cells_count(cdata);i++)
  { cell_setdim(cdata->cells[i],ndim);
  }
}
int cells_getdim(ErsCells *cdata)
{ if(!cdata) return 0;
  return cdata->ndim;
}

int cells_count(ErsCells *cdata)
{
 if(cdata == NULL) return 0;
 return cdata->ncell;
}


ErsTransform *cells_get_transx(ErsCells *cdata)
{ if(cdata == NULL) return NULL;
 return cdata->transx; }
void cells_set_transx(ErsCells *cdata, ErsTransform *tx)
{ if(cdata == NULL) return;
  cdata->transx = tx;
 return; }

ErsTransform *cells_get_transz(ErsCells *cdata)
{ if(cdata == NULL) return NULL;
 return cdata->transz; }
void cells_set_transz(ErsCells *cdata, ErsTransform *tz)
{ if(cdata == NULL) return;
  cdata->transz = tz;
 return; }

ErsTransform *cells_get_transy(ErsCells *cdata)
{ if(cdata == NULL) return NULL;
 return cdata->transy; }
void cells_set_transy(ErsCells *cdata, ErsTransform *ty)
{ if(cdata == NULL) return;
  cdata->transy = ty;
 return; }


ErsCell *cells_get_nth(ErsCells *cdata,int n)
{
 if(cdata == NULL) return NULL;
 if(n < 0) return NULL;
 if(n > cdata->ncell-1) return NULL;

 return (ErsCell *) cdata->cells[n];
}

void cells_getbyid(ErsCells *cdata, int cid, ErsCell **clist)
{ErsCell *cell;
 int n,m,num;
 if(cdata == NULL) return;
 if(cdata->ncell<1) return;

 /* return list of pointers to all matches ,caller provides memory*/
 /* list terminated by NULL entry */
 num = cdata->ncell;
 n=0;
 for(m=0;m<num;m++)
  { cell = cdata->cells[m];
    if(cid == cell_get_id(cell))
     { clist[n] = cell; n++; }
  }
 clist[n]=NULL;
 return;
}

int cells_add(ErsCells *cdata,ErsCell *cell)
{ErsCell *ct;
 int i,n;
 if(cdata == NULL) return 0;
 if(cell  == NULL) return 0;
 
 n = cdata->ncell;
 cdata->cells[n] = cell;
 cdata->ncell++;
 cell_setdim(cell,cdata->ndim);
 return cdata->ncell;
}
