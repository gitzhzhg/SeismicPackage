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
#ifndef _MODEL_CELL
#define _MODEL_CELL
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "transform.h"

/********** Cell definitions  **********/
typedef struct _ErsCell
 { char   color[24]; /* optional color*/
   char   name[32];  /* optional name */
   int    nbndpt;    /* No. of cell boundary points */
   int    cell_id;   /* cell id = horizon id  */
   int    ndim;      /* dimensionality of the data,2 or 3*/
   float  *xc;       /* cell boundarys */
   float  *zc;
   float  *yc;
   float  xin;       /* (xin,zin,yin) = point inside cell */
   float  zin;
   float  yin;
   void   *vector;   /* Vector pointer */
   void   *vectlab;  /* Vector label pointer */
 } ErsCell;

typedef struct _ErsCells
 { int ncell;
   int ndim;      /* dimensionality of the data,2 or 3*/
   ErsCell *cells[199];
   ErsCell *current_cell;
   ErsTransform *transx;
   ErsTransform *transz;
   ErsTransform *transy;
 } ErsCells;

/* Prototypes of ErsCell & ErsCells methods */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* ErsCell methods */
ErsCell *new_cell();
void destroy_cell(ErsCell *cell);
void cell_print(ErsCell *cell);
void cell_setdim(ErsCell *cell, int ndim);
int  cell_getdim(ErsCell *cell);
void cell_set( ErsCell *, float *, float *, int  *);
void cell_set3( ErsCell *, float *, float *, float *, int  *);
void cell_get( ErsCell *, float *, float *, int  *);
void cell_get3( ErsCell *, float *, float *, float *, int  *);
void cell_set_bnd(ErsCell *cell, int  npts, float *xb, float *zb);
void cell_set_bnd3(ErsCell *cell, int  npts, float *xb, float *zb,float *yb);
void cell_get_bnd(ErsCell *cell, int  *npts, float *xb, float *zb);
void cell_get_bnd3(ErsCell *cell, int  *npts, float *xb, float *zb,float *yb);
void cell_get_bndptr(ErsCell *cell, int  *npts, float **xb, float **zb);
void cell_get_bnd3ptr(ErsCell *cell, int  *npts, float **xb, float **zb,float **yb);
int  cell_get_id(ErsCell *);
void cell_set_id(ErsCell *, int);
int  cell_getnum(ErsCell *cell);
void cell_setcolor(ErsCell *cell, char *color);
char *cell_getcolor(ErsCell *cell);
void cell_setname(ErsCell *cell, char *name);
char *cell_getname(ErsCell *cell);
void cell_scalex(ErsCell *cell,ErsTransform *txi,ErsTransform *txo);
void cell_scalez(ErsCell *cell,ErsTransform *tzi,ErsTransform *tzo);
void cell_scaley(ErsCell *cell,ErsTransform *tyi,ErsTransform *tyo);
void cellSetVect(ErsCell *, void *);
void *cellGetVect(ErsCell *);

/* ErsCells methods */
ErsCells *new_cells();
void destroy_cells(ErsCells *cdata);
void cells_setdim(ErsCells *cells, int ndim);
int  cells_getdim(ErsCells *cells);
int cells_count(ErsCells *cdata);
ErsTransform *cells_get_transx(ErsCells *);
void cells_set_transx(ErsCells *, ErsTransform *);
ErsTransform *cells_get_transz(ErsCells *);
void cells_set_transz(ErsCells *, ErsTransform *);
ErsTransform *cells_get_transy(ErsCells *);
void cells_set_transy(ErsCells *, ErsTransform *);
ErsCell *cells_get_nth(ErsCells *,int );
void cells_getbyid(ErsCells *, int , ErsCell **);
int cells_add(ErsCells *,ErsCell *);

#ifdef __cplusplus
}                   // for C++
#endif

#endif

