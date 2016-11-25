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
#ifndef _MODEL_MAT
#define _MODEL_MAT

#include <string.h>
#include <stdio.h>
#include "transform.h"

/********** Material definitions  **********/
typedef struct _ErsMaterial
 { char   color[24];        /* optional color*/
   char   name[32];         /* optional name */
   int    ndof;             /* Degrees of freedom for this material*/
   int    ndim;             /* dimensionality of the mat. desc.*/
   int    mid;              /* Material ID number              */
   int    npts;             /* Number of control points.       */
   int    *stype;           /* Surface id:groups points        */
   int    *segid;           /* Segment id                      */
   float  *x;               /* Control point x-value locations */
   float  *y;               /* Control point y-value locations */
   float  *z;               /* Control point z-value locations */
   float  *pv;              /* ndof*npts Control values        */
   void   *vectarr;         /* Array of Vector pointers        */
 } ErsMaterial;

typedef struct _ErsMaterials
 { int    nmat;              /* Number of materials defined */
   int    ndim;
   ErsMaterial  *mdata[199];
   ErsTransform *transx;
   ErsTransform *transz;
   ErsTransform *transy;
 } ErsMaterials;

/* Prototypes of Model methods */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* ErsMaterial methods */
ErsMaterial *new_material();
void destroy_material(ErsMaterial *);
void material_setdim(ErsMaterial *mat, int ndim);
int material_getdim(ErsMaterial *mat);
int material_getnpts(ErsMaterial *);
int material_segcnt(ErsMaterial *);
int material_getid(ErsMaterial *);
void material_setid(ErsMaterial *, int);
int material_getdof(ErsMaterial *);
void material_setdof(ErsMaterial *, int);
void material_setname(ErsMaterial *, char *);
char *material_getname(ErsMaterial *);
void material_setcolor(ErsMaterial *, char *);
char *material_getcolor(ErsMaterial *);
int *material_gettyp(ErsMaterial *mat);

void material_set(ErsMaterial *mat, int ndof, int mid, int N,
                  int *stype, float *x, float *z, float *pv);
void material_set3(ErsMaterial *mat, int ndof, int mid, int N,
                  int *stype, int *segid,
                  float *x, float *y, float *z, float *pv);
void material_get(ErsMaterial *mat, int *ndof, int *mid, int *N,
                 int **stype, float **x, float **z, float **pv);
void material_get3(ErsMaterial *mat, int *ndof, int *mid, int *N,
                 int **stype,int **segid,
                 float **x, float **y, float **z, float **pv);
void  materialSetVectA(ErsMaterial *, void *);
void *materialGetVectA(ErsMaterial *);


/* ErsMaterials methods */
ErsMaterials *new_materials();
void destroy_materials(ErsMaterials *);
int materials_getdim(ErsMaterials *mats);
void materials_setdim(ErsMaterials *mats,int ndim);
int materials_getnpts(ErsMaterials *);
void destroy_mat_member(ErsMaterials *, ErsMaterial *);
int materials_count(ErsMaterials *);
ErsTransform *mats_get_transx(ErsMaterials *);
ErsTransform *mats_get_transz(ErsMaterials *);
ErsTransform *mats_get_transy(ErsMaterials *);
void mats_set_transx(ErsMaterials *, ErsTransform *);
void mats_set_transz(ErsMaterials *, ErsTransform *);
void mats_set_transy(ErsMaterials *, ErsTransform *);
ErsMaterial *mats_get_nth(ErsMaterials *mats,int n);
ErsMaterial *mats_getbyid(ErsMaterials *, int );
int materials_add(ErsMaterials *mats,ErsMaterial *mat);

#ifdef __cplusplus
}                   // for C++
#endif

#endif

