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
#ifndef _GLIMITS_
#define _GLIMITS_

#include <string.h>
#include "transform.h"
#include "mlimits.h"

typedef struct _GridLimits
 { int   n1;         /* Parameters to define a grid model */
   int   n2;
   int   n3;
   float o1;         /* origins */
   float o2;
   float o3;
   float d1;         /* grid sizes or increments */
   float d2;
   float d3;
   ErsTransform *trans1;
   ErsTransform *trans2;
   ErsTransform *trans3;
 } GridLimits;

/* Prototypes of GridLimits methods */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

GridLimits *new_glimits();
void destroy_glimits(GridLimits *);
void glimits_set(GridLimits *,
     int  *, float *, float *,
     int  *, float *, float *,
     int  *, float *, float *,
     ErsTransform *, ErsTransform *, ErsTransform * );
void glimits_get(GridLimits *,
     int  *, float *, float *,
     int  *, float *, float *,
     int  *, float *, float *,
     ErsTransform **, ErsTransform **, ErsTransform ** );
void glimits_set_orgs(GridLimits *glimits, float o1, float o2, float o3);
void glimits_set_incs(GridLimits *glimits, float d1, float d2, float d3);
void glimits_set_sizes(GridLimits *glimits, int  n1, int  n2, int  n3);
void glimits_get_trans(GridLimits *,
     ErsTransform **,ErsTransform **,ErsTransform **);
void glimits_set_trans(GridLimits *,
     ErsTransform *,ErsTransform *,ErsTransform *);
void glimits_setdef(GridLimits *glimits, ModLimits *mlimits);
int  glimits_trans(GridLimits *glim, ErsTransforms *tdata,
     char *xname, char *yname, char *zname);
void glimits_prt(GridLimits *glim);


#ifdef __cplusplus
}                   // for C++
#endif

#endif
