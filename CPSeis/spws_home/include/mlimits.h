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
#ifndef _MLIMITS_
#define _MLIMITS_

#include <string.h>
#include "transform.h"

typedef struct _ModLimits
 { float xmin;       /* model limits for a layer model    */
   float xmax;
   float ymin;
   float ymax;
   float zmin;
   float zmax;
   ErsTransform *transx;
   ErsTransform *transy;
   ErsTransform *transz;
 } ModLimits;



/* Prototypes of ModLimits methods */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

ModLimits *new_mlimits();
void destroy_mlimits(ModLimits *);
void mlimits_set(ModLimits *, float *, float *,
     float *, float *, float *, float *,
     ErsTransform *, ErsTransform *, ErsTransform * );
void mlimits_get(ModLimits *, float *, float *,
     float *, float *, float *, float *,
     ErsTransform **, ErsTransform **, ErsTransform ** );
void mlimits_get_trans(ModLimits *,
     ErsTransform **,ErsTransform **,ErsTransform **);
void mlimits_set_trans(ModLimits *,
     ErsTransform *,ErsTransform *,ErsTransform *);
void mlimits_setdef(ModLimits *mlimits, ErsTransforms *tdata);
int  mlimits_trans(ModLimits *mlim, ErsTransforms *tdata,
      char *xname, char *yname, char *zname);
void mlimits_prt(ModLimits *mlim);


#ifdef __cplusplus
}                   // for C++
#endif

#endif

