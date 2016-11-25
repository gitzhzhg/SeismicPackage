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
#ifndef _MODACCESS_h
#define _MODACCESS_h

#include "model.h"
#include "pick.h"

#ifdef __cplusplus  
extern "C" {                 // for C++ 
#endif

/*          FUNCTION PROTOTYPES          */
void access_pdat_(ErsModel *model, int  *npik,int  *pdof,
                      float *x1,float *z1,float *a1,
                      int  *nseg,int  *segpntr,int  *segptcnt);
void access_class_(PR_ *pr,
                      int  *nclass,int  *clid,int  *nsubcl,char *cllab);
void access_cdat_(ErsCells *cdata, int  *ncell, int  *ipntr,
     int  *nxcell, float *xcell, float *zcell);
int  access_cells(ErsCells *cdata,int  *ncell,int  **id,int  **ipntr,
     int  **nxcell,float **xcell,float **zcell);
void access_mdat_(ErsModel *model, int  *rdof, int  *nvcrd,
     int  *nmats, int  *rid, int  *matpntr, int  *matcntr,
     int  *hflag, float *xv, float *zv, float *pv);
int  access_mats(ErsModel *model, int  *rdof, int  *nvcrd,
     int  *nmats, int  **rid, int  **mpntr, int  **mcntr,
     int  **hflag, float **xv, float **zv, float **pv);
void access_mlim_(ErsModel *model, float *xmin, float *xmax,
     float *zmin, float *zmax, float *ymin, float *ymax);
void access_glim_(ErsModel *model,
                  int  *n1, float *o1, float *d1,
                  int  *n2, float *o2, float *d2,
                  int  *n3, float *o3, float *d3);


#ifdef __cplusplus  
}                   // for C++
#endif


#endif
