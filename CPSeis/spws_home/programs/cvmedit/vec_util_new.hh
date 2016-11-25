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
#ifndef VECTOR_LIST_UTIL_HH
#define VECTOR_LIST_UTIL_HH

#include "vect/ll_seis_vect.hh"
#include "oprim/modbase_data.hh"
#include "transform.h"

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
void    KillVectorList(void *vlist, void *sp);
int     VectorListCount(void *list);
int     VectorListPointCount(void *list);
void    VectorListSetVis(void *list,int vis);
void    VectorListSetVisByID(void *list,int id,int vis);
void    VectorListSetVisByName(void *list,char *name,int vis);
void    VectorListResetID(void *list,int old_id,int new_id);
void    VectorListResetLabel(void *list,int old_id,int new_id);
int     VectorList3DTransform(void *list, ErsTransforms *tdata,
        char *xname, char *yname, char *zname);
Vector *VectorListAdd(VectorLinkedList *vll,char *hname,char *cname,
        int phd, int shd, int thd, int zeit,
        int id, float xo, float yo, float zo,
        int n, float *x,float *y, float *z);
int     VectorListMaccess(void *list, int *rdof, int *nvcrd,
          int *nmats, int **rid, int **mpntr, int **mcntr,
          int **hflag, float **x,float **y, float **z, float **pv);
int     VectorListMreset(void *vlm, int rdof, int nvcrd,
          int nmats, int *rid, int *mpntr, int *mcntr,
          int *hflag, float *x, float *y, float *z, float *pv);
int     VectorListCaccess(void *list, int *ncell,int **cell_ids,
          int **ipntr,int **icntr,float **x,float **y,float **z);
int     VectorListCPaccess(void *list,int *nump,int **id,
          float **xp,float **yp, float **zp);
int     VectorListSaccess(void *list, int *nvec,
          int **ipntr,int **icntr,float **x,float **y,float **z);


#ifdef __cplusplus
}                   // for C++
#endif

#endif
