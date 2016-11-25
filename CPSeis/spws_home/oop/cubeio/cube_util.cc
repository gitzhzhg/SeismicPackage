#include <string.h>
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
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include "tfdefs.h"
#include "tf3d.h"
#include "tf_global.h"
#include "put_global.h"
#include "cubeio/cubeio.hh"
#include "cubeio/cube_slice.hh"


#ifdef __cplusplus
extern "C" {                 // for C++
#endif
int  cube_util_get_slice(char *hname, IO_request *ioreq, TF_Global *g,
     char *hd, char *tr);
#ifdef __cplusplus
}                   // for C++
#endif

int  cube_util_get_slice(char *hname, IO_request *ioreq, TF_Global *g,
     char *hd, char *data) {
  int NOTOK=0, ISOK=1;
  int ftype,axis,index, nhdr, i;
  float cpshd[64];
  CubeIO *cub=0;
  CubeSlice *cslice=0;

  if(!hname) return NOTOK;
  if(!g )   return NOTOK;
  if(tf_global_is3d(g) ==0) return NOTOK;
  ftype = getgl_str_to_ftype(tf_global_ftype(g));
  if(ftype != BRICK_TYPE) return NOTOK;
  axis = ioreq->axis;
  index = ioreq->index;
  if(axis<1 || axis>3) return NOTOK;

 // open cube for reading
 cub = new CubeIO(hname,true);
 if(!cub->isopen())  { delete cub; return NOTOK; }
 // grab the requested slice
 cslice = cub->slice_rd(axis,index);
 if(!cslice) { delete cub; return NOTOK; }
 // copy data to the trace array
 memcpy(data,cslice->data(),cslice->esize()*cslice->size());

 // set header information
 nhdr = cslice->length(2);
 for(i=0;i<64;i++) cpshd[i] = 0.;
 for(i=0;i<nhdr;i++) {
  memcpy(hd+256*i,cpshd,256);
 }
 delete cub;
 return ISOK;
}

