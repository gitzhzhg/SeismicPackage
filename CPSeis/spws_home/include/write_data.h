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
#ifndef Write_Data_
#define Write_Data_
#include "c2f_interface.h"
#include "tfio.h"
#include "wrdcnvrt.h"

#ifdef NEED_CAPITALS
#define write_datac_      WRITE_DATAC
#define write_dataf_      WRITE_DATAF
#define write_data_       WRITE_DATA
#define write_data_hist_  WRITE_DATA_HIST
#define write_data_trace_to_slice_ WRITE_DATA_TRACE_TO_SLICE
#endif

#if(VMS || _AIX || __hpux)
#define write_datac_      write_datac
#define write_dataf_      write_dataf
#define write_data_       write_data
#define write_data_hist_  write_data_hist
#define write_data_trace_to_slice_ write_data_trace_to_slice
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
 
int  write_datac_(TF_Global *g, int *ufi, int *cnt, int  *tpos,
     float *hd, int *nwih,
     float *tr, int  *ndpt,
     float *Clip, char *wrkbuff, int *wrksize);
int  write_dataf_(int *ufi, int * cnt, int  *tpos,
     WREAL *hd, int *nwih,
     WREAL *tr, int *ndpt,
     WREAL *Clip, char *wrkbuff, int *wrksize);
int  write_data_(TF_Global *g, int *lun, int  *trace_no,
     float *hd, float *tr, int  *ns);
int  write_data_trace_to_slice_(int *fd, TF_Global *g);
int  write_data_cubeslice(int fd, IO_request *ioreq,TF_Global *g,
     char *hd, char *tr);
int  write_data_tslice(int fd, TF_Global *g, int index, int n, char *slice);
int  write_data_hist_(TF_Global *g, char *hist_file );

#ifdef __cplusplus
}                   // for C++
#endif

#endif

