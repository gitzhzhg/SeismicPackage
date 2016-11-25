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
#ifndef _RD_DATA_
#define _RD_DATA_

#include "c2f_interface.h"
#include "wrdcnvrt.h"
#include "cube.h"
#include "tfio.h"

#ifdef NEED_CAPITALS
#define read_dataf_ READ_DATAF
#endif
#if(VMS || _AIX || __hpux)
#define read_dataf_ read_dataf
#endif


#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* see read_data.c     */
/* Prototypes of functions */
int  read_dataf_(int *ufi, int *cnt, int *tpos,
        int *nwih, WREAL *hd,
        int *ndpt, WREAL *tr,
        char *wrkbuff, int *wrksize);
int  read_data_group(int *lun, char *name, IO_request *Cl,
     char *hd, char *tr, float *tasf,int mgrp,
     int hkey, float *gval, float eps);
int  read_data_types(TF_Global *g);
int  read_data_open(int *lun, char *name, TF_Global *g, char *msg);
int  read_data_wfix(int ntot, int nsamp, float *hd, 
     int wdi, char *bi, int wdo, char *bo);
int  read_data_translate(IO_request *ioreq, TF_Global *g,
           char *hd, char *tr, float *TASF);
void read_data_trot(IO_request *ioreq, TF_Global *g,
           char *hd, char *tr, float *tasf);
int  read_data_tfile(IO_request *ioreq, TF_Global *g,
           char *hd, char *tr,float *tasf);
void read_data_voxet(IO_request *ioreq, TF_Global *g,
           char *hd, char *tr,float *tasf);
void read_data_hgrid(IO_request *ioreq, TF_Global *g,
           char *hd, char *tr,float *tasf);
void read_data_segy(IO_request *ioreq, TF_Global *g,
           char *hd, char *tr,float *tasf);
int  read_data_fromdisk(int fd, IO_request *ioreq, TF_Global *g,
           char *hd, char *tr);
int  read_data_cubeslice(int fd, IO_request *ioreq, TF_Global *g,
           char *hd, char *tr);
int  read_data_hdslice_byname(char *fname, float *hd, int trnsps);
int  read_data_hdslice(int fd, TF_Global *g, float *hd, int trnsps);
int  read_data_buffer(int nwd);
void read_data_madj(int toggle);
int  read_dataMadj();
void read_trcio_data(char *name, int  *istat, IO_request *Cl,
                     TF_Global *Glbls, char *hd, char *tr, float *TASF,
                     int *lun, int open_file);
CubeTrcio *read_data_check_3d (TF_Global *Glbls);
#ifdef __cplusplus
}                   // for C++
#endif

#endif
