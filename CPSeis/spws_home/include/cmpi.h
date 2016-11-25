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
#ifndef _CMPI_
#define _CMPI_

#include "c2f_interface.h"

#define _MPI_EXIST

#ifdef NEED_CAPITALS
#define  cmpi_init_          CMPI_INIT
#define cmpi_finalize_       CMPI_FINALIZE
#define cmpi_i_pel_          CMPI_I_PEL
#define cmpi_n_pel_          CMPI_N_PEL
#define cmpi_barrier_        CMPI_BARRIER
#define cmpi_wtime_          CMPI_WTIME
#define cmpi_bcast_r_        CMPI_BCAST_R
#define cmpi_bcast_i_        CMPI_BCAST_I
#define cmpi_all_reduce_r_   CMPI_ALL_REDUCE_R
#define cmpi_all_reduce_i_   CMPI_ALL_REDUCE_I
#define cmpi_reduce_r_       CMPI_REDUCE_R
#define cmpi_reduce_i_       CMPI_REDUCE_I
#define cmpi_abort_          CMPI_ABORT
#define cmpi_allintmax_      CMPI_ALLINTMAX
#define cmpi_send_i_         CMPI_SEND_I
#define cmpi_recv_i_         CMPI_RECV_I
#endif

#if(VMS || _AIX || __hpux)
#define cmpi_init_           cmpi_init
#define cmpi_finalize_       cmpi_finalize
#define cmpi_i_pel_          cmpi_i_pel
#define cmpi_n_pel_          cmpi_n_pel
#define cmpi_barrier_        cmpi_barrier
#define cmpi_wtime_          cmpi_wtime
#define cmpi_bcast_r_        cmpi_bcast_r
#define cmpi_bcast_i_        cmpi_bcast_i
#define cmpi_all_reduce_r_   cmpi_all_reduce_r
#define cmpi_all_reduce_i_   cmpi_all_reduce_i
#define cmpi_reduce_r_       cmpi_reduce_r
#define cmpi_reduce_i_       cmpi_reduce_i
#define cmpi_abort_          cmpi_abort
#define cmpi_allintmax_      cmpi_allintmax
#define cmpi_send_i_         cmpi_send_i
#define cmpi_recv_i_         cmpi_recv_i

#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* see cmpi.f    */
int cmpi_init_();
int cmpi_finalize_();
int cmpi_i_pel_();
int cmpi_n_pel_();
int cmpi_barrier_();
float cmpi_wtime_();
void cmpi_bcast_r_(int *i_pel,int *nx_inp,float *x_inp);
void cmpi_bcast_i_(int *i_pel,int *nx_inp,int *x_inp);
void cmpi_all_reduce_r_(int *n,float *r, float *w);
void cmpi_all_reduce_i_(int *n,int *r, int *w);
void cmpi_reduce_r_(int *i,int *n,float *r, float *w);
void cmpi_reduce_i_(int *i,int *n,int *r, int *w);
void cmpi_abort_(int *err);
int  cmpi_allintmax_(int *x);

void cmpi_send_i_(void *buff,int *num,int *dest,int *tag, int *ierr);
void cmpi_recv_i_(void *buff,int *num,int *source,int *tag,int *status, int *ierr);


#ifdef __cplusplus
}                   // for C++
#endif

#endif

