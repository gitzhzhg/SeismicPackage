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
/*!<CPS_v1 type="HEADER_FILE"/>*/
/*------------------------------- sine.h ----------------------------------*/
/*------------------------------- sine.h ----------------------------------*/
/*------------------------------- sine.h ----------------------------------*/

                    /* other files are:  sine.c */

/****

!<brief_doc>
!-------------------------------------------------------------------------------
!<center>                C P S   P R I M I T I V E               </center>
!
! Name       : sine.h
! Category   : miscellaneous
! Written    : 2000-12-11   by: Michael L. Sherrill
! Revised    : 2001-07-20   by: Michael L. Sherrill
! Maturity   : production   2002-02-21
! Purpose    : Rocky Hardy and Jon Gaston's time frequency analysis.
! Portability: None known
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author                  Description
!     ----        ------                  -----------
!  2. 2002-02-21  Michael L. Sherrill    Added error stats to prototypes.
!  1. 2001-02-01  Michael L. Sherrill    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _SINE_TIMEFREQ_H_
#define _SINE_TIMEFREQ_H_


#include "c2f_interface.h"


#define NR_END 1
#define FREE_ARG char*


#ifdef __cplusplus
extern "C" {
#endif


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/




                            /* Typedefs */




                           /**** Macros ****/
static float sqrarg;
#define SQR(a) ((sqrarg=(a)) == 0.0 ? 0.0 : sqrarg*sqrarg)
#define SWAP(a,b) itemp=(a);(a)=(b);(b)=itemp;
#define SWAPF(a,b) tempr=(a);(a)=(b);(b)=tempr;
#define SWAPG(a,b) {temp=(a);(a)=(b);(b)=temp;}
#define SWAPH(a,b) {swap=(a);(a)=(b);(b)=swap;}

#define M 7
#define NSTACK 50
#define NINT(a) (  (a) >= 0  ? (long)((a) + 0.5) : (long)((a) - 0.5)  )

                          /**** Prototypes ****/

/* The main function */
int   sine_timefreq(char *mode, int icomp, int iopt, float tlen, float srval, 
                    int nsamp, float *tr_in, float *tr_out); 

/* Miscellaneous internal functions */
void sine_timefreq_fitsin(float x, float a[], float *y, float dyda[], int na);
void sine_timefreq_avgvar(float data[], unsigned long n, float *ave, 
                          float *var, float *max, float *min);
void sine_timefreq_linear_interp(float x[], float y[], int n, 
                                 float xi, float *yi);
void sine_timefreq_inter_linear_trace(float y[], int npt, float yi[], int lnpt);
int  sine_timefreq_inter_spline_trace(float trace[], int npts, float tracei[], 
                                      int lpts);
int  sine_timefreq_sin_extract(float tracei[],float a[]);
int  sine_timefreq_sort4(unsigned long n, float ra[], float rb[], 
                         float rc[], float rd[]);

float *sine_timefreq_vector(long nl, long nh);
int *sine_timefreq_ivector(long nl, long nh);
unsigned long *sine_timefreq_lvector(long nl, long nh);
float **sine_timefreq_matrix(long nrl, long nrh, long ncl, long nch);
void sine_timefreq_free_vector(float *v, long nl, long nh);
void sine_timefreq_free_ivector(int *v, long nl, long nh);
void sine_timefreq_free_lvector(unsigned long *v, long nl, long nh);
void sine_timefreq_free_dvector(double *v, long nl, long nh);
void sine_timefreq_free_matrix(float **m, long nrl, long nrh, long ncl, 
                               long nch);
int  sine_timefreq_spline(float x[], float y[], int n, float yp1, 
                          float ypn, float y2[]);
int  sine_timefreq_splint(float xa[], float ya[], float y2a[], int n,
                          float x, float *y);
void sine_timefreq_realft(float data[], unsigned long n, int isign);
int  sine_timefreq_indexx(unsigned long n, float arr[], unsigned long indx[]);
int  sine_timefreq_mrqmin(float x[], float y[], float sig[], int ndata, 
        float a[], int ia[], int ma, float **covar, float **alpha, float *chisq,
        void (*funcs)(float, float [], float *, float [], int), float *alamda);
void sine_timefreq_four1(float data[], unsigned long nn, int isign);
int  sine_timefreq_gaussj(float **a, int n, float **b, int m);
int  sine_timefreq_mrqcof(float x[], float y[], float sig[], int ndata,
        float a[], int ia[], int ma, float **alpha, float beta[], float *chisq,
        void (*funcs)(float, float [], float *, float [], int));
void sine_timefreq_covsrt(float **covar, int ma, int ia[], int mfit);

/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/


#ifdef __cplusplus
}
#endif



/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/


















#endif /* _NR_MISC_H_ */
