
/*!<CPS_v1 type="HEADER_FILE"/>*/
/*-------------------------------- rice.h ----------------------------------*/
/*-------------------------------- rice.h ----------------------------------*/
/*-------------------------------- rice.h ----------------------------------*/

                    /* other files are:  rice.c */

/****
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>

!<brief_doc>
!-------------------------------------------------------------------------------
!<center>                C P S   P R I M I T I V E               </center>
!
! Name       : rice.h
! Category   : miscellaneous
! Written    : 2000-10-02   by: Michael L. Sherrill
! Revised    : 2004-03-17   by: Randall L. Selzler
! Maturity   : production
! Purpose    : Rice consortium time frequency analysis. Original code
!              derived from the consortium
! Portability: None known
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author                 Description
!     ----        ------                 -----------
!  6. 2004-03-17  Randall L. Selzler     Added Wigner option.
!  5. 2001-07-25  Randall L. Selzler     Added CPS attribute option
!  4. 2001-05-02  Michael L. Sherrill    Fixed undefined prototype fftf77
!  3. 2001-02-01  Michael L. Sherrill    Renamed this file rice.h from
!                                        rice_timefreq.h to conform to new
!                                        file name convention
!  2. 2000-10-03  Michael L. Sherrill    Removed Linux warnings
!  1. 2000-10-02  Michael L. Sherrill    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _RICE_TIMEFREQ_H_
#define _RICE_TIMEFREQ_H_


#include "c2f_interface.h"


#ifdef NEED_CAPITALS
#define fftf77_ FFTF77
#endif

#if(VMS || _AIX || __hpux)
#define fftf77_ fftf77
#endif


#ifdef __cplusplus
extern "C" {
#endif


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/




                        /* Typedefs */

/* DATA structure for manipulating and storing the full TFR */
typedef struct {
  double *re;      /* Real data vector                  */
  double *im;      /* Imaginary data vecor              */
  int nt;          /* Size of data array's              */
  char isreal[5];  /* Format indicator (REAL/CPLX)      */
} tfr;

/* DATA structure to store the TFR associated parameters */
typedef struct {
  char *tfmode;    /* Which TF distribution             */
  char *mode;      /* Mode of operanda                  */
  char *scale;     /* "lin" or "dB"                     */ 
  char *window;    /* Window type specifyer             */
  char *correct;   /* */
  int nw;          /* Window length/support             */
  int fftlen;      /* Size of DFT (pow of 2)            */
  int mfft;        /* fftlen = 2**mfft                  */
  int fstep;       /* Frequency sampling                */
  int tstep;       /* Time sampling                     */
  double dBthr;    /* */
  double vol;      /* Volume of AOK kernel              */
  double *vect;    /* Window values                     */
} tfrpar;




                          /**** Macros ****/
#define rice_timefreq_dmax(A,B) ((double) A > (double) B ? (double) A : \
                          (double)  B)
#define rice_timefreq_dmin(A,B) ((double) A > (double) B ? (double) B : \
                          (double)  A)

#define rice_timefreq_ispowp(x,p)  isint(log10((double) x)/log10((double) p))
#define rice_timefreq_isodd(x)     (((double) x/2.0 - ceil((double ) x/2.0)) \
                                    == 0.0 ? 0 : 1)
#define isint(x)     ((x - floor(x)) > 0.0 ? 0 : 1)
/*   cmr: computes real part of x times y*/
#define rice_timefreq_cmr(xr,xi,yr,yi) (double) (xr*yr - xi*yi)
/*   cmi: computes imaginary part of x times y*/
#define rice_timefreq_cmi(xr,xi,yr,yi) (double) (xi*yr + xr*yi)
/*   ccmr: computes real part of x times y'*/
#define rice_timefreq_ccmr(xr,xi,yr,yi) (double) (xr*yr + xi*yi)
/*   ccmi: computes imaginary part of x times y*/
#define rice_timefreq_ccmi(xr,xi,yr,yi) (double) (xi*yr - xr*yi)
/*   logp:     */
#define rice_timefreq_logp(x,p) (double) (log10((double) x)/log10((double) p))




                          /**** Prototypes ****/

/* The main function, used by CBYT */
int    rice_timefreq (float *trace_in, int nsamp_in,
                      float *trace_out, 
                      float srval, int num_traces_in, char *kernal,
                      char *mode, char *window, char *scale, 
                      char *correct, int tlag, int fftlen, 
                      double dBthr, double vol);
/* The main function, shared by CBYT and CPS */
int rice_timefreq2 (float *trace_in, const int nsamp_in,
               float *trace_out, 
               float srval, int num_traces_in, char *kernal,
               int mode_cnt, char (*mode)[2*sizeof(int)],
               char *window, char *scale, 
               char *correct, int winlen, int fftlen, 
               double dBthr, double vol);
/* Raw time-frequency distribution as a seismic panel */
void rice_tfdist (
  tfr tf,
  tfrpar tfpar,
  int zerotr,
  int tstart,
  int aoksw1,
  int aoksw2,
  double eps,
  float *trace_out);
/* Instantaneous bandwidth */
void rice_bandwidth(
  tfr tf,
  tfrpar tfpar,
  int tstart,
  int aoksw1,
  int aoksw2,
  double nyquist,
  double dw,
  double tiny,
  float *trace_out);
/* Instantaneous center frequency */
void rice_centfreq(
  tfr tf,
  tfrpar tfpar,
  int tstart,
  int aoksw1,
  int aoksw2,
  double nyquist,
  double dw,
  double tiny,
  float *trace_out);
/* Instantaneous dominant frequency */
void rice_domfreq(
  tfr tf,
  tfrpar tfpar,
  int tstart,
  int aoksw1,
  int aoksw2,
  double nyquist,
  double dw,
  double tiny,
  float *trace_out);
/* Instantaneous peak/max frequency */
void rice_maxfreq(
  tfr tf,
  tfrpar tfpar,
  int tstart,
  int aoksw1,
  int aoksw2,
  double nyquist,
  double dw,
  double tiny,
  float *trace_out);
/* Instantaneous Q */
void rice_q(
  tfr tf,
  tfrpar tfpar,
  int tstart,
  int aoksw1,
  int aoksw2,
  double nyquist,
  double dw,
  double tiny,
  float *trace_out);
/* Adaptive optimized kernal */
void   rice_timefreq_aok(double *ixr, double *ixi, double *tf, int xlen, 
                         int tlag, int fftlen, int fstep, int tstep,
                         double vol, const char *tfmode);

/* Short time fourier transform */
void   rice_timefreq_stft(double *ixr, double *ixi, tfr *tf, tfrpar*win);

/* The f77 style wrapper around the CPS fft f90 primitive */
int    fftf77_(char *type, int *fftlen, int *sign, 
              float *buf1, float *buf2, int *size);

/* Miscellaneous internal functions */
int    rice_timefreq_po2(int n);
int    rice_timefreq_nextpowp(int n, int p);
double rice_timefreq_ssq(double* a, int n);
void   rice_timefreq_normalize(double* a, int nt, int nf);
void   rice_timefreq_correct_time(double* a, double* b, int nt, int nf);
void   rice_timefreq_correct_freq(double* a, double* b, int nt, int nf);
void   rice_timefreq_fft(int n, int m, double* x, double* y);
void   rice_timefreq_rhilbert(double* xr, double* xi, int xlen);
void   rice_timefreq_window(tfrpar *win);
void   rice_timefreq_cshift(int len, double *x);
void   rice_timefreq_kfill(int len, double k, double *x);
void   rice_timefreq_rectamake(int nlag, int nraf, double forget, double *rar,
                               double *rai,
                               double *rarN, double *raiN);
void   rice_timefreq_pthetamake(int nrad, int nphi,int nraf, double *ptheta,
                                int *maxrad);
void   rice_timefreq_plagmake(int nrad, int nphi, int nlag, double *plag);
void   rice_timefreq_rectopol(int nraf, int nlag, int nrad, 
                              int nphi, double *req, double *pheq);
void   rice_timefreq_rectrotmake(int nraf, int nlag, double outdelay,
                                 double *rectrotr, double *rectroti);
void   rice_timefreq_rectaf(double *xr, double *xi, int nlag, int nraf, 
                            double *rar, double *rai, double *rarN,
                            double *raiN, double *rectafr, double *rectafi);
void   rice_timefreq_sigupdate(int nrad, int nphi, int nits, double vol,
                               double mu0, int *maxrad, double *polafm2, 
                               double *sigma, double *wks);
void   rice_timefreq_mkmag2(int K, double *xr, double *xi, double *xm2);
void   rice_timefreq_polafint(int nrad, int nphi, int nraf, int *maxrad, 
                              int nlag, double *plag, double *ptheta,
                              double *rectafm2, double *polafm2);






/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/


#ifdef __cplusplus
}
#endif


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/





