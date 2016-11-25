/****
!<CPS_v1 type="AUXILIARY_FILE"/>
****/
/*----------------------------- ibsma_crou.c ------------------------------*/
/*----------------------------- ibsma_crou.c ------------------------------*/
/*----------------------------- ibsma_crou.c ------------------------------*/

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
!                   C P S   P R I M I T I V E   F I L E
!
! Name       : IBSMA_CROU
! Category   : miscellaneous
! Written    : 2003-08-14   by: B. Lucas
! Revised    : 2007-11-13   by: B. Menger
! Maturity   : beta
! Purpose    : Multiple prediction.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
! 16. 2007-11-13  B. Menger  Added a function to separate writing the original
!                            data from preparation of the data for the SMA. This
!                            allows us to add an optional mute function between
!                            writing the original data and calculating the 
!                            multiples.  
! 15. 2006-01-03  Stoeckley  Mor fixes so will compile with C++.
! 14. 2005-05-31  Stoeckley  Fix so will compile with C++.
! 13. 2004-09-07  B. Lucas   Removed a seek command on the 'input_' file that
!                            was still being called after deletion. This was
!                            not causing any errors, but simply printed a
!                            message saying it could not.
! 12. 2004-08-23  B. Lucas   Added close to 'input_' file before delete.
! 11. 2004-08-18  B. Lucas   Changed code so that 'input_' file is deleted
!                            immediately after prediction is completed.
!                            Also changed MemScalar to 1.5 if it comes in as 2.
! 10. 2004-08-05  Y. Shen    change overall amplitude scaling of predicted 
!                            multiples
!  9. 2004-05-04  B. Lucas   Switched MasterMem size to 100 from 110.
!                            Fixed memory allocation bug.
!  8. 2004-04-16  B. Lucas   Added print statement to dataprep stage.
!                            Fixed freeing of "trorig" array bug.
!                            Fixed error checking for 1 overlooked pfio_write.
!  7. 2004-02-25  B. Lucas   Revised error checking on pfio read/writes.
!                            Switched off_t sizes to long-long.
!  6. 2004-02-17  B. Lucas   Added print statement in prediction loop.
!                            Fixed double-negative in output3 routine.
!                            Added datastore step to data preparation.
!  5. 2004-01-05  Y. Shen    Modified two amplitude scaling factors
!  4. 2003-12-12  Y. Shen    added general description and advice for users
!  3. 2003-11-20  B. Lucas   Added MEMORY_SIZE and fixed scaling problem.
!  2. 2003-11-10  B. Lucas   Revised version.
!  1. 2003-08-14  B. Lucas   Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


char IBSMA_IDENT[100] =
"$Id: ibsma_crou.c,v 1.16 2007/11/13 16:37:57 Menger beta sps $";

#include "c2f_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>

#include "unix.h"
#include "pfio.h"

extern int signgam;
/*
int __allmul;
*/
extern int __allmul;

/*---------------------- fortran spelling adjustments ----------------------*/
/*---------------------- fortran spelling adjustments ----------------------*/
/*---------------------- fortran spelling adjustments ----------------------*/

#ifdef NEED_UNDERSCORE
#define ibsma_cdiv      ibsma_cdiv_
#define ibsma_initx     ibsma_initx_
#define ibsma_scale     ibsma_scale_
#define ibsma_datastore ibsma_datastore_
#define ibsma_write_original ibsma_write_original_
#define ibsma_dataprep0 ibsma_dataprep0_
#define ibsma_dataprep1 ibsma_dataprep1_
#define ibsma_dataprep2 ibsma_dataprep2_
#define ibsma_dataprep3 ibsma_dataprep3_
#define ibsma_predict   ibsma_predict_
#define ibsma_output1   ibsma_output1_
#define ibsma_output2   ibsma_output2_
#define ibsma_output3   ibsma_output3_
#define ibsma_output4   ibsma_output4_
#define ibsma_main      ibsma_main_
#define ibsma_fftsetup  ibsma_fftsetup_
#define ibsma_dumpparms ibsma_dumpparms_
#define ibsma_pfarc     ibsma_pfarc_
#define ibsma_pfacr     ibsma_pfacr_
#define ibsma_pfacc     ibsma_pfacc_

#elif defined NEED_CAPITALS
#define ibsma_cdiv      IBSMA_CDIV
#define ibsma_initx     IBSMA_INITX
#define ibsma_scale     IBSMA_SCALE
#define ibsma_datastore IBSMA_DATASTORE
#define ibsma_write_original IBSMA_WRITE_ORIGINAL
#define ibsma_dataprep0 IBSMA_DATAPREP0
#define ibsma_dataprep1 IBSMA_DATAPREP1
#define ibsma_dataprep2 IBSMA_DATAPREP2
#define ibsma_dataprep3 IBSMA_DATAPREP3
#define ibsma_predict   IBSMA_PREDICT
#define ibsma_output1   IBSMA_OUTPUT1
#define ibsma_output2   IBSMA_OUTPUT2
#define ibsma_output3   IBSMA_OUTPUT3
#define ibsma_output4   IBSMA_OUTPUT4
#define ibsma_main      IBSMA_MAIN
#define ibsma_fftsetup  IBSMA_FFTSETUP
#define ibsma_dumpparms IBSMA_DUMPPARMS
#define ibsma_pfarc     IBSMA_PFARC
#define ibsma_pfacr     IBSMA_PFACR
#define ibsma_pfacc     IBSMA_PFACC
#endif

#define NWIN     20                  /* Max time shift window in # of 
                                        input trace samples */
#define NTAPE    10                  /* Tape length */
#define SU_NFLTS 32768               /* Arbitrary limit on array size */
#define LOOKFAC  2                   /* Look ahead factor for npfao */
#define PFA_MAX  720720              /* Largest allowed nfft */
#define PIBY2    1.57079632679490    /* Value of PI/2 */
#define FSLOPE   8.0                 /* In Hz. */
#define NINT(x) ((int)((x)>0.0?(x)+0.5:(x)-0.5))
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define P120 0.120536680
#define P142 0.142314838
#define P173 0.173648178
#define P222 0.222520934
#define P239 0.239315664
#define P281 0.281732557
#define P342 0.342020143
#define P354 0.354604887
#define P382 0.382683432
#define P415 0.415415013
#define P433 0.433883739
#define P464 0.464723172
#define P540 0.540640817
#define P559 0.559016994
#define P568 0.568064747
#define P587 0.587785252
#define P623 0.623489802
#define P642 0.642787610
#define P654 0.654860734
#define P663 0.663122658
#define P707 0.707106781
#define P748 0.748510748
#define P755 0.755749574
#define P766 0.766044443
#define P781 0.781831482
#define P822 0.822983866
#define P841 0.841253533
#define P866 0.866025404
#define P885 0.885456026
#define P900 0.900968868
#define P909 0.909631995
#define P923 0.923879533
#define P935 0.935016243
#define P939 0.939692621
#define P951 0.951056516
#define P959 0.959492974
#define P970 0.970941817
#define P974 0.974927912
#define P984 0.984807753
#define P989 0.989821442
#define P992 0.992708874
#define NFAX 10

/* definition moved from dot.h fril */
#define  STRING_LENGTH     200
#define  NBYTES            8
#define  NPAD              100       /* padding length for FFT */
#define  NTESTSHOT         100       /* for test run, the max # of shot */
#define  FSLOPE            8.0       /* fixed slope for bpfilter (hz) */
#define  FMIN              0.0       /* lower end of freq. (hz) */
#define  MAXORDER          5         /* max order of multiple to be predicted */
#define  MasterMem  100*1000*1000    /* in 8 bytes */

#ifndef  PI
#define  PI 3.141592653589793
#endif

#ifndef  Max 
#define  Max(a,b) ((a)>(b)?(a):(b))
#endif

#ifndef  Min 
#define  Min(a,b) ((a)>(b)?(b):(a))
#endif

#ifndef  NINT
#define  NINT(x) ((int)((x)>0.0?(x)+0.5:(x)-0.5))
#endif

#ifndef CHARACTER
#define CHARACTER char
#endif

/* Structure statements */
typedef struct COMPLEX {
  float r;
  float i;
} complex;

typedef struct TRACEINFO {
  char  INPUT_SEGY[STRING_LENGTH];      
  char  OUTPUT_SEGY[STRING_LENGTH];    
  char  WORKDIR[STRING_LENGTH];
  int   dtype;
  int   nshottotal;
  int   nshot;
  int   nrecv;
  float dshot;
  float drecv;
  int   nt;
  float dt;
  float t0;
  int   totalbadtrs;
} TraceInfo;

typedef struct FFTINFO {
  float fmax;
  int   ntfft;
  int   nwmin;
  int   nwmax;
  int   nw;
  int   nnw;
  int   nwuse;
  int   nwcorr;
  float dw;
  int   nxfft;
  float dkx;
} FftInfo;

typedef struct PREDICTION {
  int   test;
  int   maxorder;
  int   restart;
  int   ntestshot;
  int   ntestbeg;
  float vwater;
  float whitenoise;
  float mltp_scale;
  float scale;
} Prediction;

typedef struct DPREP {
  int   groupsize;
  int   ngroup;
  int   grouplast;
} DPrep;

typedef struct OUTPUT {
  int   nshotBeg;
  int   nshotEnd;
  int   totshot;
  int   shotnum;
  int   shotnread;
  int   shotlast;
} OutPut;

/* Define static variables. */
static TraceInfo  trsinfo;
static FftInfo    fftinfo;
static Prediction predict;
static DPrep      dprep;
static OutPut     output;
static char filename0[STRING_LENGTH];
static char filename1[STRING_LENGTH];
static char filename2[STRING_LENGTH];
static char filename3[STRING_LENGTH];
static char filename4[STRING_LENGTH];
static char hostname[120];
static int nwminG;
static int nwmaxG;
static int shift1;
static int shift2;
static int shift3;
static int group;
static int groupsize;
static int nhead;
static int badtrs;
static int nxffthalf;
static int nwcorrhalf;
static int nw1hz;
static int niter;
static int cleanup;
static int storecnt;
static float dwov2;
static float tape[NTAPE+1];
static float scalard;
static float MemScalar;

/* Arrays used for preparing data. */
static double  *trhead;
static float   *trorig;
static float   *trace;
static float   *filter;
static float   *rt;
static complex *ct;
static complex *data_out;
static int     *nwlow;
static int     *nwhigh;

/* Arrays used for predicting multiples. */
static float   *kx2;
static float   *obliq;
static complex *cx1;
static complex *D0_obq;
static complex *D0;
static complex *multip;
static complex *multip_save;
static complex *data_in;
static complex *fsigfinal;
static complex *cwork;
static complex *fsig; 

/* Arrays used for estimate source signal. */
static int     *itdelay;
static float   *epsa;
static complex *cauto;
static complex *ccros;
static int     ofd_save;
static int     ofd_data;
static int     ofd_head;
static int     ofd_mltp;
static int     ofd_orig;

#ifdef __cplusplus
extern "C" {
#endif


/* Prototype statements. */
void ibsma_Hann_window(float *filter, int iw3);
void ibsma_polygonalFilter(float *f, float *amps,
                     int npoly, int nfft, float dt, float *filter);
int ibsma_npfa (int nmin);
int ibsma_npfao (int nmin, int nmax);
int ibsma_npfaro (int nmin, int nmax);
void ibsma_fftsetup(int *ier, int nt, int blocklength, int npad,
              int *ntfft, int *nwcorr, int *nxfft);
int ibsma_lpfilter(float fmax,  float dt, float dw,
             int ntfft, float *filter);
void ibsma_dumpparms(
               TraceInfo  trsinfo,
               FftInfo    fftinfo,
               Prediction predict,
               DPrep      dprep,
               OutPut    output);
void ibsma_pfarc (int isign, int n, float rz[], complex cz[]);
void ibsma_pfacr (int isign, int n, complex cz[], float rz[]);
void ibsma_pfacc (int isign, int n, complex cz[]);

void ibsma_initx (INTEGER *ier, CHARACTER *wrkdr,
        INTEGER *memsize, INTEGER *cable,
        INTEGER *restart, REAL *scale, INTEGER *test, 
        INTEGER *ntestshot, INTEGER *ntestbeg, INTEGER *wmin, 
        INTEGER *wmax, INTEGER *dtype, INTEGER *nshottotal, 
        INTEGER *nrecv, REAL *dshot, REAL *drecv, REAL *fmax, 
        INTEGER *maxorder, REAL *vwater, REAL *mltp_scale, 
        INTEGER *nt, REAL *dt, REAL *t0, INTEGER *nwih,
        INTEGER *ngroups, INTEGER *groupsize0, INTEGER *groupsize1,
        INTEGER *nshotgs, INTEGER *shotgsize0, INTEGER *shotgsize1);
void ibsma_scale (INTEGER *ier, REAL *scale);
void ibsma_datastore (INTEGER *ier, DOUBLE *header, REAL *trace,
                      INTEGER *trndx);
void ibsma_write_original(INTEGER *ier, REAL *trace);
void ibsma_dataprep0 (INTEGER *ier, DOUBLE *header, REAL *trace,
                      INTEGER *trndx);
void ibsma_dataprep1 (INTEGER *ier, DOUBLE *header, REAL *trace,
                      INTEGER *trndx);
void ibsma_dataprep2 (INTEGER *ier);
void ibsma_dataprep3 (INTEGER *ier);
void ibsma_output1 (INTEGER *ier);
void ibsma_output2 (INTEGER *ier, INTEGER *shotg, INTEGER *shot, 
                    INTEGER *recv, DOUBLE *hd, REAL *tr);
void ibsma_output3 (INTEGER *ier, INTEGER *shotg, INTEGER *shot, 
                    INTEGER *recv, DOUBLE *hd, REAL *tr);
void ibsma_output4 (INTEGER *ier);
void ibsma_predict (INTEGER *ier);

/*--------------------------- ibsma_cdiv ------------------------------*/
/*--------------------------- ibsma_cdiv ------------------------------*/
/*--------------------------- ibsma_cdiv ------------------------------*/
void ibsma_cdiv (complex a, complex b, complex c)
{
   /* Perform complex number division */
   /* Copied from CWP SeismicUnix-34.6/src/cwp/lib/complex.c */
   float r,den;
   if (fabs(b.r) >= fabs(b.i)) {
      r = b.i/b.r;
      den = b.r+r*b.i;
      c.r = (a.r+r*a.i)/den;
      c.i = (a.i-r*a.r)/den;
   } else {
      r = b.r/b.i;
      den = b.i+r*b.r;
      c.r = (a.r*r+a.i)/den;
      c.i = (a.i*r-a.r)/den;
   }
   return;
}


/*--------------------------- ibsma_initx -----------------------------*/
/*--------------------------- ibsma_initx -----------------------------*/
/*--------------------------- ibsma_initx -----------------------------*/
void ibsma_initx (INTEGER *ier, CHARACTER *wrkdr,
        INTEGER *memsize, INTEGER *cable,
        INTEGER *restart, REAL *scale, INTEGER *test, INTEGER *ntestshot, 
        INTEGER *ntestbeg, INTEGER *wmin, INTEGER *wmax, INTEGER *dtype,
        INTEGER *nshottotal, INTEGER *nrecv, REAL *dshot, REAL *drecv, 
        REAL *fmax, INTEGER *maxorder, REAL *vwater, REAL *mltp_scale, 
        INTEGER *nt, REAL *dt, REAL *t0, INTEGER *nwih, INTEGER *ngroups,
        INTEGER *groupsize0, INTEGER *groupsize1, INTEGER *nshotgs, 
        INTEGER *shotgsize0, INTEGER *shotgsize1)
{

   /* Local variables. */
   int nfatal = 0;
   int itemp;
   int size;
   int isDefault = 0;
   int pid = 0;
   float temp;
   int imem;

   /* Initialization section. */
   *ier = 0;

   imem = (int)*memsize;
   if(imem == 2) {
      MemScalar = 1.5;
   } else {
      MemScalar = (float)*memsize;
   }

   badtrs = 0;
   niter = 2;        
   nhead = *nwih;
   storecnt = 0;
   /*
   nwminG = *wmin;
   nwmaxG = *wmax;
   */
   unix_get_hostname_c(hostname,sizeof(hostname));

   /* Read and parse input parameters. */
   if(!strcasecmp(wrkdr, "default") ) {
      sprintf(trsinfo.WORKDIR, "~/cpstemp/");
      isDefault = 1;
      cleanup = 1;
   } else {
      sprintf(trsinfo.WORKDIR, "%s", wrkdr);
      cleanup = 0;
   }
   trsinfo.dtype = *dtype;
   trsinfo.nshottotal = *nshottotal;
   trsinfo.nrecv = *nrecv;
   trsinfo.dshot = *dshot;
   trsinfo.drecv = *drecv;
   trsinfo.nt = *nt;
   trsinfo.dt = *dt;
   trsinfo.t0 = *t0;
   trsinfo.totalbadtrs = 0;
   fftinfo.fmax = *fmax;
   /*
   fftinfo.nwmin = nwminG;
   fftinfo.nwmax = nwmaxG;
   */
   predict.test = *test;
   predict.maxorder = *maxorder;
   predict.restart = *restart;
   predict.ntestshot = *ntestshot;
   predict.ntestbeg = *ntestbeg;
   predict.vwater = *vwater;
   predict.mltp_scale = *mltp_scale;
   predict.scale = *scale;

   trsinfo.nshot = 0;
   fftinfo.ntfft = 0;
   fftinfo.nw = 0;
   fftinfo.nnw = 0;
   fftinfo.nwuse = 0;
   fftinfo.nwcorr = 0;
   fftinfo.dw = 0;
   fftinfo.nxfft = 0;
   fftinfo.dkx = 0;

   if(*test) {
      itemp = predict.ntestshot + 2*trsinfo.nrecv - 2;
      trsinfo.nshot = Min(itemp, trsinfo.nshottotal);
   } else {
      trsinfo.nshot = trsinfo.nshottotal;
   }

   /* If there have been any fatal errors, quit the job. */
   if(nfatal > 0) {
      printf("Initialize: Fatal errors = %d\n", nfatal);
      *ier = 1;
      return;
   }

   /* FFT dimension setup. */
   ibsma_fftsetup(ier, trsinfo.nt, trsinfo.nshot, NPAD,
      &(fftinfo.ntfft), &(fftinfo.nwcorr), &(fftinfo.nxfft));

   /* scale related to temporal and spatial FFT which should
      be removed from final result */
   temp = 1024.0/fftinfo.ntfft;
   scalard = (1.0*fftinfo.nxfft/512.0)*temp;
   scalard *= (2.0*PI)*(2.0*PI);

   /* Determine dimensions. */
   /* nw  : the # of freq. from zero to nyquist. */
   /* nnw : the # of freq. from zero to fmax. */
   nxffthalf = fftinfo.nxfft/2;
   nwcorrhalf = fftinfo.nwcorr/2;
   fftinfo.nw = fftinfo.ntfft/2 + 1;
   fftinfo.dw = 2.0*PI/(fftinfo.ntfft*trsinfo.dt);
   fftinfo.nnw = Max(0,Min(fftinfo.nw-1,
   NINT(2.0*PI*fftinfo.fmax/fftinfo.dw)));
   fftinfo.dkx = 2.0*PI/(trsinfo.drecv*fftinfo.nxfft);
   nw1hz = fftinfo.nnw/fftinfo.fmax;
   nwminG = nw1hz * *wmin;
   nwmaxG = nw1hz * *wmax;
   fftinfo.nwmin = nwminG;
   fftinfo.nwmax = nwmaxG;

   /* Define frequency range. */
   if(predict.test) {
      nwminG = fftinfo.nnw/fftinfo.fmax;
      nwmaxG = fftinfo.nnw;
      fftinfo.nwmin = nwminG;
      fftinfo.nwmax = fftinfo.nnw;
      fftinfo.nwuse = nwmaxG - nwminG;
   } else {
      nwminG = fftinfo.nwmin;
      nwmaxG = fftinfo.nwmax;
      fftinfo.nwuse = nwmaxG - nwminG;
   }

   /* Calculate constant dwov2{=(dw/vwater)*(dw/vwater)}. */
   dwov2 = (fftinfo.dw/predict.vwater)*(fftinfo.dw/predict.vwater);

   /* Determine how many shots can be hold in */
   /* memory at once for preparing input data */
   itemp = (int)(MasterMem*NBYTES*MemScalar)
             /* total memory available */
        - trsinfo.nt*sizeof(float)
             /* float   trace[nt] */
        - fftinfo.nw*sizeof(float)
             /* float   filter[nw] */
        - fftinfo.ntfft*sizeof(float)
             /* float   rt[ntfft] */
        - fftinfo.nw*sizeof(complex);
             /* complex ct[nw] */

   size = trsinfo.nrecv*fftinfo.nwuse*sizeof(complex)
             /* data_out */
        + trsinfo.nrecv*nhead*sizeof(double);
             /* trhead */

   dprep.groupsize = itemp/size; 
   dprep.ngroup = (trsinfo.nshot+dprep.groupsize-1)/dprep.groupsize;
   dprep.grouplast = trsinfo.nshot - (dprep.ngroup-1)*dprep.groupsize;
   if(dprep.ngroup == 1) dprep.groupsize = dprep.grouplast;

   *ngroups = dprep.ngroup;
   *groupsize0 = dprep.groupsize;
   *groupsize1 = dprep.grouplast;

   /* Check whether the memory can hold a complete */
   /* frequency slice for predicting multiples.    */
   size = trsinfo.nshot*fftinfo.nxfft;
   itemp = (int)(MasterMem*NBYTES*MemScalar)
              /* total memory available */
         - (nxffthalf+1)*sizeof(float)
              /* float   kx2[nxfft/2+1] */
         - (nxffthalf+1)*sizeof(float)
              /* float   obliq[nxfft/2+1] */
         - fftinfo.nxfft*sizeof(complex)                
              /* complex cx1[nxfft] */
         - size*sizeof(complex)                         
              /* complex D0_obq[nshot*nxfft] */
         - size*sizeof(complex)                         
              /* complex D0[nshot*nxfft] */
         - trsinfo.nshot*trsinfo.nrecv*sizeof(complex)
              /* complex cwork[nshot*nrecv] */
         - trsinfo.nshot*trsinfo.nrecv
              /* complex multip[nshot*nrecv*maxorder] */
         * predict.maxorder*sizeof(complex);

   if(itemp < 0) {
      printf("Initialize: Memory too small to predict multiple.\n");
      *ier = 2;
      return;
   }

   /* Determine the begin, the end, and how */
   /* many shots need to be processed.      */
   if(predict.test) {
      output.nshotBeg = trsinfo.nrecv-1;
      output.nshotEnd = trsinfo.nrecv-1 + predict.ntestshot;
      output.totshot  = output.nshotEnd - output.nshotBeg;
   } else {
      output.nshotBeg = 0;
      output.nshotEnd = trsinfo.nshot;
      output.totshot  = output.nshotEnd - output.nshotBeg;
   }

   /* Determine how many shots can be processed */
   /* at once for output segy result.           */
   itemp = (int)(MasterMem*NBYTES*MemScalar)
              /* total memory available */
         - trsinfo.nt*sizeof(float)                        
              /* float   trace[nt] */
         - fftinfo.nwcorr*sizeof(float)                    
              /* float   rt[nwcorr] */
         - fftinfo.nwcorr*sizeof(complex)                  
              /* complex ct[nwcorr] */
         - fftinfo.nwuse*sizeof(complex)                   
              /* complex fsig[nwuse] */
         - predict.maxorder*fftinfo.nwuse*sizeof(complex)  
              /* complex fsigfinal[nwuse*maxorder] */
         - fftinfo.nwuse*sizeof(complex)                 
              /* complex ccros[nwuse] */
         - trsinfo.nt*sizeof(float);
              /* float   trorig[nt] */
   size = sizeof(int)                                        
           /* int     itdelay[?] */
         + sizeof(float)                                   
           /* float   epsa[?] */
        + fftinfo.nwuse*sizeof(complex)                   
           /* complex cauto[?*nwuse] */
        + trsinfo.nrecv*nhead*sizeof(double)              
           /* double  trhead[?*nrecv*nhead] */
        + fftinfo.nwuse*trsinfo.nrecv*sizeof(complex)     
           /* complex data_in[?*nwuse*nrecv] */
        + predict.maxorder*fftinfo.nwuse*trsinfo.nrecv    
           /* complex multip[?*maxorder*nwuse*nrecv] */
        * sizeof(complex);

   output.shotnum = itemp/size;
   output.shotnread = (output.totshot+output.shotnum-1)/output.shotnum;
   output.shotlast = output.totshot - (output.shotnread-1)*output.shotnum;
   if(output.shotnread == 1) output.shotnum = output.shotlast;

   *nshotgs    = output.shotnread;
   *shotgsize0 = output.shotnum;
   *shotgsize1 = output.shotlast;

   /* Dump parameters. */
   ibsma_dumpparms(trsinfo, fftinfo, predict, dprep, output);

   /* Set filename. */
   pid = getpid();
   if(isDefault) {
      sprintf(filename0,"%s/ibsma.%d.store_%s_%d",
      trsinfo.WORKDIR,*cable, hostname, pid);
      sprintf(filename1,"%s/ibsma.%d.input_%s_%d", 
      trsinfo.WORKDIR,*cable, hostname, pid);
      sprintf(filename2,"%s/ibsma.%d.trhead_%s_%d",
      trsinfo.WORKDIR,*cable, hostname, pid);
      sprintf(filename3,"%s/ibsma.%d.mltp_%s_%d",  
      trsinfo.WORKDIR,*cable, hostname, pid);
      sprintf(filename4,"%s/ibsma.%d.orig_%s_%d",  
      trsinfo.WORKDIR,*cable, hostname, pid);
   } else {
      sprintf(filename0, "%s/ibsma.%d.store_%d", trsinfo.WORKDIR, *cable, pid);
      sprintf(filename1, "%s/ibsma.%d.input_%d", trsinfo.WORKDIR, *cable, pid);
      sprintf(filename2, "%s/ibsma.%d.trhead_%d",trsinfo.WORKDIR, *cable, pid);
      sprintf(filename3, "%s/ibsma.%d.mltp_%d",  trsinfo.WORKDIR, *cable, pid);
      sprintf(filename4, "%s/ibsma.%d.orig_%d",  trsinfo.WORKDIR, *cable, pid);
   }

   if(predict.restart == 0) {

      /* Allocate arrays. */

      trace = (float *)malloc(trsinfo.nt*sizeof(float));
      if(trace == (float *)NULL) {
         printf("Initialize: Failed to allocate a trace.\n");
         nfatal++;
      }

      filter = (float *)malloc(fftinfo.nw*sizeof(float));
      if(filter == (float *)NULL) {
         printf("Initialize: Failed to allocate a filter.\n");
         nfatal++;
      }

      rt = (float *)malloc(fftinfo.ntfft*sizeof(float));
      if(rt == (float *)NULL) {
         printf("Initialize: Failed to allocate rt.\n");
         nfatal++;
      }

      ct = (complex *)malloc(fftinfo.nw*sizeof(complex));
      if(ct == (complex *)NULL) {
         printf("Initialize: Failed to allocate ct.\n");
         nfatal++;
      }

      size = dprep.groupsize*trsinfo.nrecv*nhead;
      trhead = (double *)malloc(size*sizeof(double));
      if(trhead == (double *)NULL) {
         printf("Initialize: Failed to allocate trhead.\n");
         nfatal++;
      }

      size = dprep.groupsize*trsinfo.nrecv*fftinfo.nwuse;
      data_out = (complex *)malloc(size*sizeof(complex));
      if(data_out == (complex *)NULL) {
         printf("Initialize: Failed to allocate data_out.\n");
         nfatal++;
      }

      /* Form low pass filter. */
      nfatal += ibsma_lpfilter(fftinfo.fmax, trsinfo.dt, fftinfo.dw,
      fftinfo.ntfft, filter);

      /* If there have been any fatal errors, quit the job. */
      if(nfatal > 0) {
         printf("Initialize: Fatal errors = %d.\n", nfatal);
         *ier = 3;
         return;
      }        

      /* Open output binary data and trace head files. */
      ofd_save = pfio_open(filename0, 'w');
      if(ofd_save < 0) {
         printf("Initialize: Failed to open temp file %s.\n", filename0);
         *ier = 4;
         return;
      }

      /* Open output binary data and trace head files. */
      ofd_data = pfio_open(filename1, 'w');
      if(ofd_data < 0) {
         printf("Initialize: Failed to open temp file %s.\n", filename1);
         *ier = 5;
         return;
      }

      ofd_head = pfio_open(filename2, 'w');
      if(ofd_head < 0) {
         printf("Initialize: Failed to open temp file %s.\n", filename2);
         *ier = 6;
         return;
      }

      ofd_orig = pfio_open(filename4, 'w');
      if(ofd_orig < 0) {
         printf("Initialize: Failed to open temp file %s.\n", filename4);
         *ier = 7;
         return;
      }
   }

   group = 1;
   if(group < dprep.ngroup) {
      groupsize = dprep.groupsize;
   } else {
      groupsize = dprep.grouplast;
   }
   shift1 = 0;
   shift2 = trsinfo.nrecv * groupsize;

   return;
}


/*--------------------------- ibsma_scale ------------------------------*/
/*--------------------------- ibsma_scale ------------------------------*/
/*--------------------------- ibsma_scale ------------------------------*/
void ibsma_scale (int *ier, float *scale)
{
   *ier = 0;
   predict.scale = *scale;
   return;
}


/*-------------------------- ibsma_datastore ---------------------------*/
/*-------------------------- ibsma_datastore ---------------------------*/
/*-------------------------- ibsma_datastore ---------------------------*/
void ibsma_datastore (INTEGER *ier, DOUBLE *header, REAL *trace, INTEGER *trndx)
{
   long nbytes;
   long lsize;

   *ier = 0;

   /* Write original trace header to temp file */
   lsize = nhead * sizeof(double);
   nbytes = pfio_write(ofd_save, (char *)header, lsize);
   if(nbytes != lsize) {
      printf("Data store: Failed to write header to temp save file.\n");
      *ier = 1;
      return;
   }

   /* Write original trace data to temp file */
   lsize = trsinfo.nt * sizeof(float);
   nbytes = pfio_write(ofd_save, (char *)trace, lsize);
   if(nbytes != lsize) {
      printf("Data store: Failed to write data to temp save file.\n");
      *ier = 2;
      return;
   }

   storecnt = storecnt + 1;

   return;        
}


/*-------------------------- ibsma_dataprep0 ---------------------------*/
/*-------------------------- ibsma_dataprep0 ---------------------------*/
/*-------------------------- ibsma_dataprep0 ---------------------------*/
void ibsma_dataprep0 (INTEGER *ier, DOUBLE *header, REAL *trace, INTEGER *trndx)
{
   int istat;
   long nbytes;
   long lsize;

   *ier = 0;

   if(*trndx == 1) {
      /* Reset file pointer */
      istat = pfio_seek_via_origin(ofd_save, 0L, SEEK_SET);
      if(istat != 0) {
         printf("Data prep0: Failed to seek in temp file.\n");
         *ier = 3;
         return;
      }
   }
   
   /* Write original trace header to temp file */
   lsize = nhead * sizeof(double);
   nbytes = pfio_read(ofd_save, (char *)header, lsize);
   if(nbytes != lsize) {
      printf("Data prep0: Failed to read header from temp save file.\n");
      *ier = 1;
      return;
   }

   /* Write original trace data to temp file */
   lsize = trsinfo.nt * sizeof(float);
   nbytes = pfio_read(ofd_save, (char *)trace, lsize);
   if(nbytes != lsize) {
      printf("Data prep0: Failed to read data from temp save file.\n");
      *ier = 2;
      return;
   }

   return;
}

/*-------------------------- ibsma_write_original ---------------------------*/
/*-------------------------- ibsma_write_original ---------------------------*/
/*-------------------------- ibsma_write_original ---------------------------*/
void ibsma_write_original (INTEGER *ier, REAL *trace)
{
   long nbytes,lsize;

   /* (1) read a single trace and write to temp file */

   *ier = 0;

   /* Write original data trace to temp file */
   lsize = trsinfo.nt * sizeof(float);
   nbytes = pfio_write(ofd_orig, (char *)trace, lsize);
   if(nbytes != lsize) {
      printf("write_original: Failed to write to temp original file.\n");
      *ier = 1;
   }

   return;        
}

/*-------------------------- ibsma_dataprep1 ---------------------------*/
/*-------------------------- ibsma_dataprep1 ---------------------------*/
/*-------------------------- ibsma_dataprep1 ---------------------------*/
void ibsma_dataprep1 (INTEGER *ier, DOUBLE *header, REAL *trace, INTEGER *trndx)
{
   int it, iw;

   /* Data preparation section.                                    */
   /* (1) read a single line of towed-streamer data.               */
   /* (2) apply FFT and lowpass filter.                            */
   /* (3) write frequency slices & trace head to separate files.   */

   *ier = 0;

   /* Copy trace header. */
   shift1 = nhead * (*trndx-1);
   for(it = 0; it < nhead; it++) {
      trhead[it+shift1] = header[it];
   }

   /* copy data while applying scale  */
   for(it = 0; it < trsinfo.nt; it++) {
      rt[it] = trace[it]/predict.scale;
   }

   /* Apply FFT. */
   for(it = trsinfo.nt; it < fftinfo.ntfft; it++) {
      rt[it] = 0.0;
   }
   for(it = 0; it < fftinfo.nw; it++) {
      ct[it].r = 0.0;
      ct[it].i = 0.0;
   }
   ibsma_pfarc(1, fftinfo.ntfft, rt, ct);

   /* (1) Apply lowpass filter. */
   for(iw = 0; iw < fftinfo.nnw; iw++) {
      ct[iw].r *= filter[iw];
      ct[iw].i *= filter[iw];
   }

   /* Copy each frequency to data_out. */
   /* Note: (1) data_out   [nrecv, size,  nwuse] */
   /*       (2) output file[nrecv, nshot, nwuse] */
   /*       (3) data_out is complex */
   /*shift3 = k + j*trsinfo.nrecv;*/
   shift3 = *trndx - 1;
   for(iw = 0; iw < fftinfo.nwuse; iw++) {
      data_out[shift3+iw*shift2].r = ct[iw + nwminG].r;
      data_out[shift3+iw*shift2].i = ct[iw + nwminG].i;
   }

   return;        
}


/*-------------------------- ibsma_dataprep2 ---------------------------*/
/*-------------------------- ibsma_dataprep2 ---------------------------*/
/*-------------------------- ibsma_dataprep2 ---------------------------*/
void ibsma_dataprep2 (INTEGER *ier)
{
   int iw;
   int itemp;
   long long offset;
   int istat;
   long nbytes;
   long lsize;
   /*int offset;*/

   printf("Data prep for group %d of %d.\n", group, dprep.ngroup);

   *ier = 0;

   /* Write output trace head. */
   itemp = groupsize * trsinfo.nrecv * nhead;
   lsize = itemp * sizeof(double);
   nbytes = pfio_write(ofd_head, (char*)trhead, lsize);
   if(nbytes != lsize) {
      printf("Data prep2: Failed to write to temp header file.\n");
      *ier = 1;
      return;
   }

   /* Write output data. */
   if(dprep.ngroup == 1) {
      itemp = groupsize*trsinfo.nrecv*fftinfo.nwuse;
      lsize = itemp * sizeof(complex);
      nbytes = pfio_write(ofd_data, (char*)data_out, lsize);
      if(nbytes != lsize) {
         printf("Data prep2: Failed to write to temp data file.\n");
         *ier = 1;
         return;
      }      
   } else {
      shift1 = trsinfo.nrecv*sizeof(complex);
      shift2 = (group-1)*dprep.groupsize;
      for(iw = 0; iw < fftinfo.nwuse; iw++) {
         offset = ((long long)iw * (long long)trsinfo.nshot 
                + (long long)shift2) * (long long)shift1;
         istat = pfio_seek_via_origin(ofd_data, offset, SEEK_SET);
         if(istat != 0) {
            printf("Data prep2: Failed to seek in temp data file.\n");
            *ier = 1;
            return;
         }
         shift3 = iw*groupsize*trsinfo.nrecv;
         itemp = groupsize*trsinfo.nrecv;
         lsize = itemp * sizeof(complex);
         nbytes = pfio_write(ofd_data,(char*)(data_out+shift3), lsize);
         if(nbytes != lsize) {
            printf("Data prep2: Failed to write to temp data file.\n");
            *ier = 1;
            return;
         }
      }
   }

   group++;
   if(group < dprep.ngroup) {
      groupsize = dprep.groupsize;
   } else {
      groupsize = dprep.grouplast;
   }
   shift1 = 0;
   shift2 = trsinfo.nrecv * groupsize;

   return;        
}


/*-------------------------- ibsma_dataprep3 ---------------------------*/
/*-------------------------- ibsma_dataprep3 ---------------------------*/
/*-------------------------- ibsma_dataprep3 ---------------------------*/
void ibsma_dataprep3 (INTEGER *ier)
{
   int istat;

   *ier = 0;

   /* Print info. */
   trsinfo.totalbadtrs = badtrs;
   printf("\n***** Data preparation: completed *****\n\n");
   printf("predict.scale = %g\n", predict.scale);
   printf("totalbadtrs   = %d\n\n", trsinfo.totalbadtrs);
   
   /* Free space. */
   free(trace);
   free(filter);
   free(rt);
   free(ct);
   free(trhead);
   free(data_out);

   /* Reset file pointer */
   istat = pfio_seek_via_origin(ofd_data, 0L, SEEK_SET);
   if(istat != 0) {
      printf("Data prep3: Failed to seek in temp file.\n");
      *ier = 1;
      return;
   }
   istat = pfio_seek_via_origin(ofd_head, 0L, SEEK_SET);
   if(istat != 0) {
      printf("Data prep3: Failed to seek in temp header file.\n");
      *ier = 1;
      return;
   }

   istat = pfio_close(ofd_save);
   if(istat < 0) {
      printf("Data prep3: Failed to close temp save file.\n");
      *ier = 2;
      return;
   }
   pfio_delete(filename0);

   return;        
}


/*-------------------------- ibsma_predict ----------------------------*/
/*-------------------------- ibsma_predict ----------------------------*/
/*-------------------------- ibsma_predict ----------------------------*/
void ibsma_predict (INTEGER *ier)
{
   /* Other variables. */
   int    i, j, k, iw, itemp;
   int    iorder, size, iobliq;
   int    isign, istat;
   float  temp,temp1;
   long   nbytes;
   long   lsize;

   if(predict.restart == 1) {
      printf("Restart from prediction.\n");

      ofd_data = pfio_open(filename1, 'r');
      if(ofd_data < 0) {
         printf("Predict: Failed to open temp file %s.\n", filename1);
         *ier = 1;
         return;
      }

      ofd_head = pfio_open(filename2, 'r');
      if(ofd_head < 0) {
         printf("Predict: Failed to open temp file %s.\n", filename2);
         *ier = 2;
         return;
      }

      ofd_orig = pfio_open(filename4, 'w');
      if(ofd_orig < 0) {
         printf("Predict: Failed to open temp file %s.\n", filename4);
         *ier = 1;
         return;
      }
   }
   
   /***************************************************************/
   /*                 Multiple prediction                         */
   /* loop over frequency slices, for each freq: do the following */
   /* (1) read in a freqency slice                                */
   /* (2) form matrix D0_obq                                      */
   /* (3) form matrix D0                                          */
   /* (4) calculate 1st order multiple: MT_1st = D0_obq*D0        */
   /* (5) write 1st order multiple to a temporal file             */
   /* (6) use MT_1st to form matrix D0, repeat (4)-(5)            */
   /* (7) until predict.maxorder multiple is obtained             */
   /***************************************************************/

   /* Open output file. */
   ofd_mltp = pfio_open(filename3, 'w');
   if(ofd_mltp < 0) {
      printf("Predict: Failed to open temp file %s.\n", filename3);
      *ier = 1;
      return;
   }

   /* Allocate arrays. */
   kx2 = (float *)malloc((nxffthalf+1)*sizeof(float));
   if(kx2 == (float *)NULL) {
      printf("Predict: Failed to allocate kx2.\n");
      *ier = 1;
      return;
   }

   obliq = (float *)malloc((nxffthalf+1)*sizeof(float));
   if(obliq == (float *)NULL) { 
      printf("Predict: Failed to allocate obliq.\n");
      *ier = 1;
      return;
   }

   cx1 = (complex *)malloc(fftinfo.nxfft*sizeof(complex));
   if(cx1 == (complex *)NULL) {
      printf("Predict: Failed to allocate cx1.\n");
      *ier = 1;
      return;
   }

   size = trsinfo.nshot*fftinfo.nxfft;
   D0_obq = (complex *)malloc(size*sizeof(complex));
   if(D0_obq == (complex *)NULL) {
      printf("Predict: Failed to allocate D0_obq.\n");
      *ier = 1;
      return;
   }

   D0 = (complex *)malloc(size*sizeof(complex));
   if(D0 == (complex *)NULL) {
      printf("Predict: Failed to allocate D0.\n");
      *ier = 1;
      return;
   }

   size = trsinfo.nshot*trsinfo.nrecv;
   multip = (complex *)malloc(size*predict.maxorder*sizeof(complex));
   if(multip == (complex *)NULL) {
      printf("Predict: Failed to allocate multip.\n");
      *ier = 1;
      return;
   }

   multip_save = multip;
   cwork = (complex *)malloc(size*sizeof(complex));
   if(cwork == (complex *)NULL) {         
      printf("Predict: Failed to allocate cwork.\n");
      *ier = 1;
      return;
   }

   /* Calculate kx2{=kx*kx}                 */
   /* We calculate kx2 between [0, nxfft/2] */
   /* since kx2 is symmetric with nxfft/2.   */
   for(i = 0; i <= nxffthalf; i++) {
      kx2[i] = i*fftinfo.dkx;
      kx2[i] *= kx2[i];
   }

   /* Loop over all frequencies. */
   size = trsinfo.nshot*trsinfo.nrecv;
   for(iw = 0; iw < fftinfo.nwuse; iw++) {

      printf("Working on frequency index %d of %d\n", iw+1, fftinfo.nwuse);

      /* Update point multip. */
      multip = multip_save;

      /* Calculate obliquity. */
      /* (1) Since kx2 is symmetric with nxfft/2, we only */
      /*     need to calculate obliq between [0, nxfft/2] */
      temp = dwov2*(iw + nwminG)*(iw + nwminG);
      temp1 = sqrt(1.0/temp);
      iobliq = 1;
      for(i = 0; i <= nxffthalf; i++) {
         obliq[i] = 0.0;
      }
      for(i = 0; i <= nxffthalf; i++) {
         if(temp > kx2[i]) {
         iobliq = i;
         obliq[i] = (float)(temp1*sqrt((double)(temp-kx2[i])));
      } else
         break;
      }

      /*************************************/
      /* Bring in a frequency slice.       */
      /* Note: cwork[0:nshot-1][0:nrecv-1] */
      /* The diagram below shows:          */
      /*   Row(nshot=9) & Column(nrecv=7)  */
      /*   the data store nrecv for shot1  */
      /*   then nrecv for shot2, and so on */
      /*       X   X   X   X   X   X   X   */
      /*       1   2   3   4   5   6   7   */
      /* S1   1/1 1/2 1/3 1/4 1/5 1/6 1/7  */
      /* S2   2/1 2/2 2/3 2/4 2/5 2/6 2/7  */
      /* S3   3/1 3/2 3/3 3/4 3/5 3/6 3/7  */
      /* S4   4/1 4/2 4/3 4/4 4/5 4/6 4/7  */
      /* S5   5/1 5/2 5/3 5/4 5/5 5/6 5/7  */
      /* S6   6/1 6/2 6/3 6/4 6/5 6/6 6/7  */
      /* S7   7/1 7/2 7/3 7/4 7/5 7/6 7/7  */
      /* S8   8/1 8/2 8/3 8/4 8/5 8/6 8/7  */
      /* S9   9/1 9/2 9/3 9/4 9/5 9/6 9/7  */
      /*************************************/
      lsize = size * sizeof(complex);
      nbytes = pfio_read(ofd_data, (char*)cwork, lsize);
      if(nbytes != lsize) {
         printf("Predict: Failed to read from temp data file.\n");
         *ier = 1;
         return;
      }

      /***********************************************************/
      /*  Form D0_obq & D0                                       */
      /*  Here D0_obq holds input data with obliquity applied    */
      /*  D0 holds either input data or predicted multiples      */
      /*    no obliquity has been applied on it.                 */
      /*                                                         */
      /*  Theoretically:                                         */
      /*    D0_obq has dimension [nshot][nxfft]                  */
      /*    D0 has dimension [nxfft][nshot]                      */
      /*    here we use "c" notation, which means that the       */
      /*    1st dim. is slowest, and the 2nd dim. fastest        */
      /*                                                         */
      /*  In practice:                                           */
      /*    we calculate "DOT" product between D0_obq & D0       */
      /*    for convience, we implement both D0_obq and D0 have  */
      /*    the same dimension, that is [nshot][nxfft]           */
      /*                                                         */
      /*  The matrix below can be formed by either input data    */
      /*  or predicted multiples.                                */
      /*  D0_obq is the matrix below with (+1) FFT along its Row */
      /*  D0 is the matrix below with (-1) FFT along its Column  */
      /*  The Row and Column are the same because of symmetry    */
      /*                                                         */
      /*  When we assume nshot=9, nrecv=7, we have the following */
      /*                                                         */
      /*            X   X   X   X   X   X   X   X   X            */
      /*            1   2   3   4   5   6   7   8   9            */
      /*  S1       1/1 2/2 3/3 4/4 5/5 6/6 7/7                   */
      /*  S2       2/2 2/1 3/2 4/3 5/4 6/5 7/6 8/7               */
      /*  S3       3/3 3/2 3/1 4/2 5/3 6/4 7/5 8/6 9/7           */
      /*  S4       4/4 4/3 4/2 4/1 5/2 6/3 7/4 8/5 9/6           */
      /*  S5       5/5 5/4 5/3 5/2 5/1 6/2 7/3 8/4 9/5           */
      /*  S6       6/6 6/5 6/4 6/3 6/2 6/1 7/2 8/3 9/4           */
      /*  S7       7/7 7/6 7/5 7/4 7/3 7/2 7/1 8/2 9/3           */
      /*  S8           8/7 8/6 8/5 8/4 8/3 8/2 8/1 9/2           */
      /*  S9               9/7 9/6 9/5 9/4 9/3 9/2 9/1           */
      /***********************************************************/

      /* First form D0_obq. */
      shift1 = trsinfo.nrecv+1;
      for(i = 0; i < trsinfo.nshot; i++) {

         /* Set zero. */
         for(j = 0; j < fftinfo.nxfft; j++) {
            cx1[j].r = 0.0;
            cx1[j].i = 0.0;
         }

         /* Fill in true receivers. */            
         shift2 = i*trsinfo.nrecv;
         itemp = Min(i+1, trsinfo.nrecv);
         for(j = 0; j < itemp; j++) {
            cx1[i-j].r = cwork[shift2+j].r;
            cx1[i-j].i = cwork[shift2+j].i;
         }

         /* Add data based on reciprocity. */
         itemp = Min((trsinfo.nshot-i-1), (trsinfo.nrecv-1));
         for(j = 0; j < itemp; j++) {
            cx1[i+j+1].r = cwork[shift2+(j+1)*shift1].r;
            cx1[i+j+1].i = cwork[shift2+(j+1)*shift1].i;
         }

         /* Apply spatial forward(+1) FFT for cx1. */
         isign = 1;
         ibsma_pfacc(isign, fftinfo.nxfft, cx1);

         /**********************************************/
         /* Copy cx1 to D0_obq.                        */
         /* In theory, a FFT scale need to be applied  */
         /* But here since we apply one forward FFT    */
         /* and one inverse FFT, and we only calculate */
         /* their "DOT" product, so the FFT scale are  */
         /* cancelled, so we do not apply it.          */
         /**********************************************/
         shift3 = i*fftinfo.nxfft;
         for(j = 0; j < fftinfo.nxfft; j++) {
            D0_obq[shift3+j].r = cx1[j].r;
            D0_obq[shift3+j].i = cx1[j].i;
         }
      }

      /****************************************************************/
      /* Apply obliquity and remove evanescent component.             */
      /* We allpy obliq only on D0_obq. In theory,                    */
      /* we need to remove evanescent component on both D0_obq and D0 */
      /* When we apply obliq on D0_obq, we automatically removes      */
      /* evanescent component on D0_obq. Since our final goal is to   */
      /* calculate "DOT" product of D0_obq and D0, we don't have to   */
      /* remove evanescent component on D0 explicitly.                */
      /* In practice, we only calculate those D0_obq are not zeros    */
      /****************************************************************/
      for(i = 0; i < trsinfo.nshot; i++) {
         shift1 = i*fftinfo.nxfft;

         for(j = 0; j <= iobliq; j++) {
            D0_obq[shift1+j].r *= obliq[j];
            D0_obq[shift1+j].i *= obliq[j];
         }

         if(iobliq < nxffthalf) {
            j = fftinfo.nxfft-iobliq;
            D0_obq[shift1+j].r *= obliq[iobliq];
            D0_obq[shift1+j].i *= obliq[iobliq];
         }

         for(j = fftinfo.nxfft-iobliq+1; j < fftinfo.nxfft; j++){
            D0_obq[shift1+j].r *= obliq[fftinfo.nxfft-j];
            D0_obq[shift1+j].i *= obliq[fftinfo.nxfft-j];
         }
      }

      /* Set zero. */
      for(i = 0; i < trsinfo.nshot*trsinfo.nrecv*predict.maxorder; i++) {
         multip[i].r = 0.0;
         multip[i].i = 0.0;
      }

      /* 2nd, form D0, loop over all orders predicted. */
      for(iorder = 1; iorder <= predict.maxorder; iorder++) {
         shift1 = trsinfo.nrecv+1;
         for(i = 0; i < trsinfo.nshot; i++) {

            /* Set zero. */
            for(j = 0; j < fftinfo.nxfft; j++) {
               cx1[j].r = 0.0;
               cx1[j].i = 0.0;
            }

            /* Fill in true receivers. */            
            shift2 = i*trsinfo.nrecv;
            itemp = Min(i+1, trsinfo.nrecv);
            for(j = 0; j < itemp; j++) {
               cx1[i-j].r = cwork[shift2+j].r;
               cx1[i-j].i = cwork[shift2+j].i;
            }

            /* Add data based on reciprocity. */
            itemp = Min((trsinfo.nshot-i-1), (trsinfo.nrecv-1));
            for(j = 0; j < itemp; j++) {
               cx1[i+j+1].r = cwork[shift2+(j+1)*shift1].r;
               cx1[i+j+1].i = cwork[shift2+(j+1)*shift1].i;
            }

            /* Apply spatial inverse(-1) FFT for cx1. */
            isign = -1;
            ibsma_pfacc(isign, fftinfo.nxfft, cx1);

            /* Copy cx1 to D0. */
            shift3 = i*fftinfo.nxfft;
            for(j = 0; j < fftinfo.nxfft; j++) {
               D0[shift3+j].r = cx1[j].r;
               D0[shift3+j].i = cx1[j].i;
            }
         }

         /***********************************************/
         /* form "DOT" product of D0_obq and D0: multip */
         /* multip[nshot][nshot], as shown below, holds */
         /* useful information only at those locations  */
         /* marked by shot#/recv#. So in practice, we   */
         /* declare multip[0:nshot-1][0:nrecv-1]        */
         /*                                             */
         /*          X   X   X   X   X   X   X   X   X  */
         /*          1   2   3   4   5   6   7   8   9  */
         /*  S1     1/1                                 */
         /*  S2     2/2 2/1                             */
         /*  S3     3/3 3/2 3/1                         */
         /*  S4     4/4 4/3 4/2 4/1                     */
         /*  S5     5/5 5/4 5/3 5/2 5/1                 */
         /*  S6     6/6 6/5 6/4 6/3 6/2 6/1             */
         /*  S7     7/7 7/6 7/5 7/4 7/3 7/2 7/1         */
         /*  S8         8/7 8/6 8/5 8/4 8/3 8/2 8/1     */
         /*  S9             9/7 9/6 9/5 9/4 9/3 9/2 9/1 */
         /***********************************************/   

         /* Only calculate those non-zero D0_obq. */
         for(i = 0; i < trsinfo.nshot; i++) {

            shift1 = i*fftinfo.nxfft;
            itemp = Min(i+1, trsinfo.nrecv);
            for(j = 0; j < itemp; j++) {

               shift2 = i*trsinfo.nrecv + j;
               shift3 = (i-j)*fftinfo.nxfft;

               for(k = 0; k <= iobliq; k++) {
                  multip[shift2].r += D0_obq[shift1+k].r*D0[shift3+k].r
                     - D0_obq[shift1+k].i*D0[shift3+k].i;
                  multip[shift2].i += D0_obq[shift1+k].i*D0[shift3+k].r
                     + D0_obq[shift1+k].r*D0[shift3+k].i;
               }

               if(iobliq < nxffthalf) {
                  k = fftinfo.nxfft-iobliq;
                  multip[shift2].r += D0_obq[shift1+k].r*D0[shift3+k].r
                     - D0_obq[shift1+k].i*D0[shift3+k].i;
                  multip[shift2].i += D0_obq[shift1+k].i*D0[shift3+k].r
                     + D0_obq[shift1+k].r*D0[shift3+k].i;
               }

               for(k = fftinfo.nxfft-iobliq+1; k < fftinfo.nxfft; k++) {
                  multip[shift2].r += D0_obq[shift1+k].r*D0[shift3+k].r
                     - D0_obq[shift1+k].i*D0[shift3+k].i;
                  multip[shift2].i += D0_obq[shift1+k].i*D0[shift3+k].r
                     + D0_obq[shift1+k].r*D0[shift3+k].i;
               }
            }
         }

         /* If it's not the maxorder multiple, copy multip to cwork */
         /* for generating next order multiple, and advance multip. */
         if(iorder < predict.maxorder) {
            for(i = 0; i < size; i++) {
               cwork[i].r = multip[i].r;
               cwork[i].i = multip[i].i;
            }
            multip += size;
         }
      }

      /* Write maxorder multips to a file. */
      lsize = size * predict.maxorder * sizeof(complex);
      nbytes = pfio_write(ofd_mltp, (char*)multip_save, lsize);
      if(nbytes != lsize) {
         printf("Predict: Failed to write to temp multiple file.\n");
         *ier = 1;
         return;
      }
   }

   istat = pfio_close(ofd_data);
   if(istat < 0) {
      printf("Output4: Failed to close temp data file.\n");
   }
   pfio_delete(filename1); 
  
   /* Print info. */
   printf("\n***** Multiple prediction: completed *****\n\n");

   /* Free space. */
   free(kx2);
   free(obliq);
   free(cx1);
   free(D0_obq);
   free(D0);
   free(multip_save);
   free(cwork);

   /* Reset file pointer. */

   istat = pfio_seek_via_origin(ofd_mltp, 0L, SEEK_SET);
   if(istat != 0) {
      printf("Predict: Failed to seek in temp multiple file.\n");
   }
   istat = pfio_seek_via_origin(ofd_orig, 0L, SEEK_SET);
   if(istat != 0) {
      printf("Predict: Failed to seek in temp original file.\n");
   }


   return;
}


/*------------------------- ibsma_output1 -------------------------------*/
/*------------------------- ibsma_output1 -------------------------------*/
/*------------------------- ibsma_output1 -------------------------------*/
void ibsma_output1 (INTEGER *ier)
{
   int istat, nfatal = 0;
   int i;
   int size;
   long long shiftS;
   /*int shiftS;*/

   /* Restart position. */
   if(predict.restart == 2) {
      printf("Restart from output.\n");

      ofd_data = pfio_open(filename1, 'r');
      if(ofd_data < 0) {     
         printf("Output: Failed to open temp file %s.\n", filename1);
         *ier = 1;
         return;
      }

      ofd_head = pfio_open(filename2, 'r');
      if(ofd_head < 0) {
         printf("Output: Failed to open temp file %s.\n", filename2);
         *ier = 2;
         return;
      }

      ofd_mltp = pfio_open(filename3, 'r');
      if(ofd_mltp < 0) {
         printf("Output: Failed to open temp file %s.\n", filename3);
         *ier = 3;
         return;
      }

      ofd_orig = pfio_open(filename4, 'r');
      if(ofd_orig < 0) {
         printf("Output: Failed to open temp file %s.\n", filename4);
         *ier = 4;
         return;
      }
   }

   /*********************************************************************/
   /*                    Output predicted multiple                      */
   /*  Note: Predicted multiple are stored in a file pointed by ofd_mltp*/
   /*  It is in order [0:nwuse-1][1:maxorder][0:nshot-1][0:nrecv-1].    */
   /*  Receiver is the fastest axis, then shot, then predicted          */
   /*  multiple order, and frequency is the slowest axis.               */
   /*                                                                   */
   /* Procedure:                                                        */
   /*  (1)  Generate a F-K taper function.                              */
   /*  (2)  Read in one input shot gather.                              */
   /*  (3)  Read in one predicted shot gather, which including all      */
   /*       freq & all predicted orders of multiple.                    */
   /*  (4)  Estimate source delay.                                      */
   /*  (5)  Compute AUTO & CROSS Correlation.                           */
   /*  (6)  Estimate of source signal.                                  */
   /*  (7)  Update estimated source signal (2 iterations).              */
   /*  (8)  Form predicted multiples.                                   */
   /*  (9)  Write to a SEGy file.                                       */
   /*  (10) Repeat step(2-9) until all predicted shot processed.        */
   /*********************************************************************/

   /* Allocate memory. */
   nfatal = 0;
   itdelay = (int *)malloc(output.shotnum*sizeof(int));
   if(itdelay == (int *)NULL) {
      printf("Output: Failed to allocate a itdelay.\n");
      nfatal++;
   }

   epsa = (float *)malloc(output.shotnum*sizeof(float));
   if(epsa == (float *)NULL) {
      printf("Output: Failed to allocate a epsa.\n");
      nfatal++;
   }
   
   trorig = (float *)malloc(trsinfo.nt*sizeof(float));
   if(trorig == (float *)NULL) {
      printf("Output: Failed to allocate a trorig.\n");
      nfatal++;
   }

   if(predict.test) {
      nwlow = (int *)malloc(output.shotnum*sizeof(int));
      if(nwlow == (int *)NULL) {
         printf("Output: Failed to allocate a nwlow.\n");
         nfatal++;
      }
      nwhigh = (int *)malloc(output.shotnum*sizeof(int));
      if(nwhigh == (int *)NULL) {
         printf("Output: Failed to allocate a nwhigh.\n");
         nfatal++;
      }
   }

   trace = (float *)malloc(trsinfo.nt*sizeof(float));
   if(trace == (float *)NULL) {
      printf("Output: Failed to allocate a trace.\n");
      nfatal++;
   }

   rt = (float *)malloc(fftinfo.nwcorr*sizeof(float));
   if(rt == (float *)NULL) {
      printf("Output: Failed to allocate rt.\n");
      nfatal++;
   }

   ct = (complex *)malloc(fftinfo.nwcorr*sizeof(complex));
   if(ct == (complex *)NULL) {
      printf("Output: Failed to allocate ct.\n");
      nfatal++;
   }

   cauto = (complex *)malloc(output.shotnum*fftinfo.nwuse*sizeof(complex));
   if(cauto == (complex *)NULL) {
      printf("Output: Failed to allocate cauto.\n");
      nfatal++;
   }

   ccros = (complex *)malloc(fftinfo.nwuse*sizeof(complex));
   if(ccros == (complex *)NULL) {
      printf("Output: Failed to allocate ccros.\n");
      nfatal++;
   }

   fsig = (complex *)malloc(fftinfo.nwuse*sizeof(complex));
   if(fsig == (complex *)NULL) {
      printf("Output: Failed to allocate fsig.\n");
      nfatal++;
   }

   fsigfinal = (complex *)malloc(predict.maxorder*
   fftinfo.nwuse*sizeof(complex));
   if(fsigfinal == (complex *)NULL) {
      printf("Output: Failed to allocate fsigfinal.\n");
      nfatal++;
   }

   size = output.shotnum*trsinfo.nrecv*nhead;
   trhead = (double *)malloc(size*sizeof(double));
   if(trhead == (double *)NULL) {
      printf("Output: Failed to allocate trhead.\n");
      nfatal++;
   }        

   size = output.shotnum*fftinfo.nwuse*trsinfo.nrecv;
   data_in = (complex *)malloc(size*sizeof(complex));
   if(data_in == (complex *)NULL) {
      printf("Output: Failed to allocate data_in.\n");
      nfatal++;
   }

   size = output.shotnum*predict.maxorder*fftinfo.nwuse*trsinfo.nrecv;
   multip = (complex *)malloc(size*sizeof(complex));
   if(multip == (complex *)NULL) {
      printf("Output: Failed to allocate multip.\n");
      nfatal++;
   }

   /* If we failed to allocate the memory, quit the job. */
   if(nfatal > 0) {
      printf("Output: Fatal errors = %d.\n", nfatal);
      *ier = 1;
      return;
   }

   /* Generate a time domain taper function. */
   for(i = 0; i <= NTAPE; i++) {
      tape[i] = 0.5 - 0.5*cos(PI*i/NTAPE);
   }

   /************************************************************/
   /*  Start a big loop on nshot to output all shot gathers    */
   /*  with maxorder multiples predicted.                      */
   /************************************************************/

   /* If it is test, jump to the 1st trace header. */
   if(predict.test) {
      shiftS = (long long)output.nshotBeg * (long long)trsinfo.nrecv 
             * (long long)nhead * (long long)sizeof(double);
      istat = pfio_seek_via_origin(ofd_head, shiftS, SEEK_SET);
      if(istat != 0) {
         printf("Output1: Failed to seek in temp header file.\n");
         *ier = 1;
         return;
      }
   }

   return;    
}


/*------------------------- ibsma_output3x ------------------------------*/
/*------------------------- ibsma_output3x ------------------------------*/
/*------------------------- ibsma_output3x ------------------------------*/
void ibsma_output3x (INTEGER *ier, INTEGER *shotg, INTEGER *shot, INTEGER *recv,
                DOUBLE *hd, REAL *tr)
{
        int    i, j, k;
        int    it, iw, id, itemp, itemp1, itemp2;
        int    size, ishot, irecv, nread, iter;
        int    nwmin_test, nwmax_test;
        long long shiftS;
        /*int shiftS;*/
        float  temp, temp1, temp2, ampmax;
        double v[256];
        long lsize;
        
        itemp1 = 0;
        
        nread = *shotg - 1;
        if(nread < (output.shotnread-1))
           size = output.shotnum;
        else
           size = output.shotlast;

        if(*shot == 1 && *recv == 1) goto startGath;
        if(*shot != 1 && *recv == 1) goto startShot;
        if(*recv != 1) goto startRecv;
                
startGath:
        /* Read in "size" input shot gather trace header. */
        itemp = size*trsinfo.nrecv*nhead;
        lsize = itemp * sizeof(double);
        pfio_read(ofd_head, (char*)trhead, lsize);

        /* Read in "size" of input shot gathers and of predicted */
        /* multipled shot gathers including all orders.          */
        ishot = output.nshotBeg + output.shotnum*nread;
        shiftS = (long long)ishot * (long long)trsinfo.nrecv *
                 (long long)sizeof(complex);
        pfio_seek_via_origin(ofd_mltp, shiftS, SEEK_SET);
        shift1 = (trsinfo.nshot-size)*trsinfo.nrecv*sizeof(complex);
        shift2 = 0;
        itemp = trsinfo.nrecv*size;
        for(iw = 0; iw < fftinfo.nwuse*predict.maxorder; iw++) {
           lsize = itemp * sizeof(complex);
           pfio_read(ofd_mltp, (char*)(multip+shift2), lsize);
           pfio_seek_via_origin(ofd_mltp, shift1, SEEK_CUR);
           shift2 += itemp;
        }

        pfio_seek_via_origin(ofd_data, shiftS, SEEK_SET);
        shift2 = 0;
        itemp = trsinfo.nrecv*size;
        for(iw = 0; iw < fftinfo.nwuse; iw++) {
          lsize = itemp * sizeof(complex);
          pfio_read(ofd_data, (char*)(data_in+shift2), lsize);
          pfio_seek_via_origin(ofd_data, shift1, SEEK_CUR);
          shift2 += itemp;
        }

        /* Estimate source delay:                                        */
        /* (1) correlate each input trace with 1st order multiple.       */
        /* (2) inverse FFT correlated result.                            */
        /* (3) stack over all receivers within a shot.                   */
        /* (4) find the time delay associated with the max value.        */
        itdelay[0] = nwcorrhalf;
        for(ishot = 1; ishot < size; ishot++) {
                itdelay[ishot] = 3*nwcorrhalf;
        }
        for(ishot = 0; ishot < size; ishot++) {
                for(it = 0; it < fftinfo.nwcorr; it++) {
                        rt[it] = 0.0;
                }
                for(j = 0; j < trsinfo.nrecv; j++) { 
                        for(iw = 0; iw < fftinfo.nwcorr; iw++) {
                                ct[iw].r = 0.0;
                                ct[iw].i = 0.0;
                        }
                        for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                itemp = ishot*trsinfo.nrecv + j;
                                itemp1 = iw*size*trsinfo.nrecv;
                                itemp2 = itemp1*predict.maxorder + itemp;
                                itemp1 += itemp;
                                temp1 = data_in[itemp1].r*multip[itemp2].r
                                + data_in[itemp1].i*multip[itemp2].i;
                                temp2 = data_in[itemp1].i*multip[itemp2].r
                                - data_in[itemp1].r*multip[itemp2].i;
                                ct[iw + nwminG].r = temp1;
                                ct[iw + nwminG].i = temp2;
                                ct[fftinfo.nwcorr - iw - nwminG - 1].r = temp1;
                                ct[fftinfo.nwcorr - iw - nwminG - 1].i = -temp2;
                        }
                        ibsma_pfacc(+1, fftinfo.nwcorr, ct);
                        for(iw = 0; iw < nwcorrhalf; iw++) {
                                rt[iw] += ct[iw+nwcorrhalf].r;
                                rt[iw+nwcorrhalf] += ct[iw].r;
                        }
                }
                ampmax = 0.0;
                for(it = nwcorrhalf-NWIN; it <= nwcorrhalf+NWIN; it++) {
                        temp = fabs((double)rt[it]);
                        if(ampmax < temp) {
                                ampmax = temp;
                                itdelay[ishot] = it;
                        }
                }
                itdelay[ishot] -= nwcorrhalf;
                if(abs(itdelay[ishot]) >= NWIN) {
                        itdelay[ishot] = itdelay[ishot-1];
                        printf("ishot = %d %d\n", ishot, ishot-1);
                }
        }

        /* Compute auto-correlation. */
        for(iw = 0; iw < size*fftinfo.nwuse; iw++) {
                cauto[iw].r = 0.0;
                cauto[iw].i = 0.0;
        }
        for(ishot = 0; ishot < size; ishot++) {
                shift1 = ishot*fftinfo.nwuse;
                for(j = 0; j < trsinfo.nrecv; j++) {
                        for(iw = 0; iw < fftinfo.nwcorr; iw++) {
                                ct[iw].r = 0.0;
                                ct[iw].i = 0.0;
                        }
                        for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                itemp = iw*size*trsinfo.nrecv*predict.maxorder
                                        + ishot*trsinfo.nrecv + j;
                                temp1 = multip[itemp].r*multip[itemp].r
                                        + multip[itemp].i*multip[itemp].i;
                                ct[iw + nwminG].r = temp1;
                                ct[fftinfo.nwcorr - iw - nwminG - 1].r = temp1;
                        }
                        ibsma_pfacc(+1, fftinfo.nwcorr, ct);

                        itemp1 = NTAPE+NWIN-1;
                        itemp2 = fftinfo.nwcorr-NTAPE-NWIN;
                        for(iw = NWIN; iw <= itemp1; iw++) {
                                ct[iw].r *= tape[itemp1-iw];
                        }
                        for(iw = itemp1+1; iw < itemp2; iw++) {
                                ct[iw].r = 0.0;
                        }
                        for(iw = itemp2; iw < itemp2+NTAPE; iw++) {
                                ct[iw].r *= tape[iw-itemp2];
                        }
                        for(iw = 0; iw < fftinfo.nwcorr; iw++) {
                                ct[iw].i = 0.0;
                        }

                        ibsma_pfacc(-1, fftinfo.nwcorr, ct);
                        for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                cauto[shift1+iw].r += ct[iw + nwminG].r;
                        }
                }

                /* Make sure no negative values. */
                for(iw = 0; iw < fftinfo.nwuse; iw++) {
                        cauto[shift1+iw].r = Max(0.0, cauto[shift1+iw].r);
                }
        }

        /* If it is a test, find the max cauto. */
        if(predict.test) {
                for(ishot = 0; ishot < size; ishot++) {

                        shift1 = ishot*fftinfo.nwuse;
                        ampmax = 0.0;
                        for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                if(cauto[shift1+iw].r > ampmax) {
                                        ampmax = cauto[shift1+iw].r;
                                        itemp1 = iw;
                                }
                        }
                        ampmax *= predict.whitenoise;
                        epsa[ishot] = ampmax;

                        /* Find the freq. range. */
                        for(iw = itemp1; iw >= 0; iw--) {
                                if(cauto[shift1+iw].r < ampmax) {
                                        nwlow[ishot] = iw+1;
                                        break;
                                }
                        }
                        for(iw = itemp1; iw < fftinfo.nwuse; iw++) {
                                if(cauto[shift1+iw].r < ampmax) {
                                        nwhigh[ishot] = iw;
                                        break;
                                }
                        }
                }

                /* Find the nwmin_test, nwmax_test. */
                nwmin_test = nwlow[0];
                nwmax_test = nwhigh[0];
                for(ishot = 1; ishot < size; ishot++) {
                        if(nwmin_test > nwlow[ishot]) nwmin_test=nwlow[ishot];
                        if(nwmax_test < nwhigh[ishot]) nwmax_test=nwhigh[ishot];
                }
                nwmin_test -= nw1hz;
                nwmax_test += nw1hz;

                for(i = 0; i < output.totshot; i++) {
                        printf("ishot, nwmin, nwmax = %d %d %d\n",
                                i+predict.ntestbeg, nwlow[i], nwhigh[i]);
                }
                printf("# of freq. slice for 1hz = %d\n\n", nw1hz);
                printf("--------------------------------\n");
                printf("Suggested NWMIN = %d\n", nwmin_test);
                printf("Suggested NWMAX = %d\n", nwmax_test);

        /* For real run. */
        } else {
                /* Calculate floor value used for estimate source signal */
                /* for the 1st several shots, most predicted traces      */
                /* are zero, so need to be treated differently.          */
                if(!nread) {
                        itemp1 = trsinfo.nrecv/3;
                        itemp2 = trsinfo.nrecv*2/3;
                        for(ishot = 0; ishot < itemp1; ishot++) {
                                shift1 = ishot*fftinfo.nwuse;
                                ampmax = 0.0;
                                for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                        if(cauto[shift1+iw].r > ampmax)
                                                ampmax = cauto[shift1+iw].r;
                                }
                                epsa[ishot] = 5.0*predict.whitenoise*ampmax;
                        }

                        for(ishot = itemp1; ishot < itemp2; ishot++) {
                                shift1 = ishot*fftinfo.nwuse;
                                ampmax = 0.0;
                                for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                        if(cauto[shift1+iw].r > ampmax)
                                                ampmax = cauto[shift1+iw].r;
                                }
                                epsa[ishot] = 3.0*predict.whitenoise*ampmax;
                        }

                        for(ishot = itemp2; ishot < size; ishot++) {
                                shift1 = ishot*fftinfo.nwuse;
                                ampmax = 0.0;
                                for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                        if(cauto[shift1+iw].r > ampmax)
                                                ampmax = cauto[shift1+iw].r;
                                }
                                epsa[ishot] = predict.whitenoise*ampmax;
                        }

                } else {
                        for(ishot = 0; ishot < size; ishot++) {
                                shift1 = ishot*fftinfo.nwuse;
                                ampmax = 0.0;
                                for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                        if(cauto[shift1+iw].r > ampmax)
                                        ampmax = cauto[shift1+iw].r;
                                }
                                epsa[ishot] = predict.whitenoise*ampmax;
                        }
                }
        }

startShot:

        shift3 = 0;
        ishot = *shot - 1;
        shift1 = ishot*fftinfo.nwuse;

        for(iw = 0; iw < fftinfo.nwuse*predict.maxorder; iw++) {
                fsigfinal[iw].r = 0.0;
                fsigfinal[iw].i = 0.0;
        }

        /* Estimate source signal with 2 iterations.            */
        /* Why use 2 iterations: Omega SMA documents and        */
        /* personal conversation with Dr. Ikelle.               */
        for(iter = 0; iter < niter; iter++) {

                /* Compute cross-correlation. */
                for(iw = 0; iw < fftinfo.nwuse; iw++) {
                        ccros[iw].r = 0.0;
                        ccros[iw].i = 0.0;
                }
                for(j = 0; j < trsinfo.nrecv; j++) {
                        for(iw = 0; iw < fftinfo.nwcorr; iw++) {
                                ct[iw].r = 0.0;
                                ct[iw].i = 0.0;
                        }
                        for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                itemp = ishot*trsinfo.nrecv + j;
                                itemp1 = iw*size*trsinfo.nrecv;
                                itemp2 = itemp1*predict.maxorder + itemp;
                                itemp1 += itemp;
                                temp1 = data_in[itemp1].r*multip[itemp2].r
                                        + data_in[itemp1].i*multip[itemp2].i;
                                temp2 = data_in[itemp1].i*multip[itemp2].r
                                        - data_in[itemp1].r*multip[itemp2].i;
                                ct[iw + nwminG].r = temp1;
                                ct[iw + nwminG].i = temp2;
                                ct[fftinfo.nwcorr - iw - nwminG - 1].r = temp1;
                                ct[fftinfo.nwcorr - iw - nwminG - 1].i = -temp2;
                        }
                        ibsma_pfacc(+1, fftinfo.nwcorr, ct);

                        itemp1 = itdelay[ishot]+NTAPE+NWIN-1;
                        itemp2 = fftinfo.nwcorr-itdelay[ishot]-NTAPE-NWIN;
                        for(iw = itdelay[ishot]+NWIN; iw <= itemp1; iw++) {
                                ct[iw].r *= tape[itemp1-iw];
                        }
                        for(iw = itemp1+1; iw < itemp2; iw++) {
                                ct[iw].r = 0.0;
                        }
                        for(iw = itemp2; iw < itemp2+NTAPE; iw++) {
                                ct[iw].r *= tape[iw-itemp2];
                        }
                        for(iw = 0; iw < fftinfo.nwcorr; iw++) {
                                ct[iw].i = 0.0;
                        }

                        ibsma_pfacc(-1, fftinfo.nwcorr, ct);
                        for(iw = 0; iw < fftinfo.nwuse; iw++) {
                                ccros[iw].r += ct[iw + nwminG].r;
                                ccros[iw].i += ct[iw + nwminG].i;
                        }
                }

                /* Estimate source signal. */
                for(iw = 0; iw < fftinfo.nwuse; iw++) {
                  fsig[iw].r = -ccros[iw].r/(epsa[ishot] + cauto[shift1+iw].r);
                  fsig[iw].i = -ccros[iw].i/(epsa[ishot] + cauto[shift1+iw].r);
                }

                /* Update fsigfinal. */
                for(iw = 0; iw < fftinfo.nwuse; iw++) {
                        fsigfinal[iw].r += fsig[iw].r;
                        fsigfinal[iw].i += fsig[iw].i;
                }

                /* Update data_in. */
                if(iter < niter-1) {
                  for(j = 0; j < trsinfo.nrecv; j++) {
                    for(iw = 0; iw < fftinfo.nwuse; iw++) {
                      itemp = ishot*trsinfo.nrecv + j;
                      itemp1 = iw*size*trsinfo.nrecv;
                      itemp2 = itemp1*predict.maxorder + itemp;
                      itemp1 += itemp;
                      data_in[itemp1].r += multip[itemp2].r*fsig[iw].r
                                         - multip[itemp2].i*fsig[iw].i;
                      data_in[itemp1].i += multip[itemp2].r*fsig[iw].i
                                         + multip[itemp2].i*fsig[iw].r;
                    } 
                  }
                }
        }


        /* Obtain high-order source signal. */
        for(id = 1; id < predict.maxorder; id++) {
          itemp1 = (id-1)*fftinfo.nwuse;
          itemp2 = id*fftinfo.nwuse;
          for(iw = 0; iw < fftinfo.nwuse; iw++) {
            fsigfinal[iw+itemp2].r = fsigfinal[iw].r*fsigfinal[iw+itemp1].r
                                   - fsigfinal[iw].i*fsigfinal[iw+itemp1].i;
            fsigfinal[iw+itemp2].i = fsigfinal[iw].r*fsigfinal[iw+itemp1].i
                                   + fsigfinal[iw].i*fsigfinal[iw+itemp1].r;
                }
        }
        
startRecv:

        /* Finally, compute predicted multiples. */
        ishot = *shot - 1;
        irecv = *recv - 1;                
        j = ishot;
                k = irecv;

        /* Copy trace head. */
        shift3 = (k + trsinfo.nrecv * j) * nhead;
        for(it = 0; it < nhead; it++) {
                v[it] = trhead[it+shift3];
        }
        
        /* Set trace header. */
        itemp = 0;
        for(it = 0; it < nhead; it++) {
                hd[it] = v[it];
        }
        if(itemp != 0) {
                *ier = 1;
                return;
        }

        for(iw = 0; iw < fftinfo.nwuse; iw++) {

                itemp = iw*size*trsinfo.nrecv*predict.maxorder
                        + ishot*trsinfo.nrecv + k;
                temp1 = multip[itemp].r*fsigfinal[iw].r
                        - multip[itemp].i*fsigfinal[iw].i;
                temp2 = multip[itemp].r*fsigfinal[iw].i
                        + multip[itemp].i*fsigfinal[iw].r;

                ct[iw + nwminG].r = temp1;
                ct[iw + nwminG].i = temp2;

                for(id = 1; id < predict.maxorder; id++) {
                        itemp1 = id*trsinfo.nrecv;
                        itemp2 = id*fftinfo.nwuse;
                        temp1 = multip[itemp+itemp1].r*fsigfinal[iw+itemp2].r
                                - multip[itemp+itemp1].i*fsigfinal[iw+itemp2].i;
                        temp2 = multip[itemp+itemp1].r*fsigfinal[iw+itemp2].i
                                + multip[itemp+itemp1].i*fsigfinal[iw+itemp2].r;
                        ct[iw + nwminG].r += temp1;
                        ct[iw + nwminG].i += temp2;
                }
        }

        for(iw =0; iw < nwminG; iw++) {
                ct[iw].r = 0.0;
                ct[iw].i = 0.0;
        }
        for(iw = nwmaxG; iw < fftinfo.nw; iw++) {
                ct[iw].r = 0.0;
                ct[iw].i = 0.0;
        }

        /* Apply inverse FFT. */
        for(it = 0; it < fftinfo.ntfft; it++) {
                rt[it] = 0.0;
        }

        ibsma_pfacr(-1, fftinfo.ntfft, ct, rt);

        /* Output trace. */
        if(trsinfo.dtype == 2) {
                for(it = 0; it < trsinfo.nt; it++) {
                        trace[it] = -rt[it]*predict.scale;

                }
        } else {
                for(it = 0; it < trsinfo.nt; it++) {
                        /* Add extra dt to avoid divid by zero. */
                        temp = sqrt((it+1)*trsinfo.dt+trsinfo.t0);
                        trace[it] = -rt[it]*predict.scale/temp;
                }
        }        

        itemp = 0;
        for(it = 0; it < trsinfo.nt; it++) {
                tr[it] = trace[it];
        }
        if(itemp != 0) {
                *ier = 1;
                return;
        }
        
        return;
}


/*------------------------- ibsma_output2 ------------------------------*/
/*------------------------- ibsma_output2 ------------------------------*/
/*------------------------- ibsma_output2 ------------------------------*/
void ibsma_output2 (INTEGER *ier, INTEGER *shotg, INTEGER *shot, INTEGER *recv,
                DOUBLE *hd, REAL *tr)
{
   int itemp, it;
   long nbytes;
   long lsize;

   /* Read in 1 trace header. */
   itemp = nhead;
   lsize = itemp * sizeof(double);
   nbytes = pfio_read(ofd_head, (char*)trhead, lsize);
   if(nbytes != lsize) {
      printf("Output2: Failed to read from temp header file.\n");
      *ier = 1;
      return;
   }

   /* Read in 1 original trace data. */
   itemp = trsinfo.nt;
   lsize = itemp * sizeof(float);
   nbytes = pfio_read(ofd_orig, (char*)trorig, lsize);
   if(nbytes != lsize) {
      printf("Output2: Failed to read from temp original file.\n");
      *ier = 1;
      return;
   }

   /* Set trace header. */
   for(it = 0; it < nhead; it++) {
      hd[it] = trhead[it];
   }

   for(it = 0; it < trsinfo.nt; it++) {
      tr[it] = trorig[it];
   }

   return;
}


/*------------------------- ibsma_output3 ------------------------------*/
/*------------------------- ibsma_output3 ------------------------------*/
/*------------------------- ibsma_output3 ------------------------------*/
void ibsma_output3 (INTEGER *ier, INTEGER *shotg, INTEGER *shot, INTEGER *recv,
                DOUBLE *hd, REAL *tr)
{
   int j, k;
   int it, iw, itemp;
   int size, ishot, irecv, nread;
   int istat;
   long long shiftS;
   /*int shiftS;*/
   float  temp;
   double v[256];
   long nbytes;
   long lsize;

   nread = *shotg - 1;
   if(nread < (output.shotnread-1))
      size = output.shotnum;
   else
      size = output.shotlast;

   if(*shotg == 1 && *shot == 1 && *recv == 1) {
      istat = pfio_seek_via_origin(ofd_head, 0L, SEEK_SET);
      if(istat != 0) {
         printf("Output3: Failed to seek in temp header file.\n");
         *ier = 1;
         return;
      }
   }

   if(*shot == 1 && *recv == 1) goto startGath;
   if(*shot != 1 && *recv == 1) goto startShot;
   if(*recv != 1) goto startRecv;

startGath:
   /* Read in "size" input shot gather trace header. */
   itemp = size*trsinfo.nrecv*nhead;
   lsize = itemp * sizeof(double);
   nbytes = pfio_read(ofd_head, (char*)trhead, lsize);
   if(nbytes != lsize) {
      printf("Output3: Failed to read from temp header file.\n");
      *ier = 1;
      return;
   }

   /* Read in "size" of input shot gathers and of predicted */
   /* multipled shot gathers including all orders.          */
   ishot = output.nshotBeg + output.shotnum*nread;
   shiftS = (long long)ishot * (long long)trsinfo.nrecv *
            (long long)sizeof(complex);
   istat = pfio_seek_via_origin(ofd_mltp, shiftS, SEEK_SET);
   if(istat != 0) {
      printf("Output3: Failed to seek in temp multiple file.\n");
      *ier = 1;
      return;
   }
   shift1 = (trsinfo.nshot-size)*trsinfo.nrecv*sizeof(complex);
   shift2 = 0;
   itemp = trsinfo.nrecv*size;
   for(iw = 0; iw < fftinfo.nwuse*predict.maxorder; iw++) {
      lsize = itemp * sizeof(complex);
      nbytes = pfio_read(ofd_mltp, (char*)(multip+shift2), lsize);
      if(nbytes != lsize) {
         printf("Output3: Failed to read from temp multiple file.\n");
         *ier = 1;
         return;
      }
      istat = pfio_seek_via_origin(ofd_mltp, shift1, SEEK_CUR);
      if(istat != 0) {
         printf("Output3: Failed to seek in temp multiple file.\n");
         *ier = 1;
         return;
      }
      shift2 += itemp;
   }

startShot:


startRecv:

   /* Finally, compute predicted multiples. */
   ishot = *shot - 1;
   irecv = *recv - 1;                
   j = ishot;
   k = irecv;

   /* Copy trace head. */
   shift3 = (k + trsinfo.nrecv * j) * nhead;
   for(it = 0; it < nhead; it++) {
      v[it] = trhead[it+shift3];
   }

   /* Set trace header. */
   itemp = 0;
   for(it = 0; it < nhead; it++) {
      hd[it] = v[it];
   }
   if(itemp != 0) {
      *ier = 1;
      return;
   }

   for(iw = 0; iw < fftinfo.nwuse; iw++) {

      itemp = iw*size*trsinfo.nrecv*predict.maxorder
            + ishot*trsinfo.nrecv + k;
      ct[iw + nwminG].r = multip[itemp].r;
      ct[iw + nwminG].i = multip[itemp].i;
   }

   for(iw =0; iw < nwminG; iw++) {
      ct[iw].r = 0.0;
      ct[iw].i = 0.0;
   }
   for(iw = nwmaxG; iw < fftinfo.nw; iw++) {
      ct[iw].r = 0.0;
      ct[iw].i = 0.0;
   }

   /* Apply inverse FFT. */
   for(it = 0; it < fftinfo.ntfft; it++) {
      rt[it] = 0.0;
   }

   ibsma_pfacr(-1, fftinfo.ntfft, ct, rt);
   
   /* Output trace. 
      (1) Theoreticly, predict.scale should be squred since matrix
          multiplation is involved.
          However, we also know that final output has double source
          singalature, we choose predict.scale as an approximation
          of removing the extra source signature 
      (2) scalard is a scale factor related to temporal and spatial FFT */
   /* for 2D synthetic data */
   if(trsinfo.dtype == 2) {
      for(it = 0; it < trsinfo.nt; it++) {
          trace[it] = -rt[it]*predict.scale*predict.mltp_scale/(scalard);
      }

   /* for 3D field data */
   /* sqrt(time) is used to correct 3D geometric spreading when using 
      this 2D algorithm */
   } else {
      trace[0] = 0.0;
      for(it = 1; it < trsinfo.nt; it++) {
          temp = sqrt(it*trsinfo.dt+trsinfo.t0);  
          trace[it] = -rt[it]*predict.scale*predict.mltp_scale/(scalard*temp);
      }
   }        

   itemp = 0;
   for(it = 0; it < trsinfo.nt; it++) {
      tr[it] = trace[it];
   }
   if(itemp != 0) {
      *ier = 1;
      return;
   }

   return;
}


/*------------------------- ibsma_output4 ------------------------------*/
/*------------------------- ibsma_output4 ------------------------------*/
/*------------------------- ibsma_output4 ------------------------------*/
void ibsma_output4 (INTEGER *ier)
{
   int istat, nfatal = 0;

   *ier = 0;

   /* Print info. */
   printf("\n***** Output: completed *****\n\n");

   /* Free space. */

   free(itdelay);

   free(epsa);

   free(trace);

   free(rt);

   free(ct);
   free(cauto);
   free(ccros);
   free(fsig);
   free(fsigfinal);
   free(trhead);
   free(data_in);
   free(multip);

   if(predict.test) {
      free(nwlow);
      free(nwhigh);
   }

   /* Close files. */
   istat = pfio_close(ofd_head);
   if(istat < 0) {
      printf("Output4: Failed to close temp header file.\n");
      nfatal++;
   }
   istat = pfio_close(ofd_mltp);
   if(istat < 0) {
      printf("Output4: Failed to close temp multiple file.\n");
      nfatal++;
   }
   istat = pfio_close(ofd_orig);
   if(istat < 0) {
      printf("Output4: Failed to close temp original file.\n");
      nfatal++;
   }

   if(cleanup) {
      pfio_delete(filename2);
      pfio_delete(filename3);
      pfio_delete(filename4);
   }

   return;
}


/*------------------------- ibsma_fftsetup -----------------------------*/
/*------------------------- ibsma_fftsetup -----------------------------*/
/*------------------------- ibsma_fftsetup -----------------------------*/

void ibsma_fftsetup(int *ier, int nt, int blocklength, int npad,
              int *ntfft, int *nwcorr, int *nxfft)
{
   /************************************************************/
   /*        Parameter setup of FFT                            */
   /* Name    : fftsetup                                       */
   /* Author  : Yunqing Shen                                   */
   /* Date    : Sept. 20, 2002                                 */
   /*                                                          */
   /* Input:                                                   */
   /*   int *nt         : # of trace samples for FFT           */
   /*   int *blocklength: # of space samples for FFT           */
   /*   int *npad       : input trace padding                  */
   /* Output:                                                  */
   /*   int *ntfft: # of samples needed for time FFT           */
   /*   int *nxfft: # of samples needed for space FFT          */
   /************************************************************/        

   int i,itemp;

   nt *= 2;
   *ntfft = ibsma_npfaro(nt, LOOKFAC * nt);
   if (*ntfft >= SU_NFLTS || *ntfft >= PFA_MAX) {
      printf("FFT setup: ntfft(=%d) is too big.\n", *ntfft);
      *ier = 1;
      return;
   }

   itemp = blocklength;
   for(i = 0; i < 100; i++) {
      itemp = ibsma_npfa(itemp);
      if(itemp != (itemp/2)*2) 
          itemp++;
      else
          break;
   }
   if(itemp != (itemp/2)*2) {
      printf("FFT setup: nxfft(=%d) is not an even number.\n", itemp);
      *ier = 1;
      return;
   }

   *nxfft = itemp;
   if (*nxfft >= SU_NFLTS || *nxfft >= PFA_MAX) {
      printf("FFT setup: nxfft(=%d) is too big.\n", *nxfft);
      *ier = 1;
      return;
   }

   *nwcorr = ibsma_npfa(2*((*ntfft)/2+1));
   if ((*nwcorr-1)*2 >= SU_NFLTS || (*nwcorr-1)*2 >= PFA_MAX) {
      printf("FFT setup: nwcorr(=%d) is too big.\n", *nwcorr);
      *ier = 1;
      return;
   }

   return;
}


#define NTAB 240
static struct {
int n;  float c;
} nctab[NTAB] = {
{       1, 0.000052 },
{       2, 0.000061 },
{       3, 0.000030 },
{       4, 0.000053 },
{       5, 0.000066 },
{       6, 0.000067 },
{       7, 0.000071 },
{       8, 0.000062 },
{       9, 0.000079 },
{      10, 0.000080 },
{      11, 0.000052 },
{      12, 0.000069 },
{      13, 0.000103 },
{      14, 0.000123 },
{      15, 0.000050 },
{      16, 0.000086 },
{      18, 0.000108 },
{      20, 0.000101 },
{      21, 0.000098 },
{      22, 0.000135 },
{      24, 0.000090 },
{      26, 0.000165 },
{      28, 0.000084 },
{      30, 0.000132 },
{      33, 0.000158 },
{      35, 0.000138 },
{      36, 0.000147 },
{      39, 0.000207 },
{      40, 0.000156 },
{      42, 0.000158 },
{      44, 0.000176 },
{      45, 0.000171 },
{      48, 0.000185 },
{      52, 0.000227 },
{      55, 0.000242 },
{      56, 0.000194 },
{      60, 0.000215 },
{      63, 0.000233 },
{      65, 0.000288 },
{      66, 0.000271 },
{      70, 0.000248 },
{      72, 0.000247 },
{      77, 0.000285 },
{      78, 0.000395 },
{      80, 0.000285 },
{      84, 0.000209 },
{      88, 0.000332 },
{      90, 0.000321 },
{      91, 0.000372 },
{      99, 0.000400 },
{     104, 0.000391 },
{     105, 0.000358 },
{     110, 0.000440 },
{     112, 0.000367 },
{     117, 0.000494 },
{     120, 0.000413 },
{     126, 0.000424 },
{     130, 0.000549 },
{     132, 0.000480 },
{     140, 0.000450 },
{     143, 0.000637 },
{     144, 0.000497 },
{     154, 0.000590 },
{     156, 0.000626 },
{     165, 0.000654 },
{     168, 0.000536 },
{     176, 0.000656 },
{     180, 0.000611 },
{     182, 0.000730 },
{     195, 0.000839 },
{     198, 0.000786 },
{     208, 0.000835 },
{     210, 0.000751 },
{     220, 0.000826 },
{     231, 0.000926 },
{     234, 0.000991 },
{     240, 0.000852 },
{     252, 0.000820 },
{     260, 0.001053 },
{     264, 0.000987 },
{     273, 0.001152 },
{     280, 0.000952 },
{     286, 0.001299 },
{     308, 0.001155 },
{     312, 0.001270 },
{     315, 0.001156 },
{     330, 0.001397 },
{     336, 0.001173 },
{     360, 0.001259 },
{     364, 0.001471 },
{     385, 0.001569 },
{     390, 0.001767 },
{     396, 0.001552 },
{     420, 0.001516 },
{     429, 0.002015 },
{     440, 0.001748 },
{     455, 0.001988 },
{     462, 0.001921 },
{     468, 0.001956 },
{     495, 0.002106 },
{     504, 0.001769 },
{     520, 0.002196 },
{     528, 0.002127 },
{     546, 0.002454 },
{     560, 0.002099 },
{     572, 0.002632 },
{     585, 0.002665 },
{     616, 0.002397 },
{     624, 0.002711 },
{     630, 0.002496 },
{     660, 0.002812 },
{     693, 0.002949 },
{     715, 0.003571 },
{     720, 0.002783 },
{     728, 0.003060 },
{     770, 0.003392 },
{     780, 0.003553 },
{     792, 0.003198 },
{     819, 0.003726 },
{     840, 0.003234 },
{     858, 0.004354 },
{     880, 0.003800 },
{     910, 0.004304 },
{     924, 0.003975 },
{     936, 0.004123 },
{     990, 0.004517 },
{    1001, 0.005066 },
{    1008, 0.003902 },
{    1040, 0.004785 },
{    1092, 0.005017 },
{    1144, 0.005599 },
{    1155, 0.005380 },
{    1170, 0.005730 },
{    1232, 0.005323 },
{    1260, 0.005112 },
{    1287, 0.006658 },
{    1320, 0.005974 },
{    1365, 0.006781 },
{    1386, 0.006413 },
{    1430, 0.007622 },
{    1456, 0.006679 },
{    1540, 0.007032 },
{    1560, 0.007538 },
{    1584, 0.007126 },
{    1638, 0.007979 },
{    1680, 0.007225 },
{    1716, 0.008961 },
{    1820, 0.008818 },
{    1848, 0.008427 },
{    1872, 0.009004 },
{    1980, 0.009398 },
{    2002, 0.010830 },
{    2145, 0.012010 },
{    2184, 0.010586 },
{    2288, 0.012058 },
{    2310, 0.011673 },
{    2340, 0.011700 },
{    2520, 0.011062 },
{    2574, 0.014313 },
{    2640, 0.013021 },
{    2730, 0.014606 },
{    2772, 0.013216 },
{    2860, 0.015789 },
{    3003, 0.016988 },
{    3080, 0.014911 },
{    3120, 0.016393 },
{    3276, 0.016741 },
{    3432, 0.018821 },
{    3465, 0.018138 },
{    3640, 0.018892 },
{    3696, 0.018634 },
{    3960, 0.020216 },
{    4004, 0.022455 },
{    4095, 0.022523 },
{    4290, 0.026087 },
{    4368, 0.023474 },
{    4620, 0.024590 },
{    4680, 0.025641 },
{    5005, 0.030303 },
{    5040, 0.025253 },
{    5148, 0.030364 },
{    5460, 0.031250 },
{    5544, 0.029412 },
{    5720, 0.034404 },
{    6006, 0.037500 },
{    6160, 0.034091 },
{    6435, 0.040214 },
{    6552, 0.037221 },
{    6864, 0.042735 },
{    6930, 0.040214 },
{    7280, 0.042980 },
{    7920, 0.045872 },
{    8008, 0.049505 },
{    8190, 0.049834 },
{    8580, 0.055762 },
{    9009, 0.057034 },
{    9240, 0.054945 },
{    9360, 0.056818 },
{   10010, 0.066667 },
{   10296, 0.065502 },
{   10920, 0.068182 },
{   11088, 0.065217 },
{   11440, 0.075000 },
{   12012, 0.078534 },
{   12870, 0.087719 },
{   13104, 0.081081 },
{   13860, 0.084270 },
{   15015, 0.102740 },
{   16016, 0.106383 },
{   16380, 0.105634 },
{   17160, 0.119048 },
{   18018, 0.123967 },
{   18480, 0.119048 },
{   20020, 0.137615 },
{   20592, 0.140187 },
{   21840, 0.154639 },
{   24024, 0.168539 },
{   25740, 0.180723 },
{   27720, 0.180723 },
{   30030, 0.220588 },
{   32760, 0.241935 },
{   34320, 0.254237 },
{   36036, 0.254237 },
{   40040, 0.288462 },
{   45045, 0.357143 },
{   48048, 0.357143 },
{   51480, 0.384615 },
{   55440, 0.384615 },
{   60060, 0.454545 },
{   65520, 0.517241 },
{   72072, 0.576923 },
{   80080, 0.625000 },
{   90090, 0.833333 },
{  102960, 0.789474 },
{  120120, 1.153846 },
{  144144, 1.153846 },
{  180180, 1.875000 },
{  240240, 2.500000 },
{  360360, 3.750000 },
{  720720, 7.500000 },
};


/*--------------------------- ibsma_npfa ------------------------------*/
/*--------------------------- ibsma_npfa ------------------------------*/
/*--------------------------- ibsma_npfa ------------------------------*/
int ibsma_npfa (int nmin)
{
/************************************************************************/
/* Return smallest valid n not less than nmin for prime factor fft.     */
/* **********************************************************************/
/* Input:                                                               */
/*   nmin     lower bound on returned value (see notes below)           */
/*                                                                      */
/* Returned:  valid n for prime factor fft                              */
/* **********************************************************************/
/* Notes:                                                               */
/*   The returned n will be composed of mutually prime factors from     */
/*   the set {2,3,4,5,7,8,9,11,13,16}.  Because n cannot exceed         */
/*   720720 = 5*7*9*11*13*16, 720720 is returned if nmin exceeds 720720.*/
/* **********************************************************************/
/* Author:  Dave Hale, Colorado School of Mines, 04/28/89               */
/* Modified:  Dave Hale, Colorado School of Mines, 08/05/91             */
/* For efficiency, use pre-computed table of valid n and costs.         */
/* **********************************************************************/
   int i;
   for(i = 0; i < NTAB-1 && nctab[i].n<nmin; i++);
   return(nctab[i].n);
}


/*--------------------------- ibsma_npfaro ----------------------------*/
/*--------------------------- ibsma_npfaro ----------------------------*/
/*--------------------------- ibsma_npfaro ----------------------------*/
int ibsma_npfaro (int nmin, int nmax)
{
/****************************************************************************/
/* Return optimal n between nmin and nmax for real-to-complex or            */
/* complex-to-real prime factor ffts                                        */
/* **************************************************************************/
/* Input:                                                                   */
/*   nmin       lower bound on returned value                               */
/*   nmax       desired (but not guaranteed) upper bound on returned value  */
/*                                                                          */
/* Returned:  valid n for real-to-complex/complex-to-real prime factor fft  */
/* **************************************************************************/
/* Notes:                                                                   */
/*   Current implemenations of real-to-complex and complex-to-real prime    */
/*   factor ffts require that the transform length n be even and that n/2   */
/*   be a valid length for a complex-to-complex prime factor fft.  The      */
/*   value returned by npfaro satisfies these conditions.  Also, see notes  */
/*   for npfao.                                                             */
/* **************************************************************************/
/* Author:  Dave Hale, Colorado School of Mines, 06/16/89                   */
/****************************************************************************/
   return(2*ibsma_npfao((nmin+1)/2,(nmax+1)/2));
}


/*--------------------------- ibsma_npfao -----------------------------*/
/*--------------------------- ibsma_npfao -----------------------------*/
/*--------------------------- ibsma_npfao -----------------------------*/
int ibsma_npfao (int nmin, int nmax)
{
/****************************************************************************/
/* Return optimal n between nmin and nmax for prime factor fft.             */
/* **************************************************************************/
/* Input:                                                                   */
/* nmin       lower bound on returned value (see notes below)               */
/* nmax       desired (but not guaranteed) upper bound on returned value    */
/*                                                                          */
/* Returned:  valid n for prime factor fft                                  */
/* **************************************************************************/
/* Notes:                                                                   */
/* The returned n will be composed of mutually prime factors from           */
/* the set {2,3,4,5,7,8,9,11,13,16}.  Because n cannot exceed               */
/* 720720 = 5*7*9*11*13*16, 720720 is returned if nmin exceeds 720720.      */
/* If nmin does not exceed 720720, then the returned n will not be          */
/* less than nmin.  The optimal n is chosen to minimize the estimated       */
/* cost of performing the fft, while satisfying the constraint, if          */
/* possible, that n not exceed nmax.                                        */
/* **************************************************************************/
/* Author:  Dave Hale, Colorado School of Mines, 06/13/89                   */
/* Modified:  Dave Hale, Colorado School of Mines, 08/05/91                 */
/*         For efficiency, use pre-computed table of valid n and costs.     */
/* **************************************************************************/
   int i,j;
   for (i = 0; i < NTAB-1 && nctab[i].n < nmin; i++);
   for (j = i+1; j < NTAB-1 && nctab[j].n <= nmax; j++) {
      if (nctab[j].c < nctab[i].c) i = j;
   }
   return(nctab[i].n);
}


/*------------------------- ibsma_dumpparms --------------------------------*/
/*------------------------- ibsma_dumpparms --------------------------------*/
/*------------------------- ibsma_dumpparms --------------------------------*/

void ibsma_dumpparms(
               TraceInfo  trsinfo,
               FftInfo    fftinfo,
               Prediction predict,
               DPrep      dprep,
               OutPut    output)
{
   printf("\n");
   printf("***** SMA parameter list *****\n");
   printf("\n");

   printf("Trace information(host=%s):\n", hostname);
   printf("WORKDIR     = %s\n", trsinfo.WORKDIR);
   printf("dtype       = %d\n", trsinfo.dtype);
   printf("nshottotal  = %d\n", trsinfo.nshottotal);
   printf("nshot       = %d\n", trsinfo.nshot);
   printf("nrecv       = %d\n", trsinfo.nrecv);
   printf("dshot       = %g\n", trsinfo.dshot);
   printf("drecv       = %g\n", trsinfo.drecv);
   printf("nt          = %d\n", trsinfo.nt);
   printf("dt          = %g\n", trsinfo.dt);
   printf("t0          = %g\n", trsinfo.t0);

   printf("\n");
   printf("FFT information:\n");
   printf("fmax   = %g\n", fftinfo.fmax);
   printf("ntfft  = %d\n", fftinfo.ntfft);
   printf("nw     = %d\n", fftinfo.nw);
   printf("nnw    = %d\n", fftinfo.nnw);
   printf("nwuse  = %d\n", fftinfo.nwuse);
   printf("nwcorr = %d\n", fftinfo.nwcorr);
   printf("dw     = %g\n", fftinfo.dw);
   printf("nxfft  = %d\n", fftinfo.nxfft);
   printf("dkx    = %g\n", fftinfo.dkx);
   printf("nwmin  = %d\n", fftinfo.nwmin);
   printf("nwmax  = %d\n", fftinfo.nwmax);

   printf("\n");
   printf("Prediction information:\n");
   printf("test        = %d\n", predict.test);
   printf("maxorder    = %d\n", predict.maxorder);
   printf("restart     = %d\n", predict.restart);
   printf("vwater      = %g\n", predict.vwater);
   printf("mltp_scale  = %g\n", predict.mltp_scale);
   printf("ntestshot   = %d\n", predict.ntestshot);
   printf("ntestbeg    = %d\n", predict.ntestbeg);

   printf("\n");
   printf("Data preparation information:\n");
   printf("groupsize = %d\n", dprep.groupsize);
   printf("ngroup    = %d\n", dprep.ngroup);
   printf("grouplast = %d\n", dprep.grouplast);

   printf("\n");
   printf("Output information:\n");
   printf("nshotBeg  = %d\n", output.nshotBeg);
   printf("nshotEnd  = %d\n", output.nshotEnd);
   printf("totshot   = %d\n", output.totshot);
   printf("shotnum   = %d\n", output.shotnum);
   printf("shotnread = %d\n", output.shotnread);
   printf("shotlast  = %d\n", output.shotlast);

   fflush(stdout);
}


/*------------------------- ibsma_lpfilter --------------------------------*/
/*------------------------- ibsma_lpfilter --------------------------------*/
/*------------------------- ibsma_lpfilter --------------------------------*/
int ibsma_lpfilter(float fmax,  float dt, float dw,
             int   ntfft, float *filter)
{
   /***********************************************************/
   /*                  Lowpass filter                         */
   /* Name    : lpfilter                                      */
   /* Author  : Yunqing Shen                                  */
   /* Date    : Oct. 29, 2001                                 */
   /*                                                         */
   /* Input:                                                  */
   /*   float fmax : max frequency                            */
   /*   float dt   : sample rate                              */
   /*   float dw   : frequency rate                           */
   /*   int   ntfft: # of samples needed for FFT              */
   /* Output:                                                 */
   /*   float *filter[ntfft/2+1]: designed filter             */
   /***********************************************************/        

   int nw=ntfft/2+1;
   float f[4];                        /* Array of filter frequencies */
   float amps[4] = {0.0,1.0,1.0,0.0}; /* Array of amplitude values */
   float *ftemp;
   int i,ierr=0,npoly=4,iw1,iw3;

   f[0] = 0.0;
   f[1] = FSLOPE;
   f[2] = fmax-FSLOPE;
   f[3] = fmax; 

   /* Build the polygonal filter[]. */
   for(i = 0; i < nw; i++) {
      filter[i] = 0.0;
   }
   ibsma_polygonalFilter(f,amps,npoly,ntfft,dt,filter);

   iw1 = MAX(0,MIN(nw-1,NINT(2.0*PI*FSLOPE/dw)));
   iw3 = MAX(0,MIN(nw-1,NINT(2.0*PI*fmax/dw)));
   iw1 *= 3;

   /* Find the left half Hann_window. */
   ftemp = (float*)malloc((iw1+1)*sizeof(float));
   if(ftemp == NULL) {
      printf("Filter: Failed to alloc ftemp.\n");
      ierr++;
   }
   ibsma_Hann_window(ftemp, iw1);

   /* Remove bandpass in the low end. */
   for(i = 0; i <= iw1; i++) {
      filter[i] = filter[iw1+1];
   }

   /* Superimpose Hann_window on polygonal. */
   for(i = 0; i <= iw1; i++) {
      filter[iw3-i] *= ftemp[i];
   }

   free(ftemp);
   return(ierr);
}


/*---------------------- ibsma_Hann_window ----------------------------*/
/*---------------------- ibsma_Hann_window ----------------------------*/
/*---------------------- ibsma_Hann_window ----------------------------*/
void ibsma_Hann_window(float *filter,int iw1)
{
   int iw;
   float dw,wiw;
   dw=2.0*PI/(iw1*2);
   for (iw = 0; iw <= iw1; iw++) {
      wiw = 0.5*(1.0-cos(dw*iw));
      filter[iw]=wiw;
   }
}


/*--------------------- ibsma_polygonalFilter -------------------------*/
/*--------------------- ibsma_polygonalFilter -------------------------*/
/*--------------------- ibsma_polygonalFilter -------------------------*/
void ibsma_polygonalFilter(float *f, float *amps, int npoly,
                     int nfft, float dt, float *filter)

{
   /****************************************************************/
   /*             Polygonal filter with sin^2 tapering             */
   /* **************************************************************/
   /* Input:                                                       */
   /*   f         array[npoly] of frequencies defining the filter  */
   /*   amps      array[npoly] of amplitude values                 */
   /*   npoly     size of input f and amps arrays                  */
   /*   dt        time sampling interval                           */
   /*   nfft      number of points in the fft                      */
   /*                                                              */
   /* Output:                                                      */
   /*   filter    array[nfft] filter values                        */
   /* **************************************************************/
   /* Notes: Filter is to be applied in the frequency domain       */
   /* **************************************************************/
   /* Author:  CWP: John Stockwell   1992                          */
   /****************************************************************/

   int intfr[npoly];       /* Integerizations of f */
   int icount,ifs;         /* Lop counting variables */
   int taper=0;            /* Flag counter */
   int nf;                 /* Number of frequencies (incl Nyq) */
   int nfm1;               /* nf-1 */
   float onfft;            /* Reciprocal of nfft */
   float df;               /* Frequency spacing (from dt) */

   nf = nfft/2 + 1;
   nfm1 = nf - 1;
   onfft = 1.0 / nfft;

   /* Compute array of integerized frequencies that define the filter. */
   df = onfft / dt;
   for(ifs = 0; ifs < npoly; ifs++) {
      intfr[ifs] = NINT(f[ifs]/df);
      if (intfr[ifs] > nfm1) intfr[ifs] = nfm1;
   }

   /* Build filter, with scale, and taper specified by amps[] values. */

   /* Do low frequency end first. */
   for(icount = 0; icount < intfr[0] ; icount++) {
      filter[icount] = amps[0] * onfft;
   }

   /* Now do the middle frequencies. */
   for(ifs = 0 ; ifs < npoly-1; ifs++){
      if(amps[ifs] < amps[ifs+1]) {        
         taper++;
         for(icount = intfr[ifs]; icount <= intfr[ifs+1]; icount++) {
            float c = PIBY2 / (intfr[ifs+1] - intfr[ifs] + 2);
            float s = sin(c*(icount - intfr[ifs] + 1));
            float adiff = amps[ifs+1] - amps[ifs];
            filter[icount] = (amps[ifs] + adiff*s*s) * onfft;
         }
      } else if (amps[ifs] > amps[ifs+1]) {        
         taper++;
         for(icount = intfr[ifs]; icount <= intfr[ifs+1]; icount++) {
            float c = PIBY2 / (intfr[ifs+1] - intfr[ifs] + 2);
            float s = sin(c*(intfr[ifs+1] - icount + 1));
            float adiff = amps[ifs] - amps[ifs+1];
            filter[icount] = (amps[ifs+1] + adiff*s*s) * onfft;
         }
      } else {
         if(!(taper)) {
            for(icount=intfr[ifs]; icount <= intfr[ifs+1]; icount++)
               filter[icount] = amps[ifs] * onfft;
         } else {
            for(icount=intfr[ifs]+1; icount <= intfr[ifs+1]; icount++)
               filter[icount] = amps[ifs] * onfft;
         }
      }
   }

   /* Finally do the high frequency end. */
   for(icount = intfr[npoly-1]+1; icount < nf; icount++){
      filter[icount] = amps[npoly-1] * onfft;
   }

   return;
}


/*-------------------------- ibsma_pfacr ------------------------------*/
/*-------------------------- ibsma_pfacr ------------------------------*/
/*-------------------------- ibsma_pfacr ------------------------------*/

void ibsma_pfacr (int isign, int n, complex cz[], float rz[])
{
  /****************************************************************************/
  /* Prime factor fft:  complex to real transform                             */
  /* **************************************************************************/
  /* Input:                                                                   */
  /*   isign  sign of isign is the sign of exponent in fourier kernel         */
  /*   n      length of transform (see notes below)                           */
  /*   cz     array[n/2+1] of complex values (may be equivalenced to rz)      */
  /*                                                                          */
  /* Output:                                                                  */
  /*   rz     array[n] of real values (may be equivalenced to cz)             */
  /* **************************************************************************/
  /* Notes:                                                                   */
  /*   Because pfacr uses pfacc to do most of the work, n must be even        */
  /*   and n/2 must be a valid length for pfacc.  The simplest way to         */
  /*   obtain a valid n is via n = npfar(nmin).  A more optimal n can be      */
  /*   obtained with npfaro.                                                  */
  /* **************************************************************************/
  /* References:                                                              */
  /*   Press et al, 1988, Numerical Recipes in C, p. 417.                     */
  /*                                                                          */
  /* Also, see notes and references for function pfacc.                       */
  /* **************************************************************************/
  /* Author:  Dave Hale, Colorado School of Mines, 06/13/89                   */
  /****************************************************************************/
   int i,ir,ii,jr,ji,no2;
   float *z,tempr,tempi,sumr,sumi,difr,difi;
   double wr,wi,wpr,wpi,wtemp,theta;
   int tempn;

   /* Copy input to output and fix dc and nyquist. */
   z = (float *)cz;
   for (i = 2; i < n; i++) {
      rz[i] = z[i];
   }
   rz[1] = z[0]-z[n];
   rz[0] = z[0]+z[n];
   z = rz;

   /* Initialize cosine-sine recurrence. */
   theta = 2.0*PI/(double)n;
   if (isign > 0) theta = -theta;
   wtemp = sin(0.5*theta);
   wpr = -2.0*wtemp*wtemp;
   wpi = sin(theta);
   wr = 1.0+wpr;
   wi = wpi;

   /* Twiddle. */
   no2 = n/2;
   for (ir=2,ii=3,jr=n-2,ji=n-1; ir<=no2; ir+=2,ii+=2,jr-=2,ji-=2) {
      sumr = z[ir]+z[jr];
      sumi = z[ii]+z[ji];
      difr = z[ir]-z[jr];
      difi = z[ii]-z[ji];
      tempr = wi*difr-wr*sumi;
      tempi = wi*sumi+wr*difr;
      z[ir] = sumr+tempr;
      z[ii] = difi+tempi;
      z[jr] = sumr-tempr;
      z[ji] = tempi-difi;
      wtemp = wr;
      wr += wr*wpr-wi*wpi;
      wi += wi*wpr+wtemp*wpi;
   }

   /* Do complex to complex transform. */
   tempn = n/2;
   ibsma_pfacc(isign, tempn, (complex *)z);
}


/*-------------------------- ibsma_pfarc ------------------------------*/
/*-------------------------- ibsma_pfarc ------------------------------*/
/*-------------------------- ibsma_pfarc ------------------------------*/
void ibsma_pfarc (int isign, int n, float rz[], complex cz[])
{
  /****************************************************************************/
  /* Prime factor fft:  real to complex transform                             */
  /* **************************************************************************/
  /* Input:                                                                   */
  /*   isign    sign of isign is the sign of exponent in fourier kernel       */
  /*   n        length of transform; must be even (see notes below)           */
  /*   rz       array[n] of real values (may be equivalenced to cz)           */
  /*                                                                          */
  /* Output:                                                                  */
  /*   cz       array[n/2+1] of complex values (may be equivalenced to rz)    */
  /* **************************************************************************/
  /* Notes:                                                                   */
  /*   Because pfarc uses pfacc to do most of the work, n must be even        */
  /*   and n/2 must be a valid length for pfacc.  The simplest way to         */
  /*   obtain a valid n is via n = npfar(nmin).  A more optimal n can be      */
  /*   obtained with npfaro.                                                  */
  /* **************************************************************************/
  /* References:                                                              */
  /*   Press et al, 1988, Numerical Recipes in C, p. 417.                     */
  /*                                                                          */
  /* Also, see notes and references for function pfacc.                       */
  /* **************************************************************************/
  /* Author:  Dave Hale, Colorado School of Mines, 06/13/89                   */
  /****************************************************************************/
   int i,ir,ii,jr,ji,no2;
   float *z,tempr,tempi,sumr,sumi,difr,difi;
   double wr,wi,wpr,wpi,wtemp,theta;
   int tempn;

   /* Copy input to output while scaling. */
   z = (float *)cz;
   
   for (i = 0; i < n; i++) {
      z[i] = 0.5*rz[i];
   }

   /* Do complex to complex transform. */
   tempn = n/2;
   ibsma_pfacc(isign, tempn, cz);

   /* Fix dc and nyquist */
   z[n] = 2.0*(z[0]-z[1]);
   z[0] = 2.0*(z[0]+z[1]);
   z[n+1] = 0.0;
   z[1] = 0.0;

   /* Initialize cosine-sine recurrence. */
   theta = 2.0*PI/(double)n;
   if (isign < 0) theta = -theta;
   wtemp = sin(0.5*theta);
   wpr = -2.0*wtemp*wtemp;
   wpi = sin(theta);
   wr = 1.0+wpr;
   wi = wpi;

   /* Twiddle. */
   no2 = n/2;
   for (ir=2,ii=3,jr=n-2,ji=n-1; ir<=no2; ir+=2,ii+=2,jr-=2,ji-=2) {
      sumr = z[ir]+z[jr];
      sumi = z[ii]+z[ji];
      difr = z[ir]-z[jr];
      difi = z[ii]-z[ji];
      tempr = wi*difr+wr*sumi;
      tempi = wi*sumi-wr*difr;
      z[ir] = sumr+tempr;
      z[ii] = difi+tempi;
      z[jr] = sumr-tempr;
      z[ji] = tempi-difi;
      wtemp = wr;
      wr += wr*wpr-wi*wpi;
      wi += wi*wpr+wtemp*wpi;
   }
}


/*-------------------------- ibsma_pfacc ------------------------------*/
/*-------------------------- ibsma_pfacc ------------------------------*/
/*-------------------------- ibsma_pfacc ------------------------------*/

void ibsma_pfacc (int isign, int n, complex cz[])
{
   /***************************************************************************/
   /* Prime factor fft:  complex to complex transform, in place               */
   /***************************************************************************/
   /* Input:                                                                  */
   /*   isign    sign of isign is the sign of exponent in fourier kernel      */
   /*   n        length of transform (see notes below)                        */
   /*   z        array[n] of complex numbers to be transformed in place       */
   /*                                                                         */
   /* Output:                                                                 */
   /*   z        array[n] of complex numbers transformed                      */
   /***************************************************************************/
   /* Notes:                                                                  */
   /*   n must be factorable into mutually prime factors taken                */
   /*   from the set {2,3,4,5,7,8,9,11,13,16}.  in other words,               */
   /*           n = 2**p * 3**q * 5**r * 7**s * 11**t * 13**u                 */
   /*   where,                                                                */
   /*            0 <= p <= 4,  0 <= q <= 2,  0 <= r,s,t,u <= 1                */
   /*   Is required for pfa to yield meaningful results.                      */
   /*   This restriction implies that n is restricted to the range,           */
   /*           1 <= n <= 720720 (= 5*7*9*11*13*16)                           */
   /***************************************************************************/
   /* References:                                                             */
   /*   Temperton, C., 1985, Implementation of a self-sorting                 */
   /*   in-place prime factor fft algorithm:  Journal of                      */
   /*   Computational Physics, v. 58, p. 283-299.                             */
   /*                                                                         */
   /*   Temperton, C., 1988, A new set of minimum-add rotated                 */
   /*   rotated dft modules: Journal of Computational Physics,                */
   /*   v. 75, p. 190-198.                                                    */
   /***************************************************************************/
   /* Author:  Dave Hale, Colorado School of Mines, 04/27/89                  */
   /***************************************************************************/
   static int kfax[] = { 16,13,11,9,8,7,5,4,3,2 };
   register float *z=(float *)cz;
   register int j00,j01,j2,j3,j4,j5,j6,j7,j8,j9,j10,j11,j12,j13,j14,j15,jt;
   int nleft,jfax,ifac,jfac,jinc,jmax,ndiv,m,mm=0,mu=0,l;
   float t1r,t1i,t2r,t2i,t3r,t3i,t4r,t4i,t5r,t5i,
         t6r,t6i,t7r,t7i,t8r,t8i,t9r,t9i,t10r,t10i,
         t11r,t11i,t12r,t12i,t13r,t13i,t14r,t14i,t15r,t15i,
         t16r,t16i,t17r,t17i,t18r,t18i,t19r,t19i,t20r,t20i,
         t21r,t21i,t22r,t22i,t23r,t23i,t24r,t24i,t25r,t25i,
         t26r,t26i,t27r,t27i,t28r,t28i,t29r,t29i,t30r,t30i,
         t31r,t31i,t32r,t32i,t33r,t33i,t34r,t34i,t35r,t35i,
         t36r,t36i,t37r,t37i,t38r,t38i,t39r,t39i,t40r,t40i,
         t41r,t41i,t42r,t42i,
         y1r,y1i,y2r,y2i,y3r,y3i,y4r,y4i,y5r,y5i,
         y6r,y6i,y7r,y7i,y8r,y8i,y9r,y9i,y10r,y10i,
         y11r,y11i,y12r,y12i,y13r,y13i,y14r,y14i,y15r,y15i,
         c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12;

   /* Keep track of n left after dividing by factors. */
   nleft = n;

   /* Begin loop over possible factors (from biggest to smallest). */
   for (jfax = 0; jfax < NFAX; jfax++) {

      /* Skip if not a mutually prime factor of n. */
      ifac = kfax[jfax];
      ndiv = nleft/ifac;
      if (ndiv*ifac!=nleft) continue;

      /* Update n left and determine n divided by factor. */
      nleft = ndiv;
      m = n/ifac;

      /* Determine rotation factor mu and stride mm. */
      for (jfac = 1; jfac <= ifac; jfac++) {
         mu = jfac;
         mm = jfac*m;
         if (mm%ifac == 1) break;
      }

      /* Adjust rotation factor for sign of transform. */
      if (isign < 0) mu = ifac-mu;

      /* Compute stride, limit, and pointers. */
      jinc = 2*mm;
      jmax = 2*n;
      j00 = 0;
      j01 = j00+jinc;

      /* If factor is 2. */
      if (ifac == 2) {
         for (l = 0; l < m; l++) {
            t1r = z[j00]-z[j01];
            t1i = z[j00+1]-z[j01+1];
            z[j00] = z[j00]+z[j01];
            z[j00+1] = z[j00+1]+z[j01+1];
            z[j01] = t1r;
            z[j01+1] = t1i;
            jt = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
      j2 = j01+jinc;
      if (j2 >= jmax) j2 = j2-jmax;

      /* If factor is 3. */
      if (ifac == 3) {
         if (mu == 1)
            c1 = P866;
         else
            c1 = -P866;
         for (l = 0; l < m; l++) {
            t1r = z[j01]+z[j2];
            t1i = z[j01+1]+z[j2+1];
            y1r = z[j00]-0.5*t1r;
            y1i = z[j00+1]-0.5*t1i;
            y2r = c1*(z[j01]-z[j2]);
            y2i = c1*(z[j01+1]-z[j2+1]);
            z[j00] = z[j00]+t1r;
            z[j00+1] = z[j00+1]+t1i;
            z[j01] = y1r-y2i;
            z[j01+1] = y1i+y2r;
            z[j2] = y1r+y2i;
            z[j2+1] = y1i-y2r;
            jt = j2+2;
            j2 = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
      j3 = j2+jinc;
      if (j3 >= jmax) j3 = j3-jmax;

      /* If factor is 4. */
      if (ifac == 4) {
         if (mu == 1)
            c1 = 1.0;
         else
            c1 = -1.0;
         for (l = 0; l < m; l++) {
            t1r = z[j00]+z[j2];
            t1i = z[j00+1]+z[j2+1];
            t2r = z[j01]+z[j3];
            t2i = z[j01+1]+z[j3+1];
            y1r = z[j00]-z[j2];
            y1i = z[j00+1]-z[j2+1];
            y3r = c1*(z[j01]-z[j3]);
            y3i = c1*(z[j01+1]-z[j3+1]);
            z[j00] = t1r+t2r;
            z[j00+1] = t1i+t2i;
            z[j01] = y1r-y3i;
            z[j01+1] = y1i+y3r;
            z[j2] = t1r-t2r;
            z[j2+1] = t1i-t2i;
            z[j3] = y1r+y3i;
            z[j3+1] = y1i-y3r;
            jt = j3+2;
            j3 = j2+2;
            j2 = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
      j4 = j3+jinc;
      if (j4 >= jmax) j4 = j4-jmax;

      /* If factor is 5. */
      if (ifac == 5) {
         if (mu==1) {
            c1 = P559;
            c2 = P951;
            c3 = P587;
         } else if (mu==2) {
            c1 = -P559;
            c2 = P587;
            c3 = -P951;
         } else if (mu==3) {
            c1 = -P559;
            c2 = -P587;
            c3 = P951;
         } else { 
            c1 = P559;
            c2 = -P951;
            c3 = -P587;
         }
         for (l = 0; l < m; l++) {
            t1r = z[j01]+z[j4];
            t1i = z[j01+1]+z[j4+1];
            t2r = z[j2]+z[j3];
            t2i = z[j2+1]+z[j3+1];
            t3r = z[j01]-z[j4];
            t3i = z[j01+1]-z[j4+1];
            t4r = z[j2]-z[j3];
            t4i = z[j2+1]-z[j3+1];
            t5r = t1r+t2r;
            t5i = t1i+t2i;
            t6r = c1*(t1r-t2r);
            t6i = c1*(t1i-t2i);
            t7r = z[j00]-0.25*t5r;
            t7i = z[j00+1]-0.25*t5i;
            y1r = t7r+t6r;
            y1i = t7i+t6i;
            y2r = t7r-t6r;
            y2i = t7i-t6i;
            y3r = c3*t3r-c2*t4r;
            y3i = c3*t3i-c2*t4i;
            y4r = c2*t3r+c3*t4r;
            y4i = c2*t3i+c3*t4i;
            z[j00] = z[j00]+t5r;
            z[j00+1] = z[j00+1]+t5i;
            z[j01] = y1r-y4i;
            z[j01+1] = y1i+y4r;
            z[j2] = y2r-y3i;
            z[j2+1] = y2i+y3r;
            z[j3] = y2r+y3i;
            z[j3+1] = y2i-y3r;
            z[j4] = y1r+y4i;
            z[j4+1] = y1i-y4r;
            jt = j4+2;
            j4 = j3+2;
            j3 = j2+2;
            j2 = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
      j5 = j4+jinc;
      if (j5 >= jmax) j5 = j5-jmax;
      j6 = j5+jinc;
      if (j6 >= jmax) j6 = j6-jmax;

      /* If factor is 7. */
      if (ifac == 7) {
         if (mu == 1) {
            c1 = P623;
            c2 = -P222;
            c3 = -P900;
            c4 = P781;
            c5 = P974;
            c6 = P433;
         } else if (mu == 2) {
            c1 = -P222;
            c2 = -P900;
            c3 = P623;
            c4 = P974;
            c5 = -P433;
            c6 = -P781;
         } else if (mu == 3) {
            c1 = -P900;
            c2 = P623;
            c3 = -P222;
            c4 = P433;
            c5 = -P781;
            c6 = P974;
         } else if (mu == 4) {
            c1 = -P900;
            c2 = P623;
            c3 = -P222;
            c4 = -P433;
            c5 = P781;
            c6 = -P974;
         } else if (mu == 5) {
            c1 = -P222;
            c2 = -P900;
            c3 = P623;
            c4 = -P974;
            c5 = P433;
            c6 = P781;
         } else {
            c1 = P623;
            c2 = -P222;
            c3 = -P900;
            c4 = -P781;
            c5 = -P974;
            c6 = -P433;
         }
         for (l = 0; l < m; l++) {
            t1r = z[j01]+z[j6];
            t1i = z[j01+1]+z[j6+1];
            t2r = z[j2]+z[j5];
            t2i = z[j2+1]+z[j5+1];
            t3r = z[j3]+z[j4];
            t3i = z[j3+1]+z[j4+1];
            t4r = z[j01]-z[j6];
            t4i = z[j01+1]-z[j6+1];
            t5r = z[j2]-z[j5];
            t5i = z[j2+1]-z[j5+1];
            t6r = z[j3]-z[j4];
            t6i = z[j3+1]-z[j4+1];
            t7r = z[j00]-0.5*t3r;
            t7i = z[j00+1]-0.5*t3i;
            t8r = t1r-t3r;
            t8i = t1i-t3i;
            t9r = t2r-t3r;
            t9i = t2i-t3i;
            y1r = t7r+c1*t8r+c2*t9r;
            y1i = t7i+c1*t8i+c2*t9i;
            y2r = t7r+c2*t8r+c3*t9r;
            y2i = t7i+c2*t8i+c3*t9i;
            y3r = t7r+c3*t8r+c1*t9r;
            y3i = t7i+c3*t8i+c1*t9i;
            y4r = c6*t4r-c4*t5r+c5*t6r;
            y4i = c6*t4i-c4*t5i+c5*t6i;
            y5r = c5*t4r-c6*t5r-c4*t6r;
            y5i = c5*t4i-c6*t5i-c4*t6i;
            y6r = c4*t4r+c5*t5r+c6*t6r;
            y6i = c4*t4i+c5*t5i+c6*t6i;
            z[j00] = z[j00]+t1r+t2r+t3r;
            z[j00+1] = z[j00+1]+t1i+t2i+t3i;
            z[j01] = y1r-y6i;
            z[j01+1] = y1i+y6r;
            z[j2] = y2r-y5i;
            z[j2+1] = y2i+y5r;
            z[j3] = y3r-y4i;
            z[j3+1] = y3i+y4r;
            z[j4] = y3r+y4i;
            z[j4+1] = y3i-y4r;
            z[j5] = y2r+y5i;
            z[j5+1] = y2i-y5r;
            z[j6] = y1r+y6i;
            z[j6+1] = y1i-y6r;
            jt = j6+2;
            j6 = j5+2;
            j5 = j4+2;
            j4 = j3+2;
            j3 = j2+2;
            j2 = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
          
      j7 = j6+jinc;
      if (j7 >= jmax) j7 = j7-jmax;

      /* If factor is 8. */
      if (ifac == 8) {
         if (mu == 1) {
            c1 = 1.0;
            c2 = P707;
         } else if (mu == 3) {
            c1 = -1.0;
            c2 = -P707;
         } else if (mu == 5) {
            c1 = 1.0;
            c2 = -P707;
         } else {
            c1 = -1.0;
            c2 = P707;
         }
         c3 = c1*c2;
         for (l = 0; l < m; l++) {
            t1r = z[j00]+z[j4];
            t1i = z[j00+1]+z[j4+1];
            t2r = z[j00]-z[j4];
            t2i = z[j00+1]-z[j4+1];
            t3r = z[j01]+z[j5];
            t3i = z[j01+1]+z[j5+1];
            t4r = z[j01]-z[j5];
            t4i = z[j01+1]-z[j5+1];
            t5r = z[j2]+z[j6];
            t5i = z[j2+1]+z[j6+1];
            t6r = c1*(z[j2]-z[j6]);
            t6i = c1*(z[j2+1]-z[j6+1]);
            t7r = z[j3]+z[j7];
            t7i = z[j3+1]+z[j7+1];
            t8r = z[j3]-z[j7];
            t8i = z[j3+1]-z[j7+1];
            t9r = t1r+t5r;
            t9i = t1i+t5i;
            t10r = t3r+t7r;
            t10i = t3i+t7i;
            t11r = c2*(t4r-t8r);
            t11i = c2*(t4i-t8i);
            t12r = c3*(t4r+t8r);
            t12i = c3*(t4i+t8i);
            y1r = t2r+t11r;
            y1i = t2i+t11i;
            y2r = t1r-t5r;
            y2i = t1i-t5i;
            y3r = t2r-t11r;
            y3i = t2i-t11i;
            y5r = t12r-t6r;
            y5i = t12i-t6i;
            y6r = c1*(t3r-t7r);
            y6i = c1*(t3i-t7i);
            y7r = t12r+t6r;
            y7i = t12i+t6i;
            z[j00] = t9r+t10r;
            z[j00+1] = t9i+t10i;
            z[j01] = y1r-y7i;
            z[j01+1] = y1i+y7r;
            z[j2] = y2r-y6i;
            z[j2+1] = y2i+y6r;
            z[j3] = y3r-y5i;
            z[j3+1] = y3i+y5r;
            z[j4] = t9r-t10r;
            z[j4+1] = t9i-t10i;
            z[j5] = y3r+y5i;
            z[j5+1] = y3i-y5r;
            z[j6] = y2r+y6i;
            z[j6+1] = y2i-y6r;
            z[j7] = y1r+y7i;
            z[j7+1] = y1i-y7r;
            jt = j7+2;
            j7 = j6+2;
            j6 = j5+2;
            j5 = j4+2;
            j4 = j3+2;
            j3 = j2+2;
            j2 = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
      j8 = j7+jinc;
      if (j8 >= jmax) j8 = j8-jmax;

      /* If factor is 9. */
      if (ifac == 9) {
         if (mu == 1) {
            c1 = P866;
            c2 = P766;
            c3 = P642;
            c4 = P173;
            c5 = P984;
         } else if (mu == 2) {
            c1 = -P866;
            c2 = P173;
            c3 = P984;
            c4 = -P939;
            c5 = P342;
         } else if (mu == 4) {
            c1 = P866;
            c2 = -P939;
            c3 = P342;
            c4 = P766;
            c5 = -P642;
         } else if (mu == 5) {
            c1 = -P866;
            c2 = -P939;
            c3 = -P342;
            c4 = P766;
            c5 = P642;
         } else if (mu == 7) {
            c1 = P866;
            c2 = P173;
            c3 = -P984;
            c4 = -P939;
            c5 = -P342;
         } else {
            c1 = -P866;
            c2 = P766;
            c3 = -P642;
            c4 = P173;
            c5 = -P984;
         }
         c6 = c1*c2;
         c7 = c1*c3;
         c8 = c1*c4;
         c9 = c1*c5;
         for (l = 0; l < m; l++) {
            t1r = z[j3]+z[j6];
            t1i = z[j3+1]+z[j6+1];
            t2r = z[j00]-0.5*t1r;
            t2i = z[j00+1]-0.5*t1i;
            t3r = c1*(z[j3]-z[j6]);
            t3i = c1*(z[j3+1]-z[j6+1]);
            t4r = z[j00]+t1r;
            t4i = z[j00+1]+t1i;
            t5r = z[j4]+z[j7];
            t5i = z[j4+1]+z[j7+1];
            t6r = z[j01]-0.5*t5r;
            t6i = z[j01+1]-0.5*t5i;
            t7r = z[j4]-z[j7];
            t7i = z[j4+1]-z[j7+1];
            t8r = z[j01]+t5r;
            t8i = z[j01+1]+t5i;
            t9r = z[j2]+z[j5];
            t9i = z[j2+1]+z[j5+1];
            t10r = z[j8]-0.5*t9r;
            t10i = z[j8+1]-0.5*t9i;
            t11r = z[j2]-z[j5];
            t11i = z[j2+1]-z[j5+1];
            t12r = z[j8]+t9r;
            t12i = z[j8+1]+t9i;
            t13r = t8r+t12r;
            t13i = t8i+t12i;
            t14r = t6r+t10r;
            t14i = t6i+t10i;
            t15r = t6r-t10r;
            t15i = t6i-t10i;
            t16r = t7r+t11r;
            t16i = t7i+t11i;
            t17r = t7r-t11r;
            t17i = t7i-t11i;
            t18r = c2*t14r-c7*t17r;
            t18i = c2*t14i-c7*t17i;
            t19r = c4*t14r+c9*t17r;
            t19i = c4*t14i+c9*t17i;
            t20r = c3*t15r+c6*t16r;
            t20i = c3*t15i+c6*t16i;
            t21r = c5*t15r-c8*t16r;
            t21i = c5*t15i-c8*t16i;
            t22r = t18r+t19r;
            t22i = t18i+t19i;
            t23r = t20r-t21r;
            t23i = t20i-t21i;
            y1r = t2r+t18r;
            y1i = t2i+t18i;
            y2r = t2r+t19r;
            y2i = t2i+t19i;
            y3r = t4r-0.5*t13r;
            y3i = t4i-0.5*t13i;
            y4r = t2r-t22r;
            y4i = t2i-t22i;
            y5r = t3r-t23r;
            y5i = t3i-t23i;
            y6r = c1*(t8r-t12r);
            y6i = c1*(t8i-t12i);
            y7r = t21r-t3r;
            y7i = t21i-t3i;
            y8r = t3r+t20r;
            y8i = t3i+t20i;
            z[j00] = t4r+t13r;
            z[j00+1] = t4i+t13i;
            z[j01] = y1r-y8i;
            z[j01+1] = y1i+y8r;
            z[j2] = y2r-y7i;
            z[j2+1] = y2i+y7r;
            z[j3] = y3r-y6i;
            z[j3+1] = y3i+y6r;
            z[j4] = y4r-y5i;
            z[j4+1] = y4i+y5r;
            z[j5] = y4r+y5i;
            z[j5+1] = y4i-y5r;
            z[j6] = y3r+y6i;
            z[j6+1] = y3i-y6r;
            z[j7] = y2r+y7i;
            z[j7+1] = y2i-y7r;
            z[j8] = y1r+y8i;
            z[j8+1] = y1i-y8r;
            jt = j8+2;
            j8 = j7+2;
            j7 = j6+2;
            j6 = j5+2;
            j5 = j4+2;
            j4 = j3+2;
            j3 = j2+2;
            j2 = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
      j9 = j8+jinc;
      if (j9 >= jmax) j9 = j9-jmax;
      j10 = j9+jinc;
      if (j10 >= jmax) j10 = j10-jmax;

      /* If factor is 11. */
      if (ifac == 11) {
         if (mu == 1) {
            c1 = P841;
            c2 = P415;
            c3 = -P142;
            c4 = -P654;
            c5 = -P959;
            c6 = P540;
            c7 = P909;
            c8 = P989;
            c9 = P755;
            c10 = P281;
         } else if (mu == 2) {
            c1 = P415;
            c2 = -P654;
            c3 = -P959;
            c4 = -P142;
            c5 = P841;
            c6 = P909;
            c7 = P755;
            c8 = -P281;
            c9 = -P989;
            c10 = -P540;
         } else if (mu == 3) {
            c1 = -P142;
            c2 = -P959;
            c3 = P415;
            c4 = P841;
            c5 = -P654;
            c6 = P989;
            c7 = -P281;
            c8 = -P909;
            c9 = P540;
            c10 = P755;
         } else if (mu == 4) {
            c1 = -P654;
            c2 = -P142;
            c3 = P841;
            c4 = -P959;
            c5 = P415;
            c6 = P755;
            c7 = -P989;
            c8 = P540;
            c9 = P281;
            c10 = -P909;
         } else if (mu == 5) {
            c1 = -P959;
            c2 = P841;
            c3 = -P654;
            c4 = P415;
            c5 = -P142;
            c6 = P281;
            c7 = -P540;
            c8 = P755;
            c9 = -P909;
            c10 = P989;
         } else if (mu == 6) {
            c1 = -P959;
            c2 = P841;
            c3 = -P654;
            c4 = P415;
            c5 = -P142;
            c6 = -P281;
            c7 = P540;
            c8 = -P755;
            c9 = P909;
            c10 = -P989;
         } else if (mu == 7) {
            c1 = -P654;
            c2 = -P142;
            c3 = P841;
            c4 = -P959;
            c5 = P415;
            c6 = -P755;
            c7 = P989;
            c8 = -P540;
            c9 = -P281;
            c10 = P909;
         } else if (mu == 8) {
            c1 = -P142;
            c2 = -P959;
            c3 = P415;
            c4 = P841;
            c5 = -P654;
            c6 = -P989;
            c7 = P281;
            c8 = P909;
            c9 = -P540;
            c10 = -P755;
         } else if (mu == 9) {
            c1 = P415;
            c2 = -P654;
            c3 = -P959;
            c4 = -P142;
            c5 = P841;
            c6 = -P909;
            c7 = -P755;
            c8 = P281;
            c9 = P989;
            c10 = P540;
         } else {
            c1 = P841;
            c2 = P415;
            c3 = -P142;
            c4 = -P654;
            c5 = -P959;
            c6 = -P540;
            c7 = -P909;
            c8 = -P989;
            c9 = -P755;
            c10 = -P281;
         }
         for (l = 0; l < m; l++) {
            t1r = z[j01]+z[j10];
            t1i = z[j01+1]+z[j10+1];
            t2r = z[j2]+z[j9];
            t2i = z[j2+1]+z[j9+1];
            t3r = z[j3]+z[j8];
            t3i = z[j3+1]+z[j8+1];
            t4r = z[j4]+z[j7];
            t4i = z[j4+1]+z[j7+1];
            t5r = z[j5]+z[j6];
            t5i = z[j5+1]+z[j6+1];
            t6r = z[j01]-z[j10];
            t6i = z[j01+1]-z[j10+1];
            t7r = z[j2]-z[j9];
            t7i = z[j2+1]-z[j9+1];
            t8r = z[j3]-z[j8];
            t8i = z[j3+1]-z[j8+1];
            t9r = z[j4]-z[j7];
            t9i = z[j4+1]-z[j7+1];
            t10r = z[j5]-z[j6];
            t10i = z[j5+1]-z[j6+1];
            t11r = z[j00]-0.5*t5r;
            t11i = z[j00+1]-0.5*t5i;
            t12r = t1r-t5r;
            t12i = t1i-t5i;
            t13r = t2r-t5r;
            t13i = t2i-t5i;
            t14r = t3r-t5r;
            t14i = t3i-t5i;
            t15r = t4r-t5r;
            t15i = t4i-t5i;
            y1r = t11r+c1*t12r+c2*t13r+c3*t14r+c4*t15r;
            y1i = t11i+c1*t12i+c2*t13i+c3*t14i+c4*t15i;
            y2r = t11r+c2*t12r+c4*t13r+c5*t14r+c3*t15r;
            y2i = t11i+c2*t12i+c4*t13i+c5*t14i+c3*t15i;
            y3r = t11r+c3*t12r+c5*t13r+c2*t14r+c1*t15r;
            y3i = t11i+c3*t12i+c5*t13i+c2*t14i+c1*t15i;
            y4r = t11r+c4*t12r+c3*t13r+c1*t14r+c5*t15r;
            y4i = t11i+c4*t12i+c3*t13i+c1*t14i+c5*t15i;
            y5r = t11r+c5*t12r+c1*t13r+c4*t14r+c2*t15r;
            y5i = t11i+c5*t12i+c1*t13i+c4*t14i+c2*t15i;
            y6r = c10*t6r-c6*t7r+c9*t8r-c7*t9r+c8*t10r;
            y6i = c10*t6i-c6*t7i+c9*t8i-c7*t9i+c8*t10i;
            y7r = c9*t6r-c8*t7r+c6*t8r+c10*t9r-c7*t10r;
            y7i = c9*t6i-c8*t7i+c6*t8i+c10*t9i-c7*t10i;
            y8r = c8*t6r-c10*t7r-c7*t8r+c6*t9r+c9*t10r;
            y8i = c8*t6i-c10*t7i-c7*t8i+c6*t9i+c9*t10i;
            y9r = c7*t6r+c9*t7r-c10*t8r-c8*t9r-c6*t10r;
            y9i = c7*t6i+c9*t7i-c10*t8i-c8*t9i-c6*t10i;
            y10r = c6*t6r+c7*t7r+c8*t8r+c9*t9r+c10*t10r;
            y10i = c6*t6i+c7*t7i+c8*t8i+c9*t9i+c10*t10i;
            z[j00] = z[j00]+t1r+t2r+t3r+t4r+t5r;
            z[j00+1] = z[j00+1]+t1i+t2i+t3i+t4i+t5i;
            z[j01] = y1r-y10i;
            z[j01+1] = y1i+y10r;
            z[j2] = y2r-y9i;
            z[j2+1] = y2i+y9r;
            z[j3] = y3r-y8i;
            z[j3+1] = y3i+y8r;
            z[j4] = y4r-y7i;
            z[j4+1] = y4i+y7r;
            z[j5] = y5r-y6i;
            z[j5+1] = y5i+y6r;
            z[j6] = y5r+y6i;
            z[j6+1] = y5i-y6r;
            z[j7] = y4r+y7i;
            z[j7+1] = y4i-y7r;
            z[j8] = y3r+y8i;
            z[j8+1] = y3i-y8r;
            z[j9] = y2r+y9i;
            z[j9+1] = y2i-y9r;
            z[j10] = y1r+y10i;
            z[j10+1] = y1i-y10r;
            jt = j10+2;
            j10 = j9+2;
            j9 = j8+2;
            j8 = j7+2;
            j7 = j6+2;
            j6 = j5+2;
            j5 = j4+2;
            j4 = j3+2;
            j3 = j2+2;
            j2 = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
      j11 = j10+jinc;
      if (j11 >= jmax) j11 = j11-jmax;
      j12 = j11+jinc;
      if (j12 >= jmax) j12 = j12-jmax;

      /* If factor is 13. */
      if (ifac == 13) {
         if (mu == 1) {
            c1 = P885;
            c2 = P568;
            c3 = P120;
            c4 = -P354;
            c5 = -P748;
            c6 = -P970;
            c7 = P464;
            c8 = P822;
            c9 = P992;
            c10 = P935;
            c11 = P663;
            c12 = P239;
         } else if (mu == 2) {
            c1 = P568;
            c2 = -P354;
            c3 = -P970;
            c4 = -P748;
            c5 = P120;
            c6 = P885;
            c7 = P822;
            c8 = P935;
            c9 = P239;
            c10 = -P663;
            c11 = -P992;
            c12 = -P464;
         } else if (mu == 3) {
            c1 = P120;
            c2 = -P970;
            c3 = -P354;
            c4 = P885;
            c5 = P568;
            c6 = -P748;
            c7 = P992;
            c8 = P239;
            c9 = -P935;
            c10 = -P464;
            c11 = P822;
            c12 = P663;
         } else if (mu == 4) {
            c1 = -P354;
            c2 = -P748;
            c3 = P885;
            c4 = P120;
            c5 = -P970;
            c6 = P568;
            c7 = P935;
            c8 = -P663;
            c9 = -P464;
            c10 = P992;
            c11 = -P239;
            c12 = -P822;
         } else if (mu == 5) {
            c1 = -P748;
            c2 = P120;
            c3 = P568;
            c4 = -P970;
            c5 = P885;
            c6 = -P354;
            c7 = P663;
            c8 = -P992;
            c9 = P822;
            c10 = -P239;
            c11 = -P464;
            c12 = P935;
         } else if (mu == 6) {
            c1 = -P970;
            c2 = P885;
            c3 = -P748;
            c4 = P568;
            c5 = -P354;
            c6 = P120;
            c7 = P239;
            c8 = -P464;
            c9 = P663;
            c10 = -P822;
            c11 = P935;
            c12 = -P992;
         } else if (mu == 7) {
            c1 = -P970;
            c2 = P885;
            c3 = -P748;
            c4 = P568;
            c5 = -P354;
            c6 = P120;
            c7 = -P239;
            c8 = P464;
            c9 = -P663;
            c10 = P822;
            c11 = -P935;
            c12 = P992;
         } else if (mu == 8) {
            c1 = -P748;
            c2 = P120;
            c3 = P568;
            c4 = -P970;
            c5 = P885;
            c6 = -P354;
            c7 = -P663;
            c8 = P992;
            c9 = -P822;
            c10 = P239;
            c11 = P464;
            c12 = -P935;
         } else if (mu == 9) {
            c1 = -P354;
            c2 = -P748;
            c3 = P885;
            c4 = P120;
            c5 = -P970;
            c6 = P568;
            c7 = -P935;
            c8 = P663;
            c9 = P464;
            c10 = -P992;
            c11 = P239;
            c12 = P822;
         } else if (mu == 10) {
            c1 = P120;
            c2 = -P970;
            c3 = -P354;
            c4 = P885;
            c5 = P568;
            c6 = -P748;
            c7 = -P992;
            c8 = -P239;
            c9 = P935;
            c10 = P464;
            c11 = -P822;
            c12 = -P663;
         } else if (mu == 11) {
            c1 = P568;
            c2 = -P354;
            c3 = -P970;
            c4 = -P748;
            c5 = P120;
            c6 = P885;
            c7 = -P822;
            c8 = -P935;
            c9 = -P239;
            c10 = P663;
            c11 = P992;
            c12 = P464;
         } else {
            c1 = P885;
            c2 = P568;
            c3 = P120;
            c4 = -P354;
            c5 = -P748;
            c6 = -P970;
            c7 = -P464;
            c8 = -P822;
            c9 = -P992;
            c10 = -P935;
            c11 = -P663;
            c12 = -P239;
         }
         for (l = 0; l < m; l++) {
            t1r = z[j01]+z[j12];
            t1i = z[j01+1]+z[j12+1];
            t2r = z[j2]+z[j11];
            t2i = z[j2+1]+z[j11+1];
            t3r = z[j3]+z[j10];
            t3i = z[j3+1]+z[j10+1];
            t4r = z[j4]+z[j9];
            t4i = z[j4+1]+z[j9+1];
            t5r = z[j5]+z[j8];
            t5i = z[j5+1]+z[j8+1];
            t6r = z[j6]+z[j7];
            t6i = z[j6+1]+z[j7+1];
            t7r = z[j01]-z[j12];
            t7i = z[j01+1]-z[j12+1];
            t8r = z[j2]-z[j11];
            t8i = z[j2+1]-z[j11+1];
            t9r = z[j3]-z[j10];
            t9i = z[j3+1]-z[j10+1];
            t10r = z[j4]-z[j9];
            t10i = z[j4+1]-z[j9+1];
            t11r = z[j5]-z[j8];
            t11i = z[j5+1]-z[j8+1];
            t12r = z[j6]-z[j7];
            t12i = z[j6+1]-z[j7+1];
            t13r = z[j00]-0.5*t6r;
            t13i = z[j00+1]-0.5*t6i;
            t14r = t1r-t6r;
            t14i = t1i-t6i;
            t15r = t2r-t6r;
            t15i = t2i-t6i;
            t16r = t3r-t6r;
            t16i = t3i-t6i;
            t17r = t4r-t6r;
            t17i = t4i-t6i;
            t18r = t5r-t6r;
            t18i = t5i-t6i;
            y1r = t13r+c1*t14r+c2*t15r+c3*t16r+c4*t17r+c5*t18r;
            y1i = t13i+c1*t14i+c2*t15i+c3*t16i+c4*t17i+c5*t18i;
            y2r = t13r+c2*t14r+c4*t15r+c6*t16r+c5*t17r+c3*t18r;
            y2i = t13i+c2*t14i+c4*t15i+c6*t16i+c5*t17i+c3*t18i;
            y3r = t13r+c3*t14r+c6*t15r+c4*t16r+c1*t17r+c2*t18r;
            y3i = t13i+c3*t14i+c6*t15i+c4*t16i+c1*t17i+c2*t18i;
            y4r = t13r+c4*t14r+c5*t15r+c1*t16r+c3*t17r+c6*t18r;
            y4i = t13i+c4*t14i+c5*t15i+c1*t16i+c3*t17i+c6*t18i;
            y5r = t13r+c5*t14r+c3*t15r+c2*t16r+c6*t17r+c1*t18r;
            y5i = t13i+c5*t14i+c3*t15i+c2*t16i+c6*t17i+c1*t18i;
            y6r = t13r+c6*t14r+c1*t15r+c5*t16r+c2*t17r+c4*t18r;
            y6i = t13i+c6*t14i+c1*t15i+c5*t16i+c2*t17i+c4*t18i;
            y7r = c12*t7r-c7*t8r+c11*t9r-c8*t10r+c10*t11r-c9*t12r;
            y7i = c12*t7i-c7*t8i+c11*t9i-c8*t10i+c10*t11i-c9*t12i;
            y8r = c11*t7r-c9*t8r+c8*t9r-c12*t10r-c7*t11r+c10*t12r;
            y8i = c11*t7i-c9*t8i+c8*t9i-c12*t10i-c7*t11i+c10*t12i;
            y9r = c10*t7r-c11*t8r-c7*t9r+c9*t10r-c12*t11r-c8*t12r;
            y9i = c10*t7i-c11*t8i-c7*t9i+c9*t10i-c12*t11i-c8*t12i;
            y10r = c9*t7r+c12*t8r-c10*t9r-c7*t10r+c8*t11r+c11*t12r;
            y10i = c9*t7i+c12*t8i-c10*t9i-c7*t10i+c8*t11i+c11*t12i;
            y11r = c8*t7r+c10*t8r+c12*t9r-c11*t10r-c9*t11r-c7*t12r;
            y11i = c8*t7i+c10*t8i+c12*t9i-c11*t10i-c9*t11i-c7*t12i;
            y12r = c7*t7r+c8*t8r+c9*t9r+c10*t10r+c11*t11r+c12*t12r;
            y12i = c7*t7i+c8*t8i+c9*t9i+c10*t10i+c11*t11i+c12*t12i;
            z[j00] = z[j00]+t1r+t2r+t3r+t4r+t5r+t6r;
            z[j00+1] = z[j00+1]+t1i+t2i+t3i+t4i+t5i+t6i;
            z[j01] = y1r-y12i;
            z[j01+1] = y1i+y12r;
            z[j2] = y2r-y11i;
            z[j2+1] = y2i+y11r;
            z[j3] = y3r-y10i;
            z[j3+1] = y3i+y10r;
            z[j4] = y4r-y9i;
            z[j4+1] = y4i+y9r;
            z[j5] = y5r-y8i;
            z[j5+1] = y5i+y8r;
            z[j6] = y6r-y7i;
            z[j6+1] = y6i+y7r;
            z[j7] = y6r+y7i;
            z[j7+1] = y6i-y7r;
            z[j8] = y5r+y8i;
            z[j8+1] = y5i-y8r;
            z[j9] = y4r+y9i;
            z[j9+1] = y4i-y9r;
            z[j10] = y3r+y10i;
            z[j10+1] = y3i-y10r;
            z[j11] = y2r+y11i;
            z[j11+1] = y2i-y11r;
            z[j12] = y1r+y12i;
            z[j12+1] = y1i-y12r;
            jt = j12+2;
            j12 = j11+2;
            j11 = j10+2;
            j10 = j9+2;
            j9 = j8+2;
            j8 = j7+2;
            j7 = j6+2;
            j6 = j5+2;
            j5 = j4+2;
            j4 = j3+2;
            j3 = j2+2;
            j2 = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
      j13 = j12+jinc;
      if (j13 >= jmax) j13 = j13-jmax;
      j14 = j13+jinc;
      if (j14 >= jmax) j14 = j14-jmax;
      j15 = j14+jinc;
      if (j15 >= jmax) j15 = j15-jmax;

      /* If factor is 16. */
      if (ifac == 16) {
         if (mu == 1) {
            c1 = 1.0;
            c2 = P923;
            c3 = P382;
            c4 = P707;
         } else if (mu == 3) {
            c1 = -1.0;
            c2 = P382;
            c3 = P923;
            c4 = -P707;
         } else if (mu == 5) {
            c1 = 1.0;
            c2 = -P382;
            c3 = P923;
            c4 = -P707;
         } else if (mu == 7) {
            c1 = -1.0;
            c2 = -P923;
            c3 = P382;
            c4 = P707;
         } else if (mu == 9) {
            c1 = 1.0;
            c2 = -P923;
            c3 = -P382;
            c4 = P707;
         } else if (mu == 11) {
            c1 = -1.0;
            c2 = -P382;
            c3 = -P923;
            c4 = -P707;
         } else if (mu == 13) {
            c1 = 1.0;
            c2 = P382;
            c3 = -P923;
            c4 = -P707;
         } else {
            c1 = -1.0;
            c2 = P923;
            c3 = -P382;
            c4 = P707;
         }
         c5 = c1*c4;
         c6 = c1*c3;
         c7 = c1*c2;
         for (l = 0; l < m; l++) {
            t1r = z[j00]+z[j8];
            t1i = z[j00+1]+z[j8+1];
            t2r = z[j4]+z[j12];
            t2i = z[j4+1]+z[j12+1];
            t3r = z[j00]-z[j8];
            t3i = z[j00+1]-z[j8+1];
            t4r = c1*(z[j4]-z[j12]);
            t4i = c1*(z[j4+1]-z[j12+1]);
            t5r = t1r+t2r;
            t5i = t1i+t2i;
            t6r = t1r-t2r;
            t6i = t1i-t2i;
            t7r = z[j01]+z[j9];
            t7i = z[j01+1]+z[j9+1];
            t8r = z[j5]+z[j13];
            t8i = z[j5+1]+z[j13+1];
            t9r = z[j01]-z[j9];
            t9i = z[j01+1]-z[j9+1];
            t10r = z[j5]-z[j13];
            t10i = z[j5+1]-z[j13+1];
            t11r = t7r+t8r;
            t11i = t7i+t8i;
            t12r = t7r-t8r;
            t12i = t7i-t8i;
            t13r = z[j2]+z[j10];
            t13i = z[j2+1]+z[j10+1];
            t14r = z[j6]+z[j14];
            t14i = z[j6+1]+z[j14+1];
            t15r = z[j2]-z[j10];
            t15i = z[j2+1]-z[j10+1];
            t16r = z[j6]-z[j14];
            t16i = z[j6+1]-z[j14+1];
            t17r = t13r+t14r;
            t17i = t13i+t14i;
            t18r = c4*(t15r-t16r);
            t18i = c4*(t15i-t16i);
            t19r = c5*(t15r+t16r);
            t19i = c5*(t15i+t16i);
            t20r = c1*(t13r-t14r);
            t20i = c1*(t13i-t14i);
            t21r = z[j3]+z[j11];
            t21i = z[j3+1]+z[j11+1];
            t22r = z[j7]+z[j15];
            t22i = z[j7+1]+z[j15+1];
            t23r = z[j3]-z[j11];
            t23i = z[j3+1]-z[j11+1];
            t24r = z[j7]-z[j15];
            t24i = z[j7+1]-z[j15+1];
            t25r = t21r+t22r;
            t25i = t21i+t22i;
            t26r = t21r-t22r;
            t26i = t21i-t22i;
            t27r = t9r+t24r;
            t27i = t9i+t24i;
            t28r = t10r+t23r;
            t28i = t10i+t23i;
            t29r = t9r-t24r;
            t29i = t9i-t24i;
            t30r = t10r-t23r;
            t30i = t10i-t23i;
            t31r = t5r+t17r;
            t31i = t5i+t17i;
            t32r = t11r+t25r;
            t32i = t11i+t25i;
            t33r = t3r+t18r;
            t33i = t3i+t18i;
            t34r = c2*t29r-c6*t30r;
            t34i = c2*t29i-c6*t30i;
            t35r = t3r-t18r;
            t35i = t3i-t18i;
            t36r = c7*t27r-c3*t28r;
            t36i = c7*t27i-c3*t28i;
            t37r = t4r+t19r;
            t37i = t4i+t19i;
            t38r = c3*t27r+c7*t28r;
            t38i = c3*t27i+c7*t28i;
            t39r = t4r-t19r;
            t39i = t4i-t19i;
            t40r = c6*t29r+c2*t30r;
            t40i = c6*t29i+c2*t30i;
            t41r = c4*(t12r-t26r);
            t41i = c4*(t12i-t26i);
            t42r = c5*(t12r+t26r);
            t42i = c5*(t12i+t26i);
            y1r = t33r+t34r;
            y1i = t33i+t34i;
            y2r = t6r+t41r;
            y2i = t6i+t41i;
            y3r = t35r+t40r;
            y3i = t35i+t40i;
            y4r = t5r-t17r;
            y4i = t5i-t17i;
            y5r = t35r-t40r;
            y5i = t35i-t40i;
            y6r = t6r-t41r;
            y6i = t6i-t41i;
            y7r = t33r-t34r;
            y7i = t33i-t34i;
            y9r = t38r-t37r;
            y9i = t38i-t37i;
            y10r = t42r-t20r;
            y10i = t42i-t20i;
            y11r = t36r+t39r;
            y11i = t36i+t39i;
            y12r = c1*(t11r-t25r);
            y12i = c1*(t11i-t25i);
            y13r = t36r-t39r;
            y13i = t36i-t39i;
            y14r = t42r+t20r;
            y14i = t42i+t20i;
            y15r = t38r+t37r;
            y15i = t38i+t37i;
            z[j00] = t31r+t32r;
            z[j00+1] = t31i+t32i;
            z[j01] = y1r-y15i;
            z[j01+1] = y1i+y15r;
            z[j2] = y2r-y14i;
            z[j2+1] = y2i+y14r;
            z[j3] = y3r-y13i;
            z[j3+1] = y3i+y13r;
            z[j4] = y4r-y12i;
            z[j4+1] = y4i+y12r;
            z[j5] = y5r-y11i;
            z[j5+1] = y5i+y11r;
            z[j6] = y6r-y10i;
            z[j6+1] = y6i+y10r;
            z[j7] = y7r-y9i;
            z[j7+1] = y7i+y9r;
            z[j8] = t31r-t32r;
            z[j8+1] = t31i-t32i;
            z[j9] = y7r+y9i;
            z[j9+1] = y7i-y9r;
            z[j10] = y6r+y10i;
            z[j10+1] = y6i-y10r;
            z[j11] = y5r+y11i;
            z[j11+1] = y5i-y11r;
            z[j12] = y4r+y12i;
            z[j12+1] = y4i-y12r;
            z[j13] = y3r+y13i;
            z[j13+1] = y3i-y13r;
            z[j14] = y2r+y14i;
            z[j14+1] = y2i-y14r;
            z[j15] = y1r+y15i;
            z[j15+1] = y1i-y15r;
            jt = j15+2;
            j15 = j14+2;
            j14 = j13+2;
            j13 = j12+2;
            j12 = j11+2;
            j11 = j10+2;
            j10 = j9+2;
            j9 = j8+2;
            j8 = j7+2;
            j7 = j6+2;
            j6 = j5+2;
            j5 = j4+2;
            j4 = j3+2;
            j3 = j2+2;
            j2 = j01+2;
            j01 = j00+2;
            j00 = jt;
         }
         continue;
      }
   }
}

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

