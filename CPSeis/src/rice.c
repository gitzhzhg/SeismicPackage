/*
!<CPS_v1 type="PRIMITIVE"/>
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
*/

/*-------------------------------- rice.c ----------------------------------*/
/*-------------------------------- rice.c ----------------------------------*/
/*-------------------------------- rice.c ----------------------------------*/

        /* other files are:  rice.h */
 


/****
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E 
!
! Name       : RICE (time frequency analysis)
! Category   : Miscellaneous
! Written    : 2000-10-02   by: Michael L. Sherrill
! Revised    : 2005-10-10   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Time frequency analysis per Rice Consortium
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
! Outputs a time frequency representation and instantaneous trace attributes
! of 2D seismic data using either the adaptive optimal kernel (aok) or the
! short time fourier transform spectrogram (stft) per Rice Consortium.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS 
!
! In the time frequency mode fft length / 2 + 1 traces are output for each
! input trace. In the instantaneous mode 1 trace is output for each input trace.
! Note: The number of output samples is the same as the input samples
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS  
!  None
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
! Trace headers are not passed in and therefore are not altered.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!
!   o                       i          i      
!  stat = rice_timefreq ( trace_in, nsamp_in, trace_out, srval, num_traces_in,
!                  
!                          kernal,   mode,     window,   scale, correct,
!
!                          winlen,   fftlen,   dBthr,    vol)
!
!
! float    *trace_in     =  one seismic trace input
! int      nsamp_in      =  number of samples in seismic trace
! float    *trace_out    =  transformed data output
! float    srval         =  sample rate of the input data
! int      num_traces_in =  currently this is always 1 
! char     *kernal       =  "aok" for adaptive "stft" for short time
! char     *mode         =  "tfdist" return the time-frequency distribution
!                        =  "band"   return the instantaneous bandwidth 
!                        =  "cfreq"  return the instantaneous center frequency
!                        =  "dfreq"  return the instantaneous dominant freq    
!                        =  "maxfreq"return the instantaneous maximal frequency
!                        =  "q"      return the instantaneous Q   
! char     *window       =  "Hamming"     Hamming window function for STFT
!                        =  "Rectangular" Rectangular window function for STFT
!                        =  "Hanning"     Hanning window function for STFT
!                        =  "Blackman"    Blackman window function for STFT
!                        =  "Bartlet"     Bartlet window function for STFT  
! char     *scale        =  "lin" or "db" scale the output accordingly
! char     *correct      =  "none" no normalization
!                        =  "time" time marginals preserved
!                        =  "freq" frequency marginals preserved
! int      winlen        =  num of samples of the sliding analysis window
! int      fftlen        =  length of the fft (power of 2 > power of 2 of winlen
! double   dBthr         =  db threshold (normaly 100.0)
! double   vol           =  optimization level 1.0 to 5.0
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! This program computes time-frequency distributions using either the   
! adaptive optimal kernel (AOK) methodology of Baraniuk and Jones or the
! the spectrogram (e.g., STFT). Currently either the full 2D time-freq. 
! distribution for each trace or estimate of a number of instantaneous  
! trace attributes from the seismic records (e.g., bandwidth, central   
! frequency, dominant frequency, maximal frequency and Q) are available.
! Definitions of these attributes are taken from Barnes.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 11. 2005-10-10  Stoeckley  More fixes to compile with C++.
! 10. 2005-05-31  Stoeckley  Fix to compile with C++.
!  9. 2004-03-17  Selzler    Resolved SGI compiler warning (variable not used).
!  8. 2001-10-29  Selzler    Added Wigner option.
!  7. 2001-10-25  Selzler    Corrected latent bug with ixr, ixi buffer free.
!  6. 2001-07-30  Selzler    Removed printf
!  5. 2001-07-25  Selzler    Added CPS attribute option
!  4. 2001-05-02  Sherrill   Fixed header word doc.
!  3. 2001-02-01  Sherrill   Removed Linux warnings and unitialized array warns
!  2. 2000-10-02  Sherrill   Converted from Rice code
!  1. 1997-01-01  Rice: Jan E. Odegard, Mobil: Dave F. Lane
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS 
!
! Should be compiled with "-O2" optimization level on Linux.
! This reduces run time by about 25%.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
! This program computes time-frequency distributions using either the   
! adaptive optimal kernel (AOK) methodology of Baraniuk and Jones or the
! the spectrogram (e.g., STFT). Currently either the full 2D time-freq. 
! distribution for each trace or estimate of a number of instantaneous  
! trace attributes from the seismic records (e.g., bandwidth, central   
! frequency, dominant frequency, maximal frequency and Q) are available.
! Definitions of these attributes are taken from Barnes.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES 
!
! Note this code originated from Rice.
! Permission is granted for use and distribution acording to consortium
! agreements signed by Rice and the sponsoring company providing that
! this notice be clearly maintained. The right to distribute any portion
! for profit or as part of any commercial product is specifically reserved
! for Rice University Consortium for Computational Seismic Interpretation
! under terms of the consortium agreement.
!
! Credits:
!
!      Rice: Jan E. Odegard, 1997 
!      Mobil: Dave F. Lane, 1997
!
! Reference:  Jones, Douglas L., and Richard G. Baraniuk, "An Adaptive 
!             Optimal-Kernel Time-Frequency Representation", IEEE
!             Transactions on Signal Processing, 43, No.10, Oct. 1995. 
!
!             Barnes, Arthur E., "Instantaneous spectral bandwidth and
!             dominant frequency with applications to seismic reflection
!             data", Geophysics, 58, No.3, Mar. 1993.
!
! 19 July 2001 R. Selzler
! rice_timefreq2 and its support routines were derived from Mike Sherrill's
! version of a spectral analysis code from Rice University.
! rice_timefreq2 is called from two different environments:
! 1. CBYT (interactive seismic display program) via the original
!    rice_timefreq API in non-CPS mode.
!    This mode was needed to preserve the existing CBYT API.
!    and can return the TF distribution or one TF attribute.
! 2. CPS (seismic batch processing system) via the new
!    tfatt_crou_rice_timefreq API in CPS mode.
!    CPS allows multiple attributes to be derived from the
!    from the internal arrays with minimal computations.
!-------------------------------------------------------------------------------
!</programming_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/

char RICE_IDENT[100] =
"$Id: rice.c,v 1.11 2005/10/10 11:21:15 Stoeckley prod sps $";

#include "rice.h"
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <malloc.h>




/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/






#define        ALPHA 0.01;



/* mode switches */
#define TFDIST          1
#define BANDWIDTH       2
#define CENTFREQ        3
#define DOMFREQ         4
#define MAXFREQ         5
#define Q               6

/* tfmode flaggs */
#define AOK          1
#define SP           2

#ifdef __cplusplus
extern "C" {
#endif


int rice_timefreq (float *trace_in, int nsamp_in,
               float *trace_out, 
               float srval, int num_traces_in, char *kernal,
               char *mode,  char *window, char *scale, 
               char *correct, int winlen, int fftlen, 
               double dBthr, double vol)
{
  /* API wrapper between CBYT and main rice time freq computation */
  return rice_timefreq2(trace_in, nsamp_in,
               trace_out, 
               srval, num_traces_in, kernal,
               1, (char(*)[2*sizeof(int)])mode,  window, scale, 
               correct, winlen, fftlen, 
               dBthr, vol);
}


int rice_timefreq2 (float *trace_in, const int nsamp_in,
               float *trace_out, 
               float srval, int num_traces_in, char *kernal,
               int mode_cnt, char (*mode)[2*sizeof(int)],
               char *window, char *scale, 
               char *correct, int winlen, int fftlen, 
               double dBthr, double vol)
{
  int tlen;             /* nt + winlen + 2                      */
  int tstart;           /* start index of output data           */
  int register i;       /* Loop variables                       */
  int ntr;              /* number of traces                     */
  int num_traces_out;   /* equals fft length / 2 + 1            */
  int zerotr=0;         /* flagg traces that are all zero       */
  int aoksw1;           /* Frequency index control AOK vs. STFT */
  int aoksw2;           /* Frequency index control AOK vs. STFT */
  float *data;          /* Array of data from each trace        */
  double nyquist;       /* Nyquist frequency                    */
  double dt;            /* Sample spacing                       */
  float  trd2;          /* sample spacing between traces        */
  double dw;            /* Sample rate in frequency             */
  double tiny=0;        /* Threshold for 0 -> tiny=0            */
  double eps;           /* */
  double *ixr_ptr;      /* Real part of data                    */
  double *ixi_ptr;      /* Imaginary part of data               */
  double *ixr;          /* Real part of data                    */
  double *ixi;          /* Imaginary part of data               */
  double mx, mi;
  tfrpar tfpar;         /* Time frequency parameters            */              
  tfr  tf;              /* Time frequency dist                  */

  /* Note: the mode argument is now a pointer to an array of char.
     The array length is 2*sizeof(int) which can hold a string of
     at least 7 char (plus null) on 32 bit hosts.
     Valid strings are "tfdist", "band", "cfreq", "dfreq", "maxfreq", "q".
     The number of strings is mode_cnt.
     mode_cnt for CBYT is always 1 (only one thing is computed).
     mode_cnt for CPS is 1 (if "tfdist") or 1-5 (for combinations of
     computed attributes "band" through "q").
  */

  /* Get info from first trace */

  tf.nt = nsamp_in;
  dt = ((double) srval); 
  ntr = num_traces_in;

  if(ntr > 1)
    return (0);

  /* Get analysis window length */
  tfpar.nw = winlen;
  /* Get length of fft */
  if(fftlen == 0)
    tfpar.fftlen = (int) pow(2.0,(double) rice_timefreq_po2(2*tfpar.nw));
  else
    tfpar.fftlen = fftlen;
  /* Get volume constraint on kernel */
  if(vol == 0)
    tfpar.vol = 2.0;
  else
    tfpar.vol = vol;
  /* get kernal type */
  tfpar.tfmode = kernal;
  /* Get output (mode) to generate */
  tfpar.mode = NULL;
  /* Get stft window */
  tfpar.window = window;
  /* Output scaling linear or dB */
  tfpar.scale = scale;
  /* Marginals */
  tfpar.correct = correct;
  /* Get dB threshold */
  if(dBthr == 0.0)
    tfpar.dBthr = 100.0;
  else
    tfpar.dBthr = dBthr;
  


  /* Check for parameter input errors */

  if((tfpar.nw <= 0) || !rice_timefreq_isodd(tfpar.nw))
    return (0);
  if(!rice_timefreq_ispowp(tfpar.fftlen,2.0) || tfpar.fftlen <= 0 || 
     tfpar.fftlen < tfpar.nw)
    return (0);
  if(tfpar.vol <= 0)
    return (0);
  if(strcmp(kernal,"aok") != 0 &&
     strcmp(kernal,"wig") != 0 &&
     strcmp(kernal,"stft") != 0)
    return (0);

  /* I commented the code following their comment for the fftlen check. 
   * We are allowing the fftlen to be as low as the next power of 2 of
   * the tfpar.nw  M.L. Sherrill 09-21-00 */

  /* Need to consider whether this should be a choice? */
  /*
  if(tfpar.fftlen < 2*tfpar.nw)
    tfpar.fftlen = 2*tfpar.fftlen;
  */


  num_traces_out = ntr*(tfpar.fftlen/2+1);

  /* Setup array and transform sizes */


  tlen = tf.nt + tfpar.nw + 2;
  if(strcmp(tfpar.tfmode, "aok") == 0 ||
     strcmp(tfpar.tfmode, "wig") == 0) {
    tstart = (tfpar.nw-1)/2;  /* start index at exact center of window */
    nyquist = 0;
    aoksw1 = 1;
    aoksw2 = 0;
  }
  else {
    tstart = 0;
    nyquist = 1.0/(2.0*dt);
    aoksw1 = 0;
    aoksw2 = 1;
  }
  

  tfpar.tstep  = 1;
  tfpar.fstep  = 1;

  /* Adjust frequency sampling to display correct frequency */

  trd2 = 1/(dt*tfpar.fftlen);
  dw = trd2;

  /* Initialize arrays for TFR */

  tfpar.mfft = rice_timefreq_po2(tfpar.fftlen);
  tfpar.vect = (double *) calloc( 1, tfpar.nw * sizeof(double));
  data       = (float *)  calloc( 1, tf.nt    * sizeof(float));

  

  /* Allocate space for input data and Hilbert transformed data */
  if(strcmp(tfpar.tfmode,"aok") == 0 ||
     strcmp(tfpar.tfmode, "wig") == 0) {
    ixr_ptr  = (double *) calloc( 1, tf.nt * sizeof(double));
    ixi_ptr  = (double *) calloc( 1, tf.nt * sizeof(double));
    ixr = ixr_ptr;
    ixi = ixi_ptr;
    /* Allocate storage space for TF dist */
    tf.re = (double *) calloc( 1, (tfpar.fftlen*tlen) * sizeof(double));
    tf.im = NULL;
  }
  else {/*stft mode*/
    ixr_ptr  = (double *) calloc( 1, (tf.nt + tfpar.nw)  * sizeof(double));
    ixi_ptr  = (double *) calloc( 1, (tf.nt + tfpar.nw)  * sizeof(double));
    /* Offset pointer base locatin to permit negative indexing */
    ixr = ixr_ptr + (tfpar.nw-1)/2; 
    /* Offset pointer base locatin to permit negative indexing */
    ixi = ixi_ptr + (tfpar.nw-1)/2; 
    /* Allocate storage space for TF dist */
    tf.re = (double *) calloc( 1, (tfpar.fftlen*tf.nt) * sizeof(double));
    tf.im = (double *) calloc( 1, (tfpar.fftlen*tf.nt) * sizeof(double));
  }
    
  eps = pow((double) 10.0,(double) - (tfpar.dBthr/10.0));   
  

  /* Loop over traces */

  /* Determine if current trace is a zero trace */
  mx = trace_in[0];
  mi = trace_in[0];
  for (i = 0; i < tf.nt; ++i) {
      ixr[i] = (double) trace_in[i];
      ixi[i] = 0.0;
      if(trace_in[i] >= mx)
        mx = trace_in[i];
      if(trace_in[i] <= mi)
        mi = trace_in[i];
  }
  zerotr = 0;
  if(mi == 0.0 && mx == 0.0) /* Is the trace a ZERO trace? */
      zerotr = 1;

  if(!zerotr) { 
      /* If the trace is not a ZERO trace compute TF distribution */
      /* using either the AOK algorithm or the STFT algorithm     */
      tiny = 0.001*rice_timefreq_ssq(ixr,tf.nt);
      if(tiny == 0.0)
        tiny=1.0e-20;

      if(strcmp(tfpar.tfmode,"aok") == 0 ||
         strcmp(tfpar.tfmode, "wig") == 0) {
        /* Compute tf distribution using AOK or WIG algorithm       */
        /* Construct quadrature trace with Hilbert transform */
        rice_timefreq_rhilbert(ixr,ixi,tf.nt);

        rice_timefreq_aok(ixr,ixi,tf.re,tf.nt,tfpar.nw,tfpar.fftlen,
                 tfpar.fstep,tfpar.tstep,tfpar.vol, tfpar.tfmode);
      }
      else {/*stft*/
               rice_timefreq_stft(ixr, ixi, &tf, &tfpar);

        for(i = 0; i < tf.nt*tfpar.fftlen; i++) {
          tf.re[i] = sqrt(tf.re[i]*tf.re[i]  + tf.im[i]*tf.im[i]);
          tf.im[i] = 0.0;
        }
      }       
    
      /* Correct TF dist such that either time or frequency 
         marginals are preserved */
      if(strcmp(tfpar.correct, "time") == 0)
        rice_timefreq_correct_time(&tf.re[tstart*tfpar.fftlen], ixr, 
                                   tf.nt, tfpar.fftlen);
      else if(strcmp(tfpar.correct, "freq") == 0)
        rice_timefreq_correct_freq(&tf.re[tstart*tfpar.fftlen], ixr, tf.nt,
                                   tfpar.fftlen);
  }
  else {
    if(strcmp(tfpar.tfmode,"aok") == 0 ||
       strcmp(tfpar.tfmode, "wig") == 0) {
      for(i = 0; i < tlen*tfpar.fftlen; i++)
          tf.re[i] = (double) 0.0;
    }
    else {
      for(i = 0; i < tf.nt*tfpar.fftlen; i++) {
          tf.re[i] = (double) 0.0;
          tf.im[i] = (double) 0.0;
        }
    }
      
  }
  /* AOK is not a POSITIVE distribution and hence we need 
     to deal with negative values -- either settingthem to zero or 
     magnitude them 
  */
  if(strcmp(tfpar.tfmode,"aok") == 0 ||
     strcmp(tfpar.tfmode, "wig") == 0) {
    for(i = 0; i < tlen*tfpar.fftlen; i++)
      tf.re[i] = (double) fabs(tf.re[i]);
  }


  /* Given the desired TF distribution we are now readdy to compute/deliver */
  /* requested attribute or output */
  /* CPS may request multiple attributes to be computed. */
  for(i=0; i<mode_cnt; i++) {
    if(! strcmp(mode[i],"tfdist")) {
      /* Raw time-frequency distribution as a seismic panel */
      rice_tfdist(tf, tfpar, zerotr, tstart, aoksw1, aoksw2,
        eps, (float*)trace_out);

      trace_out += num_traces_out * nsamp_in;
    } else if(! strcmp(mode[i],"band")) {
      /* Instantaneous bandwidth */
      rice_bandwidth(tf, tfpar, tstart, aoksw1, aoksw2,
        nyquist, dw, tiny, (float*)trace_out);

      trace_out += nsamp_in;
    } else if(! strcmp(mode[i],"cfreq")) {
      /* Instantaneous center frequency */
      rice_centfreq(tf, tfpar, tstart, aoksw1, aoksw2,
        nyquist, dw, tiny, (float*)trace_out);

      trace_out += nsamp_in;
    } else if(! strcmp(mode[i],"dfreq")) {
      /* Instantaneous dominant frequency */
      rice_domfreq(tf, tfpar, tstart, aoksw1, aoksw2,
        nyquist, dw, tiny, (float*)trace_out);

      trace_out += nsamp_in;
    } else if(! strcmp(mode[i],"maxfreq")) {
      /* Instantaneous peak/max frequency */
      rice_maxfreq(tf, tfpar, tstart, aoksw1, aoksw2,
        nyquist, dw, tiny, (float*)trace_out);

      trace_out += nsamp_in;
    } else if(! strcmp(mode[i],"q")) {
      /* Instantaneous Q */
      rice_q(tf, tfpar, tstart, aoksw1, aoksw2,
        nyquist, dw, tiny, (float*)trace_out);

      trace_out += nsamp_in;
    }
  }


  free((double *)tfpar.vect);
  free((float *)data);

  free((double *)ixr_ptr);
  free((double *)ixi_ptr);
  free((double *)tf.re);
  if(tf.im != NULL) free((double *)tf.im);


  return (1);

}

        
/* Raw time-frequency distribution as a seismic panel */
void rice_tfdist (
  tfr tf,
  tfrpar tfpar,
  int zerotr,
  int tstart,
  int aoksw1,
  int aoksw2,
  double eps,
  float *trace_out
  ) {
  int gx=0;             /* frequency bin for tf output          */
  long k;
  int register i, j;
  long oi;              /* trace_out index                      */
  int tracr=0;          /* trace sequence counter within reel   */
  double arg;
  int nsamp_in = tf.nt;

      gx = 0;

      /* Normalize TF dist max = 1  -- a must for dB conversion ok for lin */
      if(!zerotr)
        rice_timefreq_normalize(&tf.re[tstart*tfpar.fftlen], tf.nt,
                                tfpar.fftlen);

      if(strcmp(tfpar.tfmode,"aok") == 0 ||
         strcmp(tfpar.tfmode, "wig") == 0) { /* Aok requires special attention*/
        if(strcmp(tfpar.scale, "lin") == 0) {  /* Return linear amplitude */
          /* Loop over freq */
          for(j = tfpar.fftlen/2-aoksw2, k = 0; j >= aoksw1; j--, k++) {
            oi = k * nsamp_in;
            /* Loop over time */
            for(i = tstart; i < tf.nt+tstart; i++) {
              trace_out[i-tstart+oi] = (float) tf.re[i*tfpar.fftlen+j-aoksw1];
            }
            ++tracr;
            ++gx;
          }
        }
        else {             /* Return logarithmic amplitude "dB" */
          /* Loop over freq */
          for(j = tfpar.fftlen/2-aoksw2, k = 0; j >= aoksw1; j--, k++) {
            oi = k * nsamp_in;
            /* Loop over time */
            for(i = tstart; i < tf.nt+tstart; i++) { 
              if(tf.re[i*tfpar.fftlen+j-aoksw1] < eps)
                arg = eps;
              else
                arg = (tf.re[i*tfpar.fftlen+j-aoksw1] + eps);
              trace_out[i-tstart+oi] = 
                          (float) (10.0*log10(arg) + tfpar.dBthr)/tfpar.dBthr;
            }
            ++tracr;
            ++gx;
          }
        }
      }
      else { /* Non AOK distributions */
        if(strcmp(tfpar.scale, "lin") == 0) {  /* Return linear amplitude */
          /* Loop over freq */ 
          for(j = 0, k = 0; j < tfpar.fftlen/2; j++, k++) {
            oi = k * nsamp_in;   
            /* Loop over time */                 
            for(i = 0; i < tf.nt; i++) {
              trace_out[i+oi] = (float) tf.re[i*tfpar.fftlen+j];

            }
            ++tracr;
            ++gx;
          }
        }
        else {             /* Return logarithmic amplitude "dB" */
          /* Loop over freq */
          for(j = 0, k = 0; j < tfpar.fftlen/2; j++, k++) {
            oi = k * nsamp_in;
            /* Loop over time */                   
            for(i = 0; i < tf.nt; i++) {                          
              if(tf.re[i*tfpar.fftlen+j] < eps)
                arg = eps;
              else
                arg = tf.re[i*tfpar.fftlen+j] + eps;
              trace_out[i+oi] =
                        (float) (10.0*log10(arg) + tfpar.dBthr)/tfpar.dBthr;
            }
            ++tracr;
            ++gx;
          }
        }
      }
}

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
  float *trace_out
  ) {
  int register i, j;
  double sum;           /* Acummulator for attributes           */
  double cf;            /* Instantaneous center/dom frequency   */
  double bw;            /* Instantaneous bandwidth              */
  double freq;          /* Current frequency in tf              */

      for(i = tstart; i < tf.nt+tstart; i++) {     /* Loop over time */
        sum = 0.0;                                 /* Zero accumulators */
        cf  = 0.0;
        bw  = 0.0;
        for(j = tfpar.fftlen/2-aoksw2; j >= aoksw1; j--) {/* Loop over freq */
          freq = fabs(nyquist-(tfpar.fftlen/2-j)*dw);     /* Current freq */
          sum  += tf.re[i*tfpar.fftlen+j-aoksw1];  /* Marginal dist */
          cf   += freq*tf.re[i*tfpar.fftlen+j-aoksw1];    /* Central freq */
        }
        if(fabs(sum) <= tiny)                       /* Check for zero sum */
          trace_out[i-tstart] = 0.0; 
        else {
          cf = cf/sum;                                  /* Comput cent freq */
          for(j = tfpar.fftlen/2-aoksw2; j >= aoksw1; j--) {/* Loop over freq */
            freq = fabs(nyquist-(tfpar.fftlen/2-j)*dw); /* Current freq   */
            bw  += (freq-cf)*(freq-cf)*tf.re[i*tfpar.fftlen+j-aoksw1];
          }
          bw = fabs(bw);                              /* Check for zeros */
          trace_out[i-tstart] = (float) sqrt(bw/sum); /* Bandwidth estimate */ 
        }
      }
}

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
  float *trace_out
  ) {
  int register i, j;
  double sum;           /* Acummulator for attributes           */
  double cf;            /* Instantaneous center/dom frequency   */
  double freq;          /* Current frequency in tf              */

      for(i = tstart; i < tf.nt+tstart; i++) {     /* Loop over time */
        sum = 0.0;                                   /* Zero accumulators */
        cf  = 0.0;
        for(j = tfpar.fftlen/2-aoksw2; j >= aoksw1; j--) {/* Loop over freq */
          freq = fabs(nyquist-(tfpar.fftlen/2-j)*dw);     /* Current freq */
          sum +=  tf.re[i*tfpar.fftlen+j-aoksw1];  /* Marginal dist */
          cf  += freq*tf.re[i*tfpar.fftlen+j-aoksw1];     /* Center freq */
        }
        if(fabs(sum) <= tiny)                    /* Check for divide by zero */
          trace_out[i-tstart] = 0.0;             /* Central freq estimate */
        else
          trace_out[i-tstart] = (float) cf/sum; /* Central freq estimate */
      }
}

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
  float *trace_out
  ) {
  int register i, j;
  double sum;           /* Acummulator for attributes           */
  double cf;            /* Instantaneous center/dom frequency   */
  double freq;          /* Current frequency in tf              */

      for(i = tstart; i < tf.nt+tstart; i++) {       /* Loop over time */
        sum = 0.0;                                   /* Zero accumulators */
        cf  = 0.0;
        for(j = tfpar.fftlen/2-aoksw2; j >= aoksw1; j--) { /* Loop over freq */
          freq = fabs(nyquist-(tfpar.fftlen/2-j)*dw);      /* Current freq */
          sum +=  tf.re[i*tfpar.fftlen+j-aoksw1];          /* Marginal dist */
          cf  += freq*freq*tf.re[i*tfpar.fftlen+j-aoksw1]; /* Dominant freq */
        }
        if(fabs(sum) <= tiny)
          trace_out[i-tstart] = 0.0;  
        else/* Dominant freq estimate */
          trace_out[i-tstart] = (float) sqrt(fabs(cf)/sum);
      }
}

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
  float *trace_out
  ) {
  int register i, j;
  double arg;
  double sum;           /* Acummulator for attributes           */
  double cf;            /* Instantaneous center/dom frequency   */
  double freq;          /* Current frequency in tf              */

      for(i = tstart; i < tf.nt+tstart; i++) {     /* Loop over time */
        cf  = 0.0;
        sum = 0.0;
        arg = 0.0;
        for(j = tfpar.fftlen/2-aoksw2; j >= aoksw1; j--) { /* Loop over freq */
          arg = fabs(tf.re[i*tfpar.fftlen+j-aoksw1]);
          if(arg > tiny) {                         /* Ignore if small */
            if(arg >= sum) {
              freq = fabs(nyquist-(tfpar.fftlen/2-j)*dw);  /* Current freq */
              cf = freq;
              sum = arg;
            }
          }
        }
        trace_out[i-tstart] = (float) cf;     /* Dominant max freq estimate */
      }
}

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
  float *trace_out
  ) {
  int register i, j;
  double sum;           /* Acummulator for attributes           */
  double cf;            /* Instantaneous center/dom frequency   */
  double bw;            /* Instantaneous bandwidth              */
  double freq;          /* Current frequency in tf              */
  double pi = 4.0*atan(1.0);

      for(i = tstart; i < tf.nt+tstart; i++) {           /* Loop over time */
        sum = 0.0;                                       /* Accumulators */
        cf  = 0.0;
        bw  = 0.0;
        for(j = tfpar.fftlen/2-aoksw2; j >= aoksw1; j--) { /* Loop over freq */
          freq = fabs(nyquist-(tfpar.fftlen/2-j)*dw);      /* Current freq */
          sum += tf.re[i*tfpar.fftlen+j-aoksw1];   /* Marginal dist */
          cf  += freq*tf.re[i*tfpar.fftlen+j-aoksw1];      /* Central freq  */
        }
        if(fabs(sum) <= tiny)                          /* Check for zero sum */
          trace_out[i-tstart] = 0.0;
        else {
          cf = cf/sum;                                  /* Comput cent freq */
          for(j = tfpar.fftlen/2-aoksw2; j >= aoksw1; j--) {/* Loop over freq */
            freq = fabs(nyquist-(tfpar.fftlen/2-j)*dw);     /* Current freq   */
            bw  += (freq-cf)*(freq-cf)*tf.re[i*tfpar.fftlen+j-aoksw1];
          }
          bw = fabs(bw);                                /* Check for zeros */
          trace_out[i-tstart] = (float) pi*cf/sqrt(bw/sum); /* Q estimate */
        }
      }
}

        
/*****************************RiCE_TIMEFREQ_SSQ *****************************/
/* */
/****************************************************************************/
double rice_timefreq_ssq(double* a, int nt)
{
  double temp;
  int i;
  
  temp = 0.0;
  for(i = 0; i < nt; i++)
    temp += a[i]*a[i];
  
  return(sqrt(temp/((double) nt)));
}

/****************** RICE_TIMEFREQ_NORMALIZE *****************************/
/* Normalize TF distribution such that:                                 */
/* max = 1.0                                                            */
/************************************************************************/
void rice_timefreq_normalize(double* a, int nt, int nf)
{
  double mx;
  int i, j;

  mx = 0.0;
  for(i = 0; i < nt; i++) {
    for(j = nf/2; j >= 0; j--) {
      mx = rice_timefreq_dmax(mx,a[i*nf+j]);
    }
  }
  for(i = 0; i < nt; i++) {
    for(j = nf/2; j >= 0; j--) {
      a[i*nf+j] = a[i*nf+j]/mx;
    }
  }

}

/*********************** RICE_TIMEFREQ_CORRECT_TIME *********************/
/* Correct TF distribution such that the quadratic time marginal is     */
/* correct                                                              */
/************************************************************************/
void rice_timefreq_correct_time(double* a, double* b, int nt, int nf)
{
  double sum, factor;
  int i, j;

  for(i = 0; i < nt; i++) {
    sum = 0.0;
    for(j = nf/2; j >= 0; j--) 
      sum +=  a[i*nf+j];
    if(sum != 0.0) {
      factor = (b[i]*b[i])/sum;
      for(j = nf; j >= 0; j--)
        a[i*nf+j] *= factor;
    }
  }

}

/********************RICE_TIMEFREQ_CORRECT_FREQ**************************/
/* Correct TF distribution such that the quadratic frequency marginal   */
/* is correct                                                           */
/************************************************************************/
void rice_timefreq_correct_freq(double* a, double* b, int nt, int nf)
{
  double sum, factor;
  double *dftr, *dfti, *sqdft;
  int i, j, N, M, K;

  /* Get next longer length >= nt witch is power of 2 */
  N = rice_timefreq_nextpowp(nt,2); 
  M = rice_timefreq_po2(N);               /* N = 2**M */
  /* Length of frequency smoothing window */
  K = (int) ((double) N)/ ((double) nf); 

  dftr = (double *) calloc(N,sizeof(double));
  dfti = (double *) calloc(N,sizeof(double));
  for(i = 0; i < nt; i++) {
    dftr[i] = b[i];
    dfti[i] = 0.0;
  }    
  for(i = nt; i < N; i++) { /* Zero pad for power of 2 FFT */
    dftr[i] = 0.0;
    dfti[i] = 0.0;
  }

  /* compute an N point DFT of the input data */
  rice_timefreq_fft(N,M,dftr,dfti); 
  sqdft = (double *) calloc(nf/2+1,sizeof(double));

  /* warp/smooth from length N>=nt DFT to a lenght nf DFT */
  for(j = 0; j <= nf/2; j++) { 
    sqdft[j] = 0.0;
    for(i = 0; i < K; i++) {
      sqdft[j] += (dftr[j*K+i]*dftr[j*K+i] + 
                   dfti[j*K+i]*dfti[j*K+i])/N;
    }
  }

  free((double *)dftr);
  free((double *)dfti);

  for(j = nf/2; j >= 0; j--) {
    sum = 0.0;
    for(i = 0; i < nt; i++)
      sum +=  a[i*nf+j];
    if(sum != 0.0) {
      factor = sqdft[nf/2-j]/sum;
      for(i = 0; i < nt; i++)
        a[i*nf+j] *= factor;
    }
  }
  free((double *)sqdft);

}








/***************************************************************************
********************   Adaptive Optimize Kernal (aok) **********************
****************************************************************************/

void rice_timefreq_aok(double *ixr, double *ixi, double *tf, int xlen, 
                       int winlen,  int fftlen, int fstep, int tstep, 
                       double vol, const char *tfmode)  

{
  int i, ii, j, jj, M, N, ccsym, ftsize;
  int nits, nraf, nrad, nphi, nlag;
  int mfft, slen, itemp, tind, tlen;
  double rtemp, rtemp1, rtemp2;
  double mu, forget, outdelay;
  double pi = 4.0*atan(1.0);
  extern double  rice_timefreq_rectkern(int,int,double*,double*,double*);
  extern int rice_timefreq_po2(int);

  int *maxrad;
  double *sigma;
  double *tmp;
  double *rar;
  double *rai;
  double *tfslicer;
  double *tfslicei;
  double *xr;
  double *xi;
  double *plag;
  double *polafm2;
  double *ptheta;
  double *req;
  double *pheq;
  double *rarN;
  double *raiN;
  double *rectafr;
  double *rectafi;
  double *rectrotr;
  double *rectroti;
  double *rectafm2;

  /* num. of gradient steps to take each time */
  nits = (int) rice_timefreq_logp((double) tstep+2, 2);  
  mu = 0.5;                /* gradient descent factor        */
  forget = 0.001;        /* set no. samples to 0.5 weight on running AF */
  nraf = winlen;                /* theta size of rectangular AF        */
  nrad = winlen;                /* number of radial samples in polar AF */
  nphi = winlen;                /* number of angular samples in polar AF */
  outdelay = winlen/2;        /* delay in effective output time in samples */
                        /* nlag-1 < outdelay < nraf to prevent "echo" effect */
  nlag = winlen + 1;        /* one-sided number of AF lags        */
  mfft = rice_timefreq_po2(fftlen);
  slen = 1.42*(nlag-1) + nraf + 3;  /* number of delayed samples to maintain */
  vol = (2.0*vol*nphi*nrad*nrad)/(pi*winlen); /* normalize volume        */

  maxrad = (int *) calloc(nphi,sizeof(int));
  sigma = (double *) calloc(nphi,sizeof(double));
  tmp = (double *) calloc(nphi,sizeof(double));
  rar = (double *) calloc(nraf,sizeof(double));
  rai = (double *) calloc(nraf,sizeof(double));
  tfslicer = (double *) calloc(fftlen,sizeof(double));
  tfslicei = (double *) calloc(fftlen,sizeof(double));
  xr = (double *) calloc(slen,sizeof(double));
  xi = (double *) calloc(slen,sizeof(double));
  M = nrad*nphi;
  plag = (double *) calloc(M,sizeof(double));
  polafm2 = (double *) calloc(M,sizeof(double));
  ptheta = (double *) calloc(M,sizeof(double));
  N = nlag*nraf;
  req = (double *) calloc(N,sizeof(double));
  pheq = (double *) calloc(N,sizeof(double));
  rarN = (double *) calloc(N,sizeof(double));
  raiN = (double *) calloc(N,sizeof(double));
  rectafr = (double *) calloc(N,sizeof(double));
  rectafi = (double *) calloc(N,sizeof(double));
  rectrotr = (double *) calloc(N,sizeof(double));
  rectroti = (double *) calloc(N,sizeof(double));
  rectafm2 = (double *) calloc(2*N,sizeof(double));

  rice_timefreq_kfill(M,0.0,polafm2);
  rice_timefreq_kfill(N,0.0,rectafr);
  rice_timefreq_kfill(N,0.0,rectafi);
  rice_timefreq_kfill(slen,0.0,xr);
  rice_timefreq_kfill(slen,0.0,xi);
  rice_timefreq_kfill(nphi,1.0,sigma);

  rice_timefreq_rectamake(nlag,nraf,forget,rar,rai,rarN,raiN);
  rice_timefreq_plagmake(nrad,nphi,nlag,plag);
  rice_timefreq_pthetamake(nrad,nphi,nraf,ptheta,maxrad);
  rice_timefreq_rectrotmake(nraf,nlag,outdelay,rectrotr,rectroti);
  rice_timefreq_rectopol(nraf,nlag,nrad,nphi,req,pheq);
  tlen = xlen + nraf + 2;
  ccsym = fftlen-nlag+2;
  for (ii=0; ii < tlen; ii++) {
    rice_timefreq_cshift(slen,xr);
    rice_timefreq_cshift(slen,xi);
    if (ii < xlen) {
      xr[0] = ixr[ii];
      xi[0] = ixi[ii];
    }
    else {
      xr[0] = 0.0;
      xi[0] = 0.0;
    }
    /* Compute RECTangular AF (Ambiguity Function)
       given xr[slen] and xi[slen] (r&i array of delayed samples)
       return rectafr[nlag][nraf] and rectafi[nlag][nraf] (r&i array).
    */
    rice_timefreq_rectaf(xr,xi,nlag,nraf,rar,rai,rarN,raiN,rectafr,rectafi);

    if ((ii - (ii/tstep)*tstep) == 0) { /* output t-f slice        */
      if(strcmp(tfmode, "aok") == 0) {
        /* AOK mode */
        /* Compute magnitude squared of AF (Ambiguity Function)
           given rectafr[nlag][nraf] and rectafi[nlag][nraf] (r&i array),
           return rectafm2[nlag][nraf] (real array).
        */
        rice_timefreq_mkmag2(N,rectafr,rectafi,rectafm2);

        /* Interpolate AF values from rectangular to polar coordinates.
           given rectafm2[nlag][nraf] (real array)
           return polafm2[nphi][nrad] (real array).
        */
        rice_timefreq_polafint(nrad,nphi,nraf,maxrad,nlag,plag,ptheta,
                               rectafm2,polafm2);

        /* Apply optimization mask to AF in polar coordinates,
           given vol level and polafm2[nphi][nrad],
           update sigma[nphi] (real arrays).
        */
        rice_timefreq_sigupdate(nrad,nphi,nits,vol,mu,maxrad,polafm2,sigma,tmp);

        /* Compute square of the raw AF values and integrate over theta.
        */
        for (i=0; i < nlag-1; i++) {        /* for each tau        */
          tfslicer[i] = 0.0;
          tfslicei[i] = 0.0;
          for (j = 0; j < nraf; j++) {        /* integrate over theta */
            tind = i*nraf+j;
            rtemp = rice_timefreq_ccmr(rectafr[tind],rectafi[tind],
                         rectrotr[tind],rectroti[tind]);
            rtemp1 =rice_timefreq_ccmi(rectafr[tind],rectafi[tind],
                         rectrotr[tind],rectroti[tind]);

            /* Interpolate from polar to rectangular
               given sigma[nphi], pheq[nlag][nraf], req[nlag][nraf],
               return tsigma as rtemp2.
            */
            rtemp2 = rice_timefreq_rectkern(tind,nphi,req,pheq,sigma);

            tfslicer[i] = tfslicer[i] + rtemp*rtemp2;
            tfslicei[i] = tfslicei[i] + rtemp1*rtemp2;
          }
        }
      } else {
        /* assume WIG mode */
        /* Compute square of the raw AF values and integrate over theta.
        */
        for (i=0; i < nlag-1; i++) {        /* for each tau        */
          tfslicer[i] = 0.0;
          tfslicei[i] = 0.0;
          for (j = 0; j < nraf; j++) {        /* integrate over theta */
            tind = i*nraf+j;
            rtemp = rice_timefreq_ccmr(rectafr[tind],rectafi[tind],
                         rectrotr[tind],rectroti[tind]);
            rtemp1 =rice_timefreq_ccmi(rectafr[tind],rectafi[tind],
                         rectrotr[tind],rectroti[tind]);
            tfslicer[i] = tfslicer[i] + rtemp;
            tfslicei[i] = tfslicei[i] + rtemp1;
          }
        }
      }

      /* Results are now back in rectangular coordinates, ready for FFT */
      for (i=nlag-1; i < ccsym; i++) { /* zero pad for FFT */
        tfslicer[i] = 0.0;
        tfslicei[i] = 0.0;
      }
      for (i=ccsym; i < fftlen; i++) { /* fill in c.c. symmetric half */
        tfslicer[i] =  tfslicer[fftlen-i];
        tfslicei[i] = -tfslicei[fftlen-i];
      }
      rice_timefreq_fft(fftlen,mfft,tfslicer,tfslicei);
      itemp = fftlen/2 + fstep;
      ftsize = fftlen/fstep;
      jj = 0;
      for (i = itemp; i < fftlen; i += fstep) {
        tf[ii*ftsize+jj] = tfslicer[i];
        jj++;
      }
      for (i = 0; i < itemp; i += fstep) {
        tf[ii*ftsize+jj] = tfslicer[i];
        jj++;
      }
    }
  }
  free((int *) maxrad);
  free((double *) sigma);
  free((double *) tmp);
  free((double *) rar);
  free((double *) rai);
  free((double *) tfslicer);
  free((double *) tfslicei);
  free((double *) xr);
  free((double *) xi);
  free((double *) plag);
  free((double *) polafm2);
  free((double *) ptheta);
  free((double *) req);
  free((double *) pheq);
  free((double *) rarN);
  free((double *) raiN);
  free((double *) rectafr);
  free((double *) rectafi);
  free((double *) rectrotr);
  free((double *) rectroti);
  free((double *) rectafm2);
}

/*********************************************************************/
/*****  This is a pass thru to our CPS fft primitive              ****/
/*********************************************************************/

void rice_timefreq_fft(int n, int m, double *x, double *y)
{
int sign = -1;
int i,j;
int array_size = 2 * n;
float *array, *junk=NULL;
char opt[5];

  array = (float *)  calloc( 1, array_size * sizeof(float));
  if(array == NULL)
    {
    return;
    }

  /*Format the real and imaginary data x and y into one array*/
  for(i = 0, j = 0; i < n; i++, j++)
    {
    array[j]   = x[i];
    array[++j] = y[i];
    }

  strcpy(opt,"ctoc");

  /*Call the f77 wrapper around the f90 code */
  fftf77_( opt, &n, &sign, junk, array, &array_size);

  /*Put the data back into the x and y arrays*/
  for(i = 0, j = 0; i < n; i++, j++)
    {
    x[i] = array[j];
    y[i] = array[++j];
    }

  free ((float *)array);

}


/*                                                                */
/*   zerofill: set array elements to constant                     */
/*                                                                */
void rice_timefreq_kfill(int len, double k, double *x)
{
  int        i;

  for (i=0; i < len; i++)
    x[i] = k;
}

/*                                                                */
/*   rice_timefreq_cshift: circularly shift an array              */
/*                                                                */
void rice_timefreq_cshift(int len, double *x)
{
  int        i;
  double rtemp;

  rtemp = x[len-1];
  for (i=len-1; i > 0; i--)
    x[i] = x[i-1];
  x[0] = rtemp;
}

/*                                                                  */
/*rice_timefreq_rectamake: make vector of poles for rect running AF */
/*                                                                  */
void rice_timefreq_rectamake(int nlag, int nraf, double forget, double *rar,
                             double *rai,
                             double *rarN, double *raiN)
{
  register int i,j;
  double trig,decay;
  double trigN,decayN;
  double pi2 = 8.0*atan(1.0);

  trig = pi2/nraf;
  decay = exp(-forget);

  for (j=0; j < nraf; j++) {
    rar[j] = decay*cos(trig*j);
    rai[j] = decay*sin(trig*j);
  }
  for (i=0; i < nlag; i++) {
    trigN = pi2*(nraf-i);
    trigN = trigN/nraf;
    decayN = exp(-forget*(nraf-i));
    for (j=0; j < nraf; j++) {
      rarN[i*nraf+j] = decayN*cos(trigN*j);
      raiN[i*nraf+j] = decayN*sin(trigN*j);
    }
  }
}

/*                                                                           */
/*rice_timefreq_pthetamake: make matrix of theta indices for polar samples   */
/*                                                                           */
void rice_timefreq_pthetamake(int nrad, int nphi,int nraf, double *ptheta,
                              int *maxrad)
{
  int i, j;
  double theta, rtemp, deltheta;
  double pi = 4.0*atan(1.0);
  double pi2 = 8.0*atan(1.0);
  double sq2 = sqrt(2.0);
  double tmp;

  tmp = pi*sq2/nrad;

  deltheta = pi2/nraf;
  for (i=0; i < nphi; i++) {         /* for all phi ...*/
    maxrad[i] = nrad;
    for (j = 0; j < nrad; j++) { /* and all radii */
      theta = -(tmp*j)*cos((pi*i)/nphi);
      if (theta >= 0.0) {
        rtemp = theta/deltheta;
        if (rtemp > (nraf/2 - 1)) {
          rtemp = -1.0;
          if (j < maxrad[i])
            maxrad[i] = j;
        }
      }
      else {
        rtemp = (theta + pi2)/deltheta;
        if (rtemp < (nraf/2 + 1)) {
          rtemp = -1.0;
          if (j < maxrad[i])
            maxrad[i] = j;
        }
      }
      ptheta[i*nrad+j] = rtemp;
    }
  }
}

/*                                                                    */
/*   rice_timefreq_plagmake: make matrix of lags for polar running AF */
/*                                                                    */
void rice_timefreq_plagmake(int nrad, int nphi, int nlag, double *plag)
{
  register int i, j;
  double pi = 4.0*atan(1.0);
  double sq2 = sqrt(2.0);

  for (i=0; i < nphi; i++) {       /* for all phi ...      */
    for (j = 0; j < nrad; j++) {   /* and all radii        */
      plag[i*nrad+j] = ((sq2*(nlag-1)*j)/nrad)*sin((pi*i)/nphi);
    }
  }
}

/*                                                                           */
/*rice_timefreq_rectopol: find polar indices corresponding to rect samples   */
/*                                                                           */
void rice_timefreq_rectopol(int nraf, int nlag, int nrad, int nphi, double *req,
                            double *pheq)
{
  register int i, j, jt;
  double deltau, deltheta, delrad, delphi;
  double pih = 2.0*atan(1.0);
  double pi = 4.0*atan(1.0);

  deltau = sqrt(pi/(nlag-1));
  deltheta = 2.0*sqrt((nlag-1)*pi)/nraf;
  delrad = sqrt(2.0*pi*(nlag-1))/nrad;
  delphi = pi/nphi;

  for (i=0; i < nlag; i++) {
    for (j=0; j < nraf/2; j++) {
      req[i*nraf +j] = sqrt(i*i*deltau*deltau 
                            + j*j*deltheta*deltheta)/delrad;
      if (i == 0)
        pheq[i*nraf +j] = 0.0;
      else 
        pheq[i*nraf +j] = (atan((j*deltheta)/(i*deltau)) 
                           + pih)/delphi;
    }
    for (j=0; j < nraf/2; j++) {
      jt = j - nraf/2;
      req[i*nraf + nraf/2 + j] = 
        sqrt(i*i*deltau*deltau 
             + jt*jt*deltheta*deltheta)/delrad;
      if (i == 0)
        pheq[i*nraf + nraf/2 + j] = 0.0;
      else 
        pheq[i*nraf + nraf/2 + j] =
          (atan((jt*deltheta)/(i*deltau)) 
           + pih)/delphi;
    }
  }
}

/*                                                                  */
/*   rice_timefreq_rectrotmake: make array of rect AF phase shifts  */
/*                                                                  */
void rice_timefreq_rectrotmake(int nraf, int nlag, double outdelay,
                               double *rectrotr, double *rectroti)
{
  register int i, j, t1, t2;
  double twopin, trigarg;
  double pi2 = 8.0*atan(1.0);

  twopin = pi2/nraf;
  for (i=0; i < nlag; i++) {
    t1 = i*nraf;
    for (j=0; j < nraf/2; j++) {
      t2 = t1+j;
      trigarg = (twopin*j)*(outdelay - ((double) i)/2.0);
      rectrotr[t2] = cos(trigarg);
      rectroti[t2] = sin(trigarg);
    }
    for (j=nraf/2; j < nraf; j++) {
      t2 = t1+j;
      trigarg = (twopin*(j-nraf))*(outdelay - ((double) i)/2.0 );
      rectrotr[t2] = cos(trigarg);
      rectroti[t2] = sin(trigarg);
    }
  }
}

/*                                                                  */
/*   rice_timefreq_rectaf: generate running AF on rectangular grid; */
/*             negative lags, all DFT frequencies                   */
/*                                                                  */
void rice_timefreq_rectaf(double *xr, double *xi, int nlag, int nraf, 
                          double *rar, double *rai, double *rarN, double *raiN, 
                          double *rectafr, double *rectafi)
{
  register int i, j, t1, t2;
  double rtemp, rr, ri, rrN, riN;

  for (i=0; i < nlag; i++) {
    rr = rice_timefreq_ccmr(xr[0],xi[0],xr[i],xi[i]);
    ri = rice_timefreq_ccmi(xr[0],xi[0],xr[i],xi[i]);
    rrN = rice_timefreq_ccmr(xr[nraf-i],xi[nraf-i],xr[nraf],xi[nraf]);
    riN = rice_timefreq_ccmi(xr[nraf-i],xi[nraf-i],xr[nraf],xi[nraf]);
    t1 = i*nraf;
    for (j = 0; j < nraf; j++) {
      t2 = t1+j;
      rtemp = rice_timefreq_cmr(rectafr[t2],rectafi[t2],rar[j],rai[j])
        - rice_timefreq_cmr(rrN,riN,rarN[t2],raiN[t2]) + rr;
      rectafi[t2] = rice_timefreq_cmi(rectafr[t2],rectafi[t2],rar[j],rai[j])
        - rice_timefreq_cmi(rrN,riN,rarN[t2],raiN[t2]) + ri;
      rectafr[t2] = rtemp;
    }
  }
}

/*                                                                       */
/*   rice_timefreq_rectkern: generate kernel samples on rectangular grid */
/*                                                                       */
double rice_timefreq_rectkern(int tind, int nphi, double *req, double *pheq,
                              double *sigma)
{
  int iphi, iphi1;
  double tsigma;

  iphi = (int) pheq[tind];
  iphi1 = iphi + 1;
  if (iphi1 > (nphi-1))
    iphi1 = 0;
  tsigma = sigma[iphi] + (pheq[tind]
                          - iphi)*(sigma[iphi1]-sigma[iphi]);

  /*  Tom Polver says on his machine, exp screws up when the argument of */
  /*  the exp function is too large */

  return(exp(-req[tind]*req[tind]/(tsigma*tsigma)));
}

/*                                                                */
/*   rice_timefreq_sigupdate: update RG kernel parameters         */
/*                                                                */
void rice_timefreq_sigupdate(int nrad, int nphi, int nits, double vol,
                             double mu0, int *maxrad, double *polafm2, 
                             double *sigma, double *wks)
{
  register int ii, i, j;
  double gradsum, gradsum1;
  double tvol, volfac, eec, ee1, ee2, mu;
  double tol = 0.0000001;

  for (ii=0; ii < nits; ii++) {
    gradsum = 0.0;
    gradsum1 = 0.0;
    for (i=0; i < nphi; i++) {
      wks[i] = 0.0;
      ee1 = exp(-1.0/(sigma[i]*sigma[i])); /* use Kaiser's efficient method */
      ee2 = 1.0;
      eec = ee1*ee1;
      for (j=1; j < maxrad[i]; j++) {
        ee2 = ee1*ee2;
        ee1 = eec*ee1;
        wks[i] = wks[i] + j*j*j*ee2*polafm2[i*nrad+j];
      }
      wks[i] = wks[i]/(sigma[i]*sigma[i]*sigma[i]);
      gradsum = gradsum + wks[i]*wks[i];
      gradsum1 = gradsum1 + sigma[i]*wks[i];
    }
    gradsum1 = 2.0*gradsum1;
    if (gradsum < tol)
      gradsum = tol;
    if (gradsum1 < tol)
      gradsum1 = tol;
    mu = (sqrt(gradsum1*gradsum1 + 4.0*gradsum*vol*mu0)
          - gradsum1)/(2.0*gradsum);
    tvol = 0.0;
    for (i=0; i < nphi; i++) {
      sigma[i] = sigma[i] + mu*wks[i];
      if (sigma[i] < 0.5)
        sigma[i] = 0.5;
      tvol = tvol + sigma[i]*sigma[i];
    }
    volfac = sqrt(vol/tvol);
    for (i=0; i < nphi; i++)
      sigma[i] = volfac*sigma[i];
  }
}

/*                                                                */
/*   rice_timefreq_mkmag2: compute squared magnitude of an array  */
/*                                                                */
void rice_timefreq_mkmag2(int K, double *xr, double *xi, double *xm2)
{
  register int i;

  for (i = 0; i < K; i++)
    xm2[i] = xr[i]*xr[i] + xi[i]*xi[i];
}

/*                                                                */
/*   rice_timefreq_polafint: interpolate AF on polar grid;        */
/*                                                                */
void rice_timefreq_polafint(int nrad, int nphi, int nraf, int *maxrad, int nlag,
                            double *plag, double *ptheta, double *rectafm2,
                            double *polafm2)
{
  register int i, j, t1, t2;
  register int ilag, itheta, itheta1;
  double rlag, rtheta, rtemp, rtemp1;

  for (i=0; i < nphi/2; i++) {        /* for all phi ...  */
    t1 = i*nrad;
    for (j = 0; j < maxrad[i]; j++) { /* and all radii with |theta| < pi */
      t2 = t1+j;
      ilag = (int) plag[t2];
      rlag = plag[t2] - ilag;
      if (ilag >= nlag)
        polafm2[t2] = 0.0;
      else {
        itheta = (int) ptheta[t2];
        rtheta = ptheta[t2] - itheta;
        itheta1 = itheta + 1;
        if (itheta1 >= nraf)  
          itheta1 = 0;
        rtemp = (rectafm2[ilag*nraf+itheta1] 
                 - rectafm2[ilag*nraf+itheta])*rtheta
                     + rectafm2[ilag*nraf+itheta];
        rtemp1 = (rectafm2[(ilag+1)*nraf+itheta1] 
                  - rectafm2[(ilag+1)*nraf+itheta])*rtheta 
                     + rectafm2[(ilag+1)*nraf+itheta];
        polafm2[t2] = (rtemp1-rtemp)*rlag + rtemp;
      }
    }
  }  
  for (i = nphi/2; i < nphi; i++) { /* for all phi ...  */
    t1 = i*nrad;
    for (j = 0; j < maxrad[i]; j++) { /* and all radii with |theta| < pi */
      t2 = t1+j;
      ilag = (int) plag[t2];
      rlag = plag[t2] - ilag;
      if (ilag >= nlag)
        polafm2[t2] = 0.0;
      else {
        itheta = (int) ptheta[t2];
        rtheta = ptheta[t2] - itheta;
        rtemp =  (rectafm2[ilag*nraf+itheta+1] 
                  - rectafm2[ilag*nraf+itheta])*rtheta 
                    + rectafm2[ilag*nraf+itheta];
        rtemp1 =  (rectafm2[(ilag+1)*nraf+itheta+1]
                   - rectafm2[(ilag+1)*nraf+itheta])*rtheta
                     + rectafm2[(ilag+1)*nraf+itheta];
        polafm2[t2] = (rtemp1-rtemp)*rlag + rtemp;
      }
    }
  }
}

/*                                                              */
/*   rice_timefreq_rhilbert:                                    */
/*                                                              */
void rice_timefreq_rhilbert(double *xr, double *xi, int xlen)
{
  register int i, ii, m, n;
  double *tr, *ti;
  extern int rice_timefreq_nextpowp(int,int),rice_timefreq_po2(int);
  
  if(rice_timefreq_ispowp(xlen,2))
    n = xlen;
  else
    n = rice_timefreq_nextpowp(xlen,2);
  tr = (double *) calloc(n,sizeof(double));
  ti = (double *) calloc(n,sizeof(double));
  for (i = 0; i < xlen; i++) {
    tr[i] = xr[i];
    ti[i] = xi[i];
  }
  m = rice_timefreq_po2(n);
  rice_timefreq_fft(n,m,tr,ti);
  ii = floor((double) (n-1)/2.0);
  ti[0] = -ti[0];
  for (i = 1; i <= ii; i++) {
    tr[i] = 2.0*tr[i];
    ti[i] = -2.0*ti[i];
  }
  ti[ii+1] = -ti[ii+1];
  for (i = ii+2; i < n; i++) {
    tr[i] = 0;
    ti[i] = 0;
  }
  rice_timefreq_fft(n,m,tr,ti);
  for (i = 0; i < xlen; i++) {
    xr[i] = xr[i];
    xi[i] = ti[i]/n;
  }

  free((double *)tr);
  free((double *)ti);
}

/*                                                                */
/*   rice_timefreq_nextpowp:                                      */
/*                                                                */
int rice_timefreq_nextpowp(int n, int p)
{
  int tmp;

  tmp = ceil(log10((double) n)/log10((double) p));
  return((int) pow((double) p, (double) tmp));
}

/*                                                                    */
/*   rice_timefreq_po2: find the smallest power of two >= input value */
/*                                                                    */
int rice_timefreq_po2(int n)
{
  int        m, mm;
  
  mm = 1;
  m = 0;
  while (mm < n) {
    ++m;
    mm = 2*mm;
  }
  return(m);
}





/*****************************************************************************
 ********************** Short time freqeuncy transform (stft) ****************
 *****************************************************************************/
void rice_timefreq_stft(double *ixr, double *ixi, tfr *tf, tfrpar*win)
{
  int i,j;
  int nwh;
  double *wixr, *wixi;

  rice_timefreq_window(win);
  wixr = (double *) calloc(win->fftlen,sizeof(double));
  wixi = (double *) calloc(win->fftlen,sizeof(double));
  nwh = (win->nw-1)/2;
  for(i = -nwh; i < tf->nt-nwh; i += win->tstep) {
    for(j = 0; j < win->nw; j++) {
      wixr[j] = win->vect[j]*ixr[i+j];
      wixi[j] = win->vect[j]*ixi[i+j];
    }
    for(j = win->nw; j < win->fftlen; j++) { /* Zero pad for FFT */
      wixr[j] = 0.0;
      wixi[j] = 0.0;
    }
    rice_timefreq_fft(win->fftlen, win->mfft, wixr, wixi);
    for(j = 0; j < win->fftlen; j++) {
      tf->re[j+(i+nwh)*win->fftlen] = wixr[j];
      tf->im[j+(i+nwh)*win->fftlen] = wixi[j];
    }
  }
  free((double *) wixr);
  free((double *) wixi);
}

/*****************************************************************/
/* Generate energy-normalized (to 1) window functions to be used by STFT */
/*************************************************************************/
void rice_timefreq_window(tfrpar *win)
{
  int i;
  double nh;
  double tmp;
  double pi = 4.0*atan(1.0);

  if(!strcmp(win->window,"Rectangular")) { /* Initialize rectangular window */
    tmp = 1.0/win->nw;
    for(i = 0; i < win->nw; i++)
      win->vect[i] = tmp;
  }
  else if(!strcmp(win->window,"Hamming")) {  /* Initialize Hamming window  */
    for(i = 0; i < win->nw; i++)
      win->vect[i] = 0.54-0.46*cos(2*pi*(i+1)/(win->nw+1));  
  }
  else if(!strcmp(win->window,"Hanning")) {  /* Initialize Hanning window  */
    for(i = 0; i < win->nw; i++)
      win->vect[i] = 0.5-0.5*cos(2*pi*(i+1)/(win->nw+1));  
  }
  else if(!strcmp(win->window,"Blackman")) { /* Initialize Blackman window */
    nh = ((double) win->nw-1.0)/2.0;
    for(i = 0; i < win->nw; i++)
      win->vect[i] = 0.42 + 0.50*cos(2.0*pi*(i-nh)/win->nw)
        + 0.08*cos(4.0*pi*(i-nh)/win->nw);
  }
  else if(!strcmp(win->window,"Bartlet")) {  /* Initialize Bartlet window  */
    for(i = 0; i < win->nw; i++)
      win->vect[i] = 2.0*rice_timefreq_dmin(i+1,win->nw-i)/(win->nw+1);
  }
}

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

