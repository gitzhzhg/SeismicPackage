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
/*
C      do_byte_nmo.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C             written in c -- designed to be called from c
C
C     Utility Name:  do_byte_nmo    (do nmo on byte traces)
C          Written:  93/08/20  by:  Tom Stoeckley
C     Last revised:  93/08/20  by:  Tom Stoeckley
C
C  Purpose:  Do forward or reverse NMO on a gather of byte traces.
C            A single velocity function is used on all traces in
C            the gather .
C
C  Related Documentation:  
C-----------------------------------------------------------------------
C                            THIS UTILITY     
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/trslib  (shared)
C  library:                trslib.a           (shared)
C  header file:            trslib.h           (shared)
C  source file:            do_byte_nmo.c
C
C  static functions:       none
C  documented functions:   do_byte_nmo
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES 
C         (this utility does not reference X, Xt, and Motif)
C              (standard C references not listed here)
C
C  libraries:     trslib.a    cprim.a  
C  header files:  trslib.h    cprim.h
C  functions:
C                 convert_byte_to_float       velfun_nmo_prep
C                 return_float_to_byte        velfun_do_nmo
C                 eta_nmo_prep                nonhyp_nmo_prep
C                 eta_do_nmo                  nonhyp_do_nmo
C                 densify3
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 93/08/20  Stoeckley  Initial version created from the routine
C                            do_nmo_correction2 in the VA application.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C
C  For pointers, the flag (i,o,b) refers to the contents pointed to
C  by the pointer, not to the value of the pointer itself.  The pointer
C  value is required upon INPUT in all cases.
C-----------------------------------------------------------------------
C  To do NMO correction:
C                              i     i     i       i     i    i  i
C         error = do_byte_nmo(mode, dop, iprint, times, vnmo, n, dx,
C                             head, nwords, bbbb, nsamp, ntraces, tstrt, dt)
C                              i      i      b      i       i       i    i
C
C                                       i       i
C         error = do_byte_nonhyp_nmo(nhosign, nhoexp,
C                              i     i     i       i     i    i  i
C                             mode, dop, iprint, times, vnmo, n, dx,
C                             head, nwords, bbbb, nsamp, ntraces, tstrt, dt)
C                              i      i      b      i       i       i    i
C
C                                       i       i
C         error = do_float_nonhyp_nmo(nhosign, nhoexp,
C                              i     i     i       i     i    i  i
C                             mode, dop, iprint, times, vnmo, n, dx,
C                             head, nwords, ffff, nsamp, ntraces, tstrt, dt)
C                              i      i      b      i       i       i    i
C
C                               i     i     i       i     i     i   i  i
C     error = do_byte_eta_nmo (mode, dop, iprint, times, vnmo, eta, n, dx,
C                              head, nwords, bbbb, nsamp, ntraces, tstrt, dt)
C                               i      i      b      i       i       i    i
C
C                               i     i     i       i     i     i   i  i
C     error = do_float_eta_nmo(mode, dop, iprint, times, vnmo, eta, n, dx,
C                              head, nwords, ffff, nsamp, ntraces, tstrt, dt)
C                               i      i      b      i       i       i    i
C
C
C  float nhosign= 1.0 for normal NMO; typically -1.0 for non-hyperbolic NMO.
C  float nhoexp = 2.0 for normal NMO; typically  4.0 for non-hyperbolic NMO.
C  long    mode = FORWARD for forward nmo, REVERSE for reverse nmo.
C  float    dop = doppler mute (e.g. 1.7 to allow stretch factor 1.7).
C  long  iprint = whether to print debug information (True or False).
C  float *times = velocity function time picks.
C  float  *vnmo = velocity function stacking velocities.
C  float   *eta = velocity function eta values for anisotropic NMO.
C  long       n = number of velocity function picks.
C  float     dx = densification step size (seconds) (recommend <= 0.1).
C
C  float         *head = pointer to CPS trace headers.
C  long         nwords = number of words in each trace header.
C  unsigned char *bbbb = pointer to byte traces.
C  float         *ffff = pointer to float traces.
C  long          nsamp = number of sample values on each trace.
C  long        ntraces = number of traces and trace headers.
C  float         tstrt = time (seconds) for first sample on the trace.
C  float            dt = sample rate (seconds).
C
C  long error = error return (zero if no error).
C
C  The constants FORWARD and REVERSE are defined in the cprim.h header
C    file.
C  The byte traces are unchanged if an error occurs.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C\END DOC
*/


#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stddef.h>
#include "trslib.h"
#include "cprim.h"


/*
#define NMAX 200
*/
#define NMAX 500     /* changed 6/11/96 to allow smaller sample rates */



/*---------- do nmo correction on byte arrays --------------------------*/

long do_byte_nmo(long mode, float dop, long iprint,
              float times[], float vnmo[], long n, float dx,
              float head[], long nwords, unsigned char bbbb[],
              long nsamp, long ntraces, float tstrt, float dt)
{
  long index, error, nnn, i;
  float cnst, offset, bmute;
  float *ttt, *vvv, *ta, *tb, *a, *b;
  void densify3(float *dx, long *nmax, long *n, float x[], float z[],
                        long *nn, float xx[], float zz[], long *error);
  float tmin, tmax, times2[NMAX], vnmo2[NMAX];
  long nmax = NMAX, n2;
  unsigned char *oldbbbb=NULL;  long j;

  if(mode != FORWARD && mode != REVERSE)     return 1;
  if(dt <= 0.0 || ntraces < 1 || nwords < 6) return 1;
  if(dx <= 0.0 || nsamp < 1 || n < 2)        return 1;
  densify3(&dx, &nmax, &n, times, vnmo, &n2, times2, vnmo2, &error);
  tmin = tstrt;
  tmax = tstrt + (nsamp - 1) * dt;

  if(iprint)
       {
       printf("STARTING NMO  dop=%f  mode=%d  nsamp=%d  n=%d %d\n",
                                  dop, mode, nsamp, n, n2);
       printf("tmin=%f  tmax=%f  dt=%f\n", tmin, tmax, dt);
       }

  ttt = (float*)calloc(1, (n2 + 1) * sizeof(float));
  vvv = (float*)calloc(1, (n2 + 1) * sizeof(float));
  tb  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  velfun_nmo_prep(times2, vnmo2,
              &n2, &tmin, &tmax, &dt, &nnn, ttt, vvv, tb, &cnst, &error);
  if(error)
       {
       free(ttt); free(vvv); free(tb); return error;
       }
  ta  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  a   = (float*)calloc(1, nsamp * sizeof(float));
  b   = (float*)calloc(1, nsamp * sizeof(float));

  if(iprint)
       {
       oldbbbb = (unsigned char*)calloc(1, nsamp * sizeof(unsigned char));
       }

  for(i = 0; i < ntraces; i++)
       {
       offset = head[nwords * i + 5];
       index = nsamp * i;
       convert_byte_to_float(nsamp, &bbbb[index], a);
       if(iprint)
          {
          printf("TRACE NUMBER %d  offset=%f  index=%d\n", i+1, offset, index);
          for(j = 0; j < nsamp; j++) {  oldbbbb[j] = bbbb[index + j]; }
          }
       velfun_do_nmo(&dop, &nnn, ttt, vvv, tb, &cnst,
                &mode, &offset, ta, a, b, &nsamp, &bmute);
       return_float_to_byte(nsamp, b, &bbbb[index]);

       if(iprint)
          {
          printf("bmute = %f\n", bmute);
          printf("        time     vnmo       ttt        vvv      ta tb\n");
          for(j = 0; j < n2; j++)
               {
               printf("%4d %8.3f %7.0f %11.1f %11.8f %7.1f %7.1f\n",
                    j, times2[j], vnmo2[j], ttt[j], vvv[j], ta[j], tb[j]);
               }
          printf("    j   index+j   byte   float  (nmo)  float   byte\n");
          for(j = 0; j < nsamp; j+=200)
               {
               printf("%6d %6d %6d %8.2f         %8.2f %6d \n", j, index + j,
                     (int)oldbbbb[j], a[j], b[j], (int)bbbb[index + j]);
               }
          }
       }
  if(iprint)
       {
       if(oldbbbb) free(oldbbbb);
       }
  free(ttt); free(vvv); free(tb); free(ta); free(a); free(b); return 0;
}




/*---------- do nmo correction on byte arrays --------------------------*/

long do_byte_nonhyp_nmo(float nhosign, float nhoexp,
              long mode, float dop, long iprint,
              float times[], float vnmo[], long n, float dx,
              float head[], long nwords, unsigned char bbbb[],
              long nsamp, long ntraces, float tstrt, float dt)
{
  long index, error, nnn, i;
  float cnst, offset, bmute;
  float *ttt, *vvv, *ta, *tb, *a, *b;
  void densify3(float *dx, long *nmax, long *n, float x[], float z[],
                        long *nn, float xx[], float zz[], long *error);
  float tmin, tmax, times2[NMAX], vnmo2[NMAX];
  long nmax = NMAX, n2;
  unsigned char *oldbbbb=NULL;  long j;

  if(mode != FORWARD && mode != REVERSE)     return 1;
  if(dt <= 0.0 || ntraces < 1 || nwords < 6) return 1;
  if(dx <= 0.0 || nsamp < 1 || n < 2)        return 1;
  densify3(&dx, &nmax, &n, times, vnmo, &n2, times2, vnmo2, &error);
  tmin = tstrt;
  tmax = tstrt + (nsamp - 1) * dt;

  if(iprint)
       {
       printf("STARTING NMO  dop=%f  mode=%d  nsamp=%d  n=%d %d\n",
                                  dop, mode, nsamp, n, n2);
       printf("tmin=%f  tmax=%f  dt=%f\n", tmin, tmax, dt);
       }

  ttt = (float*)calloc(1, (n2 + 1) * sizeof(float));
  vvv = (float*)calloc(1, (n2 + 1) * sizeof(float));
  tb  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  nonhyp_nmo_prep(&nhosign, &nhoexp,
              times2, vnmo2,
              &n2, &tmin, &tmax, &dt, &nnn, ttt, vvv, tb, &cnst, &error);
  if(error)
       {
       free(ttt); free(vvv); free(tb); return error;
       }
  ta  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  a   = (float*)calloc(1, nsamp * sizeof(float));
  b   = (float*)calloc(1, nsamp * sizeof(float));

  if(iprint)
       {
       oldbbbb = (unsigned char*)calloc(1, nsamp * sizeof(unsigned char));
       }

  for(i = 0; i < ntraces; i++)
       {
       offset = head[nwords * i + 5];
       index = nsamp * i;
       convert_byte_to_float(nsamp, &bbbb[index], a);
       if(iprint)
          {
          printf("TRACE NUMBER %d  offset=%f  index=%d\n", i+1, offset, index);
          for(j = 0; j < nsamp; j++) {  oldbbbb[j] = bbbb[index + j]; }
          }
       nonhyp_do_nmo(&nhosign, &nhoexp,
                &dop, &nnn, ttt, vvv, tb, &cnst,
                &mode, &offset, ta, a, b, &nsamp, &bmute);
       return_float_to_byte(nsamp, b, &bbbb[index]);

       if(iprint)
          {
          printf("bmute = %f\n", bmute);
          printf("        time     vnmo       ttt        vvv      ta tb\n");
          for(j = 0; j < n2; j++)
               {
               printf("%4d %8.3f %7.0f %11.1f %11.8f %7.1f %7.1f\n",
                    j, times2[j], vnmo2[j], ttt[j], vvv[j], ta[j], tb[j]);
               }
          printf("    j   index+j   byte   float  (nmo)  float   byte\n");
          for(j = 0; j < nsamp; j+=200)
               {
               printf("%6d %6d %6d %8.2f         %8.2f %6d \n", j, index + j,
                     (int)oldbbbb[j], a[j], b[j], (int)bbbb[index + j]);
               }
          }
       }
  if(iprint)
       {
       if(oldbbbb) free(oldbbbb);
       }
  free(ttt); free(vvv); free(tb); free(ta); free(a); free(b); return 0;
}


/*---------- do nmo correction on float arrays --------------------------*/

long do_float_nonhyp_nmo(float nhosign, float nhoexp,
              long mode, float dop, long iprint,
              float times[], float vnmo[], long n, float dx,
              float head[], long nwords, float ffff[],
              long nsamp, long ntraces, float tstrt, float dt)
{
  long index, error, nnn, i;
  float cnst, offset, bmute;
  float *ttt, *vvv, *ta, *tb, *a, *b;
  void densify3(float *dx, long *nmax, long *n, float x[], float z[],
                        long *nn, float xx[], float zz[], long *error);
  float tmin, tmax, times2[NMAX], vnmo2[NMAX];
  long nmax = NMAX, n2;
  float *oldffff=NULL;  long j;

  if(mode != FORWARD && mode != REVERSE)     return 1;
  if(dt <= 0.0 || ntraces < 1 || nwords < 6) return 1;
  if(dx <= 0.0 || nsamp < 1 || n < 2)        return 1;
  densify3(&dx, &nmax, &n, times, vnmo, &n2, times2, vnmo2, &error);
  tmin = tstrt;
  tmax = tstrt + (nsamp - 1) * dt;

  if(iprint)
       {
       printf("STARTING NMO  dop=%f  mode=%d  nsamp=%d  n=%d %d\n",
                                  dop, mode, nsamp, n, n2);
       printf("tmin=%f  tmax=%f  dt=%f\n", tmin, tmax, dt);
       }

  ttt = (float*)calloc(1, (n2 + 1) * sizeof(float));
  vvv = (float*)calloc(1, (n2 + 1) * sizeof(float));
  tb  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  nonhyp_nmo_prep(&nhosign, &nhoexp,
              times2, vnmo2,
              &n2, &tmin, &tmax, &dt, &nnn, ttt, vvv, tb, &cnst, &error);
  if(error)
       {
       free(ttt); free(vvv); free(tb); return error;
       }
  ta  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  a   = (float*)calloc(1, nsamp * sizeof(float));
  b   = (float*)calloc(1, nsamp * sizeof(float));

  if(iprint)
       {
       oldffff = (float*)calloc(1, nsamp * sizeof(float));
       }

  for(i = 0; i < ntraces; i++)
       {
       offset = head[nwords * i + 5];
       index = nsamp * i;
       memcpy(a, &ffff[index], nsamp * sizeof(float));
       if(iprint)
          {
          printf("TRACE NUMBER %d  offset=%f  index=%d\n", i+1, offset, index);
          for(j = 0; j < nsamp; j++) {  oldffff[j] = ffff[index + j]; }
          }
       nonhyp_do_nmo(&nhosign, &nhoexp,
                &dop, &nnn, ttt, vvv, tb, &cnst,
                &mode, &offset, ta, a, b, &nsamp, &bmute);
       memcpy(&ffff[index], b, nsamp * sizeof(float));

       if(iprint)
          {
          printf("bmute = %f\n", bmute);
          printf("        time     vnmo       ttt        vvv      ta tb\n");
          for(j = 0; j < n2; j++)
               {
               printf("%4d %8.3f %7.0f %11.1f %11.8f %7.1f %7.1f\n",
                    j, times2[j], vnmo2[j], ttt[j], vvv[j], ta[j], tb[j]);
               }
          printf("    j   index+j   byte   float  (nmo)  float   byte\n");
          for(j = 0; j < nsamp; j+=200)
               {
               printf("%6d %6d %6d %8.2f         %8.2f %6d \n", j, index + j,
                     (int)oldffff[j], a[j], b[j], (int)ffff[index + j]);
               }
          }
       }
  if(iprint)
       {
       if(oldffff) free(oldffff);
       }
  free(ttt); free(vvv); free(tb); free(ta); free(a); free(b); return 0;
}



/*---------- do eta nmo correction on byte arrays --------------------------*/

long do_byte_eta_nmo(
              long mode, float dop, long iprint,
              float times[], float vnmo[], float eta[], long n, float dx,
              float head[], long nwords, unsigned char bbbb[],
              long nsamp, long ntraces, float tstrt, float dt)
{
  long index, error, nnn, i;
  float cnst, offset, bmute;
  float *ttt, *vvv, *eee, *fff, *ggg, *ta, *tb, *a, *b;
  void densify3(float *dx, long *nmax, long *n, float x[], float z[],
                        long *nn, float xx[], float zz[], long *error);
  float tmin, tmax, times2[NMAX], vnmo2[NMAX], eta2[NMAX];
  long nmax = NMAX, n2;
  unsigned char *oldbbbb=NULL;  long j;

  if(mode != FORWARD && mode != REVERSE)     return 1;
  if(dt <= 0.0 || ntraces < 1 || nwords < 6) return 1;
  if(dx <= 0.0 || nsamp < 1 || n < 2)        return 1;
  densify3(&dx, &nmax, &n, times, vnmo, &n2, times2, vnmo2, &error);
  densify3(&dx, &nmax, &n, times,  eta, &n2, times2,  eta2, &error);
  tmin = tstrt;
  tmax = tstrt + (nsamp - 1) * dt;

  if(iprint)
       {
       printf("STARTING NMO  dop=%f  mode=%d  nsamp=%d  n=%d %d\n",
                                  dop, mode, nsamp, n, n2);
       printf("tmin=%f  tmax=%f  dt=%f\n", tmin, tmax, dt);
       }

  ttt = (float*)calloc(1, (n2 + 1) * sizeof(float));
  vvv = (float*)calloc(1, (n2 + 1) * sizeof(float));
  eee = (float*)calloc(1, (n2 + 1) * sizeof(float));
  fff = (float*)calloc(1, (n2 + 1) * sizeof(float));
  ggg = (float*)calloc(1, (n2 + 1) * sizeof(float));
  tb  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  eta_nmo_prep(times2, vnmo2, eta2,
               &n2, &tmin, &tmax, &dt, &nnn, ttt, vvv, eee, fff, ggg,
               tb, &cnst, &error);
  if(error)
       {
       free(ttt); free(vvv); free(eee); free(fff); free(ggg);
       free(tb); return error;
       }
  ta  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  a   = (float*)calloc(1, nsamp * sizeof(float));
  b   = (float*)calloc(1, nsamp * sizeof(float));

  if(iprint)
       {
       oldbbbb = (unsigned char*)calloc(1, nsamp * sizeof(unsigned char));
       }

  for(i = 0; i < ntraces; i++)
       {
       offset = head[nwords * i + 5];
       index = nsamp * i;
       convert_byte_to_float(nsamp, &bbbb[index], a);
       if(iprint)
          {
          printf("TRACE NUMBER %d  offset=%f  index=%d\n", i+1, offset, index);
          for(j = 0; j < nsamp; j++) {  oldbbbb[j] = bbbb[index + j]; }
          }
       eta_do_nmo(&dop, &nnn, ttt, vvv, eee, fff, ggg, tb, &cnst,
                  &mode, &offset, ta, a, b, &nsamp, &bmute);
       return_float_to_byte(nsamp, b, &bbbb[index]);

       if(iprint)
          {
          printf("bmute = %f\n", bmute);
          printf("        time     vnmo       ttt        vvv      ta tb\n");
          for(j = 0; j < n2; j++)
               {
               printf("%4d %8.3f %7.0f %11.1f %11.8f %7.1f %7.1f\n",
                    j, times2[j], vnmo2[j], ttt[j], vvv[j], ta[j], tb[j]);
               }
          printf("    j   index+j   byte   float  (nmo)  float   byte\n");
          for(j = 0; j < nsamp; j+=200)
               {
               printf("%6d %6d %6d %8.2f         %8.2f %6d \n", j, index + j,
                     (int)oldbbbb[j], a[j], b[j], (int)bbbb[index + j]);
               }
          }
       }
  if(iprint)
       {
       if(oldbbbb) free(oldbbbb);
       }
  free(ttt); free(vvv); free(eee); free(fff); free(ggg);
  free(tb); free(ta); free(a); free(b); return 0;
}


/*---------- do eta nmo correction on float arrays --------------------------*/

long do_float_eta_nmo(
              long mode, float dop, long iprint,
              float times[], float vnmo[], float eta[], long n, float dx,
              float head[], long nwords, float ffff[],
              long nsamp, long ntraces, float tstrt, float dt)
{
  long index, error, nnn, i;
  float cnst, offset, bmute;
  float *ttt, *vvv, *eee, *fff, *ggg, *ta, *tb, *a, *b;
  void densify3(float *dx, long *nmax, long *n, float x[], float z[],
                        long *nn, float xx[], float zz[], long *error);
  float tmin, tmax, times2[NMAX], vnmo2[NMAX], eta2[NMAX];
  long nmax = NMAX, n2;
  float *oldffff=NULL;  long j;

  if(mode != FORWARD && mode != REVERSE)     return 1;
  if(dt <= 0.0 || ntraces < 1 || nwords < 6) return 1;
  if(dx <= 0.0 || nsamp < 1 || n < 2)        return 1;
  densify3(&dx, &nmax, &n, times, vnmo, &n2, times2, vnmo2, &error);
  densify3(&dx, &nmax, &n, times,  eta, &n2, times2,  eta2, &error);
  tmin = tstrt;
  tmax = tstrt + (nsamp - 1) * dt;

  if(iprint)
       {
       printf("STARTING NMO  dop=%f  mode=%d  nsamp=%d  n=%d %d\n",
                                  dop, mode, nsamp, n, n2);
       printf("tmin=%f  tmax=%f  dt=%f\n", tmin, tmax, dt);
       }

  ttt = (float*)calloc(1, (n2 + 1) * sizeof(float));
  vvv = (float*)calloc(1, (n2 + 1) * sizeof(float));
  eee = (float*)calloc(1, (n2 + 1) * sizeof(float));
  fff = (float*)calloc(1, (n2 + 1) * sizeof(float));
  ggg = (float*)calloc(1, (n2 + 1) * sizeof(float));
  tb  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  eta_nmo_prep(times2, vnmo2, eta2,
               &n2, &tmin, &tmax, &dt, &nnn, ttt, vvv, eee, fff, ggg,
               tb, &cnst, &error);
  if(error)
       {
       free(ttt); free(vvv); free(eee); free(fff); free(ggg);
       free(tb); return error;
       }
  ta  = (float*)calloc(1, (n2 + 1) * sizeof(float));
  a   = (float*)calloc(1, nsamp * sizeof(float));
  b   = (float*)calloc(1, nsamp * sizeof(float));

  if(iprint)
       {
       oldffff = (float*)calloc(1, nsamp * sizeof(float));
       }

  for(i = 0; i < ntraces; i++)
       {
       offset = head[nwords * i + 5];
       index = nsamp * i;
       memcpy(a, &ffff[index], nsamp * sizeof(float));
       if(iprint)
          {
          printf("TRACE NUMBER %d  offset=%f  index=%d\n", i+1, offset, index);
          for(j = 0; j < nsamp; j++) {  oldffff[j] = ffff[index + j]; }
          }
       eta_do_nmo(&dop, &nnn, ttt, vvv, eee, fff, ggg, tb, &cnst,
                  &mode, &offset, ta, a, b, &nsamp, &bmute);
       memcpy(&ffff[index], b, nsamp * sizeof(float));

       if(iprint)
          {
          printf("bmute = %f\n", bmute);
          printf("        time     vnmo       ttt        vvv      ta tb\n");
          for(j = 0; j < n2; j++)
               {
               printf("%4d %8.3f %7.0f %11.1f %11.8f %7.1f %7.1f\n",
                    j, times2[j], vnmo2[j], ttt[j], vvv[j], ta[j], tb[j]);
               }
          printf("    j   index+j   byte   float  (nmo)  float   byte\n");
          for(j = 0; j < nsamp; j+=200)
               {
               printf("%6d %6d %6d %8.2f         %8.2f %6d \n", j, index + j,
                     (int)oldffff[j], a[j], b[j], (int)ffff[index + j]);
               }
          }
       }
  if(iprint)
       {
       if(oldffff) free(oldffff);
       }
  free(ttt); free(vvv); free(eee); free(fff); free(ggg);
  free(tb); free(ta); free(a); free(b); return 0;
}


/*--------------------------- end --------------------------------------*/

