/***
!<CPS_v1 type="PRIMITIVE"/>
****/

/*--------------------- avocorr_crou.c -------------------------------*/
/*--------------------- avocorr_crou.c -------------------------------*/
/*--------------------- avocorr_crou.c -------------------------------*/
 
        /* other files are: avocorr.f90 avoSuite.h avoOVBCorr.h */
 

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
!                         C P S   P R I M I T I V E
!
! Name       : avocorr_crou
! Category   : velocity
! Written    : 2004-01-29   by: Michael Ried
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Routines for AVO correlation matching
! Portability: No known limitations.
! Parallel   : No. 
!
!-------------------------------------------------------------------------------
!</brief_doc>
!
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
!!!  AVO correlation matching routines written in C
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                           i        i         i        i       i
!           void avocorr_ms(avotype, pres_var, off_ang, nmo_in, imag_coeff, 
!
!             i           i       o        i     i     i         i
!             real_coeff, gathin, gathout, nsam, nens, max_nens, nhead,
!
!             i            i                i                i
!             idat_trctyp, idat_offset_ang, idat_start_time, idat_dom_freq,
!
!             i   i
!             si, headout)
!
! int    *avotype         = Type of correlation matching desired
!                           DeltaA, DeltaB, CorrA, and CorrB
! int    *pres_var        = True = preserve the original variance of A and B
! int    *off_ang         = Type of traces (Offset or Angle)
! int    *nmo_in          = Applied NMO to input traces? Yes or NO
! float  *imag_coeff      = Imaginary part of desired correlation coefficient
! float  *real_coeff      = Real part of desired correlation coefficient
! float  *gathin          = Input gather
! float  *gathout         = Output gather
! int    *nsam            = Number of samples
! int    *nens            = Number of input traces
! int    *max_nens        = Maximum # of traces
! int    *nhead           = Number of Header values
! int    *idat_trctyp     = Trace type header pointer
! int    *idat_offset_ang = AVO central angle header pointer
! int    *idat_start_time = Start time header pointer
! int    *idat_dom_freq   = Dominate frequency pointer
! float  *si              = Trace sample interval
! double *headout         = Output Headers
!
!
!                                  i    b    i  i   i    i    i      o
!           void avocorr_auxtraces(ens, aux, h, nh, is1, is2, noh2, ftrlen)
!
! float **ens             = ensemble traces
! struct AuxTraces *aux   = Auxiliary traces
! float *h                = The inversion filters
!                           The first subscript means:
!                           1: The h1(t) filter 
!                           2: The h2(t) filter 
! int nh                  = The actual size of the output inversion filters
! int is1                 = sample number
! int is2                 = sample number #2
! int noh2                =
! int ftrlen              = The transform filter length
!
!                               b          i
!           void avocorr_compt0(auxTraces, offset)
!
! Aux_traces *auxTraces   = Auxiliary traces
! float offset            = Offset
!
!                                  b    i     i
!           void avocorr_corrmatch(ens, info, ns)
!
! float **ens            = ensemble traces
! Match_info *info
! int ns                 = Number of samples
!
!                                      b    i     i    i       i
!           void avocorr_matchprestack(ens, info, aux, offset, trace)
!
! float **ens             = ensemble traces
! Match_info *info        =
! Aux_traces *aux         = Auxiliary traces
! float offset            = Offset
! int trace               = trace number
!                                    i        i          i
!           int avocorr_prestackscan(headers, itrc_type, nens)
!
! double **headers        = Ptr to list of pts to headers
! int itrc_type           = index of data trace type in header
! int nens                = Number of traces in ensemble
!
!                                    o      o        o        i   i
!           void avocorr_reapplymute(trace, tlive_s, tlive_e, si, ns)
!
! float *trace            = Trace values
! float tlive_s           = Start of a live trace
! float tlive_e           = End of a live trace
! float si                = Trace sample interval
! int   ns                = Number of samples
!
!                                  i     i   o    o
!           void avocorr_scantrace(data, ns, is1, is2)
!
! float *data            = Input data
! int ns                 = Number of samples
! int *is1               = Number of first live value
! int *is2               = Number of last live value
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  2. 2005-05-31  Stoeckley     Fix to compile with C++.
!  1. 2005-01-03  Michael Ried  Initial version.
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


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


char avocorr_crou[100] =
"$Id: avocorr_crou.c,v 1.2 2005/05/31 13:04:07 Stoeckley prod sps $";

/****
!!!
!!! Put your header files here.
!!!
*/
#include "named_constants.h"
#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define avocorr_ms avocorr_ms_
#define avocorr_auxtraces avocorr_auxtraces_
#define avocorr_compt0 avocorr_compt0_
#define avocorr_corrmatch avocorr_corrmatch_
#define avocorr_hilba avocorr_hilba_
#define avocorr_matchprestack avocorr_matchprestack_
#define avocorr_prestackscan avocorr_prestackscan_
#define avocorr_reapplymute avocorr_reapplymute_
#define avocorr_rikflt avocorr_rikflt_
#define avocorr_scantrace avocorr_scantrace_
#elif defined NEED_CAPITALS
#define avocorr_ms AVOCORR_MS
#define avocorr_auxtraces AVOCORR_AUXTRACES
#define avocorr_compt0 AVOCORR_COMPT0
#define avocorr_corrmatch AVOCORR_CORRMATCH
#define avocorr_hilba AVOCORR_HILBA
#define avocorr_matchprestack AVOCORR_MATCHPRESTACK
#define avocorr_prestackscan AVOCORR_PRESTACKSCAN
#define avocorr_reapplymute AVOCORR_REAPPLYMUTE
#define avocorr_rikflt AVOCORR_RIKFLT
#define avocorr_scantrace AVOCORR_SCANTRACE
#endif

/****
!!!
!!! Put program header files here.
!!!
*/
#include "avocorr.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/***********************************************************************/
/*
 *  This  tool adds a multiple of A into B or
 *  a multiple of B into A so that the new correlation between
 *  A and B is a specified number.
 *
 *  Author:    Herb Swan                 7-15-95
 *  Modified to run in Omega: 
 *             Gordon Grant             10-01-2001
 *  Modified to run in CPS: 
 *             Michael Ried             01-28-2004
 */

 
void avocorr_ms(int *avotype, int *pres_var, int *off_ang, int *nmo_in,
               float *imag_coeff, float *real_coeff,   
               float *gathin, float *gathout, int *nsam, int *nens,
               int *max_nens, int *nhead, int *idat_trctyp,   
               int *idat_offset_ang, int *idat_start_time, 
               int *idat_dom_freq, float *si,double *headout) {

    
    int prestack, i, j;
  
    float  real_part, imag_part,  offset;
    float  f0,   w0t, pctwns;
    float  tlive_s, tlive_e;
     
    float  pctcns;
    
    double  magrd2, pole;


   /* static char *nomem = "Cannot allocate enough memory.";  */    
    static double  **headers;     

    static float  **ensemble;     
    static int    nens_input = 0;      /* # ensembles input */
    static int    nens_valid = 0;      /* # valid ensembles */
    static int    ftrlen, nh, noh2;
    static int    code, is1, is2, ier;
    static float  *h, *corr, *work;
    static double *aux;
    static const  double PI2=6.2831853;
   
    static Match_type match_type;
    static Match_info match;

    static struct AuxTraces auxTraces;

    int i1;

/*
 *  First time through, INITIALIZE.
 */ 
    if  (nens_input == 0)  {

         /*     printf(" avotype %d pres_var %d\n",*avotype, *pres_var);
          *     printf(" real %f \n", *real_coeff);                
          *     printf(" avocorr  nens %d\n", *nens );  
          *     printf(" nhead %d\n", *nhead );  
          *     printf(" idat_trctyp %d\n", *idat_trctyp  );  
          *     printf(" idat_start_time %d\n", *idat_start_time  );  
          *     printf(" si %f \n", *si);
          */

                ensemble = (float **) malloc (*max_nens * sizeof(float *));
                headers = (double **) malloc (*max_nens * sizeof(double *));

  
                auxTraces.Sq    = (float *) malloc(*nsam * sizeof(float));
 
                auxTraces.Bstr  = (float *) malloc(*nsam * sizeof(float));
 
                auxTraces.Bqstr = (float *) malloc(*nsam * sizeof(float));
 
                auxTraces.T0    = (float *) malloc(*nsam * sizeof(float));   
 
                auxTraces.ns = *nsam;
                auxTraces.si = *si;  
                auxTraces.type = OFFSET;
                if (*off_ang == 2 ) auxTraces.type = ANGLE ;
         
 
/*
 *  Set correlation coefficient.
 */
                noh2 = 0;
                match_type = DeltaA;
                match_type = (Match_type) *avotype; 
                match.match_type = match_type;

                real_part = *real_coeff;
                imag_part = *imag_coeff;

                switch (match_type) {
                   case DeltaA:
                   case DeltaB:
                     real_part = 0.0;
                     imag_part = 0.0;
                   break;
                   case CorrA:
                   case CorrB: break;
                }

               if (match_type == DeltaA || match_type == CorrA) noh2 = 1;
/*
 *  Check the magnitude of the desired correlation coefficient.
 */
                match.real_coef = real_part;
                match.imag_coef = imag_part;
                magrd2 = (double) real_part * real_part +
                         (double) imag_part * imag_part;
                if (magrd2 >= 1.0)
                  printf ("Magnitude of desired correlation >= 1.0");
/*
 *  Determine whether to preserve the original variance.
 */
                match.preserve = *pres_var;

   
/*
 *  Set the Hilbert transform length, based on the sampling interval.
 */
                ftrlen = 31;
                if (*si < 4.5) ftrlen = 43;
                if (*si < 1.5) ftrlen = 63;
                if (*si < 0.5) ftrlen = 95;
/*
 *  Determine the length of the stretch filter.
 */
                nh = 30;
                if (*si < 2.5) nh = 60;
                if (*si < 1.5) nh = 100; 

                h    = (float *)  malloc((4*nh + 2)*sizeof(float));
                corr = (float *)  malloc((2*nh + 2)*sizeof(float));
                work = (float *)  malloc((1*nh + 1)*sizeof(float));
                aux  = (double *) malloc(4*nh*sizeof(double));

                    
    }

/*----------------------- End of Initialization ----------------------- */

/*
 *  BEGIN ENSEMBLE PROCESSING.
 */


        for (i=0; i<*nens; i++)
          ensemble[i] = &gathin[*nsam*i];

        for (i=0; i<*nens; i++)
          headers[i] = &headout[*nhead*i];

        nens_input++;

        for (i1=0; i1<*nens; i1++) {    
          code = headers[i1][*idat_trctyp - 1];
          f0 = headers[i1][*idat_dom_freq - 1]; 
          offset  = headers[i1][*idat_offset_ang - 1];    
          tlive_s = headers[i1][*idat_start_time - 1];
        }
  

            
/*
 *  Calculate the NMO stretch filters, if needed.
 */ 
             /*               PROMAX coding
                f0 = 0;
                if (f0ndx) f0 = headers[0][f0ndx];  */ 

                f0 = headers[0][*idat_dom_freq - 1]; 
                  
                if (f0 && noh2) {

                   if (nens_input==1)
                       printf("\n\n Using DOMINANT_FREQ in headers\n");
                

                   w0t  = 1e-3 * PI2 * f0 *  *si;
                   pctwns = 1.0;
                   pctcns = 0.0;
                   pole   = 0.0;
                   ier    = -1;

                   avocorr_rikflt(h, corr, work, aux, &nh, &nh, &w0t,
                     &pctwns, &pctcns, &pole, &ier); 
 
       
                   if (ier) printf
                     ("Unable to calculate NMO stretch filters.");  
    
    
                   noh2 = ier;  
               }

/*
 *  Calculate the stretched B traces, if necessary.
 */
               prestack = avocorr_prestackscan(headers, *idat_trctyp, *nens);
               if (prestack > 0) {
                 
                 auxTraces.Vrms = ensemble[prestack];
                 if (match_type == DeltaA || match_type == CorrA) {
                   avocorr_scantrace(ensemble[AVO_Br], *nsam, &is1, &is2);
                   avocorr_auxtraces(ensemble, &auxTraces, h, nh,
                     is1, is2, noh2, ftrlen);
                 }        
/*
 *  Correct the prestack data, if present.
 */
                 for (i=prestack; i<*nens; i++) {
                   code    = headers[i][*idat_trctyp - 1];
                   auxTraces.nmo = *nmo_in;
            
                   if (code == HVO_Live) {

                     offset  = headers[i][*idat_offset_ang - 1];    
                     tlive_s = headers[i][*idat_start_time - 1];
                     tlive_e = *nsam * (*si);
 
                     avocorr_scantrace(ensemble[i], *nsam, &is1, &is2);
                     avocorr_compt0(&auxTraces, offset);
                     avocorr_hilba(ensemble[i], auxTraces.Sq, nsam, &is1,
                       &is2, &ftrlen);
                     avocorr_matchprestack(ensemble, &match,
                       &auxTraces, offset, i);
 
                     avocorr_reapplymute(ensemble[i], tlive_s, tlive_e, 
                                *si, *nsam);  
 
                   }
                 } 
               }           
 
/*
 *  Now perform the poststack correlation matching.
 *  (This was the original function of a_avoOVBCorr.c.)
 */
            
        avocorr_corrmatch(ensemble, &match, *nsam);
        nens_valid++;

        for (i=0; i<*nens; i++) {
         
          for (j=0; j<*nsam; j++)
            gathout[*nsam*i+j] = ensemble[i][j];
        }
}

/***********************************************************************/
/*
 *  Compute certain auxiliary traces, which are sometimes necessary
 *  to correct prestack data in conformity with the AVO intercept and
 *  gradient.  More specifically, the responsibilities of this
 *  procedure are:
 *
 *  1)  Verify that "noh2" is zero.  If it is nonzero, some error has
 *      occurred in the calculation of the NMO stretch operator, "h2".
 *      In this case, simply zero out the "Bstr" and "Bqstr" arrays,
 *      and do nothing else.
 *
 *  2)  If "noh2" is zero, convolve the "h2" array with the "Br" trace
 *      between sample numbers "is1" and "is2".  The "h2" array is
 *      multiplexed within the "h" array, and the convolution is to
 *      be performed according to the following formula:
 *
 *                 n2
 *      Bstr[j] = SUM {h2[k] Br[j-k]}   for j=(is1,...,is2) ,
 *               k=-n1
 *
 *      where n1=max(j-is2, -nh), and n2=min(j-is1, nh).
 *
 *      Note that h2[k] = h[2*(nh+k) + 1].  The "h1" array, corresponding
 *      to even array indices of "h", is not used in this calculation.
 *
 *  3)  Set "Bqstr" to be the Hilbert transform of "Bstr", using a
 *      transform filter length of "ftrlen" samples.
 *
 *  4)  The "Bstr" and the "Bqstr" are then both scaled by -0.5 * D[j],
 *      where D[j] = 1 + 2*t(k)*V'[k]/V[k].  t(k) is the time
 *      in seconds of the k-th sample, and V[k] is the k-th velocity
 *      sample in (ft/s) or (m/s).  V'[k] is the derivative of the
 *      velocity at sample k.   
 */

void avocorr_auxtraces(float **ens, struct AuxTraces *aux,
    float *h, int nh, int is1, int is2, int noh2, int ftrlen) {

    int    j, ns, n1, n2, k;
    float  si;
    double value, time, veloc, vpr, dt;
/*
 *  Check "noh2".  Zero both arrays if noh2 != 0.
 */
    ns = aux->ns;
    si = aux->si;
    for (j=0; j<ns; j++) {
      aux->Bstr[j] = 0;
      aux->Bqstr[j] = 0;
    }
    if (noh2) return;
/*
 *  Perform the convolution.
 */
    for (j=is1; j<=is2; j++) {
      value = 0;
      n1    = j - is2;
      n2    = j - is1;
      if (n1 < -nh) n1 = -nh;
      if (n2 >  nh) n2 =  nh;

      for (k=n1; k<=n2; k++)
        value += h[2*(nh+k)+1] * ens[AVO_Br][j-k];
        aux->Bstr[j] = value;
      }
/*
 *  Form the Hilbert transform of Bstr.
 */
    avocorr_hilba(aux->Bstr, aux->Bqstr, &ns, &is1, &is2, &ftrlen);
/*
 *  Scale the data by D(t).
 */
    for (j=is1; j<=is2; j++) {
      time  = 1e-3 * j * si;
      veloc = aux->Vrms[j];
      if (veloc <= 0) continue;
      vpr   = 0;
      if (j > 0 && j < ns-1)
        vpr = -5e2 * (aux->Vrms[j+1] - aux->Vrms[j-1]) / si;
        dt    = 1 + 2*time*vpr/veloc;
        aux->Bstr[j]  *= dt;
        aux->Bqstr[j] *= dt;
      }
    }
/***********************************************************************/
/*
 *  This routine computes T0(t), the zero-offset time as
 *  a function of a moved-out time, t.  This operation is not
 *  necessary for NMO-corrected data.
 */

void avocorr_compt0(Aux_traces *auxTraces, float offset) {

/*
 *  Declare the local variables:
 */

    double t0, t, v2, targ, time, told;
    int    i, j, ns;
    float  si;

    if (auxTraces->nmo == 1) return;

/*
 *  Perform the time conversion:
 */

    i = j = 1;
    si = 1e-3 * auxTraces->si;
    targ  = si;
    auxTraces->T0[0] = 0;
    ns = auxTraces->ns;
    told = offset / auxTraces->Vrms[0];
    while (j < ns) {
        t0   = i * si;
        v2   = offset / auxTraces->Vrms[i];
        t    = sqrt(t0*t0 + v2*v2);
        while (targ <= t) {
          time = (i + (targ-told)/(t-told) - 1)*si;
          if (time < 0) time = 0;
            auxTraces->T0[j] = time;
            targ = si*j++;
            if (j >= ns) break;
          }
        told = t;
        i++;
    }
    }
/***********************************************************************/
/*
 *  This routine performs correlation matching for the AVO Seismic
 *  Workbench socket tool, avoOVBCorr.
 */

void avocorr_corrmatch(float **ens, Match_info *info, int ns) {

    int Ar, Br, Ai, Bi, Sa, Sb, Rr, Ri, Rf, i;
    double sa, sb, ar, br, ai, bi, rr, ri, k1, k2r, k2i;
    double magrd2, magr2, RadRat, sdp, rdif;
    float rdr, rdi;
     
/*
 *  The variables in this routine are reflect the transformation,
 *  B' <-- B + kA.  However, they could just as easily apply for
 *  the converse transformation, A' <-- A + kB.  In the latter,
 *  case, complex correlation coefficients must be conjugated.
 */

    rdr =  info->real_coef;
    rdi =  info->imag_coef;
   
   
    Ar = AVO_Ar;
    Br = AVO_Br;
    Ai = AVO_Ai;
    Bi = AVO_Bi;
    Sa = AVO_Sa;
    Sb = AVO_Sb;
    Rr = AVO_Rr;
    Ri = AVO_Ri;
    Rf = 1;

    if (info->match_type == DeltaA || info->match_type == CorrA) {
      Ar = AVO_Br;
      Br = AVO_Ar;
      Ai = AVO_Bi;
      Bi = AVO_Ai;
      Sa = AVO_Sb;
      Sb = AVO_Sa;
      Rf = -1;
    }

    rdif    = Rf * rdi;
    magrd2  = rdi*rdi + rdr*rdr;
/*
 *  Here is the main data loop:
 *  Skip samples having zero or negative standard deviations!
 */
  

    for (i=0; i<ns; i++) {
      sa = ens[Sa][i];
      sb = ens[Sb][i];
      if (sa <= 0 || sb <= 0) continue;
         
      ar = ens[Ar][i];
      br = ens[Br][i];
      ai = ens[Ai][i];
      bi = ens[Bi][i];
      rr = ens[Rr][i];
      ri = ens[Ri][i] * Rf;
/*
 *  Compute the radical ratio:
 */
      magr2  = rr*rr + ri*ri;
      RadRat = sqrt((1.0 - magrd2) / (1.0 - magr2));
/*
 *  Set the desired standard deviation of B':
 */
      sdp = sb;
      if (! info->preserve) sdp = sdp / RadRat;
/*
 *  Compute the complex coefficients for B' = k1 B  + k2 A .
 */
      k1  = sdp * RadRat / sb;
      k2r = sdp * ( rdr  - RadRat*rr) / sa;
      k2i = sdp * (-rdif + RadRat*ri) / sa;
/*
 *  Perform the transformation.
*/   
      ens[Br][i] = k1*br + k2r*ar - k2i*ai;
      ens[Bi][i] = k1*bi + k2r*ai + k2i*ar;  
      ens[Sb][i] = sdp;
      ens[Rr][i] = rdr;
      ens[Ri][i] = rdi;   

 
    }
     
  }
/***********************************************************************/
/*
 *  Do correlation matching on the prestack gathers.
 */

void avocorr_matchprestack(float **ens, Match_info *info,
       Aux_traces *aux, float offset, int trace) {
/*
 *  Declare the local variables:
 */
    Match_type  type;
    double      rdr, rdi, rdmag;
    int         i, j, ns, nmo;
    double      time, velocity, x, c11, c22, off2, tv2;
    double      cr, ci, rr, ri, sasb, rmag, rad, br, bi;
    double      vr, vi, o1r, o1i, o2r, o2i, si, frac, bs, bq;
    const       double RADIAN = 57.29578;
/*
 *  Get the things which never change.
 */
    type = info->match_type;
    rdr  = info->real_coef;
    rdi  = info->imag_coef;
    ns = aux->ns;
    si   = 1e-3 * aux->si;  /* SI in s */
    rdmag= 1 - rdr*rdr - rdi*rdi;
    nmo  = aux->nmo == 1;
/*
 *  Get the time & incidence x=sin**2 (theta).
 */
    x = 0;
    if (aux->type == ANGLE) {
      x = sin(offset/RADIAN);
      x = x * x;
    }

    for (i=0; i<ns; i++) {
      time  = i * si;                   /* Note: i = nonzero offset index */
      j     = i;                        /*       j = zero offset index    */
      frac  = 0;
      if (! nmo) {
        time = aux->T0[i];
        j    = time / si;
        frac = (time - j*si) / si;
      }
      velocity = aux->Vrms[j];
      if (aux->type == OFFSET) {
        x = 0;
        off2 = offset * offset;
        tv2  = time * velocity;
        tv2  = tv2 * tv2;
        if (off2 > 0 || tv2 > 0) 
          x = off2 / (off2 + tv2);
      }
/*
 *  Compute sasb, the ratio of Var{A} / Var{B}.
 */
      rr = ens[AVO_Rr][j];
      ri = ens[AVO_Ri][j];
      if (ens[AVO_Sa][j] <= 0 || ens[AVO_Sb][j] <= 0) {
        ens[trace][i] = 0;
        continue;
      }
      sasb = ((double) ens[AVO_Sa][j]) / ens[AVO_Sb][j];
      rmag = 1 - rr*rr - ri*ri;
      rad = sqrt(rmag / rdmag);
      c11 = c22 = 1;
/*
 *  Linearly interpolate the current complex sample.
 */
      vr  = ens[trace][i];
      vi  = aux->Sq[i];
      br  = ens[AVO_Br][j];
      bi  = ens[AVO_Bi][j];
      bs  = nmo ? aux->Bstr[j]  : 0;
      bq  = nmo ? aux->Bqstr[j] : 0;
      if (frac && j < ns - 1) {
        br += frac * (ens[AVO_Br][j+1] - br);
        bi += frac * (ens[AVO_Bi][j+1] - bi);
        if (nmo) {
          bs += frac * (aux->Bstr[j+1]  - bs);
          bq += frac * (aux->Bqstr[j+1] - bq);
        }
      }
/*
 *  Compute the correction on the "A" trace:
 */
      if (type == DeltaA || type == CorrA) {
        cr  =  (rdr*rad - rr) * sasb;
        ci  =  (rdi*rad - ri) * sasb;
        if (info->preserve) {
          c11 /= rad;
          cr  /= rad;
          ci  /= rad;
        }
/*
 *  Perform the correction on the prestack trace from "A":
 */
        o1r = c11;
        o2r = cr + (c22 - c11) * x;
        o2i = ci;

        vr *= o1r;
        vr += o2r * br;
        vr -= o2i * bi;
        vr -= cr*x*bs;
        vr += ci*x*bq;
      }
/*
 *  Compute the correction on the "B" trace:
 */
      else {
        cr  =  (rdr*rad - rr) / sasb;
        ci  = -(rdi*rad - ri) / sasb;
        if (info->preserve) {
          c22 /= rad;
          cr  /= rad;
          ci  /= rad;
        }
/*
 *  Perform the correction on the prestack trace from "B":
 */
        o1r = cr*x + c11;
        o1i = ci*x;
        o2r = (c22 - c11)*x;

        vr *= o1r;
        vr -= o1i * vi;
        vr += o2r * br;
      }
/*
 *  Store the corrected prestack trace.
 */
      ens[trace][i] = vr;
    }
  }

/***********************************************************************/
/*
 *
 *  Return Code:  The index in the ensemble of the prestack
 *    gather, or 0 if none.
 *
 *  Author:  Herb Swan
 */

 
int avocorr_prestackscan(double **headers, int itrc_type, int nens) {

    int code, sndx;
/*
 *  Search the ensemble for a velocity trace (denoting the start
 *  of the prestack gather.)
 */
     
    code = 0;
    for (sndx=AVO_NStd; sndx < nens; sndx ++) {
      code = headers[sndx][itrc_type - 1];
      if (code == HVO_Vs) break;
    }
    if (code == HVO_Vs) return(sndx);
    return(0);
    }

/***********************************************************************/
/*
 *  Reapply the mute pattern after the application of prestack
 *  overburden correction.
 */

 
void avocorr_reapplymute(float *trace, float tlive_s, float tlive_e,
                   float si,  int   ns)      {
 
    int    i;
    float first_live, last_live;

    first_live = tlive_s / si;
    last_live  = tlive_e / si;

    if (first_live < 0) first_live = 0;
    if (last_live >= ns) last_live = ns - 1;
 

    for (i=0; i<first_live; i++)   trace[i] = 0;
    for (i=last_live+1; i<ns; i++) trace[i] = 0;
    }

/***********************************************************************/
/*
 *  Scan a prestack trace for its first and last nonzero value.
 */

void avocorr_scantrace(float *data, int ns, int *is1, int *is2) {

    int i;
/*
 *  First, scan for the first live value:
 */

    for (i=0; i<ns; i++)
      if (data[i]) break;
    *is1 = i;
/*
 *  Scan for its last live value:
 */
    for (i=ns-1; i>=0; i--)
      if (data[i]) break;
    *is2 = i;
    }


/***********************************************************************/

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
