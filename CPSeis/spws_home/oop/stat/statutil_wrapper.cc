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

//------------------------ statutil_wrapper.cc ------------------------------//
//------------------------ statutil_wrapper.cc ------------------------------//
//------------------------ statutil_wrapper.cc ------------------------------//

//             implementation file for the StatutilWrapper class
//                     not derived from any class
//                          subdirectory stat


#include "stat/statutil_wrapper.hh"
#include "c2f_interface.h"
#include "named_constants.h"
#include "str.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//


#if NEED_UNDERSCORE
#define statutil_frou_get1               statutil_frou_get1_
#define statutil_frou_get2               statutil_frou_get2_
#define statutil_frou_get3               statutil_frou_get3_
#define statutil_frou_grade              statutil_frou_grade_
#define statutil_frou_integrate          statutil_frou_integrate_
#define statutil_frou_rep_nilx           statutil_frou_rep_nilx_
#define statutil_frou_rep_nily           statutil_frou_rep_nily_
#define statutil_frou_rep_nilx_only      statutil_frou_rep_nilx_only_
#define statutil_frou_rep_nily_only      statutil_frou_rep_nily_only_
#define statutil_frou_rep_nearby_nils    statutil_frou_rep_nearby_nils_
#define statutil_frou_runav              statutil_frou_runav_
#define statutil_frou_smooth             statutil_frou_smooth_
#define statutil_frou_reverse            statutil_frou_reverse_
#define statutil_frou_bld1               statutil_frou_bld1_
#define statutil_frou_bld2               statutil_frou_bld2_
#define statutil_frou_bld3               statutil_frou_bld3_
#define statutil_frou_taper              statutil_frou_taper_
#define statutil_frou_corr               statutil_frou_corr_
#define statutil_frou_corr_enhanced      statutil_frou_corr_enhanced_
#define statutil_frou_pick               statutil_frou_pick_
#define statutil_frou_pick_enhanced      statutil_frou_pick_enhanced_
#define statutil_frou_1d_integrate       statutil_frou_1d_integrate_
#define statutil_frou_1d_interpolate     statutil_frou_1d_interpolate_
#define statutil_frou_trimmed_mean       statutil_frou_trimmed_mean_
#define statutil_frou_1d_smooth_no_nils  statutil_frou_1d_smooth_no_nils_
#define statutil_frou_1d_runav_no_nils   statutil_frou_1d_runav_no_nils_
#define statutil_frou_1d_smooth          statutil_frou_1d_smooth_
#define statutil_frou_1d_runav           statutil_frou_1d_runav_
#define statutil_frou_scan_statics       statutil_frou_scan_statics_
#elif NEED_CAPITALS
#define statutil_frou_get1               STATUTIL_FROU_GET1
#define statutil_frou_get2               STATUTIL_FROU_GET2
#define statutil_frou_get3               STATUTIL_FROU_GET3
#define statutil_frou_grade              STATUTIL_FROU_GRADE
#define statutil_frou_integrate          STATUTIL_FROU_INTEGRATE
#define statutil_frou_rep_nilx           STATUTIL_FROU_REP_NILX
#define statutil_frou_rep_nily           STATUTIL_FROU_REP_NILY
#define statutil_frou_rep_nilx_only      STATUTIL_FROU_REP_NILX_ONLY
#define statutil_frou_rep_nily_only      STATUTIL_FROU_REP_NILY_ONLY
#define statutil_frou_rep_nearby_nils    STATUTIL_FROU_REP_NEARBY_NILS
#define statutil_frou_runav              STATUTIL_FROU_RUNAV
#define statutil_frou_smooth             STATUTIL_FROU_SMOOTH
#define statutil_frou_reverse            STATUTIL_FROU_REVERSE
#define statutil_frou_bld1               STATUTIL_FROU_BLD1
#define statutil_frou_bld2               STATUTIL_FROU_BLD2
#define statutil_frou_bld3               STATUTIL_FROU_BLD3
#define statutil_frou_taper              STATUTIL_FROU_TAPER
#define statutil_frou_corr               STATUTIL_FROU_CORR
#define statutil_frou_corr_enhanced      STATUTIL_FROU_CORR_ENHANCED
#define statutil_frou_pick               STATUTIL_FROU_PICK
#define statutil_frou_pick_enhanced      STATUTIL_FROU_PICK_ENHANCED
#define statutil_frou_1d_integrate       STATUTIL_FROU_1D_INTEGRATE
#define statutil_frou_1d_interpolate     STATUTIL_FROU_1D_INTERPOLATE
#define statutil_frou_trimmed_mean       STATUTIL_FROU_TRIMMED_MEAN
#define statutil_frou_1d_smooth_no_nils  STATUTIL_FROU_1D_SMOOTH_NO_NILS
#define statutil_frou_1d_runav_no_nils   STATUTIL_FROU_1D_RUNAV_NO_NILS
#define statutil_frou_1d_smooth          STATUTIL_FROU_1D_SMOOTH
#define statutil_frou_1d_runav           STATUTIL_FROU_1D_RUNAV
#define statutil_frou_scan_statics       STATUTIL_FROU_SCAN_STATICS
#endif


//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//

extern "C" {

float statutil_frou_get1     (const double hd[], const float statics[],
                              const int *nhx, const int *nhy,
                              const int *nhx2, const int *nhy2,
                              const float *x1, const float *y1,
                              const float *xinc, const float *yinc,
                              const int *nx, const int *ny);

float statutil_frou_get2     (const double hd[], const float statics[],
                              const int *nhx, const int *nhy,
                              const int *nhx2, const int *nhy2,
                              const float *x1, const float *y1,
                              const float *xinc, const float *yinc,
                              const int *nx, const int *ny);

float statutil_frou_get3     (const double hd[], const float statics[],
                              const int *nhx, const int *nhy,
                              const int *nhx2, const int *nhy2,
                              const float *x1, const float *y1,
                              const float *xinc, const float *yinc,
                              const int *nx, const int *ny);

void statutil_frou_grade     (const int *nx, const int *ny, float statics[],
                              const int *ixmin, const int *ixmax,
                              const int *iymin, const int *iymax);

void statutil_frou_integrate (const int *nx, const int *ny, float statics[],
                              const int *preserve);

void statutil_frou_rep_nilx  (const int *nx, const int *ny, float statics[]);
void statutil_frou_rep_nily  (const int *nx, const int *ny, float statics[]);

void statutil_frou_rep_nilx_only
                             (const int *nx, const int *ny, float statics[]);
void statutil_frou_rep_nily_only
                             (const int *nx, const int *ny, float statics[]);

void statutil_frou_rep_nearby_nils
                             (const int *nx, const int *ny, float statics[],
                              const int *ixdist, const int *iydist,
                              const int *require);

void statutil_frou_runav  (const int *nx, const int *ny, float statics[],
                           const int *nxsmooth, const int *nysmooth,
                           const int *endflag, const float *trim,
                           const int *preserve, const int *wild);

void statutil_frou_smooth (const int *nx, const int *ny, float statics[],
                           const int *nxsmooth, const int *nysmooth,
                           const int *endflag, const float *trim,
                           const int *preserve, const int *wild);

void statutil_frou_reverse (float *x1, float *y1, float *xinc, float *yinc,
                            const int *nx, const int *ny, float statics[]);

void statutil_frou_bld1 (const int *nx, const int *ny,
                         float statics[],       int kounts[]);
void statutil_frou_bld3 (const int *nx, const int *ny,
                         float statics[], const int kounts[]);

void statutil_frou_bld2 (const double hd[], float *statval,
                         float statics[], int kounts[],
                         const int *nhx, const int *nhy,
                         const int *nhx2, const int *nhy2,
                         const float *x1, const float *y1,
                         const float *xinc, const float *yinc,
                         const int *nx, const int *ny);

void statutil_frou_taper  (const float *taper, float tapr[], const int *ncorr);

void statutil_frou_corr          (const float trace[], const float rtrace[],
                                  const int *nwin, float corr[],
                                  const int *ncorr, float *denom);

void statutil_frou_corr_enhanced (const float trace[], const float rtrace[],
                                  const int *nwin, const float corr[],
                                  const int *ncorr,
                                  const int *normalize, const int *subtract,
                                  const float tapr[],
                                  float *ccoef, float *denom);

void statutil_frou_pick              (float corr[], const int *ncorr,
                                      float *shft, float *peak);

void statutil_frou_pick_enhanced     (float corr[], const int *ncorr,
                                      const int *npick,
                                      const float *ccmin, const float *denom,
                                      float *shft, float *ccoef);

void statutil_frou_1d_integrate      (float array[], const int *n,
                                      const int *preserve);

void statutil_frou_1d_interpolate    (float array[], const int *n);

float statutil_frou_trimmed_mean     (const float array[], const int *n,
                                      const float *trim);

void statutil_frou_1d_smooth_no_nils (float array[], const int *n,
                                      const int *nrun,
                                      const int *endflag, const float *trim);

void statutil_frou_1d_runav_no_nils  (float array[], const int *n,
                                      const int *nrun,
                                      const int *endflag, const float *trim);

void statutil_frou_1d_smooth         (float array[], const int *n,
                                      const int *nrun,
                                      const int *endflag, const float *trim,
                                      const int *preserve, const int *wild);

void statutil_frou_1d_runav          (float array[], const int *n,
                                      const int *nrun,
                                      const int *endflag, const float *trim,
                                      const int *preserve, const int *wild);

void statutil_frou_scan_statics (const int *n, const float array[],
                                 float *statmin, float *statmax, int *numnils);


}   // end extern "C"


//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//


float StatutilWrapper::get1 (const double hd[], const float statics[],
                             int nhx, int nhy, int nhx2, int nhy2,
                             float x1, float y1, float xinc, float yinc,
                             int nx, int ny)
{
  return statutil_frou_get1  (hd, statics,
                              &nhx, &nhy,
                              &nhx2, &nhy2,
                              &x1, &y1,
                              &xinc, &yinc,
                              &nx, &ny);
}



float StatutilWrapper::get2 (const double hd[], const float statics[],
                             int nhx, int nhy, int nhx2, int nhy2,
                             float x1, float y1, float xinc, float yinc,
                             int nx, int ny)
{
  return statutil_frou_get2  (hd, statics,
                              &nhx, &nhy,
                              &nhx2, &nhy2,
                              &x1, &y1,
                              &xinc, &yinc,
                              &nx, &ny);
}



float StatutilWrapper::get3 (const double hd[], const float statics[],
                             int nhx, int nhy, int nhx2, int nhy2,
                             float x1, float y1, float xinc, float yinc,
                             int nx, int ny)
{
  return statutil_frou_get3  (hd, statics, &nhx, &nhy, &nhx2, &nhy2,
                              &x1, &y1, &xinc, &yinc, &nx, &ny);
}



void StatutilWrapper::grade (int nx, int ny, float statics[],
                             int ixmin, int ixmax, int iymin, int iymax)
{
  statutil_frou_grade (&nx, &ny, statics, &ixmin, &ixmax, &iymin, &iymax);
}



void StatutilWrapper::integrate (int nx, int ny, float statics[], int preserve)
{
  statutil_frou_integrate (&nx, &ny, statics, &preserve);
}



void StatutilWrapper::replaceNilx (int nx, int ny, float statics[])
{
  statutil_frou_rep_nilx  (&nx, &ny, statics);
}



void StatutilWrapper::replaceNily (int nx, int ny, float statics[])
{
  statutil_frou_rep_nily  (&nx, &ny, statics);
}



void StatutilWrapper::replaceNilxOnly (int nx, int ny, float statics[])
{
  statutil_frou_rep_nilx_only  (&nx, &ny, statics);
}



void StatutilWrapper::replaceNilyOnly (int nx, int ny, float statics[])
{
  statutil_frou_rep_nily_only  (&nx, &ny, statics);
}



void StatutilWrapper::replaceNearbyNils (int nx, int ny, float statics[],
                                         int ixdist, int iydist, int require)
{
  statutil_frou_rep_nearby_nils(&nx, &ny, statics, &ixdist, &iydist, &require);
}



void StatutilWrapper::runav  (int nx, int ny, float statics[],
                              int nxsmooth, int nysmooth, int endflag,
                              float trim, int preserve, int wild)
{
  statutil_frou_runav  (&nx, &ny, statics, &nxsmooth, &nysmooth,
                        &endflag, &trim, &preserve, &wild);
}



void StatutilWrapper::smooth (int nx, int ny, float statics[],
                              int nxsmooth, int nysmooth, int endflag,
                              float trim, int preserve, int wild)
{
  statutil_frou_smooth (&nx, &ny, statics, &nxsmooth, &nysmooth,
                        &endflag, &trim, &preserve, &wild);
}



void StatutilWrapper::reverse (float *x1, float *y1, float *xinc, float *yinc,
                               int nx, int ny, float statics[])
{
  statutil_frou_reverse (x1, y1, xinc, yinc, &nx, &ny, statics);
}



void StatutilWrapper::build1 (int nx, int ny, float statics[],
                              int kounts[])
{
  statutil_frou_bld1 (&nx, &ny, statics, kounts);
}



void StatutilWrapper::build3 (int nx, int ny, float statics[],
                              const int kounts[])
{
  statutil_frou_bld3 (&nx, &ny, statics, kounts);
}



void StatutilWrapper::build2 (const double hd[], float statval,
                              float statics[], int kounts[],
                              int nhx, int nhy, int nhx2, int nhy2,
                              float x1, float y1, float xinc, float yinc,
                              int nx, int ny)
{
  statutil_frou_bld2 (hd, &statval, statics, kounts,
                      &nhx, &nhy, &nhx2, &nhy2,
                      &x1, &y1, &xinc, &yinc, &nx, &ny);
}



void StatutilWrapper::taper  (float taper, float tapr[], int ncorr)
{
  statutil_frou_taper  (&taper, tapr, &ncorr);
}



void StatutilWrapper::corr (const float trace[], const float rtrace[],
                            int nwin, float corr[], int ncorr, float *denom)
{
  statutil_frou_corr (trace, rtrace, &nwin, corr, &ncorr, denom);
}



void StatutilWrapper::corrEnhanced (const float trace[], float const rtrace[],
                                    int nwin, float corr[], int ncorr,
                                    int normalize, int subtract,
                                    const float tapr[],
                                    float *ccoef, float *denom)
{
  statutil_frou_corr_enhanced (trace, rtrace, &nwin, corr, &ncorr,
                               &normalize, &subtract, tapr, ccoef, denom);
}



void StatutilWrapper::pick (float corr[], int ncorr, float *shft, float *peak)
{
  statutil_frou_pick  (corr, &ncorr, shft, peak);
}



void StatutilWrapper::pickEnhanced (float corr[], int ncorr,
                                    int npick, float ccmin, float denom,
                                    float *shft, float *ccoef)
{
  statutil_frou_pick_enhanced
                         (corr, &ncorr, &npick, &ccmin, &denom, shft, ccoef);
}



void StatutilWrapper::integrate1d (float array[], int n, int preserve)
{
  statutil_frou_1d_integrate (array, &n, &preserve);
}



void StatutilWrapper::interpolate1d  (float array[], int n)
{
  statutil_frou_1d_interpolate  (array, &n);
}



float StatutilWrapper::trimmedMean  (const float array[], int n, float trim)
{
  return statutil_frou_trimmed_mean  (array, &n, &trim);
}



void StatutilWrapper::smooth1dNoNils (float array[], int n, int nrun,
                                      int endflag, float trim)
{
  statutil_frou_1d_smooth_no_nils (array, &n, &nrun, &endflag, &trim);
}



void StatutilWrapper::runav1dNoNils  (float array[], int n, int nrun,
                                      int endflag, float trim)
{
  statutil_frou_1d_runav_no_nils  (array, &n, &nrun, &endflag, &trim);
}



void StatutilWrapper::smooth1d (float array[], int n, int nrun,
                                int endflag, float trim,
                                int preserve, int wild)
{
  statutil_frou_1d_smooth (array, &n, &nrun, &endflag, &trim, &preserve, &wild);
}



void StatutilWrapper::runav1d  (float array[], int n, int nrun,
                                int endflag, float trim,
                                int preserve, int wild)
{
  statutil_frou_1d_runav (array, &n, &nrun, &endflag, &trim, &preserve, &wild);
}



void StatutilWrapper::scanStatics (int n, const float array[],
                                   float *statmin, float *statmax,
                                   int *numnils)
{
  statutil_frou_scan_statics (&n, array, statmin, statmax, numnils);
}



//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

