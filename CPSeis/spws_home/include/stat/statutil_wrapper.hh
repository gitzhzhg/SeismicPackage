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

//------------------------ statutil_wrapper.hh ------------------------------//
//------------------------ statutil_wrapper.hh ------------------------------//
//------------------------ statutil_wrapper.hh ------------------------------//

//              header file for the StatutilWrapper class
//                  not derived from any class
//                      subdirectory stat


#ifndef _STATUTIL_WRAPPER_HH_
#define _STATUTIL_WRAPPER_HH_

class StatutilWrapper
{

//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//

public:

  enum { ENDFLAG_N = 1,   // running average narrowed end range (graded ends).
         ENDFLAG_T = 2,   // running average truncated end range.
         ENDFLAG_E = 3,   // running average extended end range.
         ENDFLAG_S = 4    // running average shifted end range.
       };

private:

//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//

public:

  static float get1       (const double hd[], const float statics[],
                           int nhx, int nhy, int nhx2, int nhy2,
                           float x1, float y1, float xinc, float yinc,
                           int nx, int ny);

  static float get2       (const double hd[], const float statics[],
                           int nhx, int nhy, int nhx2, int nhy2,
                           float x1, float y1, float xinc, float yinc,
                           int nx, int ny);

  static float get3       (const double hd[], const float statics[],
                           int nhx, int nhy, int nhx2, int nhy2,
                           float x1, float y1, float xinc, float yinc,
                           int nx, int ny);

  static void grade       (int nx, int ny, float statics[],
                           int ixmin, int ixmax, int iymin, int iymax);

  static void integrate         (int nx, int ny, float statics[], int preserve);
  static void replaceNilx       (int nx, int ny, float statics[]);
  static void replaceNily       (int nx, int ny, float statics[]);
  static void replaceNilxOnly   (int nx, int ny, float statics[]);
  static void replaceNilyOnly   (int nx, int ny, float statics[]);
  static void replaceNearbyNils (int nx, int ny, float statics[],
                                 int ixdist, int iydist, int require);

  static void runav       (int nx, int ny, float statics[],
                           int nxsmooth, int nysmooth, int endflag,
                           float trim, int preserve, int wild);

  static void smooth      (int nx, int ny, float statics[],
                           int nxsmooth, int nysmooth, int endflag,
                           float trim, int preserve, int wild);

  static void reverse     (float *x1, float *y1, float *xinc, float *yinc,
                           int nx, int ny, float statics[]);

  static void build1 (int nx, int ny, float statics[],       int kounts[]);
  static void build3 (int nx, int ny, float statics[], const int kounts[]);

  static void build2 (const double hd[], float statval,
                      float statics[], int kounts[],
                      int nhx, int nhy, int nhx2, int nhy2,
                      float x1, float y1, float xinc, float yinc,
                      int nx, int ny);

  static void taper        (float taper, float tapr[], int ncorr);

  static void corr         (const float trace[], const float rtrace[],
                            int nwin, float corr[], int ncorr, float *denom);

  static void corrEnhanced (const float trace[], float const rtrace[],
                            int nwin, float corr[], int ncorr,
                            int normalize, int subtract,
                            const float tapr[], float *ccoef, float *denom);

  static void pick         (float corr[], int ncorr, float *shft, float *peak);

  static void pickEnhanced (float corr[], int ncorr,
                            int npick, float ccmin, float denom,
                            float *shft, float *ccoef);

  static void integrate1d    (float array[], int n, int preserve);
  static void interpolate1d  (float array[], int n);

  static float trimmedMean   (const float array[], int n, float trim);

  static void smooth1dNoNils (float array[], int n, int nrun,
                              int endflag, float trim);

  static void runav1dNoNils  (float array[], int n, int nrun,
                              int endflag, float trim);

  static void smooth1d       (float array[], int n, int nrun,
                              int endflag, float trim,
                              int preserve, int wild);

  static void runav1d        (float array[], int n, int nrun,
                              int endflag, float trim,
                              int preserve, int wild);

  static void scanStatics    (int n, const float array[],
                              float *statmin, float *statmax, int *numnils);


//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

};

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
