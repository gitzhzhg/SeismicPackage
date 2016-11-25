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

//-------------------------- vf_edit_resample.cc ----------------------//
//-------------------------- vf_edit_resample.cc ----------------------//
//-------------------------- vf_edit_resample.cc ----------------------//

//         implementation file for the VfEditResample class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_resample.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_function.hh"
#include "vf/vf_constants.hh"
#include "trslib.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//---------------- documentation for terpflag ---------------------------//
//---------------- documentation for terpflag ---------------------------//
//---------------- documentation for terpflag ---------------------------//


// _terpflag == TERPFLAG_LINEAR:
//      does linear interpolation with flat extrapolation.
//      has no effect if _stepflag == STEPFLAG_RETAIN.

// _terpflag == TERPFLAG_LINEAR2:
//      does linear interpolation with sloping extrapolation.
//      has no effect if _stepflag == STEPFLAG_RETAIN.

// _terpflag == TERPFLAG_CUBIC:
//      does four-point cubic interpolation.
//      has no effect if _stepflag == STEPFLAG_RETAIN.

// _terpflag == TERPFLAG_SPLINE:
//      does relaxed spline interpolation.
//      needs _dv.
//      has no effect if _dv == 0 and _stepflag == STEPFLAG_RETAIN.

// _terpflag == TERPFLAG_POLY:
//      does least squares polynomial fit.
//      needs _ncoef.
//      has no effect if _ncoef == n and _stepflag == STEPFLAG_RETAIN.
//      returns an average if _ncoef == 1  (nn might as well be 1).

// _terpflag == TERPFLAG_RUN:
//      first does linear interpolation with flat extrapolation.
//      then does running average smoothing.
//      needs _nrun and _endflag.
//      there is no linear interpolation if _stepflag == STEPFLAG_RETAIN.
//      there is no running average if _nrun == 1.

// _terpflag == TERPFLAG_RUN2:
//      first does linear interpolation with sloping extrapolation.
//      then does running average smoothing.
//      needs _nrun and _endflag.
//      there is no linear interpolation if _stepflag == STEPFLAG_RETAIN.
//      there is no running average if _nrun == 1.

// _terpflag == TERPFLAG_EARTH:
//      fills in points without changing earth model.
//      this means that a new point with abscissa lying between
//        x[i] and x[i+1] will have an ordinate equal to v[i+1].
//      has no effect if _stepflag == STEPFLAG_RETAIN.
//      it is illegal to use _stepflag == STEPFLAG_ARRAY.

// _terpflag == TERPFLAG_TOP:
//      resets the top velocity function pick.
//      does not use _stepflag.
//      resets the top ordinate.
//      if ordinate is velocity (..##): uses _top_velocity to reset ordinate.
//      if ordinate is depth    (VTDP): illegal.


// _terpflag == TERPFLAG_BOTTOM:
//      resets the bottom velocity function pick.
//      does not use _stepflag.
//      resets the bottom abscissa if _reset_timedepth is TRUE.
//      resets the bottom ordinate if _reset_velocity  is TRUE.
//      either or both of _reset_timedepth and _reset_velocity must be TRUE.
//      if abscissa is depth    (VZ..): uses _bottom_depth    to reset abscissa.
//      if abscissa is time     (VT..): uses _bottom_time     to reset abscissa.
//      if ordinate is velocity (..##): uses _bottom_velocity to reset ordinate.
//      if ordinate is depth    (VTDP): uses _bottom_depth    to reset ordinate.


//---------------- documentation for stepflag ---------------------------//
//---------------- documentation for stepflag ---------------------------//
//---------------- documentation for stepflag ---------------------------//


// _stepflag == STEPFLAG_DELTA:
//      if abscissa is depth:
//        uses _dx1 and _dx2 to set the increment for the new abscissae.
//      if abscissa is time:
//        uses _dt1 and _dt2 to set the increment for the new abscissae.
//      resampling starts at x[0] and finishes at x[n-1].

// _stepflag == STEPFLAG_NUMBER:
//      uses _nwant to set the increment for the new abscissae.
//      resampling starts at x[0] and finishes at x[n-1].

// _stepflag == STEPFLAG_RETAIN:
//      retains the current abscissae in array x[n].

// _stepflag == STEPFLAG_ARRAY:
//      uses the abscissae already in array xx[nn] (nn and xx are preset).
//      it is illegal to use _terpflag == TERPFLAG_EARTH.



//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//


VfEditResample::VfEditResample(VfUtilities *utilities)
           : VfEditBase(INFORM_TOTAL, "resample vertically"),
             _utilities           (utilities),
             _type                (VTNM),
             _terpflag            (TERPFLAG_LINEAR),
             _stepflag            (STEPFLAG_DELTA),
             _endflag             (ENDFLAG_NARROWED),
             _reset_timedepth     (TRUE),
             _reset_velocity      (FALSE),
             _bottom_time         (5.0),
             _bottom_depth        (99999.0),
             _bottom_velocity     (20000.0),
             _top_velocity        (5000.0),
             _nwant               (10),
             _ncoef               (2),
             _nrun                (3),
             _dx1                 (2000.0),
             _dx2                 (2000.0),
             _dt1                 (0.5),
             _dt2                 (0.5),
             _dv                  (0.0)
{
  assert(_utilities);
}



VfEditResample::~VfEditResample()
{
}



//------------------ verify valid parameter --------------------//
//------------------ verify valid parameter --------------------//
//------------------ verify valid parameter --------------------//

         // private static functions.

void VfEditResample::verifyValidType     (int type)
{
  assert(type == VTNM ||
         type == VTRM ||
         type == VTAV ||
         type == VTIN ||
         type == VTDP ||
         type == VZRM ||
         type == VZAV ||
         type == VZIN);
}


void VfEditResample::verifyValidTerpflag (int terpflag)
{
  assert(terpflag == TERPFLAG_LINEAR  ||
         terpflag == TERPFLAG_LINEAR2 ||
         terpflag == TERPFLAG_CUBIC   ||
         terpflag == TERPFLAG_SPLINE  ||
         terpflag == TERPFLAG_POLY    ||
         terpflag == TERPFLAG_RUN     ||
         terpflag == TERPFLAG_RUN2    ||
         terpflag == TERPFLAG_EARTH   ||
         terpflag == TERPFLAG_TOP     ||
         terpflag == TERPFLAG_BOTTOM);
}


void VfEditResample::verifyValidStepflag (int stepflag)
{
  assert(stepflag == STEPFLAG_DELTA  ||
         stepflag == STEPFLAG_NUMBER ||
         stepflag == STEPFLAG_RETAIN ||
         stepflag == STEPFLAG_ARRAY);
}


void VfEditResample::verifyValidEndflag  (int endflag)
{
  assert(endflag == ENDFLAG_TRUNCATED ||
         endflag == ENDFLAG_SHIFTED   ||
         endflag == ENDFLAG_EXTENDED  ||
         endflag == ENDFLAG_NARROWED);
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditResample::setResampleType   (int   value)
{
  verifyValidType(value);
  if(!allowType(value)) return;
  _type = value;
}


void   VfEditResample::setTerpflag       (int   value)
{
  verifyValidTerpflag(value);
  if(!allowTerpflag(value)) return;
  _terpflag = value;
}


void   VfEditResample::setStepflag       (int   value)
{
  verifyValidStepflag(value);
  if(!allowStepflag(value)) return;
  _stepflag = value;
}


void   VfEditResample::setEndflag        (int   value)
{
  verifyValidEndflag(value);
  _endflag = value;
}


void   VfEditResample::setResetTimedepth (int   value)
{
  _reset_timedepth = value;
  if(_reset_timedepth == FALSE && _reset_velocity == FALSE)
                   _reset_velocity = TRUE;
}


void   VfEditResample::setResetVelocity  (int   value)
{
  _reset_velocity = value;
  if(_reset_timedepth == FALSE && _reset_velocity == FALSE)
                   _reset_timedepth = TRUE;
}


void   VfEditResample::setBottomTime     (float value)
{
  float timetol = _utilities->getTimeTolerance();
  _bottom_time  = MaximumValue(value, 2.0 * timetol);
}


void   VfEditResample::setBottomDepth    (float value)
{
  float depthtol = _utilities->getDepthTolerance();
  _bottom_depth  = MaximumValue(value, 2.0 * depthtol);
}


void   VfEditResample::setBottomVelocity (float value)
{
  _bottom_velocity = MaximumValue(value, 1.0);
}


void   VfEditResample::setTopVelocity (float value)
{
  _top_velocity = MaximumValue(value, 1.0);
}


void   VfEditResample::setNwant          (long  value)
{
  _nwant = ConstrainValue(value, 1, MAXPICKS);
}


void   VfEditResample::setNcoef          (long  value)
{
  _ncoef = ConstrainValue(value, 1, 10);
}


void   VfEditResample::setNrun           (long  value)
{
  _nrun = MaximumValue(value, 1);
}


void   VfEditResample::setDx1            (float value)
{
  if(value <= 0.0) return;
  _dx1 = value;
}


void   VfEditResample::setDx2            (float value)
{
  if(value <= 0.0) return;
  _dx2 = value;
}


void   VfEditResample::setDt1            (float value)
{
  if(value <= 0.0) return;
  _dt1 = value;
}


void   VfEditResample::setDt2            (float value)
{
  if(value <= 0.0) return;
  _dt2 = value;
}


void   VfEditResample::setDv             (float value)
{
  _dv = MaximumValue(value, 0.0);
}



//-------------------- check whether parameter is allowed -------------//
//-------------------- check whether parameter is allowed -------------//
//-------------------- check whether parameter is allowed -------------//

     // returns FALSE if the specified parameter value is illegal
     // according to the current values of other parameters.


int   VfEditResample::allowType          (int value)  const
{
  verifyValidType(value);
  if(_terpflag == TERPFLAG_EARTH)
      {
      if(value != VTIN && value != VZIN) return FALSE;
      }
  return TRUE;
}


int   VfEditResample::allowTerpflag      (int value)  const
{
  verifyValidTerpflag(value);
  if(value == TERPFLAG_EARTH && _stepflag == STEPFLAG_ARRAY) return FALSE;
  if(value == TERPFLAG_EARTH)
      {
      if(_type != VTIN && _type != VZIN) return FALSE;
      }
  if(value == TERPFLAG_TOP)
      {
      if(_type == VTDP) return FALSE;
      }
  if(value == TERPFLAG_BOTTOM)
      {
      if(_type == VTDP && !_reset_timedepth) return FALSE;
      }
  if(value == TERPFLAG_LINEAR || value == TERPFLAG_CUBIC)
      {
      if(_stepflag == STEPFLAG_RETAIN) return FALSE;
      }
  return TRUE;
}


int   VfEditResample::allowStepflag      (int value)  const
{
  verifyValidStepflag(value);
  if(value == STEPFLAG_ARRAY && _terpflag == TERPFLAG_EARTH) return FALSE;
  if(value == STEPFLAG_RETAIN)
      {
      if(_terpflag == TERPFLAG_LINEAR ||
         _terpflag == TERPFLAG_CUBIC) return FALSE;
      }
  return TRUE;
}



//-------------------- check whether parameter is needed --------------//
//-------------------- check whether parameter is needed --------------//
//-------------------- check whether parameter is needed --------------//

     // returns FALSE if the parameter will not be used
     // according to the current values of other parameters.


int   VfEditResample::needStepflag        ()  const
{
  if(_terpflag == TERPFLAG_TOP)    return FALSE;
  if(_terpflag == TERPFLAG_BOTTOM) return FALSE;
  return TRUE;
}


int   VfEditResample::needEndflag        ()  const
{
  if(_terpflag != TERPFLAG_RUN &&
     _terpflag != TERPFLAG_RUN2) return FALSE;
  return TRUE;
}


int   VfEditResample::needResetTimedepth ()  const
{
  if(_terpflag != TERPFLAG_BOTTOM)       return FALSE;
  if(_utilities->abscissaIsTime (_type)) return TRUE;
  if(_utilities->abscissaIsDepth(_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needResetTime ()  const
{
  if(_terpflag != TERPFLAG_BOTTOM)       return FALSE;
  if(_utilities->abscissaIsTime (_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needResetDepth ()  const
{
  if(_terpflag != TERPFLAG_BOTTOM)       return FALSE;
  if(_utilities->abscissaIsDepth(_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needResetVelocity  ()  const
{
  if(_terpflag != TERPFLAG_BOTTOM)          return FALSE;
  if(_utilities->ordinateIsVelocity(_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needBottomTime     ()  const
{
  if(_terpflag != TERPFLAG_BOTTOM)      return FALSE;
  if(_reset_timedepth == FALSE)         return FALSE;
  if(_utilities->abscissaIsTime(_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needBottomDepth    ()  const
{
  if(_terpflag != TERPFLAG_BOTTOM)       return FALSE;
  if(_reset_timedepth == FALSE)          return FALSE;
  if(_utilities->abscissaIsDepth(_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needBottomVelocity ()  const
{
  if(_terpflag != TERPFLAG_BOTTOM)          return FALSE;
  if(_reset_velocity == FALSE)              return FALSE;
  if(_utilities->ordinateIsVelocity(_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needTopVelocity ()  const
{
  if(_terpflag != TERPFLAG_TOP)             return FALSE;
  if(_utilities->ordinateIsVelocity(_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needNwant          ()  const
{
  if(_stepflag != STEPFLAG_NUMBER) return FALSE;
  if(_terpflag == TERPFLAG_BOTTOM) return FALSE;
  if(_terpflag == TERPFLAG_TOP)    return FALSE;
  return TRUE;
}


int   VfEditResample::needNcoef          ()  const
{
  if(_terpflag != TERPFLAG_POLY) return FALSE;
  return TRUE;
}


int   VfEditResample::needNrun           ()  const
{
  if(_terpflag != TERPFLAG_RUN &&
     _terpflag != TERPFLAG_RUN2) return FALSE;
  return TRUE;
}


int   VfEditResample::needDx             ()  const
{
  if(_stepflag != STEPFLAG_DELTA)        return FALSE;
  if(_terpflag == TERPFLAG_BOTTOM)       return FALSE;
  if(_terpflag == TERPFLAG_TOP)          return FALSE;
  if(_utilities->abscissaIsDepth(_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needDt             ()  const
{
  if(_stepflag != STEPFLAG_DELTA)       return FALSE;
  if(_terpflag == TERPFLAG_BOTTOM)      return FALSE;
  if(_utilities->abscissaIsTime(_type)) return TRUE;
  return FALSE;
}


int   VfEditResample::needDv             ()  const
{
  if(_terpflag != TERPFLAG_SPLINE) return FALSE;
  return TRUE;
}



//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditResample::virtualCheck (VfKernal* /*kernal*/, char *msg)
{
  strcpy(msg, "resampling velocity functions vertically...");
  return FALSE;
}



//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditResample::virtualEdit (VfKernal *kernal, char *msg)
{
  long nfun = kernal->numVelocityFunctions();
  float last_time  = 0.0;
  float last_depth = 0.0;
  long ifun;
  for(ifun = 0; ifun < nfun; ifun++)
      {
      if(reject(kernal, ifun, NULL)) continue;
      long npicks = kernal->velfun(ifun)->numPicks();
      if(npicks == 0) continue;
      float last_time2  = kernal->velfun(ifun)->getTime (npicks - 1);
      float last_depth2 = kernal->velfun(ifun)->getDepth(npicks - 1);
      last_time  = MaximumValue(last_time , last_time2 );
      last_depth = MaximumValue(last_depth, last_depth2);
      }
  long kount  = 0;
  long kounte = 0;
  for(ifun = 0; ifun < nfun; ifun++)
      {
      if(reject(kernal, ifun, "resampling function")) continue;
      kount++;
      int ierr = resampleOneFunction
                       (kernal->velfun(ifun), last_time, last_depth);
      if(ierr) kounte++;
      }
  sprintf(msg, "resampled %d functions with %d errors", kount, kounte);
  return (kounte > 0);
}



//---------------------- resample one function ------------------------//
//---------------------- resample one function ------------------------//
//---------------------- resample one function ------------------------//

     // private.
     // last_time and last_depth should be the largest values
     //   in the entire velocity file, or can be zero.
     // returns error = TRUE or FALSE.

int VfEditResample::resampleOneFunction (VfFunction *velfun,
                             float last_time, float last_depth)
{
  long n = velfun->numPicks();

  if(n == 0) return TRUE;

  long nn;
  float  x[MAXPICKS];
  float  v[MAXPICKS];
  float xx[MAXPICKS];
  float vv[MAXPICKS];
  velfun->getAbscissaArray(x, _type);
  velfun->getOrdinateArray(v, _type);

  if(_terpflag == TERPFLAG_TOP)
      {
      resetTopPick (n, x, v, &nn, xx, vv);
      }
  else if(_terpflag == TERPFLAG_BOTTOM)
      {
      resetBottomPick (n, x, v, &nn, xx, vv);
      }
  else
      {
      int ierr = resampleHelper (last_time, last_depth,
                                      n, x, v, &nn, xx, vv);
      if(ierr) return TRUE;
      }
  velfun->resetNumPicks(nn, xx, vv, _type);
  if(velfun->getErrorFlag() != ERROR_NONE)  // might be error from updatePicks.
      {
      return TRUE;
      }
  return FALSE;
}



//------------------- reset bottom pick -------------------------------//
//------------------- reset bottom pick -------------------------------//
//------------------- reset bottom pick -------------------------------//

      // private.
      // for _terpflag == TERPFLAG_BOTTOM.
      // uses _reset_timedepth, _reset_velocity,
      //   _bottom_time, _bottom_depth, _bottom_velocity,
      //   _type, MAXPICKS, timetol, depthtol.
      // x, v, xx, and vv are dimensioned MAXPICKS.
      // calls fortran subroutine terp1.

void VfEditResample::resetBottomPick
               (long n, float *x, float *v,            // input
                long *nn, float *xx, float *vv)        // output
{
  *nn = n;
  if(n <= 0) return;
  memcpy(xx, x, (unsigned int)n * sizeof(float));
  memcpy(vv, v, (unsigned int)n * sizeof(float));
  float timetol  = _utilities->getTimeTolerance();
  float depthtol = _utilities->getDepthTolerance();
  int reset_ordinate;
  float bottom_abscissa, bottom_ordinate;
  float abscissa_tolerance;

  if(_utilities->abscissaIsTime(_type))
      {
      abscissa_tolerance = timetol;
      bottom_abscissa    = _bottom_time;
      }
  else
      {
      abscissa_tolerance = depthtol;
      bottom_abscissa    = _bottom_depth;
      }

  if(_utilities->ordinateIsDepth(_type))
      {
      reset_ordinate  = _reset_timedepth;
      bottom_ordinate = _bottom_depth;
      }
  else
      {
      reset_ordinate  = _reset_velocity;
      bottom_ordinate = _bottom_velocity;
      }

  if(_reset_timedepth)
      {
      if(xx[*nn-1] < bottom_abscissa - abscissa_tolerance)
          {
          if(*nn < MAXPICKS)
              {
              (*nn)++;
              if(!reset_ordinate) vv[*nn-1] = vv[*nn-2];
              }
          }
      else if(xx[*nn-1] > bottom_abscissa + abscissa_tolerance)
          {
          for(long i = n-1; i >= 0; i--)
              {
              if(xx[i] > bottom_abscissa - abscissa_tolerance) *nn = i+1;
              }
          if(!reset_ordinate)
              {
              int n2 = (int)n;
              vv[*nn-1] = terp1(&bottom_abscissa, x, &n2, v);
              }
          }
      xx[*nn-1] = bottom_abscissa;
      }

  if(reset_ordinate)
      {
      vv[*nn-1] = bottom_ordinate;
      }
}



//------------------- reset top pick -------------------------------//
//------------------- reset top pick -------------------------------//
//------------------- reset top pick -------------------------------//

      // private.
      // for _terpflag == TERPFLAG_TOP.
      // uses _top_velocity and _type.
      // x, v, xx, and vv are dimensioned MAXPICKS.

void VfEditResample::resetTopPick
               (long n, float *x, float *v,            // input
                long *nn, float *xx, float *vv)        // output
{
  *nn = n;
  if(n <= 0) return;
  memcpy(xx, x, (unsigned int)n * sizeof(float));
  memcpy(vv, v, (unsigned int)n * sizeof(float));
  if(_utilities->ordinateIsVelocity(_type)) vv[0] = _top_velocity;
}



//---------------------- resample  helper ----------------------------//
//---------------------- resample  helper ----------------------------//
//---------------------- resample  helper ----------------------------//

      // private.
      // x, v, xx, and vv are dimensioned MAXPICKS.
      // calls fortran subroutine densify_nonlin_c.
      // returns error == TRUE or FALSE.
      // returns message.

int VfEditResample::resampleHelper
               (float last_time, float last_depth,            // input
                long n, float *x, float *v,                   // input
                long *nn, float *xx, float *vv)               // output
{
  int ierr;
  float delta1, delta2, deltav, lastx;

  if(_utilities->abscissaIsDepth(_type))
      {
      delta1 = _dx1;
      delta2 = _dx2;
      lastx  = last_depth;
      }
  else
      {
      delta1 = _dt1;
      delta2 = _dt2;
      lastx  = last_time;
      }

  if(_utilities->ordinateIsVelocity(_type)) deltav = _dv;
  else                                      deltav = 0.0;

  if(x[n-1] < lastx)
      {
      long keep = n;
      if(n < MAXPICKS) n++;
      x[n-1] = lastx;
      v[n-1] = v[keep-1];
      }

  float *q = new float [12 * MAXPICKS];
  int maxpicks = MAXPICKS;
  int n2       = (int)n;
  int ncoef2   = (int)_ncoef;
  int nwant2   = (int)_nwant;
  int nrun2    = (int)_nrun;
  int nn2;
  char msg[222];
  densify_nonlin_c
            (&_terpflag, &_stepflag, &_endflag,       // input
             &delta1, &delta2, &nwant2,               // input
             &deltav,                                 // input
             &ncoef2, &nrun2,                         // input
             &n2, x, v,                               // input
             &nn2, xx, vv,                            // output
             &maxpicks,                               // input
              q,                                      // scratch space
              msg, &ierr);                            // output
  *nn = nn2;
  delete [] q;
  return ierr;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

