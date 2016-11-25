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

//-------------------------- vf_edit_latsample.cc ---------------------------//
//-------------------------- vf_edit_latsample.cc ---------------------------//
//-------------------------- vf_edit_latsample.cc ---------------------------//

//            implementation file for the VfEditLatsample class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_latsample.hh"
#include "vf/vf_edit_sort.hh"
#include "vf/vf_edit_names.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_informer.hh"
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

enum { XDIR, YDIR };

//---------------- documentation for terpflag ---------------------------//
//---------------- documentation for terpflag ---------------------------//
//---------------- documentation for terpflag ---------------------------//


// _terpflag == TERPFLAG_LINEAR:
//      does linear interpolation with flat extrapolation.
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
//      does running average smoothing.
//      needs _nrun and _endflag.
//      there is no running average if _nrun == 1.


//---------------- documentation for stepflag ---------------------------//
//---------------- documentation for stepflag ---------------------------//
//---------------- documentation for stepflag ---------------------------//


// _stepflag == STEPFLAG_CHANGE:
//      uses _x1, _xinc, _nx to set the new X locations (if _dirflag requests).
//      uses _y1, _yinc, _ny to set the new Y locations (if _dirflag requests).

// _stepflag == STEPFLAG_RETAIN:
//      retains the current function locations and smooths only.



//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//


VfEditLatsample::VfEditLatsample(VfUtilities *utilities)
           : VfEditBase(INFORM_TOTAL, "resample laterally"),
             _utilities           (utilities),
             _type                (VTNM),
             _terpflag            (TERPFLAG_LINEAR),
             _stepflag            (STEPFLAG_CHANGE),
             _endflag             (ENDFLAG_NARROWED),
             _dirflag             (DIRFLAG_X_ONLY),
             _x1                  (0.0),
             _y1                  (0.0),
             _xinc                (100.0),
             _yinc                (100.0),
             _nx                  (11),
             _ny                  (11),
             _smooth              (FALSE),
             _tsmooth             (0.0),
             _dsmooth             (0.0),
             _dv                  (0.0),
             _ncoef               (2),
             _nrun                (3)
{
  assert(_utilities);
}



VfEditLatsample::~VfEditLatsample()
{
}



//------------------ verify valid parameter --------------------//
//------------------ verify valid parameter --------------------//
//------------------ verify valid parameter --------------------//

         // private static functions.

void VfEditLatsample::verifyValidType     (int type)
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


void VfEditLatsample::verifyValidTerpflag (int terpflag)
{
  assert(terpflag == TERPFLAG_LINEAR  ||
         terpflag == TERPFLAG_CUBIC   ||
         terpflag == TERPFLAG_SPLINE  ||
         terpflag == TERPFLAG_POLY    ||
         terpflag == TERPFLAG_RUN);
}


void VfEditLatsample::verifyValidStepflag (int stepflag)
{
  assert(stepflag == STEPFLAG_CHANGE ||
         stepflag == STEPFLAG_RETAIN);
}


void VfEditLatsample::verifyValidEndflag  (int endflag)
{
  assert(endflag == ENDFLAG_TRUNCATED ||
         endflag == ENDFLAG_SHIFTED   ||
         endflag == ENDFLAG_EXTENDED  ||
         endflag == ENDFLAG_NARROWED);
}


void VfEditLatsample::verifyValidDirflag  (int dirflag)
{
  assert(dirflag == DIRFLAG_X_ONLY   ||
         dirflag == DIRFLAG_Y_ONLY   ||
         dirflag == DIRFLAG_X_THEN_Y ||
         dirflag == DIRFLAG_Y_THEN_X);
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//

       // public.

void   VfEditLatsample::setResampleType   (int   value)
{
  verifyValidType(value);
  if(!allowType(value)) return;
  _type = value;
}


void   VfEditLatsample::setTerpflag       (int   value)
{
  verifyValidTerpflag(value);
  if(!allowTerpflag(value)) return;
  _terpflag = value;
}


void   VfEditLatsample::setStepflag       (int   value)
{
  verifyValidStepflag(value);
  if(!allowStepflag(value)) return;
  _stepflag = value;
}


void   VfEditLatsample::setEndflag        (int   value)
{
  verifyValidEndflag(value);
  _endflag = value;
}


void   VfEditLatsample::setDirflag        (int   value)
{
  verifyValidDirflag(value);
  _dirflag = value;
}


void   VfEditLatsample::setX1             (float value)
{
  _x1 = value;
}


void   VfEditLatsample::setY1             (float value)
{
  _y1 = value;
}


void   VfEditLatsample::setXinc           (float value)
{
  if(value <= 0.0) return;
  _xinc = value;
}


void   VfEditLatsample::setYinc           (float value)
{
  if(value <= 0.0) return;
  _yinc = value;
}


void   VfEditLatsample::setNx          (long  value)
{
  if(value <= 0) return;
  _nx = value;
}


void   VfEditLatsample::setNy          (long  value)
{
  if(value <= 0) return;
  _ny = value;
}


void   VfEditLatsample::setXend        (float value)
{
  if(value < _x1) return;
  _nx = NearestInteger((value - _x1) / _xinc) + 1;
}


void   VfEditLatsample::setYend        (float value)
{
  if(value < _y1) return;
  _ny = NearestInteger((value - _y1) / _yinc) + 1;
}


void   VfEditLatsample::setSmooth         (int value)
{
  _smooth = value;
}


void   VfEditLatsample::setTsmooth        (float value)
{
  if(value < 0.0) return;
  _tsmooth = value;
}


void   VfEditLatsample::setDsmooth        (float value)
{
  if(value < 0.0) return;
  _dsmooth = value;
}


void   VfEditLatsample::setDv             (float value)
{
  _dv = MaximumValue(value, 0.0);
}


void   VfEditLatsample::setNcoef          (long  value)
{
  _ncoef = ConstrainValue(value, 1, 10);
}


void   VfEditLatsample::setNrun           (long  value)
{
  _nrun = MaximumValue(value, 1);
}



//-------------------- check whether parameter is allowed -------------//
//-------------------- check whether parameter is allowed -------------//
//-------------------- check whether parameter is allowed -------------//

     // returns FALSE if the specified parameter value is illegal
     // according to the current values of other parameters.


int   VfEditLatsample::allowType          (int value)  const
{
  verifyValidType(value);
  return TRUE;
}


int   VfEditLatsample::allowTerpflag      (int value)  const
{
  verifyValidTerpflag(value);
  if(value == TERPFLAG_LINEAR || value == TERPFLAG_CUBIC)
      {
      if(_stepflag == STEPFLAG_RETAIN) return FALSE;
      }
  return TRUE;
}


int   VfEditLatsample::allowStepflag      (int value)  const
{
  verifyValidStepflag(value);
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


int   VfEditLatsample::needStepflag        ()  const
{
  return TRUE;
}


int   VfEditLatsample::needEndflag        ()  const
{
  if(_terpflag != TERPFLAG_RUN) return FALSE;
  return TRUE;
}


int   VfEditLatsample::needXlocations     ()  const
{
  if(_stepflag == STEPFLAG_RETAIN) return FALSE;
  if(_dirflag  == DIRFLAG_Y_ONLY ) return FALSE;
  return TRUE;
}


int   VfEditLatsample::needYlocations     ()  const
{
  if(_stepflag == STEPFLAG_RETAIN) return FALSE;
  if(_dirflag  == DIRFLAG_X_ONLY ) return FALSE;
  return TRUE;
}


int   VfEditLatsample::needSmooth         ()  const
{
  if(_stepflag != STEPFLAG_RETAIN) return FALSE;
  return TRUE;
}


int   VfEditLatsample::needTsmooth         ()  const
{
  if(_stepflag != STEPFLAG_RETAIN)       return FALSE;
  if(_smooth   == FALSE)                 return FALSE;
  if(!_utilities->abscissaIsTime(_type)) return FALSE;
  return TRUE;
}


int   VfEditLatsample::needDsmooth         ()  const
{
  if(_stepflag != STEPFLAG_RETAIN)        return FALSE;
  if(_smooth   == FALSE)                  return FALSE;
  if(!_utilities->abscissaIsDepth(_type)) return FALSE;
  return TRUE;
}


int   VfEditLatsample::needDv             ()  const
{
  if(_terpflag != TERPFLAG_SPLINE) return FALSE;
  return TRUE;
}


int   VfEditLatsample::needNcoef          ()  const
{
  if(_terpflag != TERPFLAG_POLY) return FALSE;
  return TRUE;
}


int   VfEditLatsample::needNrun           ()  const
{
  if(_terpflag != TERPFLAG_RUN) return FALSE;
  return TRUE;
}



//-------------------- get bin locations and lines ----------------------//
//-------------------- get bin locations and lines ----------------------//
//-------------------- get bin locations and lines ----------------------//

 // gets   primary_bin [nfun]     for primary   sort xbin or ybin array.
 // gets secondary_bin [nfun]     for secondary sort xbin or ybin array.
 // gets original_ifun [nfun]     for original velocity function index.
 // gets first_of_line [numlines] for new index of first function of each line.
 // returns number of lines (always >= 1).
 // returns 0 if error occurs.

static long get_bin_locations_and_lines (VfKernal *kernal, int dir,
         float *primary_bin, float *secondary_bin,
         long *original_ifun, long *first_of_line)
{
  long nfun = kernal->numVelocityFunctions();
  VfUtilities *utilities = kernal->utilities();
  long ifun;
  for(ifun = 0; ifun < nfun; ifun++)
      {
      VfFunction *velfun = kernal->velfun(ifun);
      if(dir == XDIR)
          {
          primary_bin  [ifun] = utilities->ybinCenter(velfun->getYloc());
          secondary_bin[ifun] = utilities->xbinCenter(velfun->getXloc());
          }
      else
          {
          primary_bin  [ifun] = utilities->xbinCenter(velfun->getXloc());
          secondary_bin[ifun] = utilities->ybinCenter(velfun->getYloc());
          }
      original_ifun[ifun] = ifun;
      }
  triplesort (primary_bin, secondary_bin, original_ifun, &nfun);

  long numlines = 1;
  first_of_line[0] = 0;
  for(ifun = 1; ifun < nfun; ifun++)
      {
      VfFunction *velfun = kernal->velfun(ifun);
      if(primary_bin[ifun] != primary_bin[ifun-1])
          {
          first_of_line[numlines] = ifun;
          numlines++;
          }
      else if(secondary_bin[ifun] == secondary_bin[ifun-1])
          {
          return 0;
          }
      }
  return numlines;
}



//---------------------- resample helper -------------------------------//
//---------------------- resample helper -------------------------------//
//---------------------- resample helper -------------------------------//

     // returns error = TRUE or FALSE.
     // returns vv and msg.
     // either uses or returns nn and xx (depending on stepflag).
     // nn is preset even if it might get reset.

static int resample_helper (int terpflag, int stepflag, int endflag,
                            float dv, long ncoef, long nrun,
                            long n, const float *x, const float *v,
                            long *nn, float *xx, float *vv, char *msg)
{
  float  xdelta  = 1.0;      // dummy - not used.
  int    nwant   = 1;        // dummy - not used.
  int    error;
  int    ncoef2  = (int)ncoef;
  int    nrun2   = (int)nrun;
  int    nfunmax = (int)MaximumValue(*nn, n);
  int    n2      = (int)n;
  int    nn2     = (int)(*nn);
  float *q = new float [12 * nfunmax];
  densify_nonlin_c(&terpflag, &stepflag, &endflag,
                   &xdelta, &xdelta, &nwant, &dv, &ncoef2, &nrun2, 
                   &n2, x, v,
                   &nn2, xx, vv, &nfunmax,   q, msg, &error);
  delete [] q;
  *nn = nn2;
  return error;
}



//-------------------------- virtual check ---------------------------//
//-------------------------- virtual check ---------------------------//
//-------------------------- virtual check ---------------------------//

        // public virtual function overriding VfEditBase.

int VfEditLatsample::virtualCheck(VfKernal *kernal, char *msg)
{
/*
  if(kernal->numVelocityFunctionsWithErrors() > 0)
*/
  if(kernal->numVelocityFunctionsWithTypeErrors(_type) > 0)
      {
      strcpy(msg, "some velocity functions have pre-existing errors"); 
      return TRUE;
      }
  if(_dirflag != DIRFLAG_Y_ONLY && _stepflag == STEPFLAG_CHANGE &&
                    _xinc < _utilities->getXwidth())
      {
      strcpy(msg,
       "requested resampling yields more than one function at same X bin"); 
      return TRUE;
      }
  if(_dirflag != DIRFLAG_X_ONLY && _stepflag == STEPFLAG_CHANGE &&
                    _yinc < _utilities->getYwidth())
      {
      strcpy(msg,
       "requested resampling yields more than one function at same Y bin"); 
      return TRUE;
      }

  float tolerance = 0.001;
  if(_utilities->abscissaIsDepth(_type)) tolerance = 10.0;
  float timedepth[MAXPICKS];
  long npicks = kernal->velfun(0)->numPicks(); 
  for(long ipick = 0; ipick < npicks; ipick++)
      {
      timedepth[ipick] = kernal->velfun(0)->getAbscissa(ipick, _type);
      }

  long nfun = kernal->numVelocityFunctions();
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      VfFunction *velfun = kernal->velfun(ifun);
      long npicks2 = velfun->numPicks();
      if(npicks2 != npicks)
          {
          strcpy(msg, "all velocity functions must have same number of picks"); 
          return TRUE;
          }
      for(long ipick = 0; ipick < npicks; ipick++)
          {
          float abscissa = velfun->getAbscissa(ipick, _type);
          if(AbsoluteValue(abscissa - timedepth[ipick]) >= tolerance)
              {
              if(_utilities->abscissaIsTime(_type))
                strcpy(msg, "velocity functions do not have same time values");
              else
                strcpy(msg, "velocity functions do not have same depth values");
              return TRUE;
              }
          }
      }

  if(_stepflag == STEPFLAG_RETAIN && _smooth)
      {
      if(_utilities->abscissaIsTime(_type) && _tsmooth > timedepth[npicks-1])
          {
          strcpy(msg, "smoothing would start below bottom time"); 
          return TRUE;
          }
      if(_utilities->abscissaIsDepth(_type) && _dsmooth > timedepth[npicks-1])
          {
          strcpy(msg, "smoothing would start below bottom depth"); 
          return TRUE;
          }
      }

  float *primary_bin   = new float [nfun];
  float *secondary_bin = new float [nfun];
  long  *original_ifun = new long  [nfun];
  long  *first_of_line = new long  [nfun];

                                 // can be either XDIR or YDIR here:
  long numlines = get_bin_locations_and_lines(kernal, XDIR,
         primary_bin, secondary_bin, original_ifun, first_of_line);

  delete [] primary_bin;
  delete [] secondary_bin;
  delete [] original_ifun;
  delete [] first_of_line;

  if(numlines == 0)
      {
      strcpy(msg, "you have two or more velocity functions at same (X,Y) bin");
      return TRUE;
      }

  if(_stepflag == STEPFLAG_CHANGE)
       strcpy(msg, "resampling velocity functions laterally...");
  else strcpy(msg, "smoothing velocity functions laterally...");
  return FALSE;
}



//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//

        // public virtual function overriding VfEditBase.


int VfEditLatsample::virtualEdit (VfKernal *kernal, char *msg)
{
  int error;
  switch(_dirflag)
      {
      case DIRFLAG_X_ONLY:
                         error = resample(kernal, msg, XDIR);
                         break;
      case DIRFLAG_Y_ONLY:
                         error = resample(kernal, msg, YDIR);
                         break;
      case DIRFLAG_X_THEN_Y:
                         error = resample(kernal, msg, XDIR);
              if(!error) error = resample(kernal, msg, YDIR);
                         break;
      case DIRFLAG_Y_THEN_X:
                         error = resample(kernal, msg, YDIR);
              if(!error) error = resample(kernal, msg, XDIR);
                         break;
      default: assert(FALSE);
      }
  return error;
}



//------------------------- resample -----------------------------------//
//------------------------- resample -----------------------------------//
//------------------------- resample -----------------------------------//

    // private.
    // resample in X direction if dir == XDIR.
    // resample in Y direction if dir == YDIR.

int VfEditLatsample::resample (VfKernal *kernal, char *msg, int dir)
{
  VfInformer *informer = kernal->informer();
  float timedepth[MAXPICKS];
  float dummyvels[MAXPICKS];
  long npicks = kernal->velfun(0)->numPicks(); 
  for(long ipick = 0; ipick < npicks; ipick++)
      {
      timedepth[ipick] = kernal->velfun(0)->getAbscissa(ipick, _type);
      dummyvels[ipick] = 1000.0;
      }
  float tdsmooth;
  if(_utilities->abscissaIsTime(_type)) tdsmooth = _tsmooth;
  else                                  tdsmooth = _dsmooth;

////////// get bin locations and number of lines.

// primary_bin     [nfun]     for primary   sort xbin or ybin array.
// secondary_bin   [nfun]     for secondary sort xbin or ybin array.
// original_ifun   [nfun]     for original velocity function index.
// first_of_line   [numlines] for new index of first function of each line.

  informer->showMessage("getting bin locations and number of lines...");
  long nfun = kernal->numVelocityFunctions();
  float *primary_bin   = new float [nfun];
  float *secondary_bin = new float [nfun];
  long  *original_ifun = new long  [nfun];
  long  *first_of_line = new long  [nfun];

  long numlines = get_bin_locations_and_lines(kernal, dir,
         primary_bin, secondary_bin, original_ifun, first_of_line);
  assert(numlines >= 1);

////////// convert primary_bin from bin center to average location.
////////// convert secondary_bin from bin center to actual location.

  informer->showMessage("adjusting bin locations...");
  long longest = 0;    // number of velocity functions in longest line.
  long i, k;
  for(k = 0; k < numlines; k++)
      {
      long istart = first_of_line[k];
      long istop;
      if(k < numlines-1) istop = first_of_line[k+1] - 1;
      else               istop = nfun - 1;
      long kount = istop - istart + 1;
      longest = MaximumValue(longest, kount);
      float sum = 0.0;
      for(i = istart; i <= istop; i++)
          {
          long i2 = original_ifun[i];
          VfFunction *velfun = kernal->velfun(i2);
          if(dir == XDIR)
              {
              sum               += velfun->getYloc();
              secondary_bin[i]   = velfun->getXloc();
              }
          else
              {
              sum               += velfun->getXloc();
              secondary_bin[i]   = velfun->getYloc();
              }
          }
      float average = sum / kount;
      for(i = istart; i <= istop; i++)
          {
          primary_bin[i] = average;
          }
      }

////////// get all of the velocities.

  informer->showMessage("gathering up velocities for resampling...");
  float *velocities = new float [nfun * npicks];
  long ifun;
  for(ifun = 0; ifun < nfun; ifun++)
      {
      long i2 = original_ifun[ifun];
      VfFunction *velfun = kernal->velfun(i2);
      float temporary[MAXPICKS];
      velfun->getOrdinateArray(temporary, _type);
      for(long ipick = 0; ipick < npicks; ipick++)
          {
          long index = nfun * ipick + ifun;
          velocities[index] = temporary[ipick];
          }
      }

////////// get number of requested functions per line.

  long nn;
  if(_stepflag == STEPFLAG_CHANGE)
      {
      if(dir == XDIR) nn = _nx;
      else            nn = _ny;
      }
  else
      {
      nn = longest;
      }

////////// get parameters for resampling.

  int stepflag2;
  float *xx = new float [nn];
  float *vv = new float [nn];
  if(_stepflag == STEPFLAG_CHANGE)
      {
      stepflag2 = 4;                  // use xx[nn] for abscissae.
      for(long i = 0; i < nn; i++)
          {
          if(dir == XDIR) xx[i] = _x1 + i * _xinc;
          else            xx[i] = _y1 + i * _yinc;
          }
      }
  else
      {
      stepflag2 = 3;                  // retain current abscissae.
      }

////////// create new velocity functions if changing bin locations.
////////// also save old nfun.

  long nfunkeep = nfun;
  if(_stepflag == STEPFLAG_CHANGE)
      {
      char clause[40];
      strcpy(clause, "creating");
      if(dir == XDIR) strcat(clause, " (in X direction) new line");
      else            strcat(clause, " (in Y direction) new line");
      kernal->resetNumVelocityFunctions(numlines * nn);
      for(long k = 0; k < numlines; k++)
          {
          informer->showWorkingMessage(clause, k, numlines);
          long k2 = first_of_line[k];
          for(long i = 0; i < nn; i++)
              {
              long ifun = k * nn + i;
              VfFunction *velfun = kernal->velfun(ifun);
              if(dir == XDIR)
                  {
                  velfun->setXloc(xx[i]);
                  velfun->setYloc(primary_bin[k2]);
                  }
              else
                  {
                  velfun->setXloc(primary_bin[k2]);
                  velfun->setYloc(xx[i]);
                  }
              velfun->setDefaultType(_type);
              velfun->resetNumPicks(npicks, timedepth, dummyvels, _type);
              }
          }
      }

////////// before several operations.

  nfun = kernal->numVelocityFunctions();
  for(ifun = 0; ifun < nfun; ifun++)
      {
      VfFunction *velfun = kernal->velfun(ifun);
      velfun->beforeSeveralOperations();
      }

////////// begin loop, resampling each line.

  char clause[40];
  if(_stepflag == STEPFLAG_CHANGE) strcpy(clause, "resampling");
  else                             strcpy(clause, "smoothing");
  if(dir == XDIR) strcat(clause, " (in X direction) line");
  else            strcat(clause, " (in Y direction) line");
  int error = FALSE;
  for(k = 0; k < numlines; k++)
      {
      informer->showWorkingMessage(clause, k, numlines);
      long istart = first_of_line[k];
      long istop;
      if(k < numlines-1) istop = first_of_line[k+1] - 1;
      else               istop = nfunkeep - 1;
      int kount = (int)(istop - istart + 1);
      for(long ipick = 0; ipick < npicks; ipick++)
          {
          long index = nfunkeep * ipick + istart;
          int terpflag2;
          if(_smooth && _stepflag == STEPFLAG_RETAIN
                     && timedepth[ipick] < tdsmooth)
              {
              terpflag2 = TERPFLAG_LINEAR;      // do-nothing.
              }
          else
              {
              terpflag2 = _terpflag;
              }
          error = resample_helper(terpflag2, stepflag2, _endflag,
                       _dv, _ncoef, _nrun,
                       kount, &secondary_bin[istart], &velocities[index],
                       &nn, xx, vv, msg);
          if(error) break;

////////// place resampled velocity into correct location.

          for(long i = 0; i < nn; i++)
              {
              long i2;
              if(_stepflag == STEPFLAG_CHANGE) i2 = k * nn + i;
              else                             i2 = original_ifun[istart + i];
              VfFunction *velfun = kernal->velfun(i2);
              velfun->setOrdinate(ipick, vv[i], _type);
              }

////////// end loop, resampling each line.

          }
      if(error) break;
      }

  delete [] velocities;
  delete [] xx;
  delete [] vv;
  delete [] primary_bin;
  delete [] secondary_bin;
  delete [] original_ifun;
  delete [] first_of_line;

////////// after several operations.

  nfun = kernal->numVelocityFunctions();
  for(ifun = 0; ifun < nfun; ifun++)
      {
      informer->showWorkingMessage("updating velocity function", ifun, nfun);
      VfFunction *velfun = kernal->velfun(ifun);
      velfun->afterSeveralOperations(_type);
      }
  if(error) return TRUE;

////////// sort functions, and maybe reset names of functions.

  VfEditSort *sort = new VfEditSort();
  error = sort->checkForErrors(kernal, msg);
  informer->showMessage(msg);
  if(!error) error = sort->editKernal(kernal, msg);
  informer->showMessage(msg);
  delete sort;

  if(_stepflag == STEPFLAG_CHANGE)
      {
      VfEditNames *names = new VfEditNames();
      error = names->checkForErrors(kernal, msg);
      informer->showMessage(msg);
      if(!error) error = names->editKernal(kernal, msg);
      informer->showMessage(msg);
      delete names;
      }

////////// finished.

  if(_stepflag == STEPFLAG_CHANGE) strcpy(clause, "resampled");
  else                             strcpy(clause, "smoothed");
  long kounte = kernal->numVelocityFunctionsWithErrors();
  if(kounte > 0)
      {
      sprintf(msg, "velocity functions %s laterally with %d errors",
                                      clause, kounte);
      }
  else
      {
      sprintf(msg, "velocity functions successfully %s laterally",
                                      clause);
      }
  return (kounte > 0);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

