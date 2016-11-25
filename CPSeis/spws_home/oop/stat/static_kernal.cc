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

//--------------------- static_kernal.cc ---------------------//
//--------------------- static_kernal.cc ---------------------//
//--------------------- static_kernal.cc ---------------------//

//         implementation file for the StaticKernal class
//                  not derived from any class
//                      subdirectory stat


#include "stat/static_kernal.hh"
#include "stat/statio_wrapper.hh"
#include "stat/statutil_wrapper.hh"
#include "oprim/logic_array.hh"
#include "oprim/limits_keeper.hh"
#include "oprim/history_cards.hh"
#include "named_constants.h"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>



//-------------------------- get value --------------------------------//
//-------------------------- get value --------------------------------//
//-------------------------- get value --------------------------------//


static float get_value(void *data, long ival)
{
  StaticKernal *ss = (StaticKernal*)data;
  return ss->getValue(ival);
}



//--------------------------- constructor ----------------------------//
//--------------------------- constructor ----------------------------//
//--------------------------- constructor ----------------------------//


StaticKernal::StaticKernal(const char *progname)
           :
          _select              (NULL),
          _limits              (NULL),
          _ixactive            (0),
          _iyactive            (0),
          _pointer             (NULL),
          _nhx                 (7),
          _nhy                 (8),
          _nhx2                (0),
          _nhy2                (0),
          _x1                  (0.0),
          _y1                  (0.0),
          _xinc                (1.0),
          _yinc                (1.0),
          _nx                  (1),
          _ny                  (1)
{
  strcpy(_stattype, "MISC");

  _limits  = new LimitsKeeper (get_value, this);
  findLimits();
  _select  = new LogicArray   (numValues());
  _history = new HistoryCards (500, progname);
}



//------------------------ destructor -------------------------------//
//------------------------ destructor -------------------------------//
//------------------------ destructor -------------------------------//


StaticKernal::~StaticKernal()
{
  delete _select;
  delete _limits;
  delete _history;
  if(_pointer) delete [] _pointer;
}


//------------------------------- clear ---------------------------------//
//------------------------------- clear ---------------------------------//
//------------------------------- clear ---------------------------------//


void StaticKernal::clear()
{
  freePointer();
  findLimits();
  doReset();
  _nhx  = 7;
  _nhy  = 8;
  _nhx2 = 0;
  _nhy2 = 0;
  _x1   = 0.0;
  _y1   = 0.0;
  _xinc = 1.0;
  _yinc = 1.0;
  _nx   = 1;
  _ny   = 1;
  _history->deleteHistoryCards();
}



                    //----- private functions -----//
                    //----- private functions -----//
                    //----- private functions -----//
                    //----- private functions -----//
                    //----- private functions -----//
                    //----- private functions -----//
                    //----- private functions -----//
                    //----- private functions -----//
                    //----- private functions -----//
                    //----- private functions -----//
                    //----- private functions -----//



//------------------- all static values are nil -----------------------//
//------------------- all static values are nil -----------------------//
//------------------- all static values are nil -----------------------//

   // private.
   // to be called if the requested calculation is a do-nothing if all
   //   static values are nil (i.e. there are no live values).
   // returns error = TRUE (and msg) if all static values are nil.
   // returns error = FALSE if there is at least one live static value.

int StaticKernal::allStaticValuesAreNil(char *msg)  const
{
  if(numLiveValues() > 0) return FALSE;
  strcpy(msg, "all static values are nil - nothing changed");
  return TRUE;
}



//------------------- all static values are live -----------------------//
//------------------- all static values are live -----------------------//
//------------------- all static values are live -----------------------//

   // private.
   // to be called if the requested calculation is a do-nothing if all
   //   static values are live (i.e. there are no nil values).
   // returns error = TRUE (and msg) if no static values are nil.
   // returns error = FALSE if there is at least one nil static value.

int StaticKernal::allStaticValuesAreLive(char *msg)  const
{
  if(numNilValues() > 0) return FALSE;
  strcpy(msg, "no static values are nil - nothing changed");
  return TRUE;
}



//-------------------------- free pointer -------------------------//
//-------------------------- free pointer -------------------------//
//-------------------------- free pointer -------------------------//

   // private.

void StaticKernal::freePointer()
{
  if(_pointer)
      {
      delete [] _pointer;
      _pointer = NULL;
      }
}



//-------------------- alloc pointer if necessary ------------------------//
//-------------------- alloc pointer if necessary ------------------------//
//-------------------- alloc pointer if necessary ------------------------//

       // private.

void StaticKernal::allocPointerIfNecessary()
{
  if(_pointer == NULL)
       {
       int n = _nx * _ny;
       _pointer = new float [n];
       for(int ival = 0; ival < n; ival++)
            {
            _pointer[ival] = FNIL;
            }
       }
}



//-------------------- find and update limits ---------------------//
//-------------------- find and update limits ---------------------//
//-------------------- find and update limits ---------------------//

   // private.
   // finds (or updates) minimum and maximum and sum of non-nil static values.
   // also finds (or updates) the number of nil and non-nil values.

void StaticKernal::findLimits()
{
  int  n = _nx * _ny;
  _limits->getLimits(n);
  if(_limits->numLiveValues() == 0) freePointer();
}



void StaticKernal::updateLimits(float old_value, float new_value)
{
  _limits->removeValue(old_value);
  _limits->insertValue(new_value);
  if(_limits->numLiveValues() == 0) freePointer();
}



//------------------- reset active and selected items ---------------------//
//------------------- reset active and selected items ---------------------//
//------------------- reset active and selected items ---------------------//

           // private.

static int  nxkeep;
static int  nykeep;

void StaticKernal::startReset()
{
  nxkeep = _nx;
  nykeep = _ny;
}


void StaticKernal::finishReset()
{
  if(_nx != nxkeep || _ny != nykeep) doReset();
}


void StaticKernal::doReset()
{
  _select->resetNumElements(numValues());
  _ixactive = 0;
  _iyactive = 0;
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//


static inline int  get_unconstrained_index
                       (float xbin, float x1, float xinc, int adjustment = 0)
{
  float fx = (xbin - x1) / xinc;
  int  ix = NearestInteger(fx);
  if     (adjustment < 0 && fx < ix) ix--;
  else if(adjustment > 0 && fx > ix) ix++;
  return ix;
}


static inline int  get_matching_index
                          (float xbin, float x1, float xinc, int  nx)
{
  int  ix = get_unconstrained_index(xbin, x1, xinc);
  assert(ix >= 0 && ix < nx);
  return ix;
}


static inline int  get_nearest_index
              (float xbin, float x1, float xinc, int  nx, int adjustment = 0)
{
  int  ix = get_unconstrained_index(xbin, x1, xinc, adjustment);
  ix = ConstrainValue(ix, 0, nx-1);
  return ix;
}


static inline float get_bin (int  ix, float x1, float xinc, int  nx)
{
  assert(ix >= 0 && ix < nx);
  float xbin = x1 + ix * xinc;
  return xbin;
}


static inline int  get_combo_index (int  ix, int  iy, int  nx, int  ny)
{
  assert(ix >= 0 && ix < nx);
  assert(iy >= 0 && iy < ny);
  int  ival = iy * nx + ix;
  return ival;
}



                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//
                  //------- public functions -------//




//-------------------- allow changing file size -------------------//
//-------------------- allow changing file size -------------------//
//-------------------- allow changing file size -------------------//

       // public.
       // returns TRUE if nx and ny can be changed.
       // returns FALSE otherwise.

int StaticKernal::allowChangingFileSize()  const
{
  return (_pointer == NULL);
}



//-------------------- get values ---------------------------------//
//-------------------- get values ---------------------------------//
//-------------------- get values ---------------------------------//

       // public.

int  StaticKernal::numNilValues()  const
{
  return _limits->numNilValues();
}


int  StaticKernal::numLiveValues()  const
{
  return _limits->numLiveValues();
}


float StaticKernal::minimumValue()
{
  return _limits->minimumValue();
}


float StaticKernal::maximumValue()
{
  return _limits->maximumValue();
}


float StaticKernal::sumValues()  const
{
  return _limits->sumValues();
}


float StaticKernal::averageValue()  const
{
  return _limits->averageValue();
}



//--------------------- set active or selected items --------------------//
//--------------------- set active or selected items --------------------//
//--------------------- set active or selected items --------------------//

     // public.

float  StaticKernal::getActiveXbin   ()  const
{
  return getXbin(_ixactive);
}


float  StaticKernal::getActiveYbin   ()  const
{
  return getYbin(_iyactive);
}


int    StaticKernal::numSelections()  const
{
  return _select->numTrueElements();
}


int    StaticKernal::isSelected     (int  ival)  const
{
  assert(ival >= 0 && ival < numValues());
  return _select->getElement(ival);
}


int    StaticKernal::isSelected     (int  ix, int  iy)  const
{
  int  ival = get_combo_index(ix, iy, _nx, _ny);
  return isSelected(ival);
}



//----------------------- get bin or index --------------------------//
//----------------------- get bin or index --------------------------//
//----------------------- get bin or index --------------------------//

      // public.

int   StaticKernal::getMatchingIx (float xbin)  const
{
  return get_matching_index(xbin, _x1, _xinc, _nx);
}


int   StaticKernal::getMatchingIy (float ybin)  const
{
  return get_matching_index(ybin, _y1, _yinc, _ny);
}


int   StaticKernal::getNearestIx (float xbin, int adjustment)  const
{
  return get_nearest_index(xbin, _x1, _xinc, _nx, adjustment);
}


int   StaticKernal::getNearestIy (float ybin, int adjustment)  const
{
  return get_nearest_index(ybin, _y1, _yinc, _ny, adjustment);
}


int   StaticKernal::getUnconstrainedIx (float xbin)  const
{
  return get_unconstrained_index(xbin, _x1, _xinc);
}


int   StaticKernal::getUnconstrainedIy (float ybin)  const
{
  return get_unconstrained_index(ybin, _y1, _yinc);
}



float  StaticKernal::getXbin   (int  ix)  const
{
  return get_bin(ix, _x1, _xinc, _nx);
}


float  StaticKernal::getYbin   (int  iy)  const
{
  return get_bin(iy, _y1, _yinc, _ny);
}



float StaticKernal::getMatchingXbinCenter (float xbin)  const
{
  int  ix = getMatchingIx(xbin);
  return getXbin(ix);
}


float StaticKernal::getMatchingYbinCenter (float ybin)  const
{
  int  iy = getMatchingIy(ybin);
  return getYbin(iy);
}


float StaticKernal::getNearestXbinCenter (float xbin, int adjustment)  const
{
  int  ix = getNearestIx(xbin, adjustment);
  return getXbin(ix);
}


float StaticKernal::getNearestYbinCenter (float ybin, int adjustment)  const
{
  int  iy = getNearestIy(ybin, adjustment);
  return getYbin(iy);
}



//------------------------ get one static value -----------------------//
//------------------------ get one static value -----------------------//
//------------------------ get one static value -----------------------//

     // public.

float  StaticKernal::getValue     (int  ival)  const
{
  assert(ival >= 0 && ival < numValues());
  if(_pointer == NULL) return FNIL;
  return _pointer[ival];
}


float  StaticKernal::getValue     (int  ix, int  iy)  const
{
  int  ival = get_combo_index(ix, iy, _nx, _ny);
  return getValue(ival);
}


     // asserts if extrapolation is required:
     // otherwise returns nearest value:

float  StaticKernal::getMatchingValue (float xbin, float ybin)  const
{
  int  ix = getMatchingIx(xbin);
  int  iy = getMatchingIy(ybin);
  return getValue(ix, iy);
}


     // returns nearest edge value if extrapolation is required:
     // otherwise returns nearest value:

float  StaticKernal::getNearestValue (float xbin, float ybin)  const
{
  int  ix = getNearestIx(xbin);
  int  iy = getNearestIy(ybin);
  return getValue(ix, iy);
}


     // returns nil if extrapolation is required:
     // otherwise returns nearest value:

float  StaticKernal::getCenterValue (float xbin, float ybin)  const
{
  int  ix = getUnconstrainedIx(xbin);
  int  iy = getUnconstrainedIy(ybin);
  if(ix < 0 || ix >= _nx) return FNIL;
  if(iy < 0 || iy >= _ny) return FNIL;
  return getValue(ix, iy);
}


     // returns nearest interpolated value if extrapolation is required:
     // otherwise returns interpolated value:
     // returns nil if some of the values needed for interpolation are nil:

////////// later replace this with a call to terputil:

float  StaticKernal::getTerpValue (float xbin, float ybin)  const
{
  if(_pointer == NULL) return FNIL;

  float almost_zero = 0.5;
  float nearly_one  = 1.0 - almost_zero;
  float x = (xbin - _x1) / _xinc;
  float y = (ybin - _y1) / _yinc;
  int  kx = (int)x;
  int  ky = (int)y;
  int  lx = kx + 1;
  int  ly = ky + 1;

  kx = ConstrainValue(kx, 0, _nx - 1);
  ky = ConstrainValue(ky, 0, _ny - 1);
  lx = ConstrainValue(lx, 0, _nx - 1);
  ly = ConstrainValue(ly, 0, _ny - 1);
  float v1 = _pointer[ky * _nx + kx];
  float v2 = _pointer[ky * _nx + lx];
  float v3 = _pointer[ly * _nx + kx];
  float v4 = _pointer[ly * _nx + lx];

  x = ConstrainValue(x, kx, lx);
  y = ConstrainValue(y, ky, ly);
  if(v2 == FNIL && x - kx < almost_zero) v2 = v1;
  if(v1 == FNIL && x - kx > nearly_one ) v1 = v2;
  if(v4 == FNIL && x - kx < almost_zero) v4 = v3;
  if(v3 == FNIL && x - kx > nearly_one ) v3 = v4;
  if(v3 == FNIL && y - ky < almost_zero) v3 = v1;
  if(v1 == FNIL && y - ky > nearly_one ) v1 = v3;
  if(v4 == FNIL && y - ky < almost_zero) v4 = v2;
  if(v2 == FNIL && y - ky > nearly_one ) v2 = v4;
  if(v1 == FNIL || v2 == FNIL ||
     v3 == FNIL || v4 == FNIL) return FNIL;

  float sky = v1 + (x - kx) * (v2 - v1);
  float sly = v3 + (x - kx) * (v4 - v3);
  float value = sky + (y - ky) * (sly - sky);
  return value;
}



float  StaticKernal::getResampledValue (float xbin, float ybin,
                                        int interp, int extrap)  const
{
/*****
  ///// redundant:
  assert(interp == INTERP_NEAR || interp == INTERP_TERP);
  assert(extrap == EXTRAP_EDGE || extrap == EXTRAP_NILS
                               || extrap == EXTRAP_ZERO);
*****/
  if(interp == INTERP_NEAR && extrap == EXTRAP_EDGE)
      {
      return getNearestValue(xbin, ybin);
      }
  if(interp == INTERP_NEAR && extrap == EXTRAP_NILS)
      {
      return getCenterValue(xbin, ybin);
      }
  if(interp == INTERP_NEAR && extrap == EXTRAP_ZERO)
      {
      int  ix = getUnconstrainedIx(xbin);
      int  iy = getUnconstrainedIy(ybin);
      if(ix < 0 || ix >= _nx) return 0.0;
      if(iy < 0 || iy >= _ny) return 0.0;
      return getNearestValue(xbin, ybin);
   // return getCenterValue (xbin, ybin);   // equivalent.
      }
  if(interp == INTERP_TERP && extrap == EXTRAP_ZERO)
      {
      int  ix = getUnconstrainedIx(xbin);
      int  iy = getUnconstrainedIy(ybin);
      if(ix < 0 || ix >= _nx) return FNIL;
      if(iy < 0 || iy >= _ny) return FNIL;
      return getTerpValue(xbin, ybin);
      }
  if(interp == INTERP_TERP && extrap == EXTRAP_EDGE)
      {
      return getTerpValue(xbin, ybin);
      }
  assert(interp == INTERP_TERP && extrap == EXTRAP_NILS);
  int  ix = getUnconstrainedIx(xbin);
  int  iy = getUnconstrainedIy(ybin);
  if(ix < 0 || ix >= _nx) return FNIL;
  if(iy < 0 || iy >= _ny) return FNIL;
  return getTerpValue(xbin, ybin);
}



//----------------------- get static values -----------------------//
//----------------------- get static values -----------------------//
//----------------------- get static values -----------------------//

     // public.

void   StaticKernal::getStaticValues (float *values)  const
{
  assert(values);
  int ival;
  int n = _nx * _ny;

  if(_pointer == NULL)
       {
       for(ival = 0; ival < n; ival++) { values[ival] = FNIL; }
       }
  else
       {
       for(ival = 0; ival < n; ival++) { values[ival] = _pointer[ival]; }
       }
}



//------------------------- get weight ------------------------------//
//------------------------- get weight ------------------------------//
//------------------------- get weight ------------------------------//

     // public.
     // returns weight (between 0.0 and 1.0) which is reduced if the
     //   value is within xdist or ydist points of a nil value.
     // nil values will always have a weight of zero.
     // live values will never have a weight of zero.
     // all live values will have a weight of one if xdist = ydist = 0.

float  StaticKernal::getWeight (int  ival, int  xdist, int  ydist)  const
{
  assert(ival >= 0 && ival < numValues());
  if(_pointer == NULL)         return 0.0;
  if(_pointer[ival] == FNIL)   return 0.0;  // timesaver only.
  if(xdist == 0 && ydist == 0) return 1.0;  // timesaver only.
  int  iy = ival / _nx;
  int  ix = ival - iy * _nx;
  assert(ix >= 0 && ix < _nx);
  assert(iy >= 0 && iy < _ny);

  float weight = 1.0;
  for(int  ixdist = -xdist; ixdist <= xdist; ixdist++)
      {
      for(int  iydist = -ydist; iydist <= ydist; iydist++)
          {
          int  ix2 = ix + ixdist;
          int  iy2 = iy + iydist;
          float value = FNIL;
          if(ix2 >= 0 && ix2 < _nx &&
             iy2 >= 0 && iy2 < _ny)
              {
              int  ival2 = iy2 * _nx + ix2;
         ///  int  ival2 = get_combo_index(ix2, iy2, nx, ny);   /// equivalent.
              value = _pointer[ival2];
              }
          if(value == FNIL)
              {
              float xfrac = (float)AbsoluteValue(ixdist) / (float)(xdist + 1);
              float yfrac = (float)AbsoluteValue(iydist) / (float)(ydist + 1);
              float w = xfrac + yfrac;
              weight = MinimumValue(w, weight);
              }
          }
      }
  return MinimumValue(weight, 1.0);
}



float  StaticKernal::getWeight
                         (int  ix, int  iy, int  xdist, int  ydist)  const
{
  int  ival = get_combo_index(ix, iy, _nx, _ny);
  return getWeight(ival, xdist, ydist);
}



//---------------------------- set values --------------------------//
//---------------------------- set values --------------------------//
//---------------------------- set values --------------------------//

     // public.

void   StaticKernal::setStattype   (const char *stattype)
{
  if(!stattype) return;
  str_to_upper         (_stattype, (char*)stattype);
  str_remove_all_blanks(_stattype,       _stattype);
  if(strlen(_stattype) == 0) strcpy(_stattype, "MISC");
  _history->addHistoryCard("change type of static file");
}


void   StaticKernal::setNhx    (int  nhx)
{
  if(nhx == INIL) return;
  if(nhx <= 0) return;
  _nhx = nhx;
  _history->addHistoryCard("change header words");
}


void   StaticKernal::setNhy    (int  nhy)
{
  if(nhy == INIL) return;
  if(nhy < 0) return;
  _nhy = nhy;
  _history->addHistoryCard("change header words");
}


void   StaticKernal::setNhx2   (int  nhx2)
{
  if(nhx2 == INIL) return;
  if(nhx2 < 0) return;
  _nhx2 = nhx2;
  _history->addHistoryCard("change header words");
}


void   StaticKernal::setNhy2   (int  nhy2)
{
  if(nhy2 == INIL) return;
  if(nhy2 < 0) return;
  _nhy2 = nhy2;
  _history->addHistoryCard("change header words");
}


void   StaticKernal::setX1     (float x1)
{
  if(x1 == FNIL) return;
  _x1 = x1;
  _history->addHistoryCard("transform ground positions");
}


void   StaticKernal::setY1     (float y1)
{
  if(y1 == FNIL) return;
  _y1 = y1;
  _history->addHistoryCard("transform ground positions");
}


void   StaticKernal::setXinc   (float xinc)
{
  if(xinc == FNIL) return;
  if(xinc <= 0.0) return;
  _xinc = xinc;
  _history->addHistoryCard("transform ground positions");
}


void   StaticKernal::setYinc   (float yinc)
{
  if(yinc == FNIL) return;
  if(yinc <= 0.0) return;
  _yinc = yinc;
  _history->addHistoryCard("transform ground positions");
}


void   StaticKernal::setNx   (int  nx)
{
  assert(allowChangingFileSize());
  if(nx == INIL) return;
  if(nx <= 0) return;
  _nx = nx;
  findLimits();
  doReset();
  _history->addHistoryCard("change number of ground positions");
}


void   StaticKernal::setNy   (int  ny)
{
  assert(allowChangingFileSize());
  if(ny == INIL) return;
  if(ny <= 0) return;
  _ny = ny;
  findLimits();
  doReset();
  _history->addHistoryCard("change number of ground positions");
}


void   StaticKernal::setXend   (float xend)
{
  if(xend == FNIL) return;
  int  nx = (int)(1.5 + (xend - _x1) / _xinc);
  setNx(nx);
}


void   StaticKernal::setYend   (float yend)
{
  if(yend == FNIL) return;
  int  ny = (int)(1.5 + (yend - _y1) / _yinc);
  setNy(ny);
}



//--------------------- set active or selected items --------------------//
//--------------------- set active or selected items --------------------//
//--------------------- set active or selected items --------------------//

     // public.

void   StaticKernal::setActiveXbin   (float xbin)
{
  _ixactive = getNearestIx(xbin);
}


void   StaticKernal::setActiveYbin   (float ybin)
{
  _iyactive = getNearestIy(ybin);
}



void   StaticKernal::setActiveIx   (int  ix)
{
  _ixactive = ConstrainValue(ix, 0, _nx-1);
}


void   StaticKernal::setActiveIy   (int  iy)
{
  _iyactive = ConstrainValue(iy, 0, _ny-1);
}



void   StaticKernal::setSelections (int  ix, int  iy,
                                  int  nxsel, int  nysel, int value)
{
  int  ixlast = ix + nxsel - 1;
  int  iylast = iy + nysel - 1;
  ix = ConstrainValue(ix, 0, _nx-1);
  iy = ConstrainValue(iy, 0, _ny-1);
  ixlast = ConstrainValue(ixlast, ix, _nx-1);
  iylast = ConstrainValue(iylast, iy, _ny-1);
  assert(_select->numElements() == numValues());
  for(int  iy2 = iy; iy2 <= iylast; iy2++)
      {
      for(int  ix2 = ix; ix2 <= ixlast; ix2++)
          {
          int  ival = ix2 + _nx * iy2;
          _select->setElement(ival, value);
          }
      }
}


void   StaticKernal::clearSelections()
{
  _select->setAllElementsFalse();
}



//---------------------- set one static value -------------------------//
//---------------------- set one static value -------------------------//
//---------------------- set one static value -------------------------//

     // public.

void   StaticKernal::setValue     (int  ival, float value)
{
  assert(ival >= 0 && ival < numValues());
  allocPointerIfNecessary();
  float  old_value = _pointer[ival];
  _pointer[ival]    = value;
  updateLimits(old_value, value);
  _history->addHistoryCard("edit one static value");
}


void   StaticKernal::setValue     (int  ix, int  iy, float value)
{
  int  ival = get_combo_index(ix, iy, _nx, _ny);
  setValue(ival, value);
}


void   StaticKernal::setMatchingValue (float xbin, float ybin, float value)
{
  int  ix = getMatchingIx(xbin);
  int  iy = getMatchingIy(ybin);
  setValue(ix, iy, value);
}



//---------------------- set static values ----------------------//
//---------------------- set static values ----------------------//
//---------------------- set static values ----------------------//

     // public.

void   StaticKernal::setStaticValues (const float *values)
{
  if(values)
      {
      int n = _nx * _ny;
      if(_pointer == NULL) _pointer = new float [n];
      for(int ival = 0; ival < n; ival++)
           {
           _pointer[ival] = values[ival];
           }
      }
  else
      {
      freePointer();
      }
  findLimits();
  _history->addHistoryCard("reset all static values");
}



//--------------------------- copy data ---------------------------//
//--------------------------- copy data ---------------------------//
//--------------------------- copy data ---------------------------//

     // public.

void   StaticKernal::copyData (const StaticKernal *other)
{
  clear();
  setStattype    (other->getStattype());
  setNhx         (other->getNhx     ());
  setNhy         (other->getNhy     ());
  setNhx2        (other->getNhx2    ());
  setNhy2        (other->getNhy2    ());
  setX1          (other->getX1      ());
  setY1          (other->getY1      ());
  setXinc        (other->getXinc    ());
  setYinc        (other->getYinc    ());
  setNx          (other->getNx      ());
  setNy          (other->getNy      ());
  setStaticValues(other->_pointer);
  findLimits();
  doReset();
  _ixactive = other->_ixactive;
  _iyactive = other->_iyactive;
  for(int  ival = 0; ival < numValues(); ival++)
      {
      int svalue = other->_select->getElement(ival);
      _select->setElement(ival, svalue);
      }
  _history->copyHistoryCards(other->history());
}



//---------------------- grade static values ---------------------//
//---------------------- grade static values ---------------------//
//---------------------- grade static values ---------------------//

     // public.

int  StaticKernal::gradeStaticValues
               (int testing,
                int  ixmin, int  ixmax, int  iymin, int  iymax,
                char *msg)
{
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(ixmin < 0 || ixmin > ixmax || ixmax >= _nx ||
     iymin < 0 || iymin > iymax || iymax >= _ny)
      {
      strcpy(msg, "specified ground positions outside of valid range");
      return TRUE;
      }
  if(testing)
      {
      strcpy(msg, "grading static values...");
      return FALSE;
      }
/////////////////////////////////
  StatutilWrapper::grade(_nx, _ny, _pointer, ixmin, ixmax, iymin, iymax);
/////////////////////////////////
  findLimits();
  sprintf(msg, "graded over ixmin/max = %d %d iymin/max = %d %d",
                                   ixmin, ixmax, iymin, iymax);
  _history->addHistoryCard(msg);
  return FALSE;
}



//-------------------- remove running average -------------------------//
//-------------------- remove running average -------------------------//
//-------------------- remove running average -------------------------//

       // public.

int  StaticKernal::removeRunningAverage
             (int testing,
              int option, int trim, float xrun, float yrun, int endflag,
              char *msg)
{
  assert(option == OPTION_REMOVE || option == OPTION_SMOOTH);
  if(allStaticValuesAreNil(msg)) return TRUE;
  trim      = ConstrainValue(trim, 0, 100);
  xrun      = MaximumValue  (xrun, 0.0);
  yrun      = MaximumValue  (yrun, 0.0);
  int nxrun = NearestInteger(xrun / _xinc);
  int nyrun = NearestInteger(yrun / _yinc);
  if((nxrun <= 1 || _nx == 1) &&
     (nyrun <= 1 || _ny == 1))
      {
      strcpy(msg,
         "specified ground position ranges make this a do-nothing operation");
      return TRUE;
      }
  if(testing)
      {
      if(option == OPTION_REMOVE) strcpy(msg, "removing running average...");
      else                        strcpy(msg, "smoothing static values...");
      return FALSE;
      }
/////////////////////////////////
  int preserve = FALSE;
  int wild     = FALSE;
  if(option == OPTION_REMOVE)
      {
      StatutilWrapper::runav(_nx, _ny, _pointer, nxrun, nyrun, endflag,
                             float(trim), preserve, wild);
      }
  else
      {
      StatutilWrapper::smooth(_nx, _ny, _pointer, nxrun, nyrun, endflag,
                              float(trim), preserve, wild);
      }
/////////////////////////////////
  findLimits();
  if(option == OPTION_REMOVE)
      {
      sprintf(msg, "removed running average with trim percentage %d", trim);
      }
  else
      {
      sprintf(msg, "smoothed static values with trim percentage %d", trim);
      }
  _history->addHistoryCard(msg);
  sprintf(msg, "  and xrun %f and yrun %f using ", xrun, yrun);
  switch(endflag)
      {
      case ENDFLAG_N: strcat(msg, "narrowed end range" ); break;
      case ENDFLAG_T: strcat(msg, "truncated end range"); break;
      case ENDFLAG_E: strcat(msg, "extended end range" ); break;
      case ENDFLAG_S: strcat(msg, "shifted end range"  ); break;
      default       : strcat(msg, "invalid end range"  ); break;
      }
  _history->addHistoryCard(msg);
  return FALSE;
}



//-------------------- integrate static values ------------------------//
//-------------------- integrate static values ------------------------//
//-------------------- integrate static values ------------------------//

       // public.

int  StaticKernal::integrateStaticValues(int testing, char *msg)
{
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(testing)
      {
      strcpy(msg, "integrating static values...");
      return FALSE;
      }
/////////////////////////////////
  int preserve = FALSE;
  StatutilWrapper::integrate(_nx, _ny, _pointer, preserve);
/////////////////////////////////
  findLimits();
  strcpy(msg, "successfully integrated static values");
  _history->addHistoryCard(msg);
  return FALSE;
}



//-------------------- clear static values ----------------------------//
//-------------------- clear static values ----------------------------//
//-------------------- clear static values ----------------------------//

       // public.

int  StaticKernal::clearStaticValues(int testing, char *msg)
{
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(testing)
      {
      strcpy(msg, "setting all static values to nil...");
      return FALSE;
      }
/////////////////////////////////
  freePointer();
/////////////////////////////////
  findLimits();
  strcpy(msg, "all static values have been set to nil");
  _history->addHistoryCard(msg);
  return FALSE;
}



//-------------------- randomize static values ------------------------//
//-------------------- randomize static values ------------------------//
//-------------------- randomize static values ------------------------//

       // public.

int  StaticKernal::randomizeStaticValues(int testing, char *msg)
{
  if(testing)
      {
      strcpy(msg, "setting all static values to random numbers...");
      return FALSE;
      }
/////////////////////////////////
  allocPointerIfNecessary();
  int  n = numValues();
  for(int  i = 0; i < n; i++)
      {
      float value = (float)rand() / (float)RAND_MAX;     // zero to one.
      _pointer[i] = 20.0 * (value - 0.5);
      }
/////////////////////////////////
  findLimits();
  strcpy(msg, "all static values have been set to random numbers");
  _history->addHistoryCard(msg);
  return FALSE;
}



//------------------ multiply static values by constant ---------------//
//------------------ multiply static values by constant ---------------//
//------------------ multiply static values by constant ---------------//

       // public.

int  StaticKernal::multiplyStaticValuesByConstant
                               (int testing, float constant, char *msg)
{
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(constant == 1.0)
      {
      strcpy(msg, "multiplying by 1.0 is a do-nothing operation");
      return TRUE;
      }
  if(testing)
      {
      sprintf(msg, "multiplying static values by constant %f ...", constant);
      return FALSE;
      }
/////////////////////////////////
  int  n = numValues();
  for(int  i = 0; i < n; i++)
      {
      if(_pointer[i] != FNIL) _pointer[i] *= constant;
      }
/////////////////////////////////
  findLimits();
  sprintf(msg, "multiplied static values by constant %f", constant);
  _history->addHistoryCard(msg);
  return FALSE;
}



//------------------ add constant to static values --------------------//
//------------------ add constant to static values --------------------//
//------------------ add constant to static values --------------------//

       // public.

int  StaticKernal::addConstantToStaticValues
                               (int testing, float constant, char *msg)
{
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(constant == 0.0)
      {
      strcpy(msg, "adding 0.0 is a do-nothing operation");
      return TRUE;
      }
  if(testing)
      {
      sprintf(msg, "adding constant %f to static values...", constant);
      return FALSE;
      }
/////////////////////////////////
  int  n = numValues();
  for(int  i = 0; i < n; i++)
      {
      if(_pointer[i] != FNIL) _pointer[i] += constant;
      }
/////////////////////////////////
  findLimits();
  sprintf(msg, "added constant %f to static values", constant);
  _history->addHistoryCard(msg);
  return FALSE;
}



//-------------------- replace nils with specified value -----------------//
//-------------------- replace nils with specified value -----------------//
//-------------------- replace nils with specified value -----------------//

       // public.

int  StaticKernal::replaceNilsWithSpecifiedValue
                                  (int testing, float constant, char *msg)
{
  if(allStaticValuesAreLive(msg)) return TRUE;
  if(constant == FNIL)
      {
      strcpy(msg, "replacing nils with nil is a do-nothing operation");
      return TRUE;
      }
  if(testing)
      {
      sprintf(msg, "replacing %d nils with value %f...",
                                                numNilValues(), constant);
      return FALSE;
      }
/////////////////////////////////
  allocPointerIfNecessary();
  int  kount = numNilValues();
  int  n = numValues();
  for(int  i = 0; i < n; i++)
      {
      if(_pointer[i] == FNIL) _pointer[i] = constant;
      }
/////////////////////////////////
  findLimits();
  sprintf(msg, "replaced %d nils with value %f", kount, constant);
  _history->addHistoryCard(msg);
  return FALSE;
}



//-------------------- replace specified range with nils -----------------//
//-------------------- replace specified range with nils -----------------//
//-------------------- replace specified range with nils -----------------//

       // public.

int  StaticKernal::replaceRangeOfValuesWithNils
                                      (int testing,
                                       float range1, float range2,
                                       char *msg)
{
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(range1 == FNIL || range2 == FNIL)
      {
      strcpy(msg, "lower and upper limits of specified range cannot be nil");
      return TRUE;
      }
  int  n = numValues();
  int  kount = 0;
  int  i;
  for(i = 0; i < n; i++)
      {
      if(_pointer[i] == FNIL) continue;
      if(_pointer[i] >= range1 && _pointer[i] <= range2) kount++;
      }
  if(kount == 0)
      {
      strcpy(msg, "no static values found within specified range");
      return TRUE;
      }
  if(testing)
      {
      sprintf(msg, "replacing range %f thru %f with %d nils",
                                          range1, range2, kount);
      return FALSE;
      }
/////////////////////////////////
  for(i = 0; i < n; i++)
      {
      if(_pointer[i] == FNIL) continue;
      if(_pointer[i] >= range1 && _pointer[i] <= range2) _pointer[i] = FNIL;
      }
/////////////////////////////////
  findLimits();
  sprintf(msg, "replaced range %f thru %f with %d nils",
                                      range1, range2, kount);
  _history->addHistoryCard(msg);
  return FALSE;
}



//----------------- replace nils with terp values ---------------------//
//----------------- replace nils with terp values ---------------------//
//----------------- replace nils with terp values ---------------------//

       // public.

int  StaticKernal::replaceNilsWithTerpValues(int testing, char *msg)
{
  if(allStaticValuesAreLive(msg)) return TRUE;
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(testing)
      {
      sprintf(msg, "replacing %d nils with interpolated values",
                                     numNilValues());
      return FALSE;
      }
/////////////////////////////////
  int  kount = numNilValues();
  StatutilWrapper::replaceNilx(_nx, _ny, _pointer);
/////////////////////////////////
  findLimits();
  sprintf(msg, "replaced %d nils with interpolated values", kount);
  _history->addHistoryCard(msg);
  return FALSE;
}



//----------------- interp nils in x direction ------------------------//
//----------------- interp nils in x direction ------------------------//
//----------------- interp nils in x direction ------------------------//

       // public.

int  StaticKernal::interpNilsInXdirection(int testing, char *msg)
{
  if(allStaticValuesAreLive(msg)) return TRUE;
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(testing)
      {
      sprintf(msg, "interpolating up to possibly %d nils in X direction...",
                                        numNilValues());
      return FALSE;
      }
/////////////////////////////////
  int  kount = numNilValues();
  StatutilWrapper::replaceNilxOnly(_nx, _ny, _pointer);
/////////////////////////////////
  findLimits();
  kount -= numNilValues();
  sprintf(msg, "replaced %d nils with interpolated values in X direction",
                                                       kount);
  _history->addHistoryCard(msg);
  return FALSE;
}



//----------------- interp nils in y direction ------------------------//
//----------------- interp nils in y direction ------------------------//
//----------------- interp nils in y direction ------------------------//

       // public.

int  StaticKernal::interpNilsInYdirection(int testing, char *msg)
{
  if(allStaticValuesAreLive(msg)) return TRUE;
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(testing)
      {
      sprintf(msg, "interpolating up to possibly %d nils in Y direction...",
                                        numNilValues());
      return FALSE;
      }
/////////////////////////////////
  int  kount = numNilValues();
  StatutilWrapper::replaceNilyOnly(_nx, _ny, _pointer);
/////////////////////////////////
  findLimits();
  kount -= numNilValues();
  sprintf(msg, "replaced %d nils with interpolated values in Y direction",
                                                       kount);
  _history->addHistoryCard(msg);
  return FALSE;
}



//----------------- interp nils from nearby ---------------------------//
//----------------- interp nils from nearby ---------------------------//
//----------------- interp nils from nearby ---------------------------//

       // public.

int  StaticKernal::interpNilsFromNearby
                                (int testing,
                                 int  ixdist, int  iydist, int require,
                                 char *msg)
{
  if(allStaticValuesAreLive(msg)) return TRUE;
  if(allStaticValuesAreNil(msg)) return TRUE;
  if(testing)
      {
      sprintf(msg, "interpolating up to possibly %d nils from nearby...",
                                        numNilValues());
      return FALSE;
      }
/////////////////////////////////
  int  kount = numNilValues();
  StatutilWrapper::replaceNearbyNils
                         (_nx, _ny, _pointer, ixdist, iydist, require);
/////////////////////////////////
  findLimits();
  kount -= numNilValues();
  sprintf(msg, "replaced %d nils with values interpolated from nearby",
                                                       kount);
  _history->addHistoryCard(msg);
  return FALSE;
}



//----------------- transform ground positions ---------------------------//
//----------------- transform ground positions ---------------------------//
//----------------- transform ground positions ---------------------------//

       // public.
       // sets *reversing to TRUE or FALSE.

int  StaticKernal::transformGroundPositions
                       (int testing,
                        float x1old, float x2old, float y1old, float y2old,
                        float x1new, float x2new, float y1new, float y2new,
                        int *reversing,
                        char *msg)
{
  *reversing = FALSE;
  if(x1old == x2old)
      {
      strcpy(msg, "both old X ground positions cannot be the same");
      return TRUE;
      }
  if(y1old == y2old)
      {
      strcpy(msg, "both old Y ground positions cannot be the same");
      return TRUE;
      }
  if(x1new == x2new)
      {
      strcpy(msg, "both new X ground positions cannot be the same");
      return TRUE;
      }
  if(y1new == y2new)
      {
      strcpy(msg, "both new Y ground positions cannot be the same");
      return TRUE;
      }
  float xslope = (x2new - x1new) / (x2old - x1old);
  float yslope = (y2new - y1new) / (y2old - y1old);
  float old_x1   = _x1;
  float old_y1   = _y1;
  float old_xinc = _xinc;
  float old_yinc = _yinc;
  float new_x1   = x1new + (old_x1 - x1old) * xslope;
  float new_y1   = y1new + (old_y1 - y1old) * yslope;
  float new_xinc = old_xinc * xslope;
  float new_yinc = old_yinc * yslope;
  if(old_x1   == new_x1   && old_y1   == new_y1   &&
     old_xinc == new_xinc && old_yinc == new_yinc)
      {
      strcpy(msg, "these parameters define a do-nothing operation");
      return TRUE;
      }
  *reversing = (xslope < 0.0 || yslope < 0.0);
  if(testing)
      {
      if(*reversing) strcpy(msg,
              "transforming ground positions (and reversing the data)...");
      else           strcpy(msg, "transforming ground positions...");
      return FALSE;
      }
/////////////////////////////////
  if(*reversing)
      {
      int notneed = (_pointer == NULL);
      allocPointerIfNecessary();
      StatutilWrapper::reverse
              (&new_x1, &new_y1, &new_xinc, &new_yinc, _nx, _ny, _pointer);
      if(notneed) freePointer();
      }
  _x1   = new_x1;
  _y1   = new_y1;
  _xinc = new_xinc;
  _yinc = new_yinc;
/////////////////////////////////
  sprintf(msg, "transform old:  x1 %f  xinc %f  y1 %f  yinc %f",
                              old_x1, old_xinc, old_y1, old_yinc);
  _history->addHistoryCard(msg);
  sprintf(msg, "transform new:  x1 %f  xinc %f  y1 %f  yinc %f",
                              new_x1, new_xinc, new_y1, new_yinc);
  _history->addHistoryCard(msg);
  strcpy(msg, "ground positions successfully transformed");
  if(*reversing) strcat(msg, " (and data reversed)");
  return FALSE;
}



//-------------------- resample static values -------------------------//
//-------------------- resample static values -------------------------//
//-------------------- resample static values -------------------------//

       // public.

int  StaticKernal::resampleStaticValues
                       (int testing,
                        int interp, int extrap,
                        float   x1_new, float   y1_new,
                        float xinc_new, float yinc_new,
                        int     nx_new, int     ny_new,
                        char *msg)
{
  assert(interp == INTERP_NEAR || interp == INTERP_TERP);
  assert(extrap == EXTRAP_EDGE || extrap == EXTRAP_NILS
                               || extrap == EXTRAP_ZERO);
  if(xinc_new <= 0.0 || yinc_new <= 0.0)
      {
      strcpy(msg, "illegal values for new xinc or yinc - must exceed zero");
      return TRUE;
      }
  if(nx_new < 1 || ny_new < 1)
      {
      strcpy(msg, "illegal values for new nx or ny - must exceed zero");
      return TRUE;
      }
  float   x1_old = _x1;
  float   y1_old = _y1;
  float xinc_old = _xinc;
  float yinc_old = _yinc;
  int     nx_old = _nx;
  int     ny_old = _ny;
  if(  x1_new ==   x1_old &&   y1_new ==   y1_old &&
     xinc_new == xinc_old && yinc_new == yinc_old &&
       nx_new ==   nx_old &&   ny_new ==   ny_old)
      {
      strcpy(msg, "these parameters define a do-nothing operation");
      return TRUE;
      }
  if(testing)
      {
      strcpy(msg, "resampling ground positions...");
      return FALSE;
      }
/////////////////////////////////
  if(_pointer)
      {
      int  nalloc = nx_new * ny_new;
      float *pointer2 = new float [nalloc];
      for(int  iy = 0; iy < ny_new; iy++)
          {
          for(int  ix = 0; ix < nx_new; ix++)
              {
              float xbin = x1_new + ix * xinc_new;
              float ybin = y1_new + iy * yinc_new;
              int  ival = get_combo_index(ix, iy, nx_new, ny_new);
              pointer2[ival] = getResampledValue(xbin, ybin, interp, extrap);
              }
          }
      freePointer();
      _pointer = pointer2;
      }
  _x1   = x1_new;
  _y1   = y1_new;
  _xinc = xinc_new;
  _yinc = yinc_new;
  _nx   = nx_new;
  _ny   = ny_new;
/////////////////////////////////
  findLimits();
  doReset();
  sprintf(msg, "resample old: x1 %d xinc %d nx %d y1 %d yinc %d ny %d",
           NearestInteger(x1_old), NearestInteger(xinc_old), nx_old,
           NearestInteger(y1_old), NearestInteger(yinc_old), ny_old);
  _history->addHistoryCard(msg);
  sprintf(msg, "resample new: x1 %d xinc %d nx %d y1 %d yinc %d ny %d",
           NearestInteger(x1_new), NearestInteger(xinc_new), nx_new,
           NearestInteger(y1_new), NearestInteger(yinc_new), ny_new);
  _history->addHistoryCard(msg);
  strcpy(msg, "resample method:  ");
  if(interp == INTERP_NEAR) strcat(msg, "use nearest values  ");
  else                      strcat(msg, "interpolate  ");
  if     (extrap == EXTRAP_EDGE) strcat(msg, "(extrapolate with edge values)");
  else if(extrap == EXTRAP_NILS) strcat(msg, "(extrapolate with nils)");
  else                           strcat(msg, "(extrapolate with zero)");
  _history->addHistoryCard(msg);
  strcpy(msg, "successfully resampled ground positions");
  return FALSE;
}



//------------------------- read and save file -----------------------------//
//------------------------- read and save file -----------------------------//
//------------------------- read and save file -----------------------------//

     // public.


int  StaticKernal::readForeign (const char *filename,
                                StatioWrapper *statio, char *msg,
                                int skipstat)
{
  startReset();
  int error = privateReadForeign(filename, statio, msg, skipstat);
  findLimits();
  finishReset();
  return error;
}



int  StaticKernal::saveFile (const char *filename,
                             StatioWrapper *statio, char *msg)
{
  return privateSaveFile(filename, statio, msg);
}



//---------------- read and save file (convenience functions) --------------//
//---------------- read and save file (convenience functions) --------------//
//---------------- read and save file (convenience functions) --------------//

     // public.


int  StaticKernal::readFile (const char *filename, char *msg, int skipstat)
{
  StatioWrapper *statio = new StatioWrapper(0);
  int error = statio->validateFile(filename, msg);
  if(error)
      {
      delete statio;
      return FALSE;
      }
  statio->setActiveHeaderSection("static");
  error = readForeign (filename, statio, msg, skipstat);
  delete statio;
  return error;
}



int  StaticKernal::saveFile (const char *filename, char *msg,
                             const char *encoding, int skiphist)
{
  StatioWrapper *statio = new StatioWrapper(1);
  statio->initializeOutputParameters();
  if(encoding)
      {
      statio->setEncoding(encoding);
      }
  int error = privateSaveFile (filename, statio, msg, skiphist);
  delete statio;
  return error;
}



//---------------------- private read foreign ---------------------------//
//---------------------- private read foreign ---------------------------//
//---------------------- private read foreign ---------------------------//


int  StaticKernal::privateReadForeign (const char *filename,
                                       StatioWrapper *statio, char *msg,
                                       int skipstat)
{
  int error = statio->verifyParameters(msg);
  if(error) return 1;

  if(skipstat)
      {
      freePointer();
      }
  else
      {
      int nx = statio->getScalarInteger("nx");
      int ny = statio->getScalarInteger("ny");

      float *statics = new float [nx*ny + 1];

      int err;
      statio->readForeign(filename, statics, &err, msg);

      if(err != StatioWrapper::STATUS_OK)
           {
           delete [] statics;
           return 1;
           }

      freePointer();
      _pointer = statics;
      }

  const char *stattype = statio->getScalarString ("stattype");
  _nhx                 = statio->getScalarInteger("nhx"     );
  _nhy                 = statio->getScalarInteger("nhy"     );
  _nhx2                = statio->getScalarInteger("nhx2"    );
  _nhy2                = statio->getScalarInteger("nhy2"    );
  _x1                  = statio->getScalarFloat  ("x1"      );
  _y1                  = statio->getScalarFloat  ("y1"      );
  _xinc                = statio->getScalarFloat  ("xinc"    );
  _yinc                = statio->getScalarFloat  ("yinc"    );
  _nx                  = statio->getScalarInteger("nx"      );
  _ny                  = statio->getScalarInteger("ny"      );

  setStattype(stattype);

  statio->getHistoryCardsFromPjar(_history);
  return 0;
}


//------------------------- private save file ---------------------------//
//------------------------- private save file ---------------------------//
//------------------------- private save file ---------------------------//


int  StaticKernal::privateSaveFile (const char *filename,
                                    StatioWrapper *statio, char *msg,
                                    int skiphist)
{
  statio->setScalarString ("stattype" , _stattype);
  statio->setScalarInteger("nhx"      , _nhx);
  statio->setScalarInteger("nhy"      , _nhy);
  statio->setScalarInteger("nhx2"     , _nhx2);
  statio->setScalarInteger("nhy2"     , _nhy2);
  statio->setScalarFloat  ("x1"       , _x1);
  statio->setScalarFloat  ("y1"       , _y1);
  statio->setScalarFloat  ("xinc"     , _xinc);
  statio->setScalarFloat  ("yinc"     , _yinc);
  statio->setScalarInteger("nx"       , _nx);
  statio->setScalarInteger("ny"       , _ny);

  statio->putHistoryCardsIntoPjar(_history, skiphist);

  int notneed = (_pointer == NULL);
  allocPointerIfNecessary();

  int err;
  statio->writeFile(filename, _pointer, &err, msg);

  if(notneed) freePointer();

  return (err != StatioWrapper::STATUS_OK);
}



//------------------------- update pjar -------------------------------------//
//------------------------- update pjar -------------------------------------//
//------------------------- update pjar -------------------------------------//


void StaticKernal::updatePjar (StatioWrapper *statio, int skiphist)
{
  statio->setScalarString ("stattype" , _stattype);
  statio->setScalarInteger("nhx"      , _nhx);
  statio->setScalarInteger("nhy"      , _nhy);
  statio->setScalarInteger("nhx2"     , _nhx2);
  statio->setScalarInteger("nhy2"     , _nhy2);
  statio->setScalarFloat  ("x1"       , _x1);
  statio->setScalarFloat  ("y1"       , _y1);
  statio->setScalarFloat  ("xinc"     , _xinc);
  statio->setScalarFloat  ("yinc"     , _yinc);
  statio->setScalarInteger("nx"       , _nx);
  statio->setScalarInteger("ny"       , _ny);

  statio->putHistoryCardsIntoPjar(_history, skiphist);

  statio->augmentOutputParameters();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

