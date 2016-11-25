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
/******************************************************************************/
/**********                COPYRIGHT CONOCO INC 1998                 **********/
/**********        PROPRIETARY AND CONFIDENTIAL TO CONOCO INC        **********/
/**********  PROTECTED BY THE COPYRIGHT LAWS AS AN UNPUBLISHED WORK  **********/
/******************************************************************************/

//-------------------------- vf_kernal.cc ----------------------------//
//-------------------------- vf_kernal.cc ----------------------------//
//-------------------------- vf_kernal.cc ----------------------------//

//            implementation file for the VfKernal class
//                    not derived from any class
//                         subdirectory vf


#include "c2f_interface.h"
#include "vf/vf_kernal.hh"
#include "vf/vf_function_array.hh"
#include "vf/vf_function_select.hh"
#include "vf/vf_function.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "cprim.h"
#include "trslib.h"
#include "named_constants.h"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


       // VfInformer is needed only for showing working messages.


#define MIGHT  _array->neighborsMightHaveChanged();


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfKernal::VfKernal (VfInformer *informer, VfUtilities *utilities)
           :
        _informer          (informer),
        _utilities         (utilities),
        _array             (NULL),
        _select            (NULL),
        _nhx               (7),
        _nhy               (8),
        _order             (2),
        _nhosign           (1.0),
        _nhoexp            (2.0),
        _dunits            (UNSPECIFIED_UNITS),
        _name              (str_newstr("none")),
        _attname           (str_newstr("none")),
        _attunits          (str_newstr("none")),
        _tdunits           (str_newstr("none")),
        _xprev             (-1),
        _xnext             (-1),
        _yprev             (-1),
        _ynext             (-1),
        _NPICKS            (-1),
        _IFUN1             (-1),
        _IFUN2             (-1),
        _IFUN3             (-1),
        _IFUN4             (-1),
        _ORDINATES1        (NULL),
        _ORDINATES2        (NULL),
        _ORDINATES3        (NULL),
        _ORDINATES4        (NULL)
/**************
        _MULTIPLE          (FALSE),
        _IFUN1             (-1),
        _IFUN2             (-1),
        _IFUN3             (-1),
        _IFUN4             (-1)
**************/
{
  assert(_informer);
  assert(_utilities);
  _array   = new VfFunctionArray  (_utilities);
  _select  = new VfFunctionSelect (_array);
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

VfKernal::~VfKernal()
{
  delete _select;
  delete _array;
  free(_name);
  free(_attname);
  free(_attunits);
  free(_tdunits);
}



//----------------- new bin tolerances by vf dataset only ---------------//
//----------------- new bin tolerances by vf dataset only ---------------//
//----------------- new bin tolerances by vf dataset only ---------------//

       // public.

void VfKernal::newBinTolerancesByVfDatasetOnly()
{
  MIGHT
}



//------------------- replace velocity functions ----------------------//
//------------------- replace velocity functions ----------------------//
//------------------- replace velocity functions ----------------------//

       // public.

void VfKernal::replaceVelocityFunctions(const char *working,
                                        const VfKernal *storage)
{
  deleteAllVelocityFunctions();
  appendVelocityFunctions(working, storage);
}



//------------------- append velocity functions ----------------------//
//------------------- append velocity functions ----------------------//
//------------------- append velocity functions ----------------------//

       // public.

void VfKernal::appendVelocityFunctions(const char *working,
                                       const VfKernal *storage)
{
  setNhx           (storage->getNhx());
  setNhy           (storage->getNhy());
  setMoveoutOrder  (storage->getMoveoutOrder());
  setDistanceUnits (storage->getDistanceUnits());
  setName          (storage->getName());
  setAttributeName (storage->getAttributeName());
  setAttributeUnits(storage->getAttributeUnits());
  setTimeDepthUnits(storage->getTimeDepthUnits());
  long nfun  = storage->numVelocityFunctions();
  long nfun2 =          numVelocityFunctions();
  appendNumVelocityFunctions(nfun);
  beforeSettingSeveralSelectFlags();
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(working) _informer->showWorkingMessage(working, ifun, nfun);
      VfFunction *velfun1 = storage->velfun(ifun);
      VfFunction *velfun2 =          velfun(ifun + nfun2);
      velfun2->copyVelocityFunctionContents(velfun1);
      }
  afterSettingSeveralSelectFlags();
  if(nfun2 == 0)
      {
      setActiveVelocityFunction   (storage->getActiveVelocityFunction   ());
      setReferenceVelocityFunction(storage->getReferenceVelocityFunction());
      }
  MIGHT
}



//--------------------------- get values -----------------------------//
//--------------------------- get values -----------------------------//
//--------------------------- get values -----------------------------//

       // public.

const char *VfKernal::getSymbolFromUnits()  const
{
  return _utilities->getSymbolFromUnits(_dunits);
}



//--------------------------- set values -----------------------------//
//--------------------------- set values -----------------------------//
//--------------------------- set values -----------------------------//

       // public.

void VfKernal::setNhx(int value)
{
  if(value >= 1 ) _nhx = value;
  else            _nhx = 7;
}


void VfKernal::setNhy(int value)
{
  if(value >= 1) _nhy = value;
  else           _nhy = 8;
}


void VfKernal::setMoveoutOrder(int value)
{
  _nhosign = _utilities->deriveNhosign       (value);
  _nhoexp  = _utilities->deriveNhoexp        (value);
  _order   = _utilities->deriveMoveoutOrder  (_nhosign, _nhoexp);
}


void VfKernal::setMoveoutOrder(float nhosign, float nhoexp)
{
  _order   = _utilities->deriveMoveoutOrder  (nhosign, nhoexp);
  _nhosign = _utilities->deriveNhosign       (_order);
  _nhoexp  = _utilities->deriveNhoexp        (_order);
}


void VfKernal::setDistanceUnits(int dunits)
{
  assert(dunits == FEET_PER_SECOND || METERS_PER_SECOND || UNSPECIFIED_UNITS);
  _dunits = dunits;
}


void VfKernal::setUnitsFromSymbol (const char *value)
{
  int dunits = _utilities->getUnitsFromSymbol(value);
  if(dunits != UNSPECIFIED_UNITS) _dunits = dunits;
}


void VfKernal::setName(const char *value)
{
  assert(value);
  free(_name);
  _name = str_newstr(value);
}


void VfKernal::setAttributeName(const char *value)
{
  assert(value);
  free(_attname);
  _attname = str_newstr(value);
}


void VfKernal::setAttributeUnits(const char *value)
{
  assert(value);
  free(_attunits);
  _attunits = str_newstr(value);
}


void VfKernal::setTimeDepthUnits(const char *value)
{
  assert(value);
  free(_tdunits);
  _tdunits = str_newstr(value);
}



//------------------- get minimum and maximum values ------------------//
//------------------- get minimum and maximum values ------------------//
//------------------- get minimum and maximum values ------------------//

       // public.

float  VfKernal::minimumXloc() { return _array->minimumXloc(); }
float  VfKernal::maximumXloc() { return _array->maximumXloc(); }

float  VfKernal::minimumYloc() { return _array->minimumYloc(); }
float  VfKernal::maximumYloc() { return _array->maximumYloc(); }


float VfKernal::minimumXbinCenter()
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return 0.0;
  float value = minimumXloc();
  return _utilities->xbinCenter(value);
}


float VfKernal::maximumXbinCenter()
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return 0.0;
  float value = maximumXloc();
  return _utilities->xbinCenter(value);
}


float VfKernal::minimumYbinCenter()
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return 0.0;
  float value = minimumYloc();
  return _utilities->ybinCenter(value);
}


float VfKernal::maximumYbinCenter()
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return 0.0;
  float value = maximumYloc();
  return _utilities->ybinCenter(value);
}


float  VfKernal::minimumAbscissa(int type)
{ return _array->minimumAbscissa    (type); }

float  VfKernal::maximumAbscissa(int type)
{ return _array->maximumAbscissa    (type); }


float  VfKernal::minimumOrdinate(int type)
{ return _array->minimumOrdinate    (type); }

float  VfKernal::maximumOrdinate(int type)
{ return _array->maximumOrdinate    (type); }


float  VfKernal::minimumDepth() { return _array->minimumDepth(); }
float  VfKernal::maximumDepth() { return _array->maximumDepth(); }

float  VfKernal::minimumTime() { return _array->minimumTime(); }
float  VfKernal::maximumTime() { return _array->maximumTime(); }

float  VfKernal::minimumVrms() { return _array->minimumVrms(); }
float  VfKernal::maximumVrms() { return _array->maximumVrms(); }

float  VfKernal::minimumVav() { return _array->minimumVav(); }
float  VfKernal::maximumVav() { return _array->maximumVav(); }

float  VfKernal::minimumVint() { return _array->minimumVint(); }
float  VfKernal::maximumVint() { return _array->maximumVint(); }

float  VfKernal::minimumVnmo() { return _array->minimumVnmo(); }
float  VfKernal::maximumVnmo() { return _array->maximumVnmo(); }

float  VfKernal::minimumThickness() { return _array->minimumThickness(); }
float  VfKernal::maximumThickness() { return _array->maximumThickness(); }

float  VfKernal::minimumOffset() { return _array->minimumOffset(); }
float  VfKernal::maximumOffset() { return _array->maximumOffset(); }



//--------------------- get minimum/maximum time/depth -------------------//
//--------------------- get minimum/maximum time/depth -------------------//
//--------------------- get minimum/maximum time/depth -------------------//

   // public.

float VfKernal::getMinimumTime  (float depth)  const
{
  if(depth < 0.0) depth = 0.0;
  long nfun = numVelocityFunctions();
  float minimum = FNIL;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      float value = velfun(ifun)->getInterpolatedTime(depth);
      if(value == 0.0) continue;
      if(minimum == FNIL || value < minimum) minimum = value;
      }
  if(minimum == FNIL) return 0.0;
  return minimum;
}


float VfKernal::getMaximumTime  (float depth)  const
{
  if(depth < 0.0) depth = 0.0;
  long nfun = numVelocityFunctions();
  float maximum = FNIL;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      float value = velfun(ifun)->getInterpolatedTime(depth);
      if(value == 0.0) continue;
      if(maximum == FNIL || value > maximum) maximum = value;
      }
  if(maximum == FNIL) return 0.0;
  return maximum;
}


float VfKernal::getMinimumDepth (float  time)  const
{
  if(time < 0.0) time = 0.0;
  long nfun = numVelocityFunctions();
  float minimum = FNIL;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      float value = velfun(ifun)->getInterpolatedDepth(time);
      if(value == 0.0) continue;
      if(minimum == FNIL || value < minimum) minimum = value;
      }
  if(minimum == FNIL) return 0.0;
  return minimum;
}


float VfKernal::getMaximumDepth (float  time)  const
{
  if(time < 0.0) time = 0.0;
  long nfun = numVelocityFunctions();
  float maximum = FNIL;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      float value = velfun(ifun)->getInterpolatedDepth(time);
      if(value == 0.0) continue;
      if(maximum == FNIL || value > maximum) maximum = value;
      }
  if(maximum == FNIL) return 0.0;
  return maximum;
}



//----------------------- find matching velfun ----------------------//
//----------------------- find matching velfun ----------------------//
//----------------------- find matching velfun ----------------------//

   // public.
   // returns index of matching velocity function.
   // xloc and yloc designate coordinates within the desired bin.
   // if there are two or more functions in matching bin, returns
   //   the one closest to xloc in the x direction.
   // returns -1 if there are no velocity functions in the matching bin.
   // returns -1 if there are no velocity functions at all.

long VfKernal::findMatchingVelfun (float xloc, float yloc)  const
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return -1;
  float distance    = 0.0;
  long  ifun        = -1;
  long  xbin_number = _utilities->xbinNumber(xloc);
  long  ybin_number = _utilities->ybinNumber(yloc);

  for(long ifun2 = 0; ifun2 < nfun; ifun2++)
      {
      float xloc2 = velfun(ifun2)->getXloc();
      float yloc2 = velfun(ifun2)->getYloc();
      long xbin_number2 = _utilities->xbinNumber(xloc2);
      long ybin_number2 = _utilities->ybinNumber(yloc2);
      if(xbin_number2 == xbin_number && ybin_number2 == ybin_number)
           {
           float distance2 = fabs(xloc2 - xloc);
           if(ifun == -1 || distance2 < distance)
                {
                distance = distance2;
                ifun     = ifun2;
                }
           }
      }
  return ifun;
}

 

//----------------------- find nearest velfun ----------------------//
//----------------------- find nearest velfun ----------------------//
//----------------------- find nearest velfun ----------------------//
 
   // public.
   // returns index of velocity function nearest to (xloc,yloc).
   // returns -1 if there are no velocity functions.
   // uses xperpix and yperpix to normalize x and y distance scales by
   //   dividing by (utilities()->getXwidth() * xperpix) (and same for y).

long VfKernal::findNearestVelfun (float xloc, float yloc,
                                  float xperpix, float yperpix)  const
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return -1;
  float xdenom   = _utilities->getXwidth() * xperpix;
  float ydenom   = _utilities->getYwidth() * yperpix;
  float distance = 0.0;
  long  ifun     = -1;

  for(long ifun2 = 0; ifun2 < nfun; ifun2++)
      {
      float x = velfun(ifun2)->getXloc();
      float y = velfun(ifun2)->getYloc();
      float xdist = fabs(x - xloc) / xdenom;
      float ydist = fabs(y - yloc) / ydenom;
      float distance2 = xdist * xdist + ydist * ydist;
      if(ifun == -1 || distance2 < distance)
          {
          distance = distance2;
          ifun     = ifun2;
          }
      }
  return ifun;
}

 
//----------------------- find nearest velfuns ----------------------//
//----------------------- find nearest velfuns ----------------------//
//----------------------- find nearest velfuns ----------------------//
 
   // public.
   // returns indices of four velocity functions nearest to (xloc,yloc)
   //   (one function in each of four directions), and the corresponding
   //   weights to use for interpolating between these functions to create
   //   a function at (xloc,yloc).
   // velocity functions with errors corresponding to the specified type
   //   are ignored.
   // if there are no usable velocity functions, sets all indices to -1
   //   and all weights to zero.  otherwise, always sets all indices >= 0
   //   with weights which add to 1.0.
   // if there are less than four useable velocity functions, some of the
   //   indices will be duplicated and some of the weights will be zero.
   // uses xnorm and ynorm to normalize x and y distance scales by
   //   dividing by them.

void VfKernal::findNearestVelfuns (float xloc, float yloc,
                                   float xnorm, float ynorm,
                                   long *ifun1, long *ifun2,
                                   long *ifun3, long *ifun4,
                                   float *weight1, float *weight2,
                                   float *weight3, float *weight4,
                                   int type)  const
{
  assert(xnorm > 0.0 && ynorm > 0.0);
  *weight1 = 0.0; *ifun1 = -1;
  *weight2 = 0.0; *ifun2 = -1;
  *weight3 = 0.0; *ifun3 = -1;
  *weight4 = 0.0; *ifun4 = -1;
  long nfun = numVelocityFunctions();
  if(nfun == 0) return;
  *ifun1 = 0;
  *ifun2 = 0;
  *ifun3 = 0;
  *ifun4 = 0;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(velfun(ifun)->typeError(type)) continue;
      float x = (velfun(ifun)->getXloc() - xloc) / xnorm;
      float y = (velfun(ifun)->getYloc() - yloc) / ynorm;
      float distance = sqrt(x * x + y * y);
      if(distance < 0.00001)
          {
          *weight1 = 1.0; *ifun1 = ifun;
          *weight2 = 0.0; *ifun2 = ifun;
          *weight3 = 0.0; *ifun3 = ifun;
          *weight4 = 0.0; *ifun4 = ifun;
          return;
          }
      float weight = 1.0 / distance;
      if(x < 0.0 && y < 0.0)
          {
          if(weight > *weight1) { *weight1 = weight; *ifun1 = ifun; }
          }
      else if(x < 0.0 && y >= 0.0)
          {
          if(weight > *weight2) { *weight2 = weight; *ifun2 = ifun; }
          }
      else if(x >= 0.0 && y < 0.0)
          {
          if(weight > *weight3) { *weight3 = weight; *ifun3 = ifun; }
          }
      else // if(x >= 0.0 && y >= 0.0)
          {
          if(weight > *weight4) { *weight4 = weight; *ifun4 = ifun; }
          }
      }
  float sum = *weight1 + *weight2 + *weight3 + *weight4;
  if(sum == 0.0) return;
  assert(sum > 0.0);
  *weight1 /= sum;
  *weight2 /= sum;
  *weight3 /= sum;
  *weight4 /= sum;
}


 
//------------------------ check sort -------------------------------//
//------------------------ check sort -------------------------------//
//------------------------ check sort -------------------------------//

        // public;
        // sets *xdir to  2 if xbins are sorted to ascending order.
        // sets *xdir to -2 if xbins are sorted to descending order.
        // sets *xdir to  1 if xbins are not sorted but initially ascending.
        // sets *xdir to -1 if xbins are not sorted but initially descending.
        // sets *xdir to  0 if no velocity functions or all xbins are the same.

        // sets *ydir to  2 if ybins are sorted to ascending order.
        // sets *ydir to -2 if ybins are sorted to descending order.
        // sets *ydir to  1 if ybins are not sorted but initially ascending.
        // sets *ydir to -1 if ybins are not sorted but initially descending.
        // sets *ydir to  0 if no velocity functions or all ybins are the same.

        // sets *xfast to  1 if sorted with X fastest.
        // sets *xfast to -1 if sorted with Y fastest.
        // sets *xfast to  0 if no velocity functions or not sorted or
                                                // both sorts are satisfied.

        // returns sorted = TRUE if both x and y are sorted.
        // otherwise returns sorted = FALSE.

int VfKernal::checkSort (int *xdir, int *ydir, int *xfast)  const
{
  long nfun = numVelocityFunctions();
  *xdir  = 0;          // initially unknown X sort direction.
  *ydir  = 0;          // initially unknown Y sort direction.
  *xfast = 0;          // initially unknown whether sorted with X or Y fastest.
  if(nfun <= 1) return TRUE;

  int xdirX = 0;       // initially unknown X sort direction assuming X fastest.
  int ydirX = 0;       // initially unknown Y sort direction assuming X fastest.
  int xdirY = 0;       // initially unknown X sort direction assuming Y fastest.
  int ydirY = 0;       // initially unknown Y sort direction assuming Y fastest.

  for(long ifun = 1; ifun < nfun; ifun++)
      {
      float xloc1 = velfun(ifun-1)->getXloc();
      float yloc1 = velfun(ifun-1)->getYloc();
      float xloc2 = velfun(ifun  )->getXloc();
      float yloc2 = velfun(ifun  )->getYloc();
      long ixbin1 = _utilities->xbinNumber(xloc1);
      long iybin1 = _utilities->ybinNumber(yloc1);
      long ixbin2 = _utilities->xbinNumber(xloc2);
      long iybin2 = _utilities->ybinNumber(yloc2);

      if(iybin1 == iybin2)     // assume X changes fastest.
          {
          if     (xdirX ==  0 && ixbin2 > ixbin1) xdirX =  2;
          else if(xdirX ==  0 && ixbin2 < ixbin1) xdirX = -2;
          else if(xdirX ==  2 && ixbin2 < ixbin1) xdirX =  1;
          else if(xdirX == -2 && ixbin2 > ixbin1) xdirX = -1;
          }
      else                     // assume X changes fastest.
          {
          if     (ydirX ==  0 && iybin2 > iybin1) ydirX =  2;
          else if(ydirX ==  0 && iybin2 < iybin1) ydirX = -2;
          else if(ydirX ==  2 && iybin2 < iybin1) ydirX =  1;
          else if(ydirX == -2 && iybin2 > iybin1) ydirX = -1;
          }

      if(ixbin1 == ixbin2)     // assume Y changes fastest.
          {
          if     (ydirY ==  0 && iybin2 > iybin1) ydirY =  2;
          else if(ydirY ==  0 && iybin2 < iybin1) ydirY = -2;
          else if(ydirY ==  2 && iybin2 < iybin1) ydirY =  1;
          else if(ydirY == -2 && iybin2 > iybin1) ydirY = -1;
          }
      else                     // assume Y changes fastest.
          {
          if     (xdirY ==  0 && ixbin2 > ixbin1) xdirY =  2;
          else if(xdirY ==  0 && ixbin2 < ixbin1) xdirY = -2;
          else if(xdirY ==  2 && ixbin2 < ixbin1) xdirY =  1;
          else if(xdirY == -2 && ixbin2 > ixbin1) xdirY = -1;
          }
      }

  int sortedX = TRUE;
  if(xdirX == 1 || xdirX == -1) sortedX = FALSE;
  if(ydirX == 1 || ydirX == -1) sortedX = FALSE;

  int sortedY = TRUE;
  if(xdirY == 1 || xdirY == -1) sortedY = FALSE;
  if(ydirY == 1 || ydirY == -1) sortedY = FALSE;

  if (sortedX && sortedY) { *xdir = xdirX; *ydir = ydirX; *xfast =  0; }
  else if       (sortedX) { *xdir = xdirX; *ydir = ydirX; *xfast =  1; }
  else if       (sortedY) { *xdir = xdirY; *ydir = ydirY; *xfast = -1; }
  else                    { *xdir = xdirX; *ydir = ydirX; *xfast =  0; }

  int sorted = TRUE;
  if(*xdir == 1 || *xdir == -1) sorted = FALSE;
  if(*ydir == 1 || *ydir == -1) sorted = FALSE;
  return sorted;
}



//------------------------ check xdirection -------------------------------//
//------------------------ check xdirection -------------------------------//
//------------------------ check xdirection -------------------------------//

     // public.
     // returns  2 if xbins are sorted to ascending order.
     // returns -2 if xbins are sorted to descending order.
     // returns  1 if xbins are not sorted but initially ascending.
     // returns -1 if xbins are not sorted but initially descending.
     // returns  0 if no velocity functions or all xbins are the same.
     // it is assumed that x changes faster than y.

int VfKernal::checkXdirection ()  const
{
  long nfun = numVelocityFunctions();
  int  xdir = 0;

  for(long ifun = 1; ifun < nfun; ifun++)
      {
      float xloc1 = velfun(ifun-1)->getXloc();
      float yloc1 = velfun(ifun-1)->getYloc();
      float xloc2 = velfun(ifun  )->getXloc();
      float yloc2 = velfun(ifun  )->getYloc();
      long ixbin1 = _utilities->xbinNumber(xloc1);
      long iybin1 = _utilities->ybinNumber(yloc1);
      long ixbin2 = _utilities->xbinNumber(xloc2);
      long iybin2 = _utilities->ybinNumber(yloc2);

      if(iybin1 == iybin2)
          {
          if     (xdir ==  0 && ixbin2 > ixbin1) xdir =  2;
          else if(xdir ==  0 && ixbin2 < ixbin1) xdir = -2;
          else if(xdir ==  2 && ixbin2 < ixbin1) xdir =  1;
          else if(xdir == -2 && ixbin2 > ixbin1) xdir = -1;
          }
      }
  return xdir;
}



//------------------------ check ydirection -------------------------------//
//------------------------ check ydirection -------------------------------//
//------------------------ check ydirection -------------------------------//

     // public.
     // returns  2 if ybins are sorted to ascending order.
     // returns -2 if ybins are sorted to descending order.
     // returns  1 if ybins are not sorted but initially ascending.
     // returns -1 if ybins are not sorted but initially descending.
     // returns  0 if no velocity functions or all ybins are the same.
     // it is assumed that x changes faster than y.

int VfKernal::checkYdirection ()  const
{
  long nfun = numVelocityFunctions();
  int  ydir = 0;

  for(long ifun = 1; ifun < nfun; ifun++)
      {
      float yloc1 = velfun(ifun-1)->getYloc();
      float yloc2 = velfun(ifun  )->getYloc();
      long iybin1 = _utilities->ybinNumber(yloc1);
      long iybin2 = _utilities->ybinNumber(yloc2);

      if(iybin1 != iybin2)
          {
          if     (ydir ==  0 && iybin2 > iybin1) ydir =  2;
          else if(ydir ==  0 && iybin2 < iybin1) ydir = -2;
          else if(ydir ==  2 && iybin2 < iybin1) ydir =  1;
          else if(ydir == -2 && iybin2 > iybin1) ydir = -1;
          }
      }
  return ydir;
}




//------------------------- find nearby location --------------------------//
//------------------------- find nearby location --------------------------//
//------------------------- find nearby location --------------------------//

  // public.
  // find the previous or next velocity function location.
  // ifun = index of current velocity function location.
  // direction >= 0 returns index of   next   velocity function.
  // direction <  0 returns index of previous velocity function.
  // findNearbyXloc returns index of next/prev x location with same ybin.
  // findNearbyYloc returns index of next/prev y location with same xbin.
  // returns -1 if no such location is found.
  // the velocity functions can be in any order.

long VfKernal::findNearbyXloc (long ifun, int direction)  const
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return -1;
  if(ifun < 0 || ifun >= nfun) return -1;

  float yloc        = velfun(ifun)->getYloc();
  float xloc        = velfun(ifun)->getXloc();
  long  ybin_number = _utilities->ybinNumber(yloc);
  long  which       = -1;
  float distance    = 0.0;

  for(long ifun2 = 0; ifun2 < nfun; ifun2++)
       {
       float yloc2        = velfun(ifun2)->getYloc();
       long  ybin_number2 = _utilities->ybinNumber(yloc2);
       if(ybin_number2 == ybin_number)
            {
            float xloc2     = velfun(ifun2)->getXloc();
            float distance2 = xloc2 - xloc;
            if(direction < 0) distance2 = - distance2;
            if(distance2 > 0.0)
                 {
                 if(which < 0 || distance2 < distance)
                      {
                      distance = distance2;
                      which    = ifun2;
                      }
                 }
            }
       }
  return which;
}



long VfKernal::findNearbyYloc (long ifun, int direction)  const
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return -1;
  if(ifun < 0 || ifun >= nfun) return -1;

  float xloc        = velfun(ifun)->getXloc();
  float yloc        = velfun(ifun)->getYloc();
  long  xbin_number = _utilities->xbinNumber(xloc);
  long  which       = -1;
  float distance    = 0.0;

  for(long ifun2 = 0; ifun2 < nfun; ifun2++)
       {
       float xloc2        = velfun(ifun2)->getXloc();
       long  xbin_number2 = _utilities->xbinNumber(xloc2);
       if(xbin_number2 == xbin_number)
            {
            float yloc2     = velfun(ifun2)->getYloc();
            float distance2 = yloc2 - yloc;
            if(direction < 0) distance2 = - distance2;
            if(distance2 > 0.0)
                 {
                 if(which < 0 || distance2 < distance)
                      {
                      distance = distance2;
                      which    = ifun2;
                      }
                 }
            }
       }
  return which;
}



//---------------------------- find helper ---------------------------//
//---------------------------- find helper ---------------------------//
//---------------------------- find helper ---------------------------//

    // private.
    // finds appropriate ifun if it is out of range.
    // if ifun is -2, resets ifun to the active velocity function.
    // if ifun is still out of range, uses xloc and yloc to reset ifun.
    // if ifun turns out to be the active velocity function,
    //   also updates _xprev, _xnext, _yprev, and _ynext if necessary.
    // returns updated ifun.

long VfKernal::findHelper(long ifun, float xloc, float yloc)
{
  long nfun   = numVelocityFunctions();
  long active = getActiveVelocityFunction();
  if(ifun == -2) ifun = active;
  if(ifun < 0 || ifun >= nfun) ifun = findMatchingVelfun(xloc, yloc);
  if(ifun == active && _array->neighborsNeedUpdating())
      {
      int xdirection = checkXdirection();
      int ydirection = checkYdirection();
      _xprev = findNearbyXloc(active, -xdirection);
      _xnext = findNearbyXloc(active,  xdirection);
      _yprev = findNearbyYloc(active, -ydirection);
      _ynext = findNearbyYloc(active,  ydirection);
      _array->neighborsAreUpdated();
      }
  return ifun;
}



//--------------- find next or previous x or y location -----------------//
//--------------- find next or previous x or y location -----------------//
//--------------- find next or previous x or y location -----------------//

  // public.
  // ifun = index of current velocity function location.
  // findNextXloc returns index of   next   velocity function in x direction.
  // findPrevXloc returns index of previous velocity function in x direction.
  // findNextYloc returns index of   next   velocity function in y direction.
  // findPrevYloc returns index of previous velocity function in y direction.
  // returns -1 if no such location is found.
  // if ifun == -2, searches from the active velocity function.
  // if ifun >= 0 and ifun < nfun, searches from location ifun.
  // otherwise (e.g. if fun == -1) uses xloc and yloc to find ifun first.
  // the velocity functions can be in any order.
  // these are convenience functions which call findMatchingVelfun,
  //   checkXdirection, checkYdirection, findNearbyXloc, and
  //   findNearbyYloc.
  // if searching from the active function, these simply return _xprev,
  //   _xnext, _yprev, or _ynext (updating them first if necessary).

long VfKernal::findNextXloc (long ifun, float xloc, float yloc)
{
  ifun = findHelper(ifun, xloc, yloc);
  if(ifun == getActiveVelocityFunction()) return _xnext;
  int direction = checkXdirection();
  return findNearbyXloc(ifun, direction);
}


long VfKernal::findPrevXloc (long ifun, float xloc, float yloc)
{
  ifun = findHelper(ifun, xloc, yloc);
  if(ifun == getActiveVelocityFunction()) return _xprev;
  int direction = - checkXdirection();
  return findNearbyXloc(ifun, direction);
}


long VfKernal::findNextYloc (long ifun, float xloc, float yloc)
{
  ifun = findHelper(ifun, xloc, yloc);
  if(ifun == getActiveVelocityFunction()) return _ynext;
  int direction = checkYdirection();
  return findNearbyYloc(ifun, direction);
}


long VfKernal::findPrevYloc (long ifun, float xloc, float yloc)
{
  ifun = findHelper(ifun, xloc, yloc);
  if(ifun == getActiveVelocityFunction()) return _yprev;
  int direction = - checkYdirection();
  return findNearbyYloc(ifun, direction);
}



//--------------------- find where to insert ------------------------//
//--------------------- find where to insert ------------------------//
//--------------------- find where to insert ------------------------//

    // public.
    // find where to insert a matching value.
    // returns index where new velocity function should be inserted.
    // xflag TRUE  means to create ascending  x-order if either is possible.
    // xflag FALSE means to create descending x-order if either is possible.
    // recommend setting xflag to semblance file order for new velocity file.

long VfKernal::findWhereToInsert (float xloc, float yloc, int xflag)  const
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return 0;
//--------get sort flags.
  int xdir = checkXdirection();
  int ydir = checkYdirection();
  if(xdir == -1 || xdir == 1) return nfun;    // not sorted.
  if(ydir == -1 || ydir == 1) return nfun;    // not sorted.
  if(xdir == 0) { if(xflag) xdir = 2; else xdir = -2; }
  if(ydir == 0) ydir = 2;
//-------find proper place to put new function.
  for(long ifun = 0; ifun < nfun; ifun++)
      {
 //// float yloc1 = velfun(ifun-1)->getYloc();
      float yloc2 = velfun(ifun  )->getYloc();
      float xloc2 = velfun(ifun  )->getXloc();
      long iybin1 = _utilities->ybinNumber(yloc);
 //// long iybin1 = _utilities->ybinNumber(yloc1);
      long iybin2 = _utilities->ybinNumber(yloc2);
      if(iybin1 == iybin2)
           {
           if(xdir * xloc2 > xdir * xloc) return ifun;
           }
      else if(ydir * yloc2 > ydir * yloc) return ifun;
      }
  return nfun;
}



//-------------------- get velfun list2 ----------------------------//
//-------------------- get velfun list2 ----------------------------//
//-------------------- get velfun list2 ----------------------------//

     // static.
     // routine to get list of velocity functions.
     // list contains functions from xmin thru xmax at given ybin.
     // list is returned in ascending or descending order.
     // duplicate locations in the same xybin are eliminated.
     // if xmin and xmax are FNIL, all xbins are allowed.
     // if ybin_choice is FNIL, all ybins are allowed, and the ybin
     //   width is considered infinite, so that all y values are lumped
     //   into the same ybin.  since duplicate locations are eliminated,
     //   this means that only one function will be returned for each
     //   xbin.
     // list should be allocated with enough space equal to the total
     //   number of velocity functions.

static void get_velfun_list2 (float *xloc, float *yloc, long nfun,
         float xcenter, float xwidth, float ycenter, float ywidth,
         float xmin, float xmax, float ybin_choice,
         long *list, long *nlist)
{
  *nlist = 0;
  if(nfun == 0) return;
  float *bins = new float [nfun];
  long *ibins = new long  [nfun];

  long min_x = BinNumber( MinimumValue(xmin,xmax), xcenter, xwidth );
  long max_x = BinNumber( MaximumValue(xmin,xmax), xcenter, xwidth );
  long sel_y = BinNumber( ybin_choice,             ycenter, ywidth );
  for(long ifun = 0; ifun < nfun; ifun++)
       {
       long xbin_number = BinNumber(xloc[ifun], xcenter, xwidth);
       long ybin_number = BinNumber(yloc[ifun], ycenter, ywidth);
       if(ybin_choice == FNIL) ybin_number = sel_y;
       if(xmin == FNIL) min_x = xbin_number;
       if(xmax == FNIL) max_x = xbin_number;
       if( ybin_number == sel_y  &&
           xbin_number >= min_x  &&
           xbin_number <= max_x )
            {
            list [*nlist] = ifun;
            bins [*nlist] = xbin_number;
            ibins[*nlist] = xbin_number;
            (*nlist)++;
            }
       }
  if(*nlist <= 1)
      {
      delete [] bins;
      delete [] ibins;
      return;
      }
  long direction = find_iarray_direction(ibins, *nlist);
  if(direction == 0)
       {
       if(bins[*nlist - 1] >= bins[0]) direction =  1;
       else                            direction = -1;
       long i;
       if(direction < 0) { for(i=0; i < *nlist; i++) { bins[i] = -bins[i]; } }
       float *dummy = (float*)ibins;
       triplesort (bins, dummy, list, nlist);
       if(direction < 0) { for(i=0; i < *nlist; i++) { bins[i] = -bins[i]; } }
       }
  if(direction == 1 || direction == -1)
       {
       long j = 1;
       for(long i = 1; i < *nlist; i++)
            {
            if(bins[i] != bins[i-1]) { list[j] = list[i]; j++; }
            }
       *nlist = j;
       }
  delete [] bins;
  delete [] ibins;
  if( (direction > 0 && xmin >  xmax) ||
      (direction < 0 && xmin <= xmax) )
       {
       switch_iarray_direction(list, *nlist);
       }
}



//-------------------- get velfun inline/crossline ----------------//
//-------------------- get velfun inline/crossline ----------------//
//-------------------- get velfun inline/crossline ----------------//

     // public.
     // routines to get list of velocity functions.
     // list contains functions from xmin thru xmax at given ybin.
     // list is returned in ascending or descending order.
     // duplicate locations are eliminated.
     // if xmin and xmax are FNIL, all xbins are allowed.
     // if ybin_choice is FNIL, all ybins are allowed, and the ybin
     //   width is considered infinite, so that all y values are lumped
     //   into the same ybin.  since duplicate locations are eliminated,
     //   this means that only one function will be returned for each
     //   xbin.
     // list should be allocated with enough space equal to the total
     //   number of velocity functions.

void VfKernal::getVelfunInline (float xmin, float xmax,
                                float ybin_choice,
                                long *list, long *nlist)  const
{
  long nfun = numVelocityFunctions();
  float *xloc = new float [nfun+1];      // OK if nfun = 0.
  float *yloc = new float [nfun+1];      // OK if nfun = 0.
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      xloc[ifun] = velfun(ifun)->getXloc();
      yloc[ifun] = velfun(ifun)->getYloc();
      }
  get_velfun_list2 (xloc, yloc, nfun,
                    _utilities->getXcenter(),
                    _utilities->getXwidth (),
                    _utilities->getYcenter(),
                    _utilities->getYwidth (),
                    xmin, xmax, ybin_choice, list, nlist);
  delete [] xloc;
  delete [] yloc;
}



void VfKernal::getVelfunCrossline (float ymin, float ymax,
                                   float xbin_choice,
                                   long *list, long *nlist)  const
{
  long nfun = numVelocityFunctions();
  float *xloc = new float [nfun+1];      // OK if nfun = 0.
  float *yloc = new float [nfun+1];      // OK if nfun = 0.
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      xloc[ifun] = velfun(ifun)->getXloc();
      yloc[ifun] = velfun(ifun)->getYloc();
      }
  get_velfun_list2 (yloc, xloc, nfun,
                    _utilities->getYcenter(),
                    _utilities->getYwidth (),
                    _utilities->getXcenter(),
                    _utilities->getXwidth (),
                    ymin, ymax, xbin_choice, list, nlist);
  delete [] xloc;
  delete [] yloc;
}



//------------------------- xbin to float --------------------------//
//------------------------- xbin to float --------------------------//
//------------------------- xbin to float --------------------------//

     // public.
     // convert velocities into an xbin timeslice array.
     // ymin is mapped to index [0].
     // ymax is mapped to index [nsamp-1].
     // sets velocity to zero if velfun has no picks or some nil picks.

void VfKernal::xbinToFloat (float ymin, float ymax,
                            float xbin_choice, int type, float time,
                            long nsamp, float *array, int vint_grade)  const
{
  long nfun = numVelocityFunctions();
  long  *list = new long  [nfun+1];      // OK if nfun = 0.
  float *y    = new float [nfun+1];      // OK if nfun = 0.
  float *v    = new float [nfun+1];      // OK if nfun = 0.
  long nlist;

  getVelfunCrossline(FNIL, FNIL, xbin_choice,   list, &nlist);
  for(long i = 0; i < nlist; i++)
       {
       long ifun = list[i];
       y[i] = velfun(ifun)->getYloc();
       v[i] = velfun(ifun)->getInterpolatedVelocity(time, type, vint_grade);
       }
  float dt = 0.0;
  if(nsamp > 1) dt = (ymax - ymin)/(nsamp - 1);
  for(long isamp = 0; isamp < nsamp; isamp++)
       {
       float abscissa = ymin + isamp * dt;
       int nlist2 = (int)nlist;
       array[isamp] = terp1 (&abscissa, y, &nlist2, v);
       }
  delete [] list;
  delete [] y;
  delete [] v;
}



//------------------------- get interpolated velocity ---------------------//
//------------------------- get interpolated velocity ---------------------//
//------------------------- get interpolated velocity ---------------------//

     // public.

float VfKernal::getInterpolatedVelocity
                   (float xloc, float yloc, float abscissa,
                    int type, int vint_grade,
                    float xnorm, float ynorm)  const
{
  assert(!VfUtilities::abscissaIsThickness(type));
  long nfun = numVelocityFunctions();
  if(nfun == 0) return 0.0;
  long ifun1, ifun2, ifun3, ifun4;
  float weight1, weight2, weight3, weight4;
  findNearestVelfuns(xloc, yloc, xnorm, ynorm,
                     &ifun1, &ifun2, &ifun3, &ifun4,
                     &weight1, &weight2, &weight3, &weight4, type);
  if(ifun1 == -1) return 0.0;
  float vel1 = velfun(ifun1)->getInterpolatedVelocity
                                                (abscissa, type, vint_grade);
  float vel2 = velfun(ifun2)->getInterpolatedVelocity
                                                (abscissa, type, vint_grade);
  float vel3 = velfun(ifun3)->getInterpolatedVelocity
                                                (abscissa, type, vint_grade);
  float vel4 = velfun(ifun4)->getInterpolatedVelocity
                                                (abscissa, type, vint_grade);
  float vel  = vel1 * weight1 + vel2 * weight2 +
               vel3 * weight3 + vel4 * weight4;
  return vel;
}


float VfKernal::getInterpolatedTime
                   (float xloc, float yloc, float depth,
                    float xnorm, float ynorm)  const
{
  long nfun = numVelocityFunctions();
  if(nfun == 0) return 0.0;
  long ifun1, ifun2, ifun3, ifun4;
  float weight1, weight2, weight3, weight4;
  findNearestVelfuns(xloc, yloc, xnorm, ynorm,
                     &ifun1, &ifun2, &ifun3, &ifun4,
                     &weight1, &weight2, &weight3, &weight4, VTDP);
  if(ifun1 == -1) return 0.0;
  float time1 = velfun(ifun1)->getInterpolatedTime(depth);
  float time2 = velfun(ifun2)->getInterpolatedTime(depth);
  float time3 = velfun(ifun3)->getInterpolatedTime(depth);
  float time4 = velfun(ifun4)->getInterpolatedTime(depth);
  float time  = time1 * weight1 + time2 * weight2 +
                time3 * weight3 + time4 * weight4;
  return time;
}


float VfKernal::getInterpolatedDepth
                   (float xloc, float yloc, float time,
                    float xnorm, float ynorm)  const
{
  return getInterpolatedVelocity(xloc, yloc, time, VTDP, TRUE, xnorm, ynorm);
}



//----------------- start and stop multiple interpolations ---------------//
//----------------- start and stop multiple interpolations ---------------//
//----------------- start and stop multiple interpolations ---------------//

     // public.

void VfKernal::startMultipleInterpolations(long npicks)
{
/****************
  assert(!_MULTIPLE);
  _MULTIPLE = TRUE;
****************/
  assert(_NPICKS == -1);
  assert(npicks > 1);
  _NPICKS     = npicks;
  _IFUN1      = -1;
  _IFUN2      = -1;
  _IFUN3      = -1;
  _IFUN4      = -1;
  _ORDINATES1 = new float [_NPICKS];
  _ORDINATES2 = new float [_NPICKS];
  _ORDINATES3 = new float [_NPICKS];
  _ORDINATES4 = new float [_NPICKS];
/****************
  long nfun = numVelocityFunctions();
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      velfun(ifun)->rememberInterpolatedOrdinates(npicks);
      }
****************/
}


void VfKernal::stopMultipleInterpolations()
{
/****************
  assert(_MULTIPLE);
  _MULTIPLE = FALSE;
****************/
  assert(_NPICKS > 1);
  delete [] _ORDINATES1;
  delete [] _ORDINATES2;
  delete [] _ORDINATES3;
  delete [] _ORDINATES4;
  _NPICKS     = -1;
  _IFUN1      = -1;
  _IFUN2      = -1;
  _IFUN3      = -1;
  _IFUN4      = -1;
  _ORDINATES1 = NULL;
  _ORDINATES2 = NULL;
  _ORDINATES3 = NULL;
  _ORDINATES4 = NULL;
/****************
  long nfun = numVelocityFunctions();
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      velfun(ifun)->forgetInterpolatedOrdinates();
      }
****************/
}



//---------------------- merge ordered lists ---------------------------//
//---------------------- merge ordered lists ---------------------------//
//---------------------- merge ordered lists ---------------------------//

   // stepmin = minimum step to take in output ordered list.
   // nlists = number of lists of ordered values to merge (>= 1).
   // npoints[nlists] = number of values in each ordered list (>= 0).
   // weights[nlists] = weight to apply to each ordered list (>= 0.0).
   // values[nlists][npoints] = each ordered list.
   // vv[nn] (output) = values of all points in the merged list.
   // ww[nn] (output) = weights of all points in the merged list.
   // nn (returned) = number of points in merged list (<= sum of all npoints).

static long merge_ordered_lists(float stepmin, long nlists,
                                const long *npoints, const float *weights,
                                float **values, float *vv, float *ww)
{
  long ilist;
  assert(stepmin > 0.0);
  assert(nlists >= 1);
  assert(npoints && weights && values && vv && ww);
  long *ipoint = new long [nlists];
  for(ilist = 0; ilist < nlists; ilist++)
      {
      if(weights[ilist] > 0.0) ipoint[ilist] = 0;
      else                     ipoint[ilist] = npoints[ilist];
      }

/****
printf("----------------\n");
for(ilist = 0; ilist < nlists; ilist++)
{
printf("%d  %d  %f  --  ", ilist, npoints[ilist], weights[ilist]);
for(long ip = 0; ip < npoints[ilist]; ip++)
{ printf("%f  ", values[ilist][ip]); }
printf("\n");
}
****/

  long nn = 0;
  while(TRUE)
      {
      float xmin = 1.0e30;
      float w    = 0.0;
      for(ilist = 0; ilist < nlists; ilist++)
          {
          long   ipoint2  = ipoint [ilist];
          long   npoints2 = npoints[ilist];
          if(ipoint2 < npoints2)
              {
              float weight = weights[ilist];
              float value  = values [ilist][ipoint2];
              if     (value <  xmin) { xmin = value; w =  weight; }
              else if(value == xmin) {               w += weight; }
              }
          }
      if(w == 0.0) break;
      vv[nn] = xmin;
      ww[nn] = w;
      nn++;
      xmin += stepmin;
      for(ilist = 0; ilist < nlists; ilist++)
          {
          while(ipoint[ilist] < npoints[ilist] &&
                values[ilist][ipoint[ilist]] <= xmin)
                  {
                  ipoint[ilist]++;
                  }
          }
      }
  delete [] ipoint;
  return nn;
}



//-------------------------- cull ordered list ----------------------------//
//-------------------------- cull ordered list ----------------------------//
//-------------------------- cull ordered list ----------------------------//

   // nn     = number of points in the list.
   // nmax   = maximum allowed number of points in the list.
   // vv[nn] = values of all points in the list.
   // ww[nn] = weights of all points in the list.
   // arrays vv[nn] and ww[nn] are reduced so that the returned nn <= nmax.
   // first and last points are never removed.
   // lowest weights are removed first.
   // higher-index points of the same weight are removed first.
   // only the minimum necessary number of points are removed.

static long cull_ordered_list(long nn, long nmax, float *vv, float *ww)
{
  assert(nmax >= 2);
  assert(vv && ww);
  if(nn <= nmax) return nn;
  float wflag = -1.0e30;
  long ifirst = 1;
  long ilast  = nn-2;
  long jj = nn;
  long ii;
  while(jj > nmax)
      {
      float wmin = 1.0e30;
      for(ii = ifirst; ii <= ilast; ii++)
          {
          if (ww[ii] != wflag && ww[ii] < wmin) wmin = ww[ii];
          }
      jj = nn;
      for(ii = ilast; ii >= ifirst; ii--)
          {
          if(ww[ii] == wflag || ww[ii] <= wmin)
              {
              ww[ii] = wflag;
              jj--;
              if(jj <= nmax) break;
              }
          }
      }
  jj = 0;
  for(ii = 0; ii < nn; ii++)
      {
      if(ww[ii] == wflag) continue;
      vv[jj] = vv[ii];
      ww[jj] = ww[ii];
      jj++;
      }
  assert(jj <= nmax);
  return jj;
}



//----------------- get interpolated velocity function ------------------//
//----------------- get interpolated velocity function ------------------//
//----------------- get interpolated velocity function ------------------//

     // public.

long VfKernal::getInterpolatedVelocityFunction
                   (long npicks, float *abscissae, float *ordinates,
                    float xloc, float yloc,
                    int type, int vint_grade,
                    float xnorm, float ynorm)
{
  assert(abscissae && ordinates);
  assert(!VfUtilities::abscissaIsThickness(type));
  if(numVelocityFunctions() == 0) return 0;
  long ifun1, ifun2, ifun3, ifun4;
  float weight1, weight2, weight3, weight4;
  findNearestVelfuns(xloc, yloc, xnorm, ynorm,
                     &ifun1, &ifun2, &ifun3, &ifun4,
                     &weight1, &weight2, &weight3, &weight4, type);
  if(ifun1 == -1) return 0;

  if(_NPICKS > 1 && npicks == _NPICKS)  // doing multiple interpolations.
      {
      float *ordinates1 = NULL;
      float *ordinates2 = NULL;
      float *ordinates3 = NULL;
      float *ordinates4 = NULL;
      int USING1 = FALSE;
      int USING2 = FALSE;
      int USING3 = FALSE;
      int USING4 = FALSE;
      if     (ifun1 == _IFUN1) { ordinates1 = _ORDINATES1; USING1 = TRUE; }
      else if(ifun1 == _IFUN2) { ordinates1 = _ORDINATES2; USING2 = TRUE; }
      else if(ifun1 == _IFUN3) { ordinates1 = _ORDINATES3; USING3 = TRUE; }
      else if(ifun1 == _IFUN4) { ordinates1 = _ORDINATES4; USING4 = TRUE; }
           if(ifun2 == _IFUN1) { ordinates2 = _ORDINATES1; USING1 = TRUE; }
      else if(ifun2 == _IFUN2) { ordinates2 = _ORDINATES2; USING2 = TRUE; }
      else if(ifun2 == _IFUN3) { ordinates2 = _ORDINATES3; USING3 = TRUE; }
      else if(ifun2 == _IFUN4) { ordinates2 = _ORDINATES4; USING4 = TRUE; }
           if(ifun3 == _IFUN1) { ordinates3 = _ORDINATES1; USING1 = TRUE; }
      else if(ifun3 == _IFUN2) { ordinates3 = _ORDINATES2; USING2 = TRUE; }
      else if(ifun3 == _IFUN3) { ordinates3 = _ORDINATES3; USING3 = TRUE; }
      else if(ifun3 == _IFUN4) { ordinates3 = _ORDINATES4; USING4 = TRUE; }
           if(ifun4 == _IFUN1) { ordinates4 = _ORDINATES1; USING1 = TRUE; }
      else if(ifun4 == _IFUN2) { ordinates4 = _ORDINATES2; USING2 = TRUE; }
      else if(ifun4 == _IFUN3) { ordinates4 = _ORDINATES3; USING3 = TRUE; }
      else if(ifun4 == _IFUN4) { ordinates4 = _ORDINATES4; USING4 = TRUE; }
      if(USING1 == FALSE) _IFUN1 = -1;
      if(USING2 == FALSE) _IFUN2 = -1;
      if(USING3 == FALSE) _IFUN3 = -1;
      if(USING4 == FALSE) _IFUN4 = -1;
      if(ordinates1 == NULL)
          {
          if     (_IFUN1 == -1) { ordinates1 = _ORDINATES1; _IFUN1 = ifun1; }
          else if(_IFUN2 == -1) { ordinates1 = _ORDINATES2; _IFUN2 = ifun1; }
          else if(_IFUN3 == -1) { ordinates1 = _ORDINATES3; _IFUN3 = ifun1; }
          else if(_IFUN4 == -1) { ordinates1 = _ORDINATES4; _IFUN4 = ifun1; }
          assert(ordinates1);
          velfun(ifun1)->getInterpolatedVelocityFunction
                           (npicks, abscissae, ordinates1, type, vint_grade);
          }
      if(ordinates2 == NULL)
          {
          if     (_IFUN1 == -1) { ordinates2 = _ORDINATES1; _IFUN1 = ifun2; }
          else if(_IFUN2 == -1) { ordinates2 = _ORDINATES2; _IFUN2 = ifun2; }
          else if(_IFUN3 == -1) { ordinates2 = _ORDINATES3; _IFUN3 = ifun2; }
          else if(_IFUN4 == -1) { ordinates2 = _ORDINATES4; _IFUN4 = ifun2; }
          assert(ordinates2);
          velfun(ifun2)->getInterpolatedVelocityFunction
                           (npicks, abscissae, ordinates2, type, vint_grade);
          }
      if(ordinates3 == NULL)
          {
          if     (_IFUN1 == -1) { ordinates3 = _ORDINATES1; _IFUN1 = ifun3; }
          else if(_IFUN2 == -1) { ordinates3 = _ORDINATES2; _IFUN2 = ifun3; }
          else if(_IFUN3 == -1) { ordinates3 = _ORDINATES3; _IFUN3 = ifun3; }
          else if(_IFUN4 == -1) { ordinates3 = _ORDINATES4; _IFUN4 = ifun3; }
          assert(ordinates3);
          velfun(ifun3)->getInterpolatedVelocityFunction
                           (npicks, abscissae, ordinates3, type, vint_grade);
          }
      if(ordinates4 == NULL)
          {
          if     (_IFUN1 == -1) { ordinates4 = _ORDINATES1; _IFUN1 = ifun4; }
          else if(_IFUN2 == -1) { ordinates4 = _ORDINATES2; _IFUN2 = ifun4; }
          else if(_IFUN3 == -1) { ordinates4 = _ORDINATES3; _IFUN3 = ifun4; }
          else if(_IFUN4 == -1) { ordinates4 = _ORDINATES4; _IFUN4 = ifun4; }
          assert(ordinates4);
          velfun(ifun4)->getInterpolatedVelocityFunction
                           (npicks, abscissae, ordinates4, type, vint_grade);
          }
      for(long ipick = 0; ipick < npicks; ipick++)
          {
          ordinates[ipick] =
                 ordinates1[ipick] * weight1 + ordinates2[ipick] * weight2 +
                 ordinates3[ipick] * weight3 + ordinates4[ipick] * weight4;
          }
      return npicks;
      }

  if(npicks == NEAREST_ABSCISSAE)
      {
      long  ifun   = ifun1;
      float weight = weight1;
      if(weight2 > weight) { ifun = ifun2; weight = weight2; }
      if(weight3 > weight) { ifun = ifun3; weight = weight3; }
      if(weight4 > weight) { ifun = ifun4; weight = weight4; }
      assert(ifun >= 0);
      assert(weight > 0.0);
      velfun(ifun)->getAbscissaArray(abscissae, type);
      npicks = velfun(ifun)->numPicks();
      }
  else if(npicks == RESTRICTED_ABSCISSAE ||
	  npicks == UNRESTRICTED_ABSCISSAE)
      {
      long         nlists = 4;
      long         npoints[4];
      float        weights[4];
      float       *values [4];
      float        vv[4 * MAXPICKS];
      float        ww[4 * MAXPICKS];
      npoints[0] = velfun(ifun1)->numPicks();
      npoints[1] = velfun(ifun2)->numPicks();
      npoints[2] = velfun(ifun3)->numPicks();
      npoints[3] = velfun(ifun4)->numPicks();
      weights[0] = weight1;
      weights[1] = weight2;
      weights[2] = weight3;
      weights[3] = weight4;
      values [0] = velfun(ifun1)->createAbscissaArrayPointer(type);
      values [1] = velfun(ifun2)->createAbscissaArrayPointer(type);
      values [2] = velfun(ifun3)->createAbscissaArrayPointer(type);
      values [3] = velfun(ifun4)->createAbscissaArrayPointer(type);
      float stepmin = _utilities->getTimeTolerance();
      if(VfUtilities::abscissaIsDepth(type))
                                stepmin = _utilities->getDepthTolerance();
      long npicks_keep = npicks;

      npicks = merge_ordered_lists
                      (stepmin, nlists, npoints, weights, values, vv, ww);
            // merge_ordered_lists sets arrays vv and ww.

      velfun(ifun1)->deleteAbscissaArrayPointer(values[0], type);
      velfun(ifun2)->deleteAbscissaArrayPointer(values[1], type);
      velfun(ifun3)->deleteAbscissaArrayPointer(values[2], type);
      velfun(ifun4)->deleteAbscissaArrayPointer(values[3], type);

      if(npicks_keep == RESTRICTED_ABSCISSAE)
          {
          npicks = cull_ordered_list (npicks, MAXPICKS, vv, ww);
                // cull_ordered_list reduces length of arrays vv and ww.
          }

      memcpy(abscissae, vv, (int)npicks * sizeof(float));
      }

  assert(npicks > 0);
  float *ordinates1 = new float [npicks];
  float *ordinates2 = new float [npicks];
  float *ordinates3 = new float [npicks];
  float *ordinates4 = new float [npicks];
  velfun(ifun1)->getInterpolatedVelocityFunction
                            (npicks, abscissae, ordinates1, type, vint_grade);
  velfun(ifun2)->getInterpolatedVelocityFunction
                            (npicks, abscissae, ordinates2, type, vint_grade);
  velfun(ifun3)->getInterpolatedVelocityFunction
                            (npicks, abscissae, ordinates3, type, vint_grade);
  velfun(ifun4)->getInterpolatedVelocityFunction
                            (npicks, abscissae, ordinates4, type, vint_grade);
  for(long ipick = 0; ipick < npicks; ipick++)
      {
      ordinates[ipick] =
                 ordinates1[ipick] * weight1 + ordinates2[ipick] * weight2 +
                 ordinates3[ipick] * weight3 + ordinates4[ipick] * weight4;
      }

/****************
  if(_MULTIPLE)
      {
      if(_IFUN1 >= 0 && _IFUN1 != ifun1 && _IFUN1 != ifun2 &&
                        _IFUN1 != ifun3 && _IFUN1 != ifun4)
                           velfun(_IFUN1)->deleteInterpolatedOrdinates();
      if(_IFUN2 >= 0 && _IFUN2 != ifun1 && _IFUN2 != ifun2 &&
                        _IFUN2 != ifun3 && _IFUN2 != ifun4)
                           velfun(_IFUN2)->deleteInterpolatedOrdinates();
      if(_IFUN3 >= 0 && _IFUN3 != ifun1 && _IFUN3 != ifun2 &&
                        _IFUN3 != ifun3 && _IFUN3 != ifun4)
                           velfun(_IFUN3)->deleteInterpolatedOrdinates();
      if(_IFUN4 >= 0 && _IFUN4 != ifun1 && _IFUN4 != ifun2 &&
                        _IFUN4 != ifun3 && _IFUN4 != ifun4)
                           velfun(_IFUN4)->deleteInterpolatedOrdinates();
      _IFUN1 = ifun1;
      _IFUN2 = ifun2;
      _IFUN3 = ifun3;
      _IFUN4 = ifun4;
      }
****************/
  delete [] ordinates1;
  delete [] ordinates2;
  delete [] ordinates3;
  delete [] ordinates4;
  return npicks;
}



//-------------------------- vel to float -------------------------------//
//-------------------------- vel to float -------------------------------//
//-------------------------- vel to float -------------------------------//

     // public.

void VfKernal::velToFloat (float xloc, float yloc, float tmin, float tmax,
                           long npicks, float *ordinates,
                           int type, int vint_grade)
{
  assert(tmax > tmin);
  assert(npicks > 1);
  assert(ordinates);
  float dt = (tmax - tmin)/(npicks - 1);
  for(long ipick = 0; ipick < npicks; ipick++)
      {
      ordinates[ipick] = tmin + ipick * dt;  // really abscissae at this stage.
      }
  getInterpolatedVelocityFunction
             (npicks, ordinates, ordinates, xloc, yloc, type, vint_grade);
}



//------------------------ fortran prototype -----------------------------//
//------------------------ fortran prototype -----------------------------//
//------------------------ fortran prototype -----------------------------//

#if (ultrix || sun || __sgi || LINUXP || LINUXI)
#define dyncc            dyncc_
#endif

#ifdef NEED_CAPITALS
#define dyncc            DYNCC
#endif

extern "C"
{
  void dyncc (float *dop, long *nnn, float *ta, float *tb, long *nval,
              float *a, float *b, float *bmute);
}



//---------------------- static functions ------------------------------//
//---------------------- static functions ------------------------------//
//---------------------- static functions ------------------------------//


static void dyncc_in_place(long npicks, float *times, float *depths,
                           long nsamp, float *array)
{
  float dop = 999.0;
  float bmute;
  float *temporary = new float [nsamp];
  dyncc(&dop, &npicks, times, depths, &nsamp, array, temporary, &bmute);
  memcpy(array, temporary, (int)nsamp * sizeof(float));
  delete [] temporary;
}



static void get_times_and_depths (VfKernal *kernal,
                                  float xloc, float yloc,
                                  long nsamp,
                                  float tmin, float tmax,
                                  float dmin, float dmax,
                                  long npicks, float *times, float *depths)
{
  assert(tmax > tmin);
  assert(dmax > dmin);
  assert(nsamp > 1);
  long ipick;
  float delta = tmax / (npicks - 1);
  for(ipick = 0; ipick < npicks; ipick++)
      {
      times[ipick] = ipick * delta;
      }
  kernal->getInterpolatedVelocityFunction
                                 (npicks, times, depths, xloc, yloc, VTDP);
  float dt = (tmax - tmin) / (nsamp - 1);
  float dd = (dmax - dmin) / (nsamp - 1);
  for(ipick = 0; ipick < npicks; ipick++)
      {
      times [ipick] = 1.0 + (times [ipick] - tmin) / dt;    // now indices.
      depths[ipick] = 1.0 + (depths[ipick] - dmin) / dd;    // now indices.
      }
}



static void do_conversion (long nsamp, void *array, int array_type,
                           long npicks, float *times, float *depths)
{
  if(array_type == VfKernal::BYTE_ARRAY)
      {
      float *good = new float [nsamp];
      convert_byte_to_float(nsamp, (unsigned char*)array, good);
      dyncc_in_place(npicks, times, depths, nsamp, good);
      return_float_to_byte(nsamp, good, (unsigned char*)array);
      delete [] good;
      }
  else if(array_type == VfKernal::FLOAT_ARRAY)
      {
      dyncc_in_place(npicks, times, depths, nsamp, (float*)array);
      }
  else
      {
      assert(FALSE);
      }
}



//-------------------------- time to depth ------------------------------//
//-------------------------- time to depth ------------------------------//
//-------------------------- time to depth ------------------------------//

     // public.

void VfKernal::timeToDepth (float xloc, float yloc, float tmin, float tmax,
                            long nsamp, void *array, int array_type,
                            float dmin, float dmax)
{
  assert(array);
  float times [MAXPICKS];
  float depths[MAXPICKS];
  long npicks = MAXPICKS;
  get_times_and_depths(this, xloc, yloc, nsamp,
                       tmin, tmax, dmin, dmax,
                       npicks, times, depths);
  do_conversion(nsamp, array, array_type, npicks, times, depths);
}



//-------------------------- depth to time ------------------------------//
//-------------------------- depth to time ------------------------------//
//-------------------------- depth to time ------------------------------//

     // public.

void VfKernal::depthToTime (float xloc, float yloc, float tmin, float tmax,
                            long nsamp, void *array, int array_type,
                            float dmin, float dmax)
{
  assert(array);
  float times [MAXPICKS];
  float depths[MAXPICKS];
  long npicks = MAXPICKS;
  get_times_and_depths(this, xloc, yloc, nsamp,
                       tmin, tmax, dmin, dmax,
                       npicks, times, depths);
  do_conversion(nsamp, array, array_type, npicks, depths, times);
}



         //----------- pass thru to VfFunctionArray -----------//
         //----------- pass thru to VfFunctionArray -----------//
         //----------- pass thru to VfFunctionArray -----------//
         //----------- pass thru to VfFunctionArray -----------//
         //----------- pass thru to VfFunctionArray -----------//
         //----------- pass thru to VfFunctionArray -----------//
         //----------- pass thru to VfFunctionArray -----------//
         //----------- pass thru to VfFunctionArray -----------//
         //----------- pass thru to VfFunctionArray -----------//
         //----------- pass thru to VfFunctionArray -----------//



// ------------------clearing and resetting functions -------------------//
// ------------------clearing and resetting functions -------------------//
// ------------------clearing and resetting functions -------------------//

      // public.

void VfKernal::clearEverything()
{
  deleteAllVelocityFunctions();
  free(_name);
  free(_attname);
  free(_attunits);
  free(_tdunits);
  _nhx       = 7;
  _nhy       = 8;
  _order     = 2;
  _nhosign   = 1.0;
  _nhoexp    = 2.0;
  _dunits    = UNSPECIFIED_UNITS;
  _name      = str_newstr("none");
  _attname   = str_newstr("none");
  _attunits  = str_newstr("none");
  _tdunits   = str_newstr("none");
}


void VfKernal::resetNumVelocityFunctions     (long nfun)
{
  _select->beforeSettingSeveralSelectFlags();
  _array->resetNumElements(nfun);
  _select->afterSettingSeveralSelectFlags();
  MIGHT
}


void VfKernal::appendNumVelocityFunctions     (long nappend)
{
  _select->beforeSettingSeveralSelectFlags();
  _array->appendNumElements(nappend);
  _select->afterSettingSeveralSelectFlags();
  MIGHT
}


void VfKernal::deleteAllVelocityFunctions    ()
{
  _select->beforeSettingSeveralSelectFlags();
  _array->removeAllElements();
  _select->afterSettingSeveralSelectFlags();
  MIGHT
}


void VfKernal::deleteSelectedVelocityFunctions    ()
{
  long nfun = _array->numElements();
  _select->beforeSettingSeveralSelectFlags();
  for(long ifun = nfun - 1; ifun >= 0; ifun--)    // must count backward.
      {
      if(!_select->velocityFunctionIsSelected(ifun)) continue;
      _array->removeElement(ifun);
      }
  _select->afterSettingSeveralSelectFlags();
  MIGHT
}


long VfKernal::deleteEmptyVelocityFunctions    ()
{
  long nfun = _array->numElements();
  long kount = 0;
  _select->beforeSettingSeveralSelectFlags();
  for(long ifun = nfun - 1; ifun >= 0; ifun--)    // must count backward.
      {
      if(_array->velfun(ifun)->numPicks() > 0) continue;
      _array->removeElement(ifun);
      kount++;
      }
  _select->afterSettingSeveralSelectFlags();
  MIGHT
  return kount;
}



//-------------------------- get values ------------------------------//
//-------------------------- get values ------------------------------//
//-------------------------- get values ------------------------------//

           // public.

long VfKernal::numVelocityFunctions       ()  const
{
  return _array->numElements();
}


long  VfKernal::numVelocityFunctionsWithErrors  ()  const
{
  return _array->numVelocityFunctionsWithErrors();
}


long  VfKernal::numRaytracedVelocityFunctions  ()  const
{
  return _array->numRaytracedVelocityFunctions();
}


long  VfKernal::numVelocityFunctionsWithBlankNames  ()  const
{
  return _array->numVelocityFunctionsWithBlankNames();
}


long  VfKernal::numVelocityFunctionsWithTypeErrors  (int type)  const
{
  long nfun = _array->numElements();
  long kount = 0;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(_array->velfun(ifun)->typeError(type)) kount++;
      }
  return kount;
}


long VfKernal::getActiveVelocityFunction  ()  const
{
  return _array->getActiveIndex();
}


long VfKernal::getReferenceVelocityFunction  ()  const
{
  return _array->getReferenceIndex();
}


class VfFunction  *VfKernal::velfun  (long ifun)  const
{
  return _array->velfun(ifun);
}


class VfFunction  *VfKernal::activeVelfun  ()  const
{
  return _array->activeVelfun();
}


class VfFunction  *VfKernal::referenceVelfun  ()  const
{
  return _array->referenceVelfun();
}



//-------------------------- set values ------------------------------//
//-------------------------- set values ------------------------------//
//-------------------------- set values ------------------------------//

           // public.

void VfKernal::setActiveVelocityFunction  (long ifun)
{
  _array->setActiveIndex(ifun);
  MIGHT
}


void VfKernal::setReferenceVelocityFunction  (long ifun)
{
  _array->setReferenceIndex(ifun);
}



//------------- insert or remove one velocity function ------------//
//------------- insert or remove one velocity function ------------//
//------------- insert or remove one velocity function ------------//

      // public.
      // appendVelocityFunction and insertVelocityFunction insert
      //   an empty function containing no picks.

void  VfKernal::appendVelocityFunction           ()
{
  _select->beforeSettingSeveralSelectFlags();
  _array->appendNullElement();
  _select->afterSettingSeveralSelectFlags();
  MIGHT
}


void  VfKernal::insertVelocityFunction           (long ifun)
{
  _select->beforeSettingSeveralSelectFlags();
  _array->insertNullElement(ifun);
  _select->afterSettingSeveralSelectFlags();
  MIGHT
}


void  VfKernal::insertVelocityFunctionFromBuffer (long ifun)
{
  _select->beforeSettingSeveralSelectFlags();
  _array->insertElementFromBuffer(ifun);
  _array->copyElementToBuffer(ifun);
  _select->afterSettingSeveralSelectFlags();
  MIGHT
}


void  VfKernal::removeVelocityFunction           (long ifun)
{
  _select->beforeSettingSeveralSelectFlags();
  _array->removeElement(ifun);
  _select->afterSettingSeveralSelectFlags();
  MIGHT
}


void  VfKernal::removeVelocityFunctionToBuffer   (long ifun)
{
  _select->beforeSettingSeveralSelectFlags();
  _array->removeElementToBuffer(ifun);
  _select->afterSettingSeveralSelectFlags();
  MIGHT
}



            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//
            //------- pass thru to VfFunctionSelect -------//



//---------------------- get select flags ----------------------------//
//---------------------- get select flags ----------------------------//
//---------------------- get select flags ----------------------------//

           // public.

long VfKernal::numSelectedVelocityFunctions      ()           const
{
  return _select->numSelectedVelocityFunctions();
}


int  VfKernal::velocityFunctionIsSelected        (long ifun)  const
{
  return _select->velocityFunctionIsSelected(ifun);
}


int  VfKernal::getSelectFlag                     (long ifun)  const
{
  return _select->getSelectFlag(ifun);
}


int  VfKernal::severalSelectionsInProgress       ()  const
{
  return _select->severalSelectionsInProgress();
}



//---------------------- set select flags ----------------------------//
//---------------------- set select flags ----------------------------//
//---------------------- set select flags ----------------------------//

           // public.

void VfKernal::clearSelectFlags                  ()
{
  _select->clearSelectFlags();
}


void VfKernal::incrementSelectFlag               (long ifun)
{
  _select->incrementSelectFlag(ifun);
}


void VfKernal::setSelectFlag                     (long ifun, int select)
{
  _select->setSelectFlag(ifun, select);
}



void VfKernal::beforeSettingSeveralSelectFlags   ()
{
  _select->beforeSettingSeveralSelectFlags();
}



void VfKernal::afterSettingSeveralSelectFlags    ()
{
  _select->afterSettingSeveralSelectFlags();
}



//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//
//------------------------------- end -------------------------------//

