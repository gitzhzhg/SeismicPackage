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

//----------------------- vf_function_array.cc -------------------------//
//----------------------- vf_function_array.cc -------------------------//
//----------------------- vf_function_array.cc -------------------------//

//        implementation file for the VfFunctionArray class
//                derived from the SmartArray class
//                         subdirectory vf


#include "vf/vf_function_array.hh"
#include "vf/vf_function.hh"
#include "oprim/limits_keeper.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>

#define STEP 50


//---------------------- static routines ----------------------------//
//---------------------- static routines ----------------------------//
//---------------------- static routines ----------------------------//


static float get_xloc(void *data, long index)
{
  VfFunctionArray *array = (VfFunctionArray*)data;
  return array->velfun(index)->getXloc();
}


static float get_yloc(void *data, long index)
{
  VfFunctionArray *array = (VfFunctionArray*)data;
  return array->velfun(index)->getYloc();
}


#define ROUTINE(get_depth, minimumDepth, maximumDepth)  \
                                                        \
static float get_depth(void *data, long index)          \
{                                                       \
  VfFunctionArray *array = (VfFunctionArray*)data;      \
  long index2 = index / 2;                              \
  if(index == 2 * index2)                               \
  return array->velfun(index2)->minimumDepth();         \
  return array->velfun(index2)->maximumDepth();         \
}


ROUTINE (get_depth , minimumDepth    , maximumDepth    )
ROUTINE (get_time  , minimumTime     , maximumTime     )
ROUTINE (get_vrms  , minimumVrms     , maximumVrms     )
ROUTINE (get_vav   , minimumVav      , maximumVav      )
ROUTINE (get_vint  , minimumVint     , maximumVint     )
ROUTINE (get_vnmo  , minimumVnmo     , maximumVnmo     )
ROUTINE (get_thick , minimumThickness, maximumThickness)
ROUTINE (get_offset, minimumOffset   , maximumOffset   )



//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfFunctionArray::VfFunctionArray(VfUtilities *utilities)
           : SmartArray(STEP),
        _utilities    (utilities),
        _nerr         (0),
        _nray         (0),
        _nblank       (0),
        _might        (TRUE)
{
  assert(_utilities);
    _xloc_limits = new LimitsKeeper (get_xloc  , this);
    _yloc_limits = new LimitsKeeper (get_yloc  , this);
   _depth_limits = new LimitsKeeper (get_depth , this);
    _time_limits = new LimitsKeeper (get_time  , this);
    _vrms_limits = new LimitsKeeper (get_vrms  , this);
     _vav_limits = new LimitsKeeper (get_vav   , this);
    _vint_limits = new LimitsKeeper (get_vint  , this);
    _vnmo_limits = new LimitsKeeper (get_vnmo  , this);
   _thick_limits = new LimitsKeeper (get_thick , this);
  _offset_limits = new LimitsKeeper (get_offset, this);
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

VfFunctionArray::~VfFunctionArray()
{
  removeAllElements();   // required in derived class destructor.
  delete   _xloc_limits;
  delete   _yloc_limits;
  delete  _depth_limits;
  delete   _time_limits;
  delete   _vrms_limits;
  delete    _vav_limits;
  delete   _vint_limits;
  delete   _vnmo_limits;
  delete  _thick_limits;
  delete _offset_limits;
}



//------------------------ abscissa and ordinate limits -----------------//
//------------------------ abscissa and ordinate limits -----------------//
//------------------------ abscissa and ordinate limits -----------------//

       // private.

LimitsKeeper *VfFunctionArray::abscissaLimits(int type)  const
{
  LimitsKeeper *limits;
  switch(type)
      {
      case VTNM: limits =  _time_limits; break;
      case VTRM: limits =  _time_limits; break;
      case VZRM: limits = _depth_limits; break;
      case VLRM: limits = _thick_limits; break;
      case VTAV: limits =  _time_limits; break;
      case VZAV: limits = _depth_limits; break;
      case VLAV: limits = _thick_limits; break;
      case VTIN: limits =  _time_limits; break;
      case VZIN: limits = _depth_limits; break;
      case VLIN: limits = _thick_limits; break;
      case VTDP: limits =  _time_limits; break;
      default: assert(FALSE);
      }
  return limits;
}


LimitsKeeper *VfFunctionArray::ordinateLimits(int type)  const
{
  LimitsKeeper *limits;
  switch(type)
      {
      case VTNM: limits =  _vnmo_limits; break;
      case VTRM: limits =  _vrms_limits; break;
      case VZRM: limits =  _vrms_limits; break;
      case VLRM: limits =  _vrms_limits; break;
      case VTAV: limits =   _vav_limits; break;
      case VZAV: limits =   _vav_limits; break;
      case VLAV: limits =   _vav_limits; break;
      case VTIN: limits =  _vint_limits; break;
      case VZIN: limits =  _vint_limits; break;
      case VLIN: limits =  _vint_limits; break;
      case VTDP: limits = _depth_limits; break;
      default: assert(FALSE);
      }
  return limits;
}



//------------------- get minimum and maximum values ------------------//
//------------------- get minimum and maximum values ------------------//
//------------------- get minimum and maximum values ------------------//

       // public.

#define LIMITS(minimumXloc, maximumXloc, _xloc_limits)   \
                                                         \
float VfFunctionArray::minimumXloc()                     \
{                                                        \
  return _xloc_limits->minimumValue();                   \
}                                                        \
                                                         \
float VfFunctionArray::maximumXloc()                     \
{                                                        \
  return _xloc_limits->maximumValue();                   \
}


LIMITS (minimumXloc     , maximumXloc     ,  _xloc_limits)
LIMITS (minimumYloc     , maximumYloc     ,  _yloc_limits)
LIMITS (minimumDepth    , maximumDepth    , _depth_limits)
LIMITS (minimumTime     , maximumTime     ,  _time_limits)
LIMITS (minimumVrms     , maximumVrms     ,  _vrms_limits)
LIMITS (minimumVav      , maximumVav      ,   _vav_limits)
LIMITS (minimumVint     , maximumVint     ,  _vint_limits)
LIMITS (minimumVnmo     , maximumVnmo     ,  _vnmo_limits)
LIMITS (minimumThickness, maximumThickness, _thick_limits)
LIMITS (minimumOffset   , maximumOffset   ,_offset_limits)



float VfFunctionArray::minimumAbscissa(int type)
{
  return abscissaLimits(type)->minimumValue();
}


float VfFunctionArray::maximumAbscissa(int type)
{
  return abscissaLimits(type)->maximumValue();
}


float VfFunctionArray::minimumOrdinate(int type)
{
  return ordinateLimits(type)->minimumValue();
}


float VfFunctionArray::maximumOrdinate(int type)
{
  return ordinateLimits(type)->maximumValue();
}



//------------------- overriding virtual functions ------------------//
//------------------- overriding virtual functions ------------------//
//------------------- overriding virtual functions ------------------//

       // private.

void VfFunctionArray::switchElements(long index1, long index2)
{
  SmartArray::switchElements(index1, index2);
  _might = TRUE;
}


void *VfFunctionArray::doCreateObject()
{
  return new VfFunction(_utilities);
}


void  VfFunctionArray::doDeleteObject (void *object)
{
  delete (VfFunction*)object;
}


void  VfFunctionArray::objectHasBeenInserted (long ifun)
{
  afterVelfunChange(velfun(ifun));
  velfun(ifun)->pleaseNotifyArray(this);
}


void  VfFunctionArray::objectWillBeRemoved   (long ifun)
{
  beforeVelfunChange(velfun(ifun));
  velfun(ifun)->pleaseNotifyArray(NULL);
}


void  VfFunctionArray::doCopyToBuffer  (void *object, void *buffer)
{
  ((VfFunction*)buffer)->copyVelocityFunctionContents((VfFunction*)object);
}



//------------- before and after velfun change -------------------------//
//------------- before and after velfun change -------------------------//
//------------- before and after velfun change -------------------------//

      // public.
      // to be called by any VfFunction in the array before and after
      //   any change in the VfFunction object.
      // currently these monitor only the error and raytrace flags,
      //   whether the velocity function has a blank name,
      //   the coordinate values, and the minimum/maximum array values.

void  VfFunctionArray::beforeVelfunChange (VfFunction *object)
{
  int error    = object->getErrorFlag();
  int raytrace = object->getRaytraceFlag();
  int blank    = object->vfidIsBlank();
  if(error    != ERROR_NONE ) _nerr--;
  if(raytrace != RAYTRACE_NO) _nray--;
  if(blank                  ) _nblank--;
    _xloc_limits->removeValue(object->getXloc         ());
    _yloc_limits->removeValue(object->getYloc         ());
   _depth_limits->removeValue(object->minimumDepth    ());
   _depth_limits->removeValue(object->maximumDepth    ());
    _time_limits->removeValue(object->minimumTime     ());
    _time_limits->removeValue(object->maximumTime     ());
    _vrms_limits->removeValue(object->minimumVrms     ());
    _vrms_limits->removeValue(object->maximumVrms     ());
     _vav_limits->removeValue(object->minimumVav      ());
     _vav_limits->removeValue(object->maximumVav      ());
    _vint_limits->removeValue(object->minimumVint     ());
    _vint_limits->removeValue(object->maximumVint     ());
    _vnmo_limits->removeValue(object->minimumVnmo     ());
    _vnmo_limits->removeValue(object->maximumVnmo     ());
   _thick_limits->removeValue(object->minimumThickness());
   _thick_limits->removeValue(object->maximumThickness());
  _offset_limits->removeValue(object->minimumOffset   ());
  _offset_limits->removeValue(object->maximumOffset   ());
}


void  VfFunctionArray::afterVelfunChange (VfFunction *object)
{
  int error    = object->getErrorFlag();
  int raytrace = object->getRaytraceFlag();
  int blank    = object->vfidIsBlank();
  if(error    != ERROR_NONE ) _nerr++;
  if(raytrace != RAYTRACE_NO) _nray++;
  if(blank                  ) _nblank++;
    _xloc_limits->insertValue(object->getXloc         ());
    _yloc_limits->insertValue(object->getYloc         ());
   _depth_limits->insertValue(object->minimumDepth    ());
   _depth_limits->insertValue(object->maximumDepth    ());
    _time_limits->insertValue(object->minimumTime     ());
    _time_limits->insertValue(object->maximumTime     ());
    _vrms_limits->insertValue(object->minimumVrms     ());
    _vrms_limits->insertValue(object->maximumVrms     ());
     _vav_limits->insertValue(object->minimumVav      ());
     _vav_limits->insertValue(object->maximumVav      ());
    _vint_limits->insertValue(object->minimumVint     ());
    _vint_limits->insertValue(object->maximumVint     ());
    _vnmo_limits->insertValue(object->minimumVnmo     ());
    _vnmo_limits->insertValue(object->maximumVnmo     ());
   _thick_limits->insertValue(object->minimumThickness());
   _thick_limits->insertValue(object->maximumThickness());
  _offset_limits->insertValue(object->minimumOffset   ());
  _offset_limits->insertValue(object->maximumOffset   ());
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

