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

//-------------------------- vf_update.cc --------------------------//
//-------------------------- vf_update.cc --------------------------//
//-------------------------- vf_update.cc --------------------------//

//         implementation file for the VfUpdate class
//                    not derived from any class
//                         subdirectory vf



#include "vf/vf_update.hh"
#include "vf/vf_offsets.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "trslib.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//---------------------- constructor and destructor ---------------------//
//---------------------- constructor and destructor ---------------------//
//---------------------- constructor and destructor ---------------------//


VfUpdate::VfUpdate
            (VfUtilities *utilities, VfOffsets *offsets, int bogus_velocities)
     :
       _utilities            (utilities),
       _offsets              (offsets),
       _bogus_velocities     (bogus_velocities),
       _startvel             (FNIL),
       _maxtime              (FNIL),
       _nray                 (0)
{
  assert(_utilities && _offsets);
}



VfUpdate::~VfUpdate()
{
}



//------------------------------ set values -----------------------//
//------------------------------ set values -----------------------//
//------------------------------ set values -----------------------//


void VfUpdate::setStartvel(float value)
{
  if(value != FNIL && value < 0.0) value = 0.0;
  _startvel = value;
}


void VfUpdate::setMaxtime(float value)
{
  if(value != FNIL && value < 0.0) value = 0.0;
  _maxtime = value;
}



//------------------------ attrib update --------------------------------//
//------------------------ attrib update --------------------------------//
//------------------------ attrib update --------------------------------//

// if abscissa is time  (VTNM VTRM VTAV VTIN VTDP), no arrays are changed.
// if abscissa is depth (VZRM VZAV VZIN),           thick is calculated from depth.
// if abscissa is thick (VLRM VLAV VLIN),           depth is calculated from thick.

// if abscissa is time  (VTNM VTRM VTAV VTIN VTDP), error is generated if time  values are bad.
// if abscissa is depth (VZRM VZAV VZIN),           error is generated if depth values are bad.
// if abscissa is thick (VLRM VLAV VLIN),           error is generated if thick values are bad.

// arrays time, vnmo, vrms, vav, vint are never changed.
// arrays vnmo, vrms, vav, vint are never accessed.

static void attrib_update (int type, int npicks,             //    input
                           VfUtilities *utilities,           //    input
                           float *depth,                     // input/output
                           float *time,                      //    input
                           float *vnmo, float *vrms,         //    unused
                           float *vav, float *vint,          //    unused
                           float *thick,                     // input/output
                           int *nray,                        //    output
                           int *ierr)                        //    output
{
  *ierr = FALSE;
  if(utilities->abscissaIsTime(type))
      {
      for(int i = 0; i < npicks; i++)
          {
          if(time[i] == FNIL)               { *ierr = TRUE; break; }
          if(i == 0 && time[i] < 0.0)       { *ierr = TRUE; break; }
          if(i > 0 && time[i] <= time[i-1]) { *ierr = TRUE; break; }
          }
      }
  else if(utilities->abscissaIsDepth(type))
      {
      for(int i = 0; i < npicks; i++)
          {
          if(depth[i] == FNIL)                { *ierr = TRUE; break; }
          if(i == 0 && depth[i] < 0.0)        { *ierr = TRUE; break; }
          if(i > 0 && depth[i] <= depth[i-1]) { *ierr = TRUE; break; }
          if(i == 0) thick[i] = depth[i];
          else       thick[i] = depth[i] - depth[i-1];
          }
      }
  else
      {
      for(int i = 0; i < npicks; i++)
          {
          if(thick[i] == FNIL)                { *ierr = TRUE; break; }
          if(i == 0 && thick[i] < 0.0)        { *ierr = TRUE; break; }
          if(i == 0) depth[i] = thick[i];
          else       depth[i] = thick[i] + depth[i-1];
          }
      }
  *nray = 0;

  if(utilities->ordinateIsVNMO(type))
      {
      for(int i = 0; i < npicks; i++)
          {
          vrms[i] = FNIL;   // SimpleShortArray will free this array to save memory.
          vav [i] = FNIL;   // SimpleShortArray will free this array to save memory.
          vint[i] = FNIL;   // SimpleShortArray will free this array to save memory.
          }
      }
  if(utilities->ordinateIsVRMS(type))
      {
      for(int i = 0; i < npicks; i++)
          {
          vnmo[i] = FNIL;   // SimpleShortArray will free this array to save memory.
          vav [i] = FNIL;   // SimpleShortArray will free this array to save memory.
          vint[i] = FNIL;   // SimpleShortArray will free this array to save memory.
          }
      }
  if(utilities->ordinateIsVAV(type))
      {
      for(int i = 0; i < npicks; i++)
          {
          vrms[i] = FNIL;   // SimpleShortArray will free this array to save memory.
          vnmo[i] = FNIL;   // SimpleShortArray will free this array to save memory.
          vint[i] = FNIL;   // SimpleShortArray will free this array to save memory.
          }
      }
  if(utilities->ordinateIsVINT(type))
      {
      for(int i = 0; i < npicks; i++)
          {
          vrms[i] = FNIL;   // SimpleShortArray will free this array to save memory.
          vav [i] = FNIL;   // SimpleShortArray will free this array to save memory.
          vnmo[i] = FNIL;   // SimpleShortArray will free this array to save memory.
          }
      }
}



//---------------------- update picks -------------------------------//
//---------------------- update picks -------------------------------//
//---------------------- update picks -------------------------------//

   // public.
   // to be called by VfFunction only.
   // no damage is done if a user calls this function, since no
   //   member variables are changed in this VfUpdate class,
   //   and only const member functions are called in the _utilities
   //   and _offsets classes.  The only exceptions are the following
   //   variables, which get reset every time this function is called:
   //   _nray, _oray, and _tray.  Therefore this function could
   //   legitimately be called by a user to do some raytracing.

   // Given the velocity function type, and the values in the
   //   corresponding velocity arrays, fills out the rest of
   //   the arrays for all other types.
   // The velocity function does not have to start at zero time.
   // If invoke is TRUE: does ray-tracing to convert to or from type VTNM.
   // If invoke is FALSE: types VTNM and VTRM will be identical upon output.
   // The arrays must be dimensioned MAXPICKS since their length (and npicks)
   //   might be increased.
   // Sets values to nil if they cannot be calculated.
   // Returns ierr =  0 if all is OK.
   // Returns ierr = -1 if there are no picks.
   // Returns ierr = -2 if the type is invalid.
   // Returns ierr = -3 if difficult raytracing is not completed.
   // Returns ierr = first bad index (1 thru n) if some values are set to nil.


int VfUpdate::updatePicks
                      (const int  invoke,       //    input
                       const int  type,         //    input
                       long      *npicks,       // input/output
                       float     *depth,        // input/output
                       float     *time,         // input/output
                       float     *vnmo,         // input/output
                       float     *vrms,         // input/output
                       float     *vav,          // input/output
                       float     *vint,         // input/output
                       float     *thick,        // input/output
                       float     *offset)       //    output
{
  if(*npicks == 0) return -1;

  maybeTruncateTime(type, *npicks, time);

  int         npicks2     = (int)(*npicks);
  int         maxpicks    = MAXPICKS;
  const char *type_symbol = _utilities->typeSymbol(type);
  float       timetol     = _utilities->getTimeTolerance();
  float       depthtol    = _utilities->getDepthTolerance();

  velfun_fixup_first_pick_c (type_symbol,                      // input
                             &npicks2,                         // input/output
                             &maxpicks,                        // input
                             &_startvel, &timetol, &depthtol,  // input
                             depth, time, vnmo, vrms,          // input/output
                             vav, vint, thick);                // input/output

  const int    muteflag =      _offsets->getMuteFlag();
  const float  offmin   =      _offsets->getMinimumOffset();
  const float  offmax   =      _offsets->getMaximumOffset();
  const int    nmute    = (int)_offsets->numMuteTimes();
  const float *omute    =      _offsets->getMuteOffsetPointer();
  const float *tmute    =      _offsets->getMuteTimePointer();
  int          nray2;
  int          ierr;
  char         msg[222];

  for(long i = 0; i < MAXPICKS; i++) { offset[i] = -1; }

  if(_bogus_velocities)
      {
      attrib_update   (type, npicks2, _utilities,      //    input
                       depth,                          // input/output
                       time,                           //    input
                       vnmo, vrms, vav, vint,          //    unused
                       thick,                          // input/output
                       &nray2,                         //    output
                       &ierr);                         //    output
      }
  else
      {
      velfun_update_c (&invoke, type_symbol, &npicks2, //    input
                       &muteflag, &offmin, &offmax,    //    input
                       &nmute, omute, tmute,           //    input
                       depth, time,                    // input/output
                       vnmo, vrms, vav, vint,          // input/output
                       thick,                          // input/output
                       offset, &nray2, _oray, _tray,   //    output
                       msg, &ierr);                    //    output
      }

  *npicks = npicks2;
  _nray = nray2;
  return ierr;
}



//---------------------- maybe truncate time ----------------------//
//---------------------- maybe truncate time ----------------------//
//---------------------- maybe truncate time ----------------------//

     // private.
     // the last time is reset to _maxtime under strict circumstances.
     // the number of picks is not changed.

void VfUpdate::maybeTruncateTime(int type, long npicks, float *time)
{
  float timetol  = _utilities->getTimeTolerance();
  if(time == NULL) return;       // added 9 june 2010 by stoeckley
  if(_maxtime == FNIL || _maxtime <= 0.0) return;
  if(npicks == 0) return;
  if(type != VTNM && type != VTRM && type != VTAV && type != VTIN) return;
  if(time[npicks-1] == FNIL) return;
  if(time[npicks-1] < _maxtime - timetol) return;
  if(npicks >= 2)
      {
      if(time[npicks-2] == FNIL) return;
      if(time[npicks-2] >= _maxtime) return;
      }
  time[npicks-1] = _maxtime;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

