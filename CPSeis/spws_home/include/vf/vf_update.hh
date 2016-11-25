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

//-------------------------- vf_update.hh -------------------------//
//-------------------------- vf_update.hh -------------------------//
//-------------------------- vf_update.hh -------------------------//

//                header file for the VfUpdate class
//                    not derived from any class
//                          subdirectory vf

           // This class contains the algorithm for updating
           // velocity function picks for all velocity types.
           // This class also is provided a pointer to offset
           // information for ray tracing.  This class contains
           // information about the rays that were last traced.

           // If the velocities in this dataset are not really
           // velocities, but are some other kind of attribute,
           // you should set the optional BOGUS_VELOCITIES argument
           // to true.  When using BOGUS_VELOCITIES, all velocity
           // types are mutually independent of each other, and
           // no conversions are made between time and depth.
           // There is no check on the validity of the attribute
           // stored in a velocity column; the attribute can be
           // positive, negative, zero, or nil for any pick.
           // Changing one attribute has no effect on any other
           // attribute.  Since only two columns (normally a
           // velocity and a time or depth) are written to a
           // velocity file, you should save attributes as only
           // one velocity type and not change the type.  It is
           // recommended that the velocity type be either VTIN
           // or VZIN.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_UPDATE_HH_
#define _VF_UPDATE_HH_

#include "vf/vf_constants.hh"
#include "named_constants.h"


class VfUpdate
{

//-------------------------- data ---------------------------------//
//-------------------------- data ---------------------------------//
//-------------------------- data ---------------------------------//

private:   // _startvel and _maxtime can be FNIL or zero to disable.

  class VfUtilities  *_utilities;
  class VfOffsets    *_offsets;

  int    _bogus_velocities;   // true if not really velocities.

  float  _startvel;     // velocity to use when fixing up first pick.
  float  _maxtime;      // maximum time to use when maybe truncating.
  long   _nray;         // number of rays traced.
  float  _oray[200];    // offsets of traced rays.
  float  _tray[200];    // travel times of traced rays.


//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//

public:    // constructor and destructor.

           VfUpdate (VfUtilities *utilities, VfOffsets *offsets,
                                int bogus_velocities = FALSE);
  virtual ~VfUpdate ();

public:    // get values.

  int           velocitiesAreBogus   ()  const  { return _bogus_velocities; }
  float         getStartvel          ()  const  { return _startvel; }
  float         getMaxtime           ()  const  { return _maxtime; }
  VfOffsets    *getVfOffsets         ()  const  { return _offsets; }
  long          numRays              ()  const  { return _nray; }
  const float  *raytracedOffsets     ()  const  { return _oray; }
  const float  *raytracedTravelTimes ()  const  { return _tray; }
  
public:    // set values.

  void         setStartvel          (float value);
  void         setMaxtime           (float value);

public:    // update velocity function picks.
           // to be called by VfFunction only.
           // no harm is done if called by a user.
           // uses information in _offsets to trace rays if invoke is TRUE.
           // sets _nray, _oray, and _tray.
           // returns error == TRUE or FALSE.

  int updatePicks (const int   invoke,       //    input
                   const int   type,         //    input
                   long       *npicks,       // input/output
                   float      *depth,        // input/output
                   float      *time,         // input/output
                   float      *vnmo,         // input/output
                   float      *vrms,         // input/output
                   float      *vav,          // input/output
                   float      *vint,         // input/output
                   float      *thick,        // input/output
                   float      *offset);      //    output

private:

  void maybeTruncateTime (int type, long npicks, float *time);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
