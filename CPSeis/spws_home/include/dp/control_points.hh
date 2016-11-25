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
// control_points.hh:  User interface file for ControlPoints class
#ifndef CONTROL_POINTS_HH
#define CONTROL_POINTS_HH
#include "dp/grid_error_codes.hh"

class ControlPoints {

public:
  ControlPoints					// constructor
    (long point_count,				//   number of control points
     int num_attribs=3);			//   number of attributes

  ControlPoints					// copy constructor
    (const ControlPoints &init);		//   left side of equal sign

  ~ControlPoints ();				// destructor

  float getMinimum				// return minimum value of attr
    (int attribute_index);			//   which attribute to return

  float getMaximum				// return maximum value of attr
    (int attribute_index);			//   which attribute to return

  void setExtremaLimits				// set the extrema limits
    (int attribute_index,			//   which attribute to set
     float minimum,				//   set to this minimum value
     float maximum);				//   set to this maximum value

  int imposeLimits ();				// throw out any control points
						//   that have an attribute
						//   outside the set limits.
  						//   reduce array size if loss

  float getMinimumLimit				// return set min value of attr
    (int attribute_index);			//   which attribute to return

  float getMaximumLimit				// return set max value of attr
    (int attribute_index);			//   which attribute to return

  float *getArray ()				// return cntrl pnt array ptr
    { return _control_points; }

  float get					// return a control point value
    (int attribute_index,			//   which attribute to return
     long spatial_index);			//   index of the control point

  float getNext					// return the next cntl pnt vlu
    (int attribute_index);			//   which attribute to return

  void set					// set a control point value
    (int attribute_index,			//   which attribute to set
     long spatial_index,			//   index of the control point
     float value);				//   set attrib to this value

  void setNext					// set the next cntrl pnt value
    (int attribute_index,			//   which attribute to set
     float value);				//   set attrib to this value

  long getCount ()				// return number of cntrl pnts
    { return _count; }

  int failed ();				// returns 1 if unsuccessful

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

private:
  int findExtrema ();				// find the extrema of cntl pts

  int
    _num_attribs,				// number of attributes
    _extrema_ready;				// flag (F=0): are extrma known

  long
    _count,					// number of control points
    _array_size,				// size of control point array
    *_get_indexes,				// attribute index for next get
    *_set_indexes,				// attribute index for next set
    *_offsets,					// ofst to 1st vlu of an attrib
    *_increments;				// incr btwn vlus of an attrib

  float
    **_ptr,					// temporary array of ptrs
    **_from_ptr,				// temporary array of ptrs
    **_to_ptr,					// temporary array of ptrs
    *_control_points,				// ptr to cntl pnt data array
    *_minimums,					// minimums for each attribute
    *_maximums,					// maximums for each attribute
    *_min_limits,				// min limit to impose ea attrb
    *_max_limits;				// max limit to impose ea attrb

  GridErrorCodes
    _error_status;				// error status flag
    
};

#endif
