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
#ifndef CBB_VECT_DATA_HH
#define CBB_VECT_DATA_HH

#include "oprim/base_data.hh"
#include <assert.h>

class CBBVectData : public BaseData
{

public:
  CBBVectData					// Constructor
    (int numPts,				//   Number of points
     float x,					//   constant X-coordinate
     float *y,					//   Y-coordinates
     long id = defaultId);			//   id of vector

  virtual ~CBBVectData ();			// destructor

  virtual int getNumPts				// return number of points
    (long id = defaultId)			//   id of vector
    { assert (id == _id);  return _numPts; }

  virtual float getX				// return X-coordinate given
    (int /* i */,				//   index of point, and
     long id = defaultId)			//   id of vector
    { assert (id == _id);  return _xConst; }

  virtual float getY				// return Y-coordinate given
    (int i,					//   index of point, and
     long id = defaultId)			//   id of vector
    { assert (id == _id);  return _yData[i]; }

  virtual void setXConst			// set X-coordinate given
    (float x,					//   constant X-coordinate
     long id = defaultId)			//   id of vector
    { assert (id == _id);  _xConst = x; }

  void insert					// insert into vector
    (int index,					//   beg at this index
     int numIns,				//   number of pnts to insert
     float *y);					//   Y-coordinates to insert

  void remove					// remove from vector
    (int index,					//   beg at this index
     int numRem);				//   number of pnts to remove

  void replace					// remove from vector
    (int index,					//   beg at this index
     int numRep,				//   number of pnts to replace
     float *y);					//   Y-coordinates to insert

  void replace					// remove from vector
    (int index,					//   beg at this index
     int numRem,				//   number of pnts to remove
     int numIns,				//   number of pnts to insert
     float *y);					//   Y-coordinates to insert

  void setId					// set id of vector
    (long id)					//   id of vector
    { _id = id; }

  long getId ()					// get id of vector
    { return _id; }

protected:
  int
    _numPts;					// length of vector

  float
    *_yData;					// Y-coordinate array

private:
  long
    _id;					// id of vector

  float
    _xConst;					// constant X-coordinate

  CBBVectData () {}				// no acess to def construct

  CBBVectData					// no acess to cpy construct
    (CBBVectData &) {}				//   reference vector

  CBBVectData& operator=			// no acess to = operator
    (CBBVectData &p)				//   reference vector
    { return p; }

};

#endif
