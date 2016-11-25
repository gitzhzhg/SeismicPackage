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

//----------------------- vect_ydata.hh --------------------//
//----------------------- vect_ydata.hh --------------------//
//----------------------- vect_ydata.hh --------------------//

#ifndef _VECT_YDATA_HH_
#define _VECT_YDATA_HH_

//  This class is identical to the VectData class except that the
//      X coordinate is assumed to be the index plus one, and no X array
//      is used inside the class.  Also, some extra features are
//      included.
//  See vect_ydata.cc for further rationale and documentation.

#include "oprim/base_data.hh"
#include <assert.h>

class VectYdata : public BaseData
{

//------------------------- contents of class -----------------------//
//------------------------- contents of class -----------------------//
//------------------------- contents of class -----------------------//

public:

	VectYdata(int numPts, float *y, long id = defaultId);
	virtual ~VectYdata();

	virtual int getNumPts              (long id = defaultId)
		{ assert(id == _id);  return _numPts; }
	virtual float getX          (int i, long id = defaultId)
		{ assert(id == _id);  return (float)(i + 1); }
	virtual float getY          (int i, long id = defaultId);
        virtual int   getMarkerType (int i, long id = defaultId);
        virtual int   getYOffsetType(int i, long id = defaultId);

	void  insert (int index, int numIns, float *y);
	void  remove (int index, int numRem);
	void  replace(int index, int numRep, float *y);
	void  replace(int index, int numRem, int numIns, float *y);
	float fetch  (int index);
	void  fetch  (int index, int numFetch, float *y);

	void setId(long id)
		{ _id = id; }
	long getId()
		{ return _id; }

        void useNormalMarker  (int   normal_marker);
        void useSpecialMarker (float special_yvalue,
                               int   special_marker,
                               float special_ycoord,
                               int   offset_flag);

private:

	long   _id;
	int    _numPts;
	float *_yData;

        enum  { _MAX_NUM_SPECIAL_MARKERS = 4 };
        int     _num_special_markers;
        int     _normal_marker;
        int     _special_marker [_MAX_NUM_SPECIAL_MARKERS];
        float   _special_yvalue [_MAX_NUM_SPECIAL_MARKERS];
        float   _special_ycoord [_MAX_NUM_SPECIAL_MARKERS];
        int     _offset_flag    [_MAX_NUM_SPECIAL_MARKERS];

	VectYdata()
		{ /* private, no access to default constructor */ }
	VectYdata(VectYdata &)
		{ /* private, no access to copy constructor */ }
	VectYdata& operator=(VectYdata &p)
		{ /* private, no access to = */ return p; }

//----------------------- end of contents --------------------------//
//----------------------- end of contents --------------------------//
//----------------------- end of contents --------------------------//

};

#endif

//----------------------------- end --------------------------------//
//----------------------------- end --------------------------------//
//----------------------------- end --------------------------------//

