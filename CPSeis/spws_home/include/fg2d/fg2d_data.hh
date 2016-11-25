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
#ifndef _FG2D_DATA_HH
#define _FG2D_DATA_HH

/*
 * Fg2DData includes marker color change methods needed
 * in all fg2d data classes.
 */

#include "oprim/base_data.hh"

class Fg2DData : public BaseData
{
	public:

		Fg2DData(class FieldGeometry *fg, long id = defaultId);
		virtual ~Fg2DData();
		virtual int getRange(float *xMin, float *xMax,
			float *yMin, float *yMax) = 0;
		void markerColorFlush();
		void  storeMarkerColorsIndices();
		void updateMarkerColorsIndices();
		void forgetIt();

		/*
		 * Virtual functions from BaseData.
		 */
		virtual int getNumPts(long id = defaultId);

	protected:

		class FieldGeometry *_fg;
		enum { _ALLOCATION_INC = 1000 };
		long _id;
		int _numPts;
                int *_modifiedMarkerIndex;
                int  _numModifiedMarkers ;

	private:

		int *_oldMarkerColorIndex;

		Fg2DData(Fg2DData &)
			{ /* private, no access to copy constructor */ }
		Fg2DData& operator=(Fg2DData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_DATA_HH */
