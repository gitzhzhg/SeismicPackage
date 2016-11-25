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
#ifndef _FG2D_STATICS_DATA_HH
#define _FG2D_STATICS_DATA_HH

#include "fg2d/fg2d_data.hh"
#include "fgmap/fg_loc_out.hh"

class Fg2DStaticsData : public Fg2DData
{
	public:

		Fg2DStaticsData(class FieldGeometry *fg, long lineIndex,
			class StaticsFile *sf,
			int activeColorIndex, int selectedColorIndex,
			int normalColorIndex,
			int activeDependent , int selectedDependent ,
			long id = defaultId);
		virtual ~Fg2DStaticsData();

		/*
		 * Virtual functions from BaseData.
		 */
		virtual int   getNumPts       (       long id = defaultId);
		virtual float getX            (int i, long id = defaultId);
		virtual float getY            (int i, long id = defaultId);
		virtual int  getMarkerType    (int i, long id = defaultId);
		virtual int  getAltMarkerColor(int i, long id = defaultId);

		/*
		 * Virtual function from Fg2DData.
		 */
		virtual int getRange(float *xMin, float *xMax,
			float *yMin, float *yMax);

		void getMarkerPrecedence(int *activeDependent,
			int *selectedDependent);
		void setMarkerPrecedence(int  activeDependent,
			int  selectedDependent);
		long getLineIndex();
		void setActiveByIndex(int index);
		void setSelectedByIndex(int *indices, int num,
			char c, int threshold);
		void outputByXlatByIndex(int index, long *line_index,
			long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type);

	private:

		long _lineIndex, _firstGroundPosition;
		class StaticsFile *_sf;
		int _activeColorIndex, _selectedColorIndex, _normalColorIndex;
		int _activeDependent , _selectedDependent ;

		float getStatic(int i);

		Fg2DStaticsData()
			: Fg2DData((class FieldGeometry *) 0, defaultId)
			{ /* private, no access to default constructor */ }
		Fg2DStaticsData(Fg2DStaticsData &)
			: Fg2DData((class FieldGeometry *) 0, defaultId)
			{ /* private, no access to copy constructor */ }
		Fg2DStaticsData& operator=(Fg2DStaticsData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_STATICS_DATA_HH */
