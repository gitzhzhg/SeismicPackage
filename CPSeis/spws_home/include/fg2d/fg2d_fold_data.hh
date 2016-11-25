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
#ifndef _FG2D_FOLD_DATA_HH
#define _FG2D_FOLD_DATA_HH

#include "fg2d/fg2d_data.hh"
#include "fgmap/fg_loc_out.hh"

/*
 * Fg2DFold uses 2 Fg2DFoldDatas, 1 for total and 1 for live.
 */

class Fg2DFoldData : public Fg2DData
{
	public:

		Fg2DFoldData(class FieldGeometry *fg,
			int activeColorIndex, int selectedColorIndex,
			int normalColorIndex,
			int activeDependent , int selectedDependent ,
			long id = defaultId);
		virtual ~Fg2DFoldData();

		/*
		 * Add all your points before creating vector.
		 * addPoint does not notify data users.
		 */
		void addPoint(float gridBinCenter, long ixcmp, long fold);
		void adjustArray();

		/*
		 * Virtual functions from BaseData.
		 */
		virtual float getX           (int i, long id = defaultId);
		virtual float getY           (int i, long id = defaultId);
		virtual int getAltMarkerColor(int i, long id = defaultId);

		/*
		 * Virtual function from Fg2DData.
		 */
		virtual int getRange(float *xMin, float *xMax,
			float *yMin, float *yMax);

		void getMarkerPrecedence(int *activeDependent,
			int *selectedDependent);
		void setMarkerPrecedence(int  activeDependent,
			int  selectedDependent);
		void setActiveByIndex(int index);
		void setSelectedByIndex(int *indices, int num,
			char c, int threshold);
		void outputByXlatByIndex(int index,
			long *line_index, long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type);

	private:

		typedef struct {
			float gridBinCenter;
			long ixcmp;
			long fold;
		} FoldPoint;

		FoldPoint *_foldPoint;
		int _arraySize;
		float _minGridBinCenter, _maxGridBinCenter;
		long _maxFold;	/* Use 0 for min. */

		int _activeColorIndex, _selectedColorIndex, _normalColorIndex;
		int _activeDependent , _selectedDependent ;

		Fg2DFoldData()
			: Fg2DData((class FieldGeometry *) 0, defaultId)
			{ /* private, no access to default constructor */ }
		Fg2DFoldData(Fg2DFoldData &)
			: Fg2DData((class FieldGeometry *) 0, defaultId)
			{ /* private, no access to copy constructor */ }
		Fg2DFoldData& operator=(Fg2DFoldData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_FOLD_DATA_HH */
