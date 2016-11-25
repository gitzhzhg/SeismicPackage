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
#ifndef _FG2D_DATA_SC_HH
#define _FG2D_DATA_SC_HH

#include "fg2d/fg2d_chart_data.hh"
#include "fgmap/fg_loc_out.hh"

class Fg2DScData : public Fg2DChartData
{
	public:

		Fg2DScData(class FieldGeometry *fg,
			int activeColorIndex, int selectedColorIndex,
			int normalColorIndex,
			int activeDependent , int selectedDependent ,
			ActiveDriver activeDriver = ActiveFlag  ,
			SelectDriver selectDriver = SelectedFlag,
			long id = defaultId);
		virtual ~Fg2DScData();

		/*
		 * Add all your points before creating vector.
		 * addPoint does not notify data users.
		 */
		void addPoint(float gridBinCenter, long ixcmp,
			long groupNumber, long traceNumber);
		void adjustArray();

		/*
		 * Virtual functions from BaseData.
		 */
		virtual float getX(int i, long id = defaultId);
		virtual float getY(int i, long id = defaultId);

		/*
		 * Virtual function from Fg2DData.
		 */
		virtual int getRange(float *xMin, float *xMax,
			float *yMin, float *yMax);

		/*
		 * Virtual function from Fg2DChartData.
		 */
		virtual void determineTraceTypes();
		virtual void setActiveByIndex(int index);
		virtual void setSelectedByIndex(int *indices, int num,
			char c, int threshold);
		virtual void outputByXlatByIndex(int index,
			long *line_index, long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type);

		/*
		 * This is also used by the FgInform part of Fg2DScPlot
		 * to change marker colors.
		 */
		void actSelDeadRevMissChanged(long flagIndex, int num);

	private:

		typedef struct {
			long gridBinCenter;
			long ixcmp;
			long groupNumber;
			long traceNumber;
			TraceType traceType;
			long srcLineIndex;
			long srcFlagIndex;
			long rcvLineIndex;
			long rcvFlagIndex;
		} ScPoint;

		ScPoint *_scPoint;
		int _arraySize;
		float _minGridBinCenter, _maxGridBinCenter;
		long  _minGroupNumber  , _maxGroupNumber  ;
		static Fg2DScData *_thisData;

		virtual int isActive  (int i);
		virtual int isSelected(int i);
		virtual int getIndexByGroup(long group, long seqGndPos);

		static int comparCmps(const void *element1,
			const void *element2);
		static int comparRcvs(const void *element1,
			const void *element2);
		static int scCom(const void *element1, const void *element2);

		Fg2DScData()
			: Fg2DChartData((class FieldGeometry *) 0,
			0, 0, 0, 0, 0,
			ActiveFlag, SelectedFlag, defaultId)
			{ /* private, no access to default constructor */ }
		Fg2DScData(Fg2DScData &)
			: Fg2DChartData((class FieldGeometry *) 0,
			0, 0, 0, 0, 0,
			ActiveFlag, SelectedFlag, defaultId)
			{ /* private, no access to copy constructor */ }
		Fg2DScData& operator=(Fg2DScData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_DATA_SC_HH */
