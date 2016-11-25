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
#ifndef _FG2D_GPC_DATA_HH
#define _FG2D_GPC_DATA_HH

#include "fg2d/fg2d_chart_data.hh"
#include "fgmap/fg_loc_out.hh"

class Fg2DGpcData : public Fg2DChartData
{
	public:

		enum MarkerType
		{
			Source  ,
			Receiver
		};

		Fg2DGpcData(class FieldGeometry *fg, long lineIndex,
			int srcMarker, int rcvMarker,
			int activeColorIndex, int selectedColorIndex,
			int normalColorIndex,
			int activeDependent , int selectedDependent ,
			ActiveDriver activeDriver = ActiveFlag  ,
			SelectDriver selectDriver = SelectedFlag,
			long id = defaultId);
		virtual ~Fg2DGpcData();

		/*
		 * Add all your points before creating vector.
		 * addPoint does not notify data users.
		 */
		void addPoint(long flagIndex, long groupNumber,
			long traceNumber, MarkerType markerType);
		void adjustArray();

		/*
		 * Virtual functions from BaseData.
		 */
		virtual float getX       (int i, long id = defaultId);
		virtual float getY       (int i, long id = defaultId);
		virtual int getMarkerType(int i, long id = defaultId);

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
		 * This is also used by the FgInform part of Fg2DGpcPlot
		 * to change marker colors.
		 */
		void actSelDeadRevMissChanged(long flagIndex, int num);

		long getLineIndex();

	private:

		typedef struct {
			long flagIndex;
			long groupNumber;
			long traceNumber;	/* only for rcvs, -1 for srcs */
			long srcFlagIndex;	/*  flagIndex for srcs */
						/* -1 for rcvs with src */
						/* on different line */
			MarkerType markerType;
			TraceType traceType;
		} GpcPoint;

		GpcPoint *_gpcPoint;
		int _arraySize;
		long _minFlagIndex  , _maxFlagIndex  ;
		long _minGroupNumber, _maxGroupNumber;

		long _lineIndex, _firstGroundPosition;

		int _srcMarker, _rcvMarker;

		virtual int isActive  (int i);
		virtual int isSelected(int i);
		virtual int getIndexByGroup(long group, long seqGndPos);

		static int compar(const void *element1, const void *element2);
		static int gpcCom(const void *element1, const void *element2);

		Fg2DGpcData()
			: Fg2DChartData((class FieldGeometry *) 0,
			0, 0, 0, 0, 0,
			ActiveFlag, SelectedFlag, defaultId)
			{ /* private, no access to default constructor */ }
		Fg2DGpcData(Fg2DGpcData &)
			: Fg2DChartData((class FieldGeometry *) 0,
			0, 0, 0, 0, 0,
			ActiveFlag, SelectedFlag, defaultId)
			{ /* private, no access to copy constructor */ }
		Fg2DGpcData& operator=(Fg2DGpcData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_GPC_DATA_HH */
