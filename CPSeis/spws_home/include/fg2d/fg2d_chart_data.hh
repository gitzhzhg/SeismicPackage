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
#ifndef _FG2D_CHART_DATA_HH
#define _FG2D_CHART_DATA_HH

#include "fg2d/fg2d_data.hh"
#include "fgmap/fg_loc_out.hh"

class Fg2DChartData : public Fg2DData
{
	public:

		enum TraceType
		{
			Dead   ,
			RevPol ,
			Missing,
			Normal ,
			Unknown
		};

		enum ActiveDriver
		{
			ActiveFlag         ,
			ActiveFlagSrc      ,
			ActiveFlagRcv      ,
			ActiveCmp          ,
			ActiveTrace        ,
			ActiveGroup        ,
			ActiveFlagSrcGroups
		};

		enum SelectDriver
		{
			SelectedFlag   ,
			SelectedFlagSrc,
			SelectedFlagRcv,
			SelectedCmp    ,
			DeadTraces     ,
			RevPolTraces   ,
			MissingTraces
		};

		Fg2DChartData(class FieldGeometry *fg,
			int activeColorIndex, int selectedColorIndex,
			int normalColorIndex,
			int activeDependent , int selectedDependent ,
			ActiveDriver activeDriver = ActiveFlag  ,
			SelectDriver selectDriver = SelectedFlag,
			long id = defaultId);
		virtual ~Fg2DChartData()
			{ /* do nothing */ }

		virtual void determineTraceTypes() = 0;
		virtual void setActiveByIndex(int index) = 0;
		virtual void setSelectedByIndex(int *indices, int num, char c,
			int threshold) = 0;
		virtual void outputByXlatByIndex(int index, long *line_index,
			long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type) = 0;

		/*
		 * Virtual functions from BaseData.
		 */
		virtual int getAltMarkerColor(int i, long id = defaultId);

		void setActiveDriver(ActiveDriver activeDriver);
		void setSelectDriver(SelectDriver selectDriver);
		void getMarkerPrecedence(int *activeDependent,
			int *selectedDependent);
		void setMarkerPrecedence(int  activeDependent,
			int  selectedDependent);
		int setActiveByGroup(long group, long seqGndPos);
		Boolean outputByXlatByGroup(long group, long seqGndPos,
			long *line_index, long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type);
		Fg2DChartData::TraceType getTraceType(long trace);

	protected:

		ActiveDriver _activeDriver;
		SelectDriver _selectDriver;

	private:

		int _activeColorIndex, _selectedColorIndex, _normalColorIndex;
		int _activeDependent , _selectedDependent ;

		virtual int isActive  (int i) = 0;
		virtual int isSelected(int i) = 0;
		virtual int getIndexByGroup(long group, long seqGndPos) = 0;

		Fg2DChartData()
			: Fg2DData((class FieldGeometry *) 0, defaultId)
			{ /* private, no access to default constructor */ }
		Fg2DChartData(Fg2DChartData &)
			: Fg2DData((class FieldGeometry *) 0, defaultId)
			{ /* private, no access to copy constructor */ }
		Fg2DChartData& operator=(Fg2DChartData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_CHART_DATA_HH */
