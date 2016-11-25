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
#ifndef _FG2D_CHART_HH
#define _FG2D_CHART_HH

/*
 * Fg2DChart is to provide functionality common to ground postition
 * charts and stacking charts.  This is to reset the marker colors
 * whenever thers is a zero-trace table inform.
 */

#include "fg2d/fg2d_plot.hh"
#include "fg2d/fg2d_chart_data.hh"

class Fg2DChart : public Fg2DPlot
{
	public:

		Fg2DChart(class FieldGeometry *fg,
			class FgSeisPlotList *spList,
			Fg2DChartData::ActiveDriver activeDriver
				= Fg2DChartData::ActiveFlag,
			Fg2DChartData::SelectDriver selectDriver
				= Fg2DChartData::SelectedFlag);
		virtual ~Fg2DChart();

		/*
		 * Fg2DPlot virtual functions.
		 */
		virtual void setMarkerColors(char *active, char *selected,
			char *normal);
		virtual void setMarkerPrecedence(int active, int selected);
		virtual void setLineColors(char * /*active*/,
			char * /*selected*/, char * /*normal*/)
			{ /* Ignore, no lines */ }
		virtual void setLinePrecedence(int /*active*/, int /*selected*/)
			{ /* Ignore, no lines */ }
		virtual void setFlagMode(FlagMode flagMode);
		virtual int setActive(int x, int y, class PlotBase *plot);
		virtual Boolean outputByXlat(class SeisPlot *sp, int x, int y,
			long *line_index, long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type);

		void setActiveDriver(Fg2DChartData::ActiveDriver activeDriver);
		void setSelectDriver(Fg2DChartData::SelectDriver selectDriver);

	protected:

		enum {
			  _ACTIVE_COLOR_INDEX = 1,
			_SELECTED_COLOR_INDEX = 2,
			  _NORMAL_COLOR_INDEX = 3
		};

		Fg2DChartData::ActiveDriver _activeDriver;
		Fg2DChartData::SelectDriver _selectDriver;

		/*
		 * FgInform virtual functions.
		 */
		virtual void  preZt1ValuesChanged(FieldGeometry *fg,
			int ident, long index, long nrem, long nins);
		virtual void postZt1ValuesChanged(FieldGeometry *fg,
			int ident, long index, long nrem, long nins);
		virtual void  preZt2ValuesChanged(FieldGeometry *fg,
			int ident, long index, long nrem, long nins);
		virtual void postZt2ValuesChanged(FieldGeometry *fg,
			int ident, long index, long nrem, long nins);
		virtual void  preZt3ValuesChanged(FieldGeometry *fg,
			int ident, long index, long nrem, long nins);
		virtual void postZt3ValuesChanged(FieldGeometry *fg,
			int ident, long index, long nrem, long nins);
		virtual void  preZt4ValuesChanged(FieldGeometry *fg,
			int ident, long index, long nrem, long nins);
		virtual void postZt4ValuesChanged(FieldGeometry *fg,
			int ident, long index, long nrem, long nins);
		virtual void  preTredValuesChanged(FieldGeometry *fg);
		virtual void postTredValuesChanged(FieldGeometry *fg);
		virtual void startingChanges(FieldGeometry *fg);
		virtual void finishedChanges(FieldGeometry *fg);

	private:

		int _mustDetermineTraceTypes;

		virtual Fg2DChartData *getChartData() = 0;

		void  preZtAnyValuesChanged();
		void postZtAnyValuesChanged();

		Fg2DChart()
			: Fg2DPlot((class FieldGeometry  *) 0,
				   (class FgSeisPlotList *) 0)
			{ /* private, no access to default constructor */ }
		Fg2DChart(Fg2DChart &)
			: Fg2DPlot((class FieldGeometry  *) 0,
				   (class FgSeisPlotList *) 0)
			{ /* private, no access to copy constructor */ }
		Fg2DChart& operator=(Fg2DChart &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_CHART_HH */
