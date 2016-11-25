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
#ifndef _FG2D_SC_HH
#define _FG2D_SC_HH

#include "fg2d/fg2d_chart.hh"

class Fg2DSc : public Fg2DChart
{
	public:

		Fg2DSc(class FieldGeometry *fg, class FgSeisPlotList *spList,
			class SeisPlot *sp,
			Fg2DChartData::ActiveDriver activeDriver
				= Fg2DChartData::ActiveFlag,
			Fg2DChartData::SelectDriver selectDriver
				= Fg2DChartData::SelectedFlag,
			int inLine = 1,
			unsigned int markerSize = 9,
			unsigned int markerLineWidth = 1);
		virtual ~Fg2DSc();

	protected:

		/*
		 * FgInform virtual functions.
		 */
		virtual void  preNewActiveCmp(FieldGeometry *fg);
		virtual void postNewActiveCmp(FieldGeometry *fg);
		virtual void  midpointGathersOutOfDate(FieldGeometry *fg);
		virtual void  preUpdateMidpointGathers(FieldGeometry *fg);
		virtual void postUpdateMidpointGathers(FieldGeometry *fg);
		virtual void startingChanges     (FieldGeometry *fg);
		virtual void  preNewGridTransform(FieldGeometry *fg);
		virtual void postNewGridTransform(FieldGeometry *fg);
		virtual void finishedChanges     (FieldGeometry *fg);

		/*
		 * Virtual functions from Fg2DPlot.
		 */
		virtual int isPlotted ();

	private:

		class SeisPlot   *_sp  ;
		class Fg2DScData *_data;

		int _inLine;
		int _newGridTransform;
		double _grid;
		unsigned int _markerSize, _markerLineWidth;

		/*
		 * Virtual functions from Fg2DPlot.
		 */
		virtual int okToPlot  ();
		virtual int changePlot();
		virtual void displayPlot();
		virtual void  removePlot();
		virtual void setActiveByIndex(int index);
		virtual void setSelectedByIndex(int *indices, int num,
			char c, int threshold);
		virtual void outputByXlatByIndex(int index,
			long *line_index, long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type);

		/*
		 * Virtual functions from Fg2DChart.
		 */
		virtual class Fg2DChartData *getChartData();

		Fg2DSc()
			: Fg2DChart((class FieldGeometry  *) 0,
				    (class FgSeisPlotList *) 0)
			{ /* private, no access to default constructor */ }
		Fg2DSc(Fg2DSc &)
			: Fg2DChart((class FieldGeometry  *) 0,
				    (class FgSeisPlotList *) 0)
			{ /* private, no access to copy constructor */ }
		Fg2DSc& operator=(Fg2DSc &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_SC_HH */
