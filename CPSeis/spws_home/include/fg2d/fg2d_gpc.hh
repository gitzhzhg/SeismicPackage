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
#ifndef _FG2D_GPC_HH
#define _FG2D_GPC_HH

#include "fg2d/fg2d_chart.hh"
#include "vect/vector.hh"

class Fg2DGpc : public Fg2DChart
{
	public:

		Fg2DGpc(class FieldGeometry *fg, class FgSeisPlotList *spList,
			class SeisPlot *sp,
			Fg2DChartData::ActiveDriver activeDriver
				= Fg2DChartData::ActiveFlag,
			Fg2DChartData::SelectDriver selectDriver
				= Fg2DChartData::SelectedFlag,
			unsigned int markerSize = 9,
			unsigned int markerLineWidth = 1,
			Vector::VectorMarker srcMarker = Vector::SquareMarker,
			Vector::VectorMarker rcvMarker = Vector::XMarker);
		virtual ~Fg2DGpc();

	protected:

		/*
		 * FgInform virtual functions.
		 */
		virtual void  preNewActiveLine(FieldGeometry *fg);
		virtual void postNewActiveLine(FieldGeometry *fg);
		virtual void  receiverGathersOutOfDate(FieldGeometry *fg);
		virtual void  preUpdateReceiverGathers(FieldGeometry *fg);
		virtual void postUpdateReceiverGathers(FieldGeometry *fg);
		virtual void  preUpdateMidpointGathers(FieldGeometry *fg);
		virtual void postUpdateMidpointGathers(FieldGeometry *fg);

		/*
		 * Virtual functions from Fg2DPlot.
		 */
		virtual int isPlotted ();

	private:

		class SeisPlot    *_sp  ;
		class Fg2DGpcData *_data;

		unsigned int _markerSize, _markerLineWidth;
		int _srcMarker, _rcvMarker;
		int _waiting_for_midpoints;

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

		Fg2DGpc()
			: Fg2DChart((class FieldGeometry  *) NULL,
				    (class FgSeisPlotList *) NULL)
			{ /* private, no access to default constructor */ }
		Fg2DGpc(Fg2DGpc &)
			: Fg2DChart((class FieldGeometry  *) NULL,
				    (class FgSeisPlotList *) NULL)
			{ /* private, no access to copy constructor */ }
		Fg2DGpc& operator=(Fg2DGpc &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_GPC_HH */
