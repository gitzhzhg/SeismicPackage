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
#ifndef _FG2D_STATICS_HH
#define _FG2D_STATICS_HH

#include "fg2d/fg2d_plot.hh"

class Fg2DStatics : public Fg2DPlot
{
	public:

		Fg2DStatics(FieldGeometry *fg,
			class FgSeisPlotList *spList,
			class SeisPlot *sp,
			class StaticsReadPop *pop,
			unsigned int width = 2,
			unsigned int markerSize = 9,
			unsigned int markerLineWidth = 1);
		virtual ~Fg2DStatics();

		void newStaticsFile();

		/*
		 * Fg2DPlot virtual functions.
		 */
		virtual void setMarkerColors(char *active, char *selected,
			char *normal);
		virtual void setMarkerPrecedence(int active, int selected);
		virtual void setLineColors(char *active, char *selected,
			char *normal);
		virtual void setLinePrecedence(int active, int selected);
		virtual void setFlagMode(FlagMode flagMode);

	protected:

		/*
		 * FgInform virtual functions.
		 */
		virtual void  startingChanges(FieldGeometry *fg);
		virtual void  finishedChanges(FieldGeometry *fg);
		virtual void   preRemoveInsertFlags(FieldGeometry *fg,
			long ixl, long index, long nrem, long nins);
		virtual void  postRemoveInsertFlags(FieldGeometry *fg,
			long ixl, long index, long nrem, long nins);
		virtual void  preNewActiveLine(FieldGeometry *fg);
		virtual void postNewActiveLine(FieldGeometry *fg);
		virtual void  sourceGathersOutOfDate(FieldGeometry *fg);
		virtual void  preUpdateSourceGathers(FieldGeometry *fg);
		virtual void postUpdateSourceGathers(FieldGeometry *fg);

		/*
		 * Virtual functions from Fg2DPlot.
		 */
		virtual int isPlotted ();

	private:

		enum {
			  _ACTIVE_COLOR_INDEX = 1,
			_SELECTED_COLOR_INDEX = 2,
			  _NORMAL_COLOR_INDEX = 3
		};

		class SeisPlot        *_sp  ;
		class StaticsReadPop  *_pop ;
		class Fg2DStaticsData *_data;
		unsigned int _width, _markerSize, _markerLineWidth;
		int _flagsRemIns;

		/*
		 * Virtual functions from Fg2DPlot.
		 */
		virtual int okToPlot  ();
		virtual int changePlot();
		virtual void displayPlot();
		virtual void removePlot ();
		virtual void setActiveByIndex(int index);
		virtual void setSelectedByIndex(int *indices, int num,
			char c, int threshold);
		virtual void outputByXlatByIndex(int index,
			long *line_index, long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type);

		Fg2DStatics()
			: Fg2DPlot((      FieldGeometry  *) 0,
				   (class FgSeisPlotList *) 0)
			{ /* private, no access to default constructor */ }
		Fg2DStatics(Fg2DStatics &)
			: Fg2DPlot((      FieldGeometry  *) 0,
				   (class FgSeisPlotList *) 0)
			{ /* private, no access to copy constructor */ }
		Fg2DStatics& operator=(Fg2DStatics &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_STATICS_HH */
