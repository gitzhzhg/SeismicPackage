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
#ifndef _FG2D_PLOT_HH
#define _FG2D_PLOT_HH

/*
 * Fg2DPlot is to provide virtual functions to set colors and
 * color precidence.
 */

#include "geom/fg_inform.hh"
#include "fgmap/fg_loc_out.hh"
#include "fgxp/fgxp_constants.hh"

class Fg2DPlot : public FgInform
{
	public:

		Fg2DPlot(class FieldGeometry *fg, class FgSeisPlotList *spList);
		virtual ~Fg2DPlot();

		virtual void setMarkerColors(char *active, char *selected,
			char *normal) = 0;
		virtual void setMarkerPrecedence(int active, int selected) = 0;
		virtual void setLineColors(char *active, char *selected,
			char *normal) = 0;
		virtual void setLinePrecedence(int active, int selected) = 0;
		virtual void setFlagMode(FlagMode flagMode) = 0;

		/*
		 * For Fg2DPick
		 * Fg2DPlot setActive and setSelected will work.
		 * For performance, setActive is overridden by
		 * Fg2DGpc and Fg2DSc.
		 */
		virtual int setActive(int x, int y, class PlotBase *plot);
		virtual int setSelected(int x1, int x2, int y1, int y2,
			class PlotBase *plot, char c, int threshold = 10000); 

		/*
		 * For Fg2DLocOut
		 * Fg2DPlot outputByXlat will work.
		 * For performance, outputByXlat is overridden by
		 * Fg2DGpc and Fg2DSc.
		 */
		virtual Boolean outputByXlat(class SeisPlot *sp, int x, int y,
			long *line_index, long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type);

	protected:

		class FgSeisPlotList *_spList;
		class SeisVectLinkedList *_vectors;

		/*
		 * FgInform virtual functions.
		 */
		virtual void startingChanges           (FieldGeometry *fg);
		virtual void finishedChanges           (FieldGeometry *fg);
		virtual void freezingDependentUpdates  (FieldGeometry *fg);
		virtual void dependentValuesOutOfDate  (FieldGeometry *fg);
		virtual void  preResumeDependentUpdates(FieldGeometry *fg);
		virtual void postResumeDependentUpdates(FieldGeometry *fg);

		virtual int isPlotted () = 0;

	private:

		int _ignoreFinishedChanges;

		virtual int okToPlot  () = 0;
		virtual int changePlot() = 0;
		virtual void displayPlot() = 0;
		virtual void  removePlot() = 0;
		virtual void setActiveByIndex(int index) = 0;
		virtual void setSelectedByIndex(int *indices, int num,
			char c, int threshold) = 0;
		virtual void outputByXlatByIndex(int index,
			long *line_index, long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type) = 0;

		Fg2DPlot()
			: FgInform((class FieldGeometry *) 0)
			{ /* private, no access to default constructor */ }
		Fg2DPlot(Fg2DPlot &)
			: FgInform((class FieldGeometry *) 0)
			{ /* private, no access to copy constructor */ }
		Fg2DPlot& operator=(Fg2DPlot &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_PLOT_HH */
