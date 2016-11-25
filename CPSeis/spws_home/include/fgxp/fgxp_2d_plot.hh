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
#ifndef _FGXP_2D_PLOT_HH
#define _FGXP_2D_PLOT_HH

#include "fgxp/fgxp_plot.hh"

class FgXp2DPlot : public FgXpPlot
{
	public:

		FgXp2DPlot(class FieldGeometry *fg,
			class FgXpPlotLinkedList *fgXpPlotList,
			SelectMode selectMode = AllLines,
			DisplayMode displayMode = LinesAndFlags,
			int numIndices = 1,
			CardType xCardType = LdCardType,
			int xDataType = FG_XLOC, long xIndex = -1,
			CardType yCardType = LdCardType,
			int yDataType = FG_YLOC, long yIndex = -1,
			unsigned int width = 2,
			unsigned int markerSize = 9,
			unsigned int markerLineWidth = 1,
			Vector::VectorMarker srcMarker = Vector::SquareMarker,
			Vector::VectorMarker rcvMarker = Vector::XMarker,
			Vector::VectorMarker bothMarker
				= Vector::XInSquareMarker,
			Vector::VectorMarker neitherMarker
				= Vector::TriangleMarker ,
			Vector::VectorMarker noMarker = Vector::NoMarker,
			const char *label = (char *) NULL,
			const char *font = "fixed");
		virtual ~FgXp2DPlot();
		virtual char *getPlotLabel();

	protected:

		virtual void delayedDelete();
		virtual void postResumeDependentUpdates(FieldGeometry *fg);

	private:

		virtual BaseData *addData(long index    );
		virtual void   removeData(BaseData *data);

		FgXp2DPlot() : FgXpPlot((class FieldGeometry *) NULL,
			(class FgXpPlotLinkedList *) NULL)
			{ /* private, no access to default constructor */ }
		FgXp2DPlot(FgXp2DPlot &)
			: FgXpPlot((class FieldGeometry *) NULL,
			(class FgXpPlotLinkedList *) NULL)
			{ /* private, no access to copy constructor */ }
		FgXp2DPlot& operator=(FgXp2DPlot &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FGXP_2D_PLOT_HH */
