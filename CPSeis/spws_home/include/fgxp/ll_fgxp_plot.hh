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
#ifndef _LL_FGXP_PLOT_HH
#define _LL_FGXP_PLOT_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include "fgxp/fgxp_constants.hh"

#include <iostream.h>
#include <locale.h>
#include <assert.h>

class FgXpPlotElement : public Element
{
	friend class FgXpPlotLinkedList;

	private:

		class FgXpPlot *_fgXpPlot;

		FgXpPlotElement()
			{ assert(0); }
		FgXpPlotElement(class FgXpPlot *fgXpPlot)
			: _fgXpPlot(fgXpPlot)
			{ /* do nothing */ }
		~FgXpPlotElement()
			{ /* do nothing */ }
		int operator ==(void * const fgXpPlot) const
		{
			return((class FgXpPlot *) fgXpPlot
				== _fgXpPlot);
		}
		void print() const
			{ cout << " " << _fgXpPlot; }
};

class FgXpPlotLinkedList : public BaseLinkedList
{
	public:

		FgXpPlotLinkedList(
			const char *normalLineColor    = "red"    ,
			const char *sourceLineColor    = "cyan"   ,
			const char *receiverLineColor  = "magenta",
			const char *srcAndRcvLineColor = "purple" ,
			const char *selectedLineColor  = "green"  ,
			const char *activeLineColor    = "blue"   ,
			const char *normalFlagColor    = "red"    ,
			const char *dependentFlagColor = "yellow" ,
			const char *selectedFlagColor  = "green"  ,
			const char *activeFlagColor    = "blue"   ,
			int activeLineDependent    = 1,
			int selectedLineDependent  = 1,
			int srcAndRcvLineDependent = 1,
			int receiverLineDependent  = 1,
			int sourceLineDependent    = 1,
			int activeFlagDependent    = 1,
			int selectedFlagDependent  = 1,
			int dependentFlagDependent = 1,
			FlagMode flagMode = ShowAll,
			int isFg2DPlot = 0);
		~FgXpPlotLinkedList();
		void add(class FgXpPlot *fgXpPlot);
		void remove(class FgXpPlot *fgXpPlot);
		class FgXpPlot *find(class FgXpPlot *fgXpPlot,
			void **p = NULL);
		class FgXpPlot *top    (void **p = NULL);
		class FgXpPlot *bottom (void **p = NULL);
		class FgXpPlot *next   (void **p = NULL);
		class FgXpPlot *prev   (void **p = NULL);
		class FgXpPlot *current(void **p = NULL);

		/*
		 * Scale returns 1 if there is data to scale, 0 if no data.
		 * rounder must be >= 0 and < 1.
		 * Increasing rounder yields a wider scale.
		 */
		int scale(float *xMin, float *xMax, float *yMin, float *yMax,
			float *zMin = (float *) NULL,
			float *zMax = (float *) NULL,
			float rounder = 0.5);
		class Vector *getClosest(int x, int y, class PlotBase *plot,
			class FgXpPlot **closestPlot =
			(class FgXpPlot **) NULL);
		void areaSelect(int x1, int x2, int y1, int y2, char c,
			class PlotBase *plot,
			int *interpolate = (int *) NULL,
			int threshold = 10000);
		int selectRcvsFromSrc(int x, int y, class PlotBase *plot,
			char c, int threshold = 10000);
		int selectSrcsFromRcv(int x, int y, class PlotBase *plot,
			char c, int threshold = 10000);
		int getFlag(int x, int y, class PlotBase *plot,
			long *ixl, long *ixf,
			long **srcIndices = (long **) NULL,
			int *numSrcs = (int *) NULL);
		void clearSelections(int threshold = 10);
		void getLineColors(const char **normalLineColor,
			const char **sourceLineColor   ,
			const char **receiverLineColor ,
			const char **srcAndRcvLineColor,
			const char **selectedLineColor ,
			const char **activeLineColor   );
		void setLineColors(const char *normalLineColor,
			const char *sourceLineColor   ,
			const char *receiverLineColor ,
			const char *srcAndRcvLineColor,
			const char *selectedLineColor ,
			const char *activeLineColor   );
		void getFlagColors(const char **normalFlagColor,
			const char **dependentFlagColor,
			const char **selectedFlagColor ,
			const char **activeFlagColor   );
		void setFlagColors(const char *normalFlagColor,
			const char *dependentFlagColor,
			const char *selectedFlagColor ,
			const char *activeFlagColor   );
		void getLinePrecedence(int *activeLineDependent,
			int *selectedLineDependent ,
			int *srcAndRcvLineDependent,
			int *receiverLineDependent ,
			int *sourceLineDependent   );
		void setLinePrecedence(int activeLineDependent,
			int selectedLineDependent ,
			int srcAndRcvLineDependent,
			int receiverLineDependent ,
			int sourceLineDependent   );
		void getFlagPrecedence(int *activeFlagDependent,
			int *selectedFlagDependent ,
			int *dependentFlagDependent);
		void setFlagPrecedence(int activeFlagDependent,
			int selectedFlagDependent ,
			int dependentFlagDependent);
		const char *getNormalLineColor()
			{ return _normalLineColor; }
		const char *getSourceLineColor()
			{ return _sourceLineColor; }
		const char *getReceiverLineColor()
			{ return _receiverLineColor; }
		const char *getSrcAndRcvLineColor()
			{ return _srcAndRcvLineColor; }
		const char *getSelectedLineColor()
			{ return _selectedLineColor; }
		const char *getActiveLineColor()
			{ return _activeLineColor; }
		const char *getNormalFlagColor()
			{ return _normalFlagColor; }
		const char *getDependentFlagColor()
			{ return _dependentFlagColor; }
		const char *getSelectedFlagColor()
			{ return _selectedFlagColor; }
		const char *getActiveFlagColor()
			{ return _activeFlagColor; }
		int getActiveLineDependent()
			{ return _activeLineDependent; }
		int getSelectedLineDependent()
			{ return _selectedLineDependent; }
		int getSrcAndRcvLineDependent()
			{ return _srcAndRcvLineDependent; }
		int getReceiverLineDependent()
			{ return _receiverLineDependent; }
		int getSourceLineDependent()
			{ return _sourceLineDependent; }
		int getActiveFlagDependent()
			{ return _activeFlagDependent; }
		int getSelectedFlagDependent()
			{ return _selectedFlagDependent; }
		int getDependentFlagDependent()
			{ return _dependentFlagDependent; }
		FlagMode getFlagMode()
			{ return _flagMode; }
		void setFlagMode(FlagMode flagMode);
		void setFg2DPlot(class Fg2DPlot *fg2DPlot)
		  { assert(_isFg2DPlot);  _fg2DPlot = fg2DPlot; }
		void clearFg2DPlot()
		  { assert(_isFg2DPlot);  _fg2DPlot = (class Fg2DPlot*) NULL; }

	private:

		char *_normalLineColor, *_sourceLineColor, *_receiverLineColor,
			*_srcAndRcvLineColor, *_selectedLineColor,
			*_activeLineColor;
		char *_normalFlagColor, *_dependentFlagColor,
			*_selectedFlagColor, *_activeFlagColor;
		int _activeLineDependent, _selectedLineDependent,
			_srcAndRcvLineDependent, _receiverLineDependent,
			_sourceLineDependent;
		int _activeFlagDependent, _selectedFlagDependent,
			_dependentFlagDependent;
		FlagMode _flagMode;
		int           _isFg2DPlot;
		class Fg2DPlot *_fg2DPlot;

		static void niceScale(float *min, float *max, float rounder);
};

#endif
