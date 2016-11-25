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
#ifndef _FGXP_PLOT_HH
#define _FGXP_PLOT_HH

#include "geom/fg_inform.hh"
#include "fgxp/fgxp_constants.hh"
#include "geom/fg_constants.hh"
#include "vect/vector.hh"

class FgXpPlot : public FgInform
{
	public:

		FgXpPlot(class FieldGeometry *fg,
			class FgXpPlotLinkedList *fgXpPlotList,
			SelectMode selectMode = AllLines,
			DisplayMode displayMode = LinesAndFlags,
			CardType xCardType =LdCardType, int xDataType =FG_XLOC,
			CardType yCardType =LdCardType, int yDataType =FG_YLOC,
			CardType zCardType =LdCardType, int zDataType =FG_ELEV,
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
		virtual ~FgXpPlot();
		void addPlot(class SeisPlot *sp, Bool newPlot = False);
		void removePlot(class SeisPlot *sp);
		DisplayMode getDisplayMode();
		void setDisplayMode(DisplayMode displayMode);
		void getLineColors(const char **normalLineColor,
			const char **sourceLineColor,
			const char **receiverLineColor,
			const char **srcAndRcvLineColor,
			const char **selectedLineColor,
			const char **activeLineColor);
		void setLineColors(const char *normalLineColor,
			const char *sourceLineColor,
			const char *receiverLineColor,
			const char *srcAndRcvLineColor,
			const char *selectedLineColor,
			const char *activeLineColor);
		void getFlagColors(const char **normalFlagColor,
			const char **dependentFlagColor,
			const char **selectedFlagColor,
			const char **activeFlagColor);
		void setFlagColors(const char *normalFlagColor,
			const char *dependentFlagColor,
			const char *selectedFlagColor,
			const char *activeFlagColor);
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
		FlagMode getFlagMode();
		void setFlagMode(FlagMode flagMode);
		void getRange(float *xMin, float *xMax,
			float *yMin, float *yMax,
			float *zMin = (float *) NULL,
			float *zMax = (float *) NULL);
		int getFirstPnt(float *x, float *y, float *z = (float *) NULL);
		Vector *getClosest(int x, int y, class PlotBase *plot,
			float *distPtr);
		void getFlagsInArea(int x1, int y1, int x2, int y2,
			class PlotBase *plot,
			int *numSelectedLines, int *numSelectedFlags,
			int *flagsInLongestLine, VectorAreaPick ***vap);
		class FgXpVectLinkedList *getVectLL()
			{ return _delayedDeletion ?
				(class FgXpVectLinkedList *) NULL : _vectors; }
		FieldGeometry *getFg()
			{ return _delayedDeletion ?
				(FieldGeometry *) NULL : _fg; }
		virtual char *getPlotLabel() = 0;

	protected:

		class FgXpDataLinkedList *_data        ;
		class FgXpVectLinkedList *_vectors     ;
		class FgXpPlotLinkedList *_fgXpPlotList;
		SelectMode _selectMode;
		static Bool _outOfDate;
		Bool _delayedDeletion;

		virtual void delayedDelete() = 0;
		static void clientMessageFunc(void *obj);

		/*
		 * FgInform virtual functions.
		 */
		virtual void startingChanges(FieldGeometry *fg);
		virtual void finishedChanges(FieldGeometry *fg);
		virtual void freezingDependentUpdates  (FieldGeometry *fg);
		virtual void dependentValuesOutOfDate  (FieldGeometry *fg);
		virtual void  preResumeDependentUpdates(FieldGeometry *fg);
		virtual void postResumeDependentUpdates(FieldGeometry *fg) = 0;
		virtual void  preFlagValuesChanged(FieldGeometry *fg, long ixl,
			int ident, long index, long nrem, long nins);
		virtual void postFlagValuesChanged(FieldGeometry *fg, long ixl,
			int ident, long index, long nrem, long nins);
		virtual void  preRemoveInsertLines(FieldGeometry *fg,
			long index, long nrem, long nins);
		virtual void postRemoveInsertLines(FieldGeometry *fg,
			long index, long nrem, long nins);
		virtual void  preNewActiveLine(FieldGeometry *fg);
		virtual void postNewActiveLine(FieldGeometry *fg);
		virtual void preNewActiveFlag (FieldGeometry *fg, long ixl);
		virtual void postNewActiveFlag(FieldGeometry *fg, long ixl);
		virtual void  preNewGridTransform(FieldGeometry *fg);
		virtual void postNewGridTransform(FieldGeometry *fg);
		virtual void  preSortByLineNumber(FieldGeometry *fg);
		virtual void postSortByLineNumber(FieldGeometry *fg);
		virtual void  sourceGathersOutOfDate(FieldGeometry *fg);
		virtual void  preUpdateSourceGathers(FieldGeometry *fg);
		virtual void postUpdateSourceGathers(FieldGeometry *fg);
		virtual void  receiverGathersOutOfDate(FieldGeometry *fg);
		virtual void  preUpdateReceiverGathers(FieldGeometry *fg);
		virtual void postUpdateReceiverGathers(FieldGeometry *fg);
		virtual void postPpValuesChanged(FieldGeometry *fg,
			int ident, long index, long nrem, long nins);
		virtual void  preNewChaining(FieldGeometry *fg);
		virtual void postNewChaining(FieldGeometry *fg);
		virtual void  preLineSelectionsChanged(FieldGeometry *fg,
			long index, long nrem, long nins);
		virtual void postLineSelectionsChanged(FieldGeometry *fg,
			long index, long nrem, long nins);

/* not in FgInform
 *
 * 		virtual void  preFlagSourceReceiverChanged(FieldGeometry *fg,
 * 			long ixl, long index, long num);
 * 		virtual void postFlagSourceReceiverChanged(FieldGeometry *fg,
 * 			long ixl, long index, long num);
 */

		static const char *dataTypeName(int dataType);

	private:

		enum {
			   _NORMAL_COLOR_INDEX = 1,
			_DEPENDENT_COLOR_INDEX = 2,
			 _SELECTED_COLOR_INDEX = 3,
			   _ACTIVE_COLOR_INDEX = 4
		};

		Bool _changing, _lineSelectionsChanging;
		static int _changesCount;
		long _oldActiveFlag, _oldActiveFlagLine;
		int _numOldActiveLines;
		Vector **_oldActiveLines;

		void preValuesChanged (long ixl, int ident, long index,
			long nrem, long nins);
		void preSelectChanged (long ixl, int ident, long index,
			long nrem, long nins);
		void postValuesChanged(long ixl, int ident, long index,
			long nrem, long nins);
		void postSelectChanged(long ixl, int ident, long index,
			long nrem, long nins);
		void updateSrcRcvs();

		virtual BaseData *addData(long index    ) = 0;
		virtual void   removeData(BaseData *data) = 0;
		void finishedChangesSelectedLines();
		static int compar(const void *element1, const void *element2);
		void removeSelectedLineByIndex(long index);

		FgXpPlot() : FgInform((class FieldGeometry *) NULL)
			{ /* private, no access to default constructor */ }
		FgXpPlot(FgXpPlot &)
			: FgInform((class FieldGeometry *) NULL)
			{ /* private, no access to copy constructor */ }
		FgXpPlot& operator=(FgXpPlot &p)
			{ /* private, no access to = */ return p; }
};

void freeVectorAreaPickArray(int numSelectedLines, VectorAreaPick **vap);

#endif /* _FGXP_PLOT_HH */
