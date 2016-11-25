#include <string.h>
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
#include "fgxp/fgxp_plot.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/ll_fgxp_vect.hh"
#include "fgxp/ll_fgxp_data.hh"
#include "geom/field_geometry.hh"
#include "oprim/static_utils.hh"

#include <stdlib.h>
#include <assert.h>

int  FgXpPlot::_changesCount = 0    ;
Bool FgXpPlot::_outOfDate    = False;

FgXpPlot::FgXpPlot(FieldGeometry *fg, FgXpPlotLinkedList *fgXpPlotList,
	SelectMode selectMode, DisplayMode displayMode,
	CardType xCardType, int xDataType,
	CardType yCardType, int yDataType,
	CardType zCardType, int zDataType,
	unsigned int width, unsigned int markerSize,
	unsigned int markerLineWidth,
	Vector::VectorMarker srcMarker , Vector::VectorMarker rcvMarker    ,
	Vector::VectorMarker bothMarker, Vector::VectorMarker neitherMarker,
	Vector::VectorMarker noMarker  ,
	const char *label, const char *font)
	: FgInform(fg), _fgXpPlotList(fgXpPlotList),
	_selectMode(selectMode), _changing(False),
	_lineSelectionsChanging(False), _delayedDeletion(False),
	_numOldActiveLines(0)
{
	_data    = new FgXpDataLinkedList(_fg,
		(int) srcMarker    , (int) rcvMarker, (int) bothMarker,
		(int) neitherMarker, (int) noMarker ,
		xCardType, xDataType, yCardType, yDataType,
		zCardType, zDataType, 
		_NORMAL_COLOR_INDEX  , _DEPENDENT_COLOR_INDEX,
		_SELECTED_COLOR_INDEX, _ACTIVE_COLOR_INDEX,
		_fgXpPlotList->getActiveFlagDependent   (),
		_fgXpPlotList->getSelectedFlagDependent (),
		_fgXpPlotList->getDependentFlagDependent(),
		_fgXpPlotList->getFlagMode              ());

	_vectors = new FgXpVectLinkedList(displayMode,
		_fgXpPlotList->getNormalLineColor   (),
		_fgXpPlotList->getSourceLineColor   (),
		_fgXpPlotList->getReceiverLineColor (),
		_fgXpPlotList->getSrcAndRcvLineColor(),
		_fgXpPlotList->getSelectedLineColor (),
		_fgXpPlotList->getActiveLineColor   (),
		_fgXpPlotList->getNormalFlagColor   (),
		_fgXpPlotList->getDependentFlagColor(),
		_fgXpPlotList->getSelectedFlagColor (),
		_fgXpPlotList->getActiveFlagColor   (),
		_NORMAL_COLOR_INDEX  , _DEPENDENT_COLOR_INDEX,
		_SELECTED_COLOR_INDEX, _ACTIVE_COLOR_INDEX   ,
		_fgXpPlotList->getActiveLineDependent   (),
		_fgXpPlotList->getSelectedLineDependent (),
		_fgXpPlotList->getSrcAndRcvLineDependent(),
		_fgXpPlotList->getReceiverLineDependent (),
		_fgXpPlotList->getSourceLineDependent   (),
		width, markerSize, markerLineWidth, label, font);

	_fgXpPlotList->add(this);

	if (_fg->dependentUpdatesFrozen() && _fg->dependentValuesOutOfDate()
		&& !_outOfDate)
	{
		_outOfDate = True;
	}
}

FgXpPlot::~FgXpPlot()
{
	if (_delayedDeletion)
	{
		assert(!_changing);

		if (_numOldActiveLines > 0)
			free((void *) _oldActiveLines);
	}
	else
	{
		_fgXpPlotList->remove(this);

		if (_changing)
			if (0 == --_changesCount)
				SU::flushVectors();
	}
}

void FgXpPlot::addPlot(class SeisPlot *sp, Bool newPlot)
{
	assert(!_delayedDeletion);

	_vectors->addPlot(sp, newPlot);
}

void FgXpPlot::removePlot(class SeisPlot *sp)
{
	assert(!_delayedDeletion);

	_vectors->removePlot(sp);
}

DisplayMode FgXpPlot::getDisplayMode()
{
	assert(!_delayedDeletion);

	return _vectors->getDisplayMode();
}

void FgXpPlot::setDisplayMode(DisplayMode displayMode)
{
	assert(!_delayedDeletion);

	_vectors->setDisplayMode(displayMode);
}

void FgXpPlot::getLineColors(const char **normalLineColor,
	const char **sourceLineColor   , const char **receiverLineColor,
	const char **srcAndRcvLineColor, const char **selectedLineColor,
	const char **activeLineColor   )
{
	assert(!_delayedDeletion);

	_vectors->getLineColors(normalLineColor, sourceLineColor,
		receiverLineColor, srcAndRcvLineColor, selectedLineColor,
		activeLineColor);
}

void FgXpPlot::setLineColors(const char *normalLineColor,
	const char *sourceLineColor   , const char *receiverLineColor,
	const char *srcAndRcvLineColor, const char *selectedLineColor,
	const char *activeLineColor   )
{
	assert(!_delayedDeletion);

	_vectors->setLineColors(normalLineColor, sourceLineColor,
		receiverLineColor, srcAndRcvLineColor, selectedLineColor,
		activeLineColor);
}

void FgXpPlot::getFlagColors(const char **normalFlagColor,
	const char **dependentFlagColor, const char **selectedFlagColor,
	const char **activeFlagColor)
{
	assert(!_delayedDeletion);

	_vectors->getFlagColors(normalFlagColor, dependentFlagColor,
		selectedFlagColor, activeFlagColor);
}

void FgXpPlot::setFlagColors(const char *normalFlagColor,
	const char *dependentFlagColor, const char *selectedFlagColor,
	const char *activeFlagColor)
{
	assert(!_delayedDeletion);

	_vectors->setFlagColors(normalFlagColor, dependentFlagColor,
		selectedFlagColor, activeFlagColor);
}

void FgXpPlot::getLinePrecedence(int *activeLineDependent,
	int *selectedLineDependent, int *srcAndRcvLineDependent,
	int *receiverLineDependent, int *sourceLineDependent   )
{
	assert(!_delayedDeletion);

	_vectors->getPrecedence(activeLineDependent,
		selectedLineDependent ,
		srcAndRcvLineDependent,
		receiverLineDependent ,
		sourceLineDependent   );
}

void FgXpPlot::setLinePrecedence(int activeLineDependent,
	int selectedLineDependent, int srcAndRcvLineDependent,
	int receiverLineDependent, int sourceLineDependent   )
{
	assert(!_delayedDeletion);

	_vectors->setPrecedence(activeLineDependent,
		selectedLineDependent ,
		srcAndRcvLineDependent,
		receiverLineDependent ,
		sourceLineDependent   );
}

void FgXpPlot::getFlagPrecedence(int *activeFlagDependent,
	int *selectedFlagDependent, int *dependentFlagDependent)
{
	assert(!_delayedDeletion);

	_data->getPrecedence(activeFlagDependent, selectedFlagDependent,
		dependentFlagDependent);
}

void FgXpPlot::setFlagPrecedence(int activeFlagDependent,
	int selectedFlagDependent, int dependentFlagDependent)
{
	assert(!_delayedDeletion);

	_data->setPrecedence(activeFlagDependent, selectedFlagDependent,
		dependentFlagDependent);
}

FlagMode FgXpPlot::getFlagMode()
{
	return _data->getFlagMode();
}

void FgXpPlot::setFlagMode(FlagMode flagMode)
{
   FlagMode oldFlagMode = _data->getFlagMode();

   _data->setFlagMode(flagMode);

   switch (getDisplayMode())
   {
      case Lines:
         /* do nothing */
         break;
      case Flags:
      case LinesAndFlags:
         if ((flagMode == ShowAll)
          || (oldFlagMode == ShowSrcsOnly && flagMode == HideUnassigned)
          || (oldFlagMode == ShowRcvsOnly && flagMode == HideUnassigned))
         {
            /*
             * Under these conditions, no markers can possibly need
             * to be erased, so just redraw to add the new ones.
             */
            Vector *v;
            void   *p;
            for (v = _vectors->top(&p); v; v = _vectors->next(&p))
               v->redraw();
         }
         else
         {
            _vectors->redisplay();
         }
         break;
      case LinesAndAutoFlags:
         /*
          * In LinesAndAutoFlags display mode, erasing can
          * happen with any change since roomForFlags can
          * change.
          */
         _vectors->redisplay();
         break;
      default:
         assert(False);
   }
}

void FgXpPlot::getRange(float *xMin, float *xMax, float *yMin, float *yMax,
	float *zMin, float *zMax)
{
	assert(!_delayedDeletion);

	FgXpData *ptr;
	void *p;
	for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
		ptr->getRange(xMin, xMax, yMin, yMax, zMin, zMax);
}

int FgXpPlot::getFirstPnt(float *x, float *y, float *z)
{
	assert(!_delayedDeletion);

	int retval;

	FgXpData *ptr;
	void *p;
	for (ptr = _data->top(&p), retval = 0;
		ptr && !retval;
		ptr = _data->next(&p))
	{
		retval = ptr->getFirstPnt(x, y, z);
	}

	return retval;
}

Vector *FgXpPlot::getClosest(int x, int y, class PlotBase *plot,
	float *distPtr)
{
	assert(!_delayedDeletion);

	return _vectors->closest(x, y, plot, distPtr);
}

void FgXpPlot::getFlagsInArea(int x1, int y1, int x2, int y2,
	class PlotBase *plot, int *numSelectedLines, int *numSelectedFlags,
	int *flagsInLongestLine, VectorAreaPick ***vap)
{
	assert(!_delayedDeletion);

	*vap = new VectorAreaPick *[_vectors->count()];

	Vector *ptr;
	void *p;
	VectorAreaPick *vapPtr;
	int flagsInLine, i;
	for (*numSelectedLines = *numSelectedFlags = *flagsInLongestLine = 0,
		ptr = _vectors->top (&p);
		ptr;
		ptr = _vectors->next(&p))
	{
		vapPtr = ptr->getIndicesInArea(x1, y1, x2, y2, plot);

		if (vapPtr)
		{
			FgXpData *data =
				(FgXpData *) ptr->getData()->actualData();

			vapPtr->vectorId[0] = data->getXIndex();
			vapPtr->vectorId[1] = data->getYIndex();
			vapPtr->vectorId[2] = data->getZIndex();
			vapPtr->vectorId[3] = (long) _data->getXDataType();
			vapPtr->vectorId[4] = (long) _data->getYDataType();
			vapPtr->vectorId[5] = (long) _data->getZDataType();

			(*vap)[(*numSelectedLines)++] = vapPtr;

			*numSelectedFlags += vapPtr->numIndices;

			flagsInLine = data->getNumPts();
			if (flagsInLine > *flagsInLongestLine)
				*flagsInLongestLine = flagsInLine;

			if (data->hasSkids())
				for (i = 0; i < vapPtr->numIndices; i++)
					vapPtr->index[i] =
						data->translateDataIndexToFlag(
							vapPtr->index[i]);
		}
	}

	if (_vectors->count() != *numSelectedLines)
	{
		if (*numSelectedLines)
		{
			VectorAreaPick **temp = *vap;
			*vap = new VectorAreaPick *[*numSelectedLines];
			memcpy(*vap, temp, (size_t) *numSelectedLines
				* sizeof(VectorAreaPick *));
			delete temp;
		}
		else
		{
			delete *vap;
		}
	}
}

void freeVectorAreaPickArray(int num, VectorAreaPick **vap)
{
	for (int i = 0; i < num; i++)
		freeVectorAreaPick(vap[i]);

	delete vap;
}

void FgXpPlot::clientMessageFunc(void *obj)
{
	delete (FgXpPlot *) obj;
}

void FgXpPlot::startingChanges(FieldGeometry *fg)
{
	assert(fg == _fg && !_changing);

	_changing = True;

	if (0 == _changesCount++)
		SU::holdVectors();
}

void FgXpPlot::finishedChanges(FieldGeometry *fg)
{
	assert(fg == _fg && _changing);

	_changing = False;

	if (_delayedDeletion)
	{
		/* do nothing */
	}
	else if (_lineSelectionsChanging && (SelectedLines == _selectMode))
	{
		finishedChangesSelectedLines();
	}
	else
	{
		Vector *v;
		FgXpData *d;
		void *p;
		for (v = _vectors->top(&p); v; v = _vectors->next(&p))
		{
			d = (FgXpData *) v->getData()->actualData();

			if (_lineSelectionsChanging)
			{
				/*
				 * Set all colors since Vector is smart enough
				 * to only redraw when color has changed.
				 * Call depSelActFlush if setColor did not
				 * cause redraw.
				 */
				if (_vectors->setColor(v, True))
					d->forgetDepSelActChanged();
				else
					d->depSelActFlush        ();
			}
			else
			{
				d->depSelActFlush();
			}
		}
	}

	if (_lineSelectionsChanging)
		_lineSelectionsChanging = False;

	if (0 == --_changesCount)
		SU::flushVectors();
}

void FgXpPlot::freezingDependentUpdates(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	if (_outOfDate)
		_outOfDate = False;

	FgXpData *ptr;
	void *p;

	switch (_selectMode)
	{
		case OneLine:
		case RangeOfLines:
			for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
				ptr->saveLineNumber();
			break;
		case AllLines:
		case ActiveLine:
		case SelectedLines:
			/* do nothing */
			break;
		default:
			assert(False);
	}
}

void FgXpPlot::dependentValuesOutOfDate(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	if (!_outOfDate)
		_outOfDate = True;
}

void FgXpPlot::preResumeDependentUpdates(FieldGeometry *fg)
{
	assert(fg == _fg);

	/* do nothing */
}

void FgXpPlot::preFlagValuesChanged(FieldGeometry *fg, long ixl,
	int ident, long index, long nrem, long nins)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	switch (ident)
	{
		case FG_NONE:
			assert(0);
		case FG_SHOT:
		case FG_DIST:
		case FG_XLOC:
		case FG_YLOC:
		case FG_ELEV:
		case FG_XGRID:
		case FG_YGRID:
		case FG_HD:
		case FG_TUH:
		case FG_RSTAT:
		case FG_SSTAT:
		case FG_XSKID:
		case FG_YSKID:
		case FG_ESKID:
		case FG_CUM:
		case FG_AZIM:
			preValuesChanged(ixl, ident, index, nrem, nins);
			break;
		case FG_SEL:
			preSelectChanged(ixl, ident, index, nrem, nins);
			break;
		case FG_COORDS:
			/* FG_XLOC and/or FG_YLOC will handle this */
			break;
		default:
			assert(0);
	}
}

void FgXpPlot::preValuesChanged(long ixl, int ident, long index,
	long nrem, long nins)
{
	if (_data->needUpdate(ident))
	{
		FgXpData *ptr;
		void *p;
		for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
			if (ptr->getXIndex() == ixl
			 || ptr->getYIndex() == ixl
			 || ptr->getZIndex() == ixl)
			{
				/*
				 * If there are skids and a there is
				 * a net loss or gain of points,
				 * the skid arrays will not be correct
				 * in postValuesChanged.  We can just
				 * ignore it because the gathers will
				 * go out of date and redraw everything.
				 */
				if (!ptr->hasSkids() || (nrem == nins))
					ptr->pointsRemoved(
						(int) index, (int) nrem);
			}
	}
}

void FgXpPlot::preSelectChanged(long /*ixl*/, int ident, long /*index*/,
	long /*nrem*/, long /*nins*/)
{
	assert(FG_SEL == ident);

	/* do nothing */
}

void FgXpPlot::postFlagValuesChanged(FieldGeometry *fg, long ixl,
	int ident, long index, long nrem, long nins)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	switch (ident)
	{
		case FG_NONE:
			assert(0);
		case FG_XSKID:
		case FG_YSKID:
			if (!_fg->receiverGathersOutOfDate() && (nrem == nins))
				updateSrcRcvs();
			/* no break */
		case FG_SHOT:
		case FG_DIST:
		case FG_XLOC:
		case FG_YLOC:
		case FG_ELEV:
		case FG_XGRID:
		case FG_YGRID:
		case FG_HD:
		case FG_TUH:
		case FG_RSTAT:
		case FG_SSTAT:
		case FG_ESKID:
		case FG_CUM:
		case FG_AZIM:
			postValuesChanged(ixl, ident, index, nrem, nins);
			break;
		case FG_SEL:
			postSelectChanged(ixl, ident, index, nrem, nins);
			break;
		case FG_COORDS:
			/* FG_XLOC and/or FG_YLOC will handle this */
			break;
		default:
			assert(0);
	}
}

void FgXpPlot::postValuesChanged(long ixl, int ident, long index,
	long nrem, long nins)
{
	if (_data->needUpdate(ident))
	{
		FgXpData *ptr;
		void *p;
		for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
			if (ptr->getXIndex() == ixl
			 || ptr->getYIndex() == ixl
			 || ptr->getZIndex() == ixl)
			{
				/*
				 * If there are skids and a there is
				 * a net loss or gain of points,
				 * the skid arrays will not be correct
				 * in postValuesChanged.  We can just
				 * ignore it because the gathers will
				 * go out of date and redraw everything.
				 */
				if (!ptr->hasSkids() || (nrem == nins))
					ptr->pointsInserted(
						(int) index, (int) nins);
			}
	}
}

void FgXpPlot::postSelectChanged(long ixl, int ident, long index,
	long /*nrem*/, long nins)
{
	assert(FG_SEL == ident);

	FgXpData *ptr;
	void *p;
	for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
		if (ptr->getXIndex() == ixl
		 || ptr->getYIndex() == ixl
		 || ptr->getZIndex() == ixl)
		{
			ptr->depSelActChanged((int) index, (int) nins);
		}
}

void FgXpPlot::preRemoveInsertLines(FieldGeometry *fg,
	long index, long nrem, long /*nins*/)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	BaseData *data1;
	FgXpData *data2;
	long xIndex, yIndex, zIndex;
	Vector *ptr;
	Vector *nextPtr = (Vector *) NULL; /* Silence bogus compiler warning. */
	void *p;

	switch (_selectMode)
	{
		case OneLine:
		case RangeOfLines:
		case AllLines:
		case SelectedLines:
			for (ptr = _vectors->top(&p); ptr; ptr = nextPtr)
			{
				/* In case ptr removed.  */
				nextPtr = _vectors->next(&p);

				data1 = ptr->getData();
				data2 = (FgXpData*) data1->actualData();

				xIndex = data2->getXIndex();
				if (xIndex >= index && xIndex < index + nrem)
				{
					_vectors->remove(ptr);
					removeData(data1);
					/* Do not check yIndex or zIndex. */
					continue;
				}

				yIndex = data2->getYIndex();
				if (yIndex >= index && yIndex < index + nrem)
				{
					_vectors->remove(ptr);
					removeData(data1);
					continue;  /* Do not check zIndex. */
				}

				zIndex = data2->getZIndex();
				if (zIndex >= index && zIndex < index + nrem)
				{
					_vectors->remove(ptr);
					removeData(data1);
				}
			}
			break;
		case ActiveLine:
			/* NewActiveLine will handle. */
			break;
		default:
			assert(False);
	}
}

void FgXpPlot::postRemoveInsertLines(FieldGeometry *fg,
	long index, long nrem, long nins)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	FgXpData *data;
	long xIndex, yIndex, zIndex;
	Vector *ptr;
	void *p;
	long i;

	switch (_selectMode)
	{
		case OneLine:
		case RangeOfLines:
		case AllLines:
		case SelectedLines:
			for (ptr = _vectors->top(&p);
				ptr;
				ptr = _vectors->next(&p))
			{
				data = (FgXpData*) ptr->getData()->actualData();
		
				xIndex = data->getXIndex();
				if (xIndex >= index)
				{
					assert(xIndex >= index + nrem);
					data->setXIndex(xIndex - nrem + nins);
				}
		
				yIndex = data->getYIndex();
				if (yIndex >= index)
				{
					assert(yIndex >= index + nrem);
					data->setYIndex(yIndex - nrem + nins);
				}
		
				zIndex = data->getZIndex();
				if (zIndex >= index)
				{
					assert(zIndex >= index + nrem);
					data->setZIndex(zIndex - nrem + nins);
				}
			}

			if (AllLines == _selectMode)
			{
				for (i = index; i < index + nins; i++)
					_vectors->add(addData(i));
			}
			else if (SelectedLines == _selectMode)
			{
				/* do nothing */
			}
			else if (_vectors->count() == 0)
			{
				assert(_data->count() == 0);
				delayedDelete();
			}
			break;
		case ActiveLine:
			/* NewActiveLine will handle. */
			break;
		default:
			assert(False);
	}
}

/*
 * Active line and flag stuff updates for OneLine, RangeOfLines, and
 * AllLines after post call without waiting for finishedChanges.
 * This is probably poor design, but since the performance is OK I will
 * leave it.  Everything else waits for finishedChanges.
 */
void FgXpPlot::preNewActiveLine(FieldGeometry *fg)
{
   assert(fg == _fg);

   if (_delayedDeletion)
      return;

   Vector *ptr;
   void *p;
   FgXpData *fgXpData;

   switch (_selectMode)
   {
      case OneLine:
      case RangeOfLines:
      case AllLines:
      case SelectedLines:
         for (ptr = _vectors->top(&p); ptr; ptr = _vectors->next(&p))
            if (((FgXpData *) ptr->getData()->actualData())->numActiveAtLine())
            {
               /*
                * I use malloc 1st time because we have had trouble
                * with 1st time realloc on SUN.
                */
               if (_numOldActiveLines++)
                  assert(_oldActiveLines = (Vector **) realloc(
                     (void *) _oldActiveLines,
                     (size_t) _numOldActiveLines * sizeof(Vector *)));
               else
                  assert(_oldActiveLines = (Vector **) malloc(
                     sizeof(Vector *)));

               _oldActiveLines[_numOldActiveLines - 1] = ptr;
            }
         break;
      case ActiveLine:
         /*
          * This will needlessly draw active line if a line with a
          * lower line index is deleted since that will change the
          * active line index.
          */
         assert(_data->count() == 1);
         fgXpData = _data->top();
         fgXpData->pointsRemoved(0,
		(int) _fg->numFlagsOnLine(_fg->getActiveLineIndex()));
         break;
      default:
         assert(False);
   }
}

void FgXpPlot::postNewActiveLine(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	Vector *ptr;
	void *p;
	FgXpData *fgXpData;
	int i;

	long activeLine;

	switch (_selectMode)
	{
		case OneLine:
		case RangeOfLines:
		case AllLines:
		case SelectedLines:
			for (i = 0; i < _numOldActiveLines; i++)
				/* Make sure line has not been deleted.  */
				if (_vectors->find(_oldActiveLines[i]))
					_vectors->setColor(_oldActiveLines[i],
						True);

			if (_numOldActiveLines > 0)
				free((void *) _oldActiveLines);

			for (ptr = _vectors->top(&p);
				ptr;
				ptr = _vectors->next(&p))
			{
				if (((FgXpData *) ptr->getData()->actualData())
					->numActiveAtLine())
				{
					_vectors->setColor(ptr, True);
				}
			}
			_numOldActiveLines = 0;
			break;
		case ActiveLine:
			activeLine = _fg->getActiveLineIndex();

			if (-1 == activeLine)
			{
				delayedDelete();
			}
			else
			{
				fgXpData = _data->top();
				fgXpData->setXIndex(activeLine);
				fgXpData->setYIndex(activeLine);
				fgXpData->setZIndex(activeLine);
				fgXpData->setSkids();
				fgXpData->pointsInserted(0,
					(int) _fg->numFlagsOnLine(activeLine));

				ptr = _vectors->top();
				_vectors->setColor(ptr, False);
			}

			break;
		default:
			assert(False);
	}
}

void FgXpPlot::preNewActiveFlag(FieldGeometry *fg, long ixl)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	_oldActiveFlag     = fg->getActiveFlagIndexOnLine(ixl);
	_oldActiveFlagLine = ixl;
}

void FgXpPlot::postNewActiveFlag(FieldGeometry *fg, long ixl)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	assert(ixl == _oldActiveFlagLine);

	long newActiveFlag = fg->getActiveFlagIndexOnLine(ixl);

	FgXpData *ptr;
	void *p;
	for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
		if (ptr->getXIndex() == ixl
		 || ptr->getYIndex() == ixl
		 || ptr->getZIndex() == ixl)
		{
			if (_oldActiveFlag >= 0
				&&_fg->numFlagsOnLine(_oldActiveFlagLine)
					> _oldActiveFlag)
			{
				ptr->depSelActChanged((int) _oldActiveFlag, 1);
			}

			if (newActiveFlag != -1)
				ptr->depSelActChanged((int)  newActiveFlag, 1);
		}
}

void FgXpPlot::preNewGridTransform(FieldGeometry *fg)
{
	assert(fg == _fg);

	/* nothing else */
}

void FgXpPlot::postNewGridTransform(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	if (_data->affectedByGridTransform())
		_vectors->redisplay();
}

void FgXpPlot::preSortByLineNumber(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	FgXpData *ptr;
	void *p;

	switch (_selectMode)
	{
		case OneLine:
		case RangeOfLines:
		case AllLines:
		case SelectedLines:
			for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
				ptr->saveLineNumber();
			break;
		case ActiveLine:
			/* NewActiveLine will handle. */
			break;
		default:
			assert(False);
	}
}

void FgXpPlot::postSortByLineNumber(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	FgXpData *ptr;
	void *p;

	switch (_selectMode)
	{
		case OneLine:
		case RangeOfLines:
		case AllLines:
		case SelectedLines:
			for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
				assert(ptr->restoreLineNumber());
			break;
		case ActiveLine:
			/* NewActiveLine will handle. */
			break;
		default:
			assert(False);
	}
}

void FgXpPlot::sourceGathersOutOfDate(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	if (_fg->dependentUpdatesFrozen())
	{
		if (!_outOfDate)
			_outOfDate = True;
	}
	else
	{
		updateSrcRcvs();
	}
}

void FgXpPlot::preUpdateSourceGathers(FieldGeometry *fg)
{
	assert(fg == _fg);

	/* do nothing */
}

void FgXpPlot::postUpdateSourceGathers(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	updateSrcRcvs();
}

void FgXpPlot::postPpValuesChanged(FieldGeometry *fg, int ident,
	long /*index*/, long nrem, long nins)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	if (!_fg->sourceGathersOutOfDate()
		&& (ident == PP_XSKID || ident == PP_YSKID || ident == PP_HOLD)
		&& (nrem == nins))
	{
		updateSrcRcvs();
	}
}

void FgXpPlot::receiverGathersOutOfDate(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	if (_fg->dependentUpdatesFrozen())
	{
		if (!_outOfDate)
			_outOfDate = True;
	}
	else
	{
		updateSrcRcvs();
	}
}

void FgXpPlot::preUpdateReceiverGathers(FieldGeometry *fg)
{
	assert(fg == _fg);

	/* do nothing */
}

void FgXpPlot::postUpdateReceiverGathers(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	updateSrcRcvs();
}

void FgXpPlot::updateSrcRcvs()
{
	FgXpData *d;
	Vector *v;
	void *p;
	Bool anySkids;	/* Before or after. */

	for (anySkids = False, d = _data->top(&p); d; d = _data->next(&p))
	{
		if (d->hasSkids())
			anySkids = True;

		d->setSkids();

		if (d->hasSkids())
			anySkids = True;

		/*
		 * If any flag colors need to be updated,
		 * they will be handled by _vectors->redisplay().
		 * depSelActFlush can have problems since
		 * skids can change number of data indices in line.
		 */
		d->forgetDepSelActChanged();
	}

	switch (_vectors->getDisplayMode())
	{
		case Lines:
			for (v = _vectors->top(&p); v; v = _vectors->next(&p))
				_vectors->setColor(v, !anySkids);
			if (anySkids)
				_vectors->redisplay();
			break;
		case Flags:
		case LinesAndFlags:
		case LinesAndAutoFlags:
			for (v = _vectors->top(&p); v; v = _vectors->next(&p))
				_vectors->setColor(v, False);
			_vectors->redisplay();
			break;
		default:
			assert(False);
	}
}

void FgXpPlot::preNewChaining(FieldGeometry *fg)
{
	assert(fg == _fg);
}

void FgXpPlot::postNewChaining(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	FgXpData *ptr;
	void *p;
	for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
		ptr->depSelActChanged(0, ptr->getNumPts());
}

void FgXpPlot::preLineSelectionsChanged(FieldGeometry *fg,
	long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);
}

void FgXpPlot::postLineSelectionsChanged(FieldGeometry *fg,
	long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	_lineSelectionsChanging = True;
}

/*
 * not in FgInform
 *
 * void FgXpPlot::preFlagSourceReceiverChanged(FieldGeometry *fg, long, long,
 * 	long)
 * {
 * 	assert(fg == _fg);
 * }
 * 
 * void FgXpPlot::postFlagSourceReceiverChanged(FieldGeometry *fg, long ixl,
 * 	long index, long num)
 * {
 * 	assert(fg == _fg);
 * 
 * 	FgXpData *ptr;
 * 	void *p;
 * 	for (ptr = _data->top(&p); ptr; ptr = _data->next(&p))
 * 		if (ptr->getXIndex() == ixl
 * 		 || ptr->getYIndex() == ixl
 * 		 || ptr->getZIndex() == ixl)
 * 		{
 * 			ptr->srcRcvChanged((int) index, (int) num);
 * 		}
 * }
 */

const char *FgXpPlot::dataTypeName(int dataType)
{
	const char *retval;

	switch (dataType)
	{
		case FG_SHOT:
			retval = "Shot Point";
			break;
		case FG_DIST:
			retval = "Inc Distance";
			break;
		case FG_XLOC:
			retval = "X loc";
			break;
		case FG_YLOC:
			retval = "Y loc";
			break;
		case FG_ELEV:
			retval = "Elevation";
			break;
		case FG_XGRID:
			retval = "X grid";
			break;
		case FG_YGRID:
			retval = "Y grid";
			break;
		case FG_HD:
			retval = "Hole depth";
			break;
		case FG_TUH:
			retval = "Uphole time";
			break;
		case FG_RSTAT:
			retval = "Rcv static";
			break;
		case FG_SSTAT:
			retval = "Src static";
			break;
		case FG_XSKID:
			retval = "Rcv X skid";
			break;
		case FG_YSKID:
			retval = "Rcv Y skid";
			break;
		case FG_ESKID:
			retval = "Rcv elev skid";
			break;
		case FG_CUM:
			retval = "Cum Distance";
			break;
		case FG_AZIM:
			retval = "Azimuth";
			break;
		default:
			assert(0);
	}

	return retval;
}

void FgXpPlot::finishedChangesSelectedLines()
{
	/*
	 * pre/post RemoveInsertLines and SortByLineNumber informs
	 * have already reset line numbers and removed deleted
	 * lines.  All that must be done here is to add newly
	 * selected lines and delete unselected lines.
	 */

	Vector   *ptr_v;
	Vector   *next_ptr_v = (Vector *) NULL; /* Silence compiler warning. */
	BaseData *ptr_b;
	FgXpData *ptr_d;
	void *p;
	int i;

	int numGot = _vectors->count();
	assert(_data->count() == numGot);
	long *got;
	int   got_i;

	if (numGot)
	{
		got = new long[numGot];

		for (i = 0, ptr_d = _data->top(&p);
			ptr_d;
			i++, ptr_d = _data->next(&p))
		{
			got[i] = ptr_d->getXIndex();
		}

		assert(i == numGot);
	}

	int numNeed = (int) _fg->numSelectedLines();
	long *need;
	int   need_i;

	if (numNeed)
	{
		need = new long[numNeed];

		for (need_i = i = 0; i < (int) _fg->numLines(); i++)
			if (_fg->lineIsSelected((long) i))
				need[need_i++] = (long) i;

		assert(need_i == numNeed);
	}

	/*
	 * The else claus could handle all cases, but we make
	 * the easier cases more efficient.
	 */
	if (!numGot && !numNeed)
	{
		/*
		 * Empty plot and stay that way.
		 */
	}
	else if (!numGot)
	{
		/*
		 * Add all needs.
		 */
		for (i = 0; i < numNeed; i++)
			_vectors->add(addData(need[i]));

		delete [] need;
	}
	else if (!numNeed)
	{
		/*
		 * Delete all you got.
		 */
		for (ptr_v = _vectors->top(&p); ptr_v; ptr_v = next_ptr_v)
		{
			next_ptr_v = _vectors->next(&p);
			ptr_b = ptr_v->getData();
			_vectors->remove(ptr_v);
			removeData(ptr_b);
		}

		delete [] got;
	}
	else
	{
		/*
		 * Life is complicated here.
		 * I think the only way we can have some
		 * lines selected and others deselected in the
		 * same inform package is if the lines were
		 * resorted.
		 */

		/*
		 * The user will not necessarily select lines in order.
		 */
		qsort((void *) got, (size_t) numGot, sizeof(long), compar);

		for (need_i = got_i = 0;
			(need_i < numNeed) || (got_i < numGot);)
		{
			if (got_i == numGot)
			{
				_vectors->add(addData(need[need_i++]));
			}
			else if (need_i == numNeed)
			{
				removeSelectedLineByIndex(got[got_i++]);
			}
			else if (got[got_i] > need[need_i])
			{
				_vectors->add(addData(need[need_i++]));
			}
			else if (got[got_i] < need[need_i])
			{
				removeSelectedLineByIndex(got[got_i++]);
			}
			else
			{
				need_i++;
				 got_i++;
			}
		}

		delete [] need;
		delete [] got ;
	}
}

int FgXpPlot::compar(const void *element1, const void *element2)
{
	return (*((long *) element1)  < *((long *) element2)) ? -1 :
	      ((*((long *) element1) == *((long *) element2)) ?  0 : 1);
}

void FgXpPlot::removeSelectedLineByIndex(long index)
{
	assert(SelectedLines == _selectMode);

	Vector   *ptr_v;
	BaseData *ptr_b;
	FgXpData *ptr_d;
	void *p;

	for (ptr_v = _vectors->top(&p); ptr_v; ptr_v = _vectors->next(&p))
	{
		ptr_b = ptr_v->getData();
		ptr_d = (FgXpData*) ptr_b->actualData();

		if (ptr_d->getXIndex() == index)
		{
			/*
			 * Do not worry about screwing up ptr_v since breaking.
			 */
			_vectors->remove(ptr_v);
			removeData(ptr_b);
			break;
		}
	}

	assert(ptr_v);	/* must find it */
}
