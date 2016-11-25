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
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/fgxp_plot.hh"
#include "fgxp/fgxp_data.hh"
#include "fg2d/fg2d_plot.hh"
#include "geom/field_geometry.hh"

#include <stdlib.h>
#include <math.h>

#define VALID_IDENT(ident)	\
(((ident) > FG_NONE && (ident) <= FG_AZIM && (ident) != FG_SEL) ? 1 : 0)

FgXpPlotLinkedList::FgXpPlotLinkedList( const char *normalLineColor   ,
	const char *sourceLineColor   , const char *receiverLineColor ,
	const char *srcAndRcvLineColor, const char *selectedLineColor ,
	const char *activeLineColor   ,
	const char *normalFlagColor   , const char *dependentFlagColor,
	const char *selectedFlagColor , const char *activeFlagColor   ,
	int activeLineDependent   , int selectedLineDependent ,
	int srcAndRcvLineDependent, int receiverLineDependent ,
	int sourceLineDependent   ,
	int activeFlagDependent   , int selectedFlagDependent ,
	int dependentFlagDependent,
	FlagMode flagMode, int isFg2DPlot)
	: _normalLineColor  ((char *) NULL), _sourceLineColor   ((char *) NULL),
	  _receiverLineColor((char *) NULL), _srcAndRcvLineColor((char *) NULL),
	  _selectedLineColor((char *) NULL), _activeLineColor   ((char *) NULL),
	  _normalFlagColor  ((char *) NULL), _dependentFlagColor((char *) NULL),
	  _selectedFlagColor((char *) NULL), _activeFlagColor   ((char *) NULL),
	  _activeLineDependent   (activeLineDependent   ),
	  _selectedLineDependent (selectedLineDependent ),
	  _srcAndRcvLineDependent(srcAndRcvLineDependent),
	  _receiverLineDependent (receiverLineDependent ),
	  _sourceLineDependent   (sourceLineDependent   ),
	  _activeFlagDependent   (activeFlagDependent   ),
	  _selectedFlagDependent (selectedFlagDependent ),
	  _dependentFlagDependent(dependentFlagDependent),
	  _flagMode(flagMode), _isFg2DPlot(isFg2DPlot),
	  _fg2DPlot((Fg2DPlot *) NULL)
{
	Vector::smartStrcpy(&_normalLineColor   , normalLineColor   );
	Vector::smartStrcpy(&_sourceLineColor   , sourceLineColor   );
	Vector::smartStrcpy(&_receiverLineColor , receiverLineColor );
	Vector::smartStrcpy(&_srcAndRcvLineColor, srcAndRcvLineColor);
	Vector::smartStrcpy(&_selectedLineColor , selectedLineColor );
	Vector::smartStrcpy(&_activeLineColor   , activeLineColor   );

	Vector::smartStrcpy(&_normalFlagColor   , normalFlagColor   );
	Vector::smartStrcpy(&_dependentFlagColor, dependentFlagColor);
	Vector::smartStrcpy(&_selectedFlagColor , selectedFlagColor );
	Vector::smartStrcpy(&_activeFlagColor   , activeFlagColor   );
}

FgXpPlotLinkedList::~FgXpPlotLinkedList()
{
	if (_normalLineColor   )
		delete [] _normalLineColor   ;

	if (_sourceLineColor   )
		delete [] _sourceLineColor   ;

	if (_receiverLineColor )
		delete [] _receiverLineColor ;

	if (_srcAndRcvLineColor)
		delete [] _srcAndRcvLineColor;

	if (_selectedLineColor )
		delete [] _selectedLineColor ;

	if (_activeLineColor   )
		delete [] _activeLineColor   ;

	if (_normalFlagColor   )
		delete [] _normalFlagColor   ;

	if (_dependentFlagColor)
		delete [] _dependentFlagColor;

	if (_selectedFlagColor )
		delete [] _selectedFlagColor ;

	if (_activeFlagColor   )
		delete [] _activeFlagColor   ;
}

void FgXpPlotLinkedList::add(class FgXpPlot *fgXpPlot)
{
	assert(!_isFg2DPlot);

	FgXpPlotElement *theElement = new FgXpPlotElement(fgXpPlot);

	BaseLinkedList::add((Element *) theElement);
}

void FgXpPlotLinkedList::remove(class FgXpPlot *fgXpPlot)
{
	assert(!_isFg2DPlot);

	BaseLinkedList::remove((void *) fgXpPlot);
}

class FgXpPlot *FgXpPlotLinkedList::find(class FgXpPlot *fgXpPlot,
	void **p)
{
	assert(!_isFg2DPlot);

	FgXpPlotElement *ptr = (FgXpPlotElement *)
		BaseLinkedList::find((void *) fgXpPlot, p);

	return (ptr ? ptr->_fgXpPlot : (class FgXpPlot *) NULL);
}

class FgXpPlot *FgXpPlotLinkedList::top    (void **p)
{
	assert(!_isFg2DPlot);

	FgXpPlotElement *ptr = (FgXpPlotElement *) BaseLinkedList::top    (p);

	return (ptr ? ptr->_fgXpPlot : (class FgXpPlot *) NULL);
}

class FgXpPlot *FgXpPlotLinkedList::bottom (void **p)
{
	assert(!_isFg2DPlot);

	FgXpPlotElement *ptr = (FgXpPlotElement *) BaseLinkedList::bottom (p);

	return (ptr ? ptr->_fgXpPlot : (class FgXpPlot *) NULL); }

class FgXpPlot *FgXpPlotLinkedList::next   (void **p)
{
	assert(!_isFg2DPlot);

	FgXpPlotElement *ptr = (FgXpPlotElement *) BaseLinkedList::next   (p);

	return (ptr ? ptr->_fgXpPlot : (class FgXpPlot *) NULL);
}

class FgXpPlot *FgXpPlotLinkedList::prev   (void **p)
{
	assert(!_isFg2DPlot);

	FgXpPlotElement *ptr = (FgXpPlotElement *) BaseLinkedList::prev   (p);

	return (ptr ? ptr->_fgXpPlot : (class FgXpPlot *) NULL);
}

class FgXpPlot *FgXpPlotLinkedList::current(void **p)
{
	assert(!_isFg2DPlot);

	FgXpPlotElement *ptr = (FgXpPlotElement *) BaseLinkedList::current(p);

	return (ptr ? ptr->_fgXpPlot : (class FgXpPlot *) NULL);
}

int FgXpPlotLinkedList::scale(float *xMin, float *xMax,
	float *yMin, float *yMax, float *zMin, float *zMax, float rounder)
{
	assert(!_isFg2DPlot);

	int retval;

	FgXpPlot *ptr;
	void *p;
	float x, y, z;
	for (ptr = top(&p), retval = 0; ptr && !retval; ptr = next(&p))
	{
		if (zMin || zMax)
			retval = ptr->getFirstPnt(&x, &y, &z);
		else
			retval = ptr->getFirstPnt(&x, &y    );
	}

	if (retval)
	{
		*xMin = x;
		*xMax = x;
		*yMin = y;
		*yMax = y;

		if (zMin)
			*zMin = z;

		if (zMax)
			*zMax = z;

		for (ptr = top(&p); ptr; ptr = next(&p))
			ptr->getRange(xMin, xMax, yMin, yMax, zMin, zMax);

		niceScale(xMin, xMax, rounder);
		niceScale(yMin, yMax, rounder);

		if (zMin && zMax)
			niceScale(zMin, zMax, rounder);
	}

	return retval;
}

void FgXpPlotLinkedList::niceScale(float *min, float *max, float rounder)
{
	/*
	 * rounder must be >= 0 and < 1.
	 * Increasing rounder yields a wider scale.
	 */
	assert(0.0 <= rounder && 1.0 > rounder);

	double dMin = (double) *min;
	double dMax = (double) *max;
	double range = dMax - dMin;
	double incr;

	assert(range >= (double) 0.0);

	if (range > (double) 0.0)
		incr = pow((double) 10.0,
			floor(log10(range) + (double) rounder));
	else
		incr = (double) 1.0;

	dMin = floor(dMin / incr) * incr;
	dMax = ceil (dMax / incr) * incr;

	*min = (float) ((dMin == (double) *min) ? dMin - incr : dMin);
	*max = (float) ((dMax == (double) *max) ? dMax + incr : dMax);
}

class Vector *FgXpPlotLinkedList::getClosest(int x, int y,
	class PlotBase *plot, FgXpPlot **closestPlot)
{
	assert(!_isFg2DPlot);

	Vector *retval = (Vector *) NULL;
	if (closestPlot)
		*closestPlot = (FgXpPlot *) NULL;

	Vector *checkVector;
	float closestDistance = 0.0;	/* Inited so no compiler warning. */
	float distance;

	FgXpPlot *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
	{
		if (retval)
		{
			checkVector = ptr->getClosest(x, y, plot, &distance);

			if (checkVector && distance < closestDistance)
			{
				retval = checkVector;
				closestDistance = distance;

				if (closestPlot)
					*closestPlot = ptr;
			}
		}
		else
		{
			retval = ptr->getClosest(x, y, plot, &closestDistance);

			if (retval && closestPlot)
				*closestPlot = ptr;
		}
	}

	return retval;
}

void FgXpPlotLinkedList::areaSelect(int x1, int x2, int y1, int y2, char c,
	class PlotBase *plot, int *interpolate, int threshold)
{
  assert(!_isFg2DPlot);

  VectorAreaPick ***vap = new VectorAreaPick **[count()]; 
  int *selectedLines = new int[count()];

  /*
   * If the same flag is in different FgXpPlots, it will be counted twice.
   */
  FgXpPlot *ptr;
  void *p;
  int numSelectedLines , numSelectedFlags  , flagsInLongestLine       ;
  int selectedFgXpPlots, totalSelectedFlags, flagsInOverallLongestLine;

  for (selectedFgXpPlots = totalSelectedFlags = flagsInOverallLongestLine = 0,
    ptr = top (&p);
    ptr;
    ptr = next(&p))
  {
    ptr->getFlagsInArea(x1, y1, x2, y2, plot,
      &numSelectedLines, &numSelectedFlags, &flagsInLongestLine,
      vap + selectedFgXpPlots);

    if (numSelectedLines)
    {
      selectedLines[selectedFgXpPlots++] = numSelectedLines;
      totalSelectedFlags += numSelectedFlags;
      if (flagsInLongestLine > flagsInOverallLongestLine)
        flagsInOverallLongestLine = flagsInLongestLine;
    }
  }

  if (selectedFgXpPlots)
  {
    FieldGeometry *fg = top()->getFg();
    Bool doFreeze = (totalSelectedFlags * flagsInOverallLongestLine > threshold)
      && (totalSelectedFlags > 1);

    if (doFreeze)
       fg->freezeDependentUpdates();
    else
       fg->preMultipleOperations ();


    int fgXpPlot, line, flag;

    for (fgXpPlot = 0; fgXpPlot < selectedFgXpPlots; fgXpPlot++)
    {
      for (line = 0; line < selectedLines[fgXpPlot]; line++)
      {
        if (interpolate)
        {
          for (flag = 0; flag < vap[fgXpPlot][line]->numIndices; flag++)
          {
            if ((interpolate[0]))
            {
              assert(VALID_IDENT((int) vap[fgXpPlot][line]->vectorId[3]));
              fg->setDependentFlagValue(vap[fgXpPlot][line]->vectorId[0],
                (long) vap[fgXpPlot][line]->index[flag],
                (int) vap[fgXpPlot][line]->vectorId[3], 0.0);
            }

            if ((interpolate[1]))
            {
              assert(VALID_IDENT((int) vap[fgXpPlot][line]->vectorId[4]));
              fg->setDependentFlagValue(vap[fgXpPlot][line]->vectorId[1],
                (long) vap[fgXpPlot][line]->index[flag],
                (int) vap[fgXpPlot][line]->vectorId[4], 0.0);
            }

            if ((interpolate[2]))
            {
              assert(VALID_IDENT((int) vap[fgXpPlot][line]->vectorId[5]));
              fg->setDependentFlagValue(vap[fgXpPlot][line]->vectorId[2],
                (long) vap[fgXpPlot][line]->index[flag],
                (int) vap[fgXpPlot][line]->vectorId[5], 0.0);
            }
          }
        }
        else
        {
          for (flag = 0; flag < vap[fgXpPlot][line]->numIndices; flag++)
            fg->setFlagSelectValue(vap[fgXpPlot][line]->vectorId[0],
              (long) vap[fgXpPlot][line]->index[flag], c);

          if(vap[fgXpPlot][line]->vectorId[1]!=vap[fgXpPlot][line]->vectorId[0])
            for (flag = 0; flag < vap[fgXpPlot][line]->numIndices; flag++)
              fg->setFlagSelectValue(vap[fgXpPlot][line]->vectorId[1],
                (long) vap[fgXpPlot][line]->index[flag], c);

          if(vap[fgXpPlot][line]->vectorId[2]!=vap[fgXpPlot][line]->vectorId[0]
           &&vap[fgXpPlot][line]->vectorId[2]!=vap[fgXpPlot][line]->vectorId[1])
          {
            for (flag = 0; flag < vap[fgXpPlot][line]->numIndices; flag++)
              fg->setFlagSelectValue(vap[fgXpPlot][line]->vectorId[2],
                (long) vap[fgXpPlot][line]->index[flag], c);
          }
        }
      }

      freeVectorAreaPickArray(selectedLines[fgXpPlot], vap[fgXpPlot]);
    }

    if (doFreeze)
      fg->resumeDependentUpdates();
    else
      fg->postMultipleOperations();
  }

  delete vap;
  delete selectedLines;
}

int FgXpPlotLinkedList::selectRcvsFromSrc(int x, int y, PlotBase *plot, char c,
  int threshold)
{
  assert(!_isFg2DPlot);

  int retval;
  long ixl, ixf, *srcIndices;

  if (getFlag(x, y, plot, &ixl, &ixf, &srcIndices, &retval))
  {
    FieldGeometry *fg = top()->getFg();
    long flagsInLongest, chanCtr, srcIndex, grp, chans, chan, tr;
    long *lines = (long *) NULL;
    long *flags = (long *) NULL;

    for (flagsInLongest = chanCtr = srcIndex = 0;
      srcIndex < retval;
      srcIndex++)
    {
      fg->startHeadersFromScratch();

      grp   = fg->sourceGroupNumber     (ixl, ixf, srcIndices[srcIndex]);
      chans = fg->findNumChannelsInGroup(grp);

      if (lines)
      {
        assert(flags);

        assert(lines = (long *) realloc((void *) lines,
          (size_t) (chans + chanCtr) * sizeof(long)));
        assert(flags = (long *) realloc((void *) flags,
          (size_t) (chans + chanCtr) * sizeof(long)));
      }
      else
      {
        assert(!flags);

        if (chans > 0)
        {
          assert(lines = (long *) malloc((size_t) chans * sizeof(long)));
          assert(flags = (long *) malloc((size_t) chans * sizeof(long)));
        }
      }

      for (chan = 1; chan <= chans; chan++)
      {
        tr = fg->findTraceNumber(grp, chan);
        if (!fg->calculateHeaderWords(tr, 0))
        {
          lines[chanCtr] = fg->getHeaderReceiverLineIndex();
          flags[chanCtr] = fg->getHeaderReceiverFlagIndex();
          if (fg->numFlagsOnLine(lines[chanCtr]) > flagsInLongest)
            flagsInLongest = fg->numFlagsOnLine(lines[chanCtr]);
          chanCtr++;
        }
      }
    }

    if (chanCtr > 0)
    {
      Bool doFreeze = (chanCtr * flagsInLongest > (long) threshold)
        && (chanCtr > 1);

      if (doFreeze)
        fg->freezeDependentUpdates();
      else
        fg->preMultipleOperations ();

      for (chan = 0; chan < chanCtr; chan++)
        fg->setFlagSelectValue(lines[chan], flags[chan], c);

      if (doFreeze)
        fg->resumeDependentUpdates();
      else
        fg->postMultipleOperations();
    }

    if (lines)
    {
      assert(flags);

      free((void *) lines);
      free((void *) flags);
    }
    else
    {
      assert(!flags);
    }

    if (retval)
      delete [] srcIndices;
  }
  else
  {
    retval = 0;
  }

  return retval;
}

int FgXpPlotLinkedList::selectSrcsFromRcv(int x, int y, PlotBase *plot, char c,
	int threshold)
{
  assert(!_isFg2DPlot);

  int retval;
  long ixl, ixf;

  if (getFlag(x, y, plot, &ixl, &ixf))
  {
    FieldGeometry *fg = top()->getFg();
    long shots = fg->numReceiversAtFlag(ixl, ixf);

    if (shots)
    {
      long shot, tr, flagsInLongest;
      long *lines, *flags;
      assert(lines = (long *) malloc((size_t) shots * sizeof(long)));
      assert(flags = (long *) malloc((size_t) shots * sizeof(long)));

      fg->startHeadersFromScratch();

      for (flagsInLongest = shot = 0; shot< shots; shot++)
      {
        tr = fg->receiverTraceNumber(ixl, ixf, shot);
        assert(!fg->calculateHeaderWords(tr, 0));
        lines[shot] = fg->getHeaderSourceLineIndex();
        flags[shot] = fg->getHeaderSourceFlagIndex();
        if (fg->numFlagsOnLine(lines[shot]) > flagsInLongest)
          flagsInLongest = fg->numFlagsOnLine(lines[shot]);
      }

      Bool doFreeze = (shots * flagsInLongest > threshold) && (shots > 1);

      if (doFreeze)
        fg->freezeDependentUpdates();
      else
        fg->preMultipleOperations ();

      for (shot = 0; shot < shots; shot++)
        fg->setFlagSelectValue(lines[shot], flags[shot], c);

      if (doFreeze)
        fg->resumeDependentUpdates();
      else
        fg->postMultipleOperations();

      free((void*) lines);
      free((void*) flags);

      retval = 1;
    }
    else
    {
      retval = 0;
    }
  }
  else
  {
    retval = 0;
  }

  return retval;
}

int FgXpPlotLinkedList::getFlag(int x, int y, PlotBase *plot,
	long *ixl, long *ixf, long **srcIndices, int *numSrcs)
{
	assert(!_isFg2DPlot);

	int retval;

	Vector *v = getClosest(x, y, plot);

	if (v)
	{
		FgXpData *data = (FgXpData *) v->getData()->actualData();

		*ixl = data->getXIndex();

		int dataIndex = v->closestVertex(x, y, plot);
		*ixf = (long) data->translateDataIndexToFlag(dataIndex,
			srcIndices, numSrcs);

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

void FgXpPlotLinkedList::clearSelections(int threshold)
{
	assert(!_isFg2DPlot);

	if (count())
	{
		FieldGeometry *fg = top()->getFg();
		long numLines = fg->numLines();

		Bool doFreeze = numLines > threshold;
		if (doFreeze)
			fg->freezeDependentUpdates();

		for (long line = 0; line < numLines; line++)
			fg->clearFlagSelections(line);

		if (doFreeze)
			fg->resumeDependentUpdates();
	}
}

void FgXpPlotLinkedList::getLineColors(  const char **normalLineColor,
	const char **sourceLineColor   , const char **receiverLineColor,
	const char **srcAndRcvLineColor, const char **selectedLineColor,
	const char **activeLineColor   )
{
	*normalLineColor    = _normalLineColor   ;
	*sourceLineColor    = _sourceLineColor   ;
	*receiverLineColor  = _receiverLineColor ;
	*srcAndRcvLineColor = _srcAndRcvLineColor;
	*selectedLineColor  = _selectedLineColor ;
	*activeLineColor    = _activeLineColor   ;
}

void FgXpPlotLinkedList::setLineColors( const char *normalLineColor  ,
	const char *sourceLineColor   , const char *receiverLineColor,
	const char *srcAndRcvLineColor, const char *selectedLineColor,
	const char *activeLineColor   )
{
	if (strcmp(_normalLineColor   , normalLineColor   )
	 || strcmp(_sourceLineColor   , sourceLineColor   )
	 || strcmp(_receiverLineColor , receiverLineColor )
	 || strcmp(_srcAndRcvLineColor, srcAndRcvLineColor)
	 || strcmp(_selectedLineColor , selectedLineColor )
	 || strcmp(_activeLineColor   , activeLineColor  ))
	{
		Vector::smartStrcpy(&_normalLineColor   , normalLineColor   );
		Vector::smartStrcpy(&_sourceLineColor   , sourceLineColor   );
		Vector::smartStrcpy(&_receiverLineColor , receiverLineColor );
		Vector::smartStrcpy(&_srcAndRcvLineColor, srcAndRcvLineColor);
		Vector::smartStrcpy(&_selectedLineColor , selectedLineColor );
		Vector::smartStrcpy(&_activeLineColor   , activeLineColor   );

		if (_isFg2DPlot)
		{
			if (_fg2DPlot)
				_fg2DPlot->setLineColors(  _activeLineColor,
							 _selectedLineColor,
							   _normalLineColor);
		}
		else
		{
			FgXpPlot *ptr;
			void *p;
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setLineColors(_normalLineColor,
						   _sourceLineColor,
						 _receiverLineColor,
						_srcAndRcvLineColor,
						 _selectedLineColor,
						   _activeLineColor);
		}
	}
}

void FgXpPlotLinkedList::getFlagColors(const char **normalFlagColor,
	const char **dependentFlagColor, const char **selectedFlagColor,
	const char **activeFlagColor   )
{
	*normalFlagColor    = _normalFlagColor   ;
	*dependentFlagColor = _dependentFlagColor;
	*selectedFlagColor  = _selectedFlagColor ;
	*activeFlagColor    = _activeFlagColor   ;
}

void FgXpPlotLinkedList::setFlagColors(const char *normalFlagColor,
	const char *dependentFlagColor, const char *selectedFlagColor,
	const char *activeFlagColor   )
{
	if (strcmp(_normalFlagColor   , normalFlagColor   )
	 || strcmp(_dependentFlagColor, dependentFlagColor)
	 || strcmp(_selectedFlagColor , selectedFlagColor )
	 || strcmp(_activeFlagColor   , activeFlagColor   ))
	{
		Vector::smartStrcpy(&_normalFlagColor   , normalFlagColor   );
		Vector::smartStrcpy(&_dependentFlagColor, dependentFlagColor);
		Vector::smartStrcpy(&_selectedFlagColor , selectedFlagColor );
		Vector::smartStrcpy(&_activeFlagColor   , activeFlagColor   );

		if (_isFg2DPlot)
		{
			if (_fg2DPlot)
				_fg2DPlot->setMarkerColors(_activeFlagColor,
							 _selectedFlagColor,
							   _normalFlagColor);
		}
		else
		{
			FgXpPlot *ptr;
			void *p;
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setFlagColors(_normalFlagColor,
						_dependentFlagColor,
						 _selectedFlagColor,
						   _activeFlagColor);
		}
	}
}

void FgXpPlotLinkedList::getLinePrecedence(int *activeLineDependent,
	int *selectedLineDependent, int *srcAndRcvLineDependent,
	int *receiverLineDependent, int *sourceLineDependent   )
{
	*activeLineDependent    = _activeLineDependent   ;
	*selectedLineDependent  = _selectedLineDependent ;
	*srcAndRcvLineDependent = _srcAndRcvLineDependent;
	*receiverLineDependent  = _receiverLineDependent ;
	*sourceLineDependent    = _sourceLineDependent   ;
}

void FgXpPlotLinkedList::setLinePrecedence(int activeLineDependent,
	int selectedLineDependent, int srcAndRcvLineDependent,
	int receiverLineDependent, int sourceLineDependent   )
{
	if (_activeLineDependent    != activeLineDependent
	 || _selectedLineDependent  != selectedLineDependent
	 || _srcAndRcvLineDependent != srcAndRcvLineDependent
	 || _receiverLineDependent  != receiverLineDependent
	 || _sourceLineDependent    != sourceLineDependent)
	{
		_activeLineDependent    = activeLineDependent   ;
		_selectedLineDependent  = selectedLineDependent ;
		_srcAndRcvLineDependent = srcAndRcvLineDependent;
		_receiverLineDependent  = receiverLineDependent ;
		_sourceLineDependent    = sourceLineDependent   ;

		if (_isFg2DPlot)
		{
			if (_fg2DPlot)
				_fg2DPlot->setLinePrecedence(
					_activeLineDependent  ,
					_selectedLineDependent);
		}
		else
		{
			FgXpPlot *ptr;
			void *p;
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setLinePrecedence(_activeLineDependent,
						     _selectedLineDependent,
						    _srcAndRcvLineDependent,
						     _receiverLineDependent,
						       _sourceLineDependent);
		}
	}
}

void FgXpPlotLinkedList::getFlagPrecedence(int *activeFlagDependent,
	int *selectedFlagDependent ,int *dependentFlagDependent)
{
	*activeFlagDependent    = _activeFlagDependent   ;
	*selectedFlagDependent  = _selectedFlagDependent ;
	*dependentFlagDependent = _dependentFlagDependent;
}

void FgXpPlotLinkedList::setFlagPrecedence(int activeFlagDependent,
	int selectedFlagDependent, int dependentFlagDependent)
{
	if (_activeFlagDependent    != activeFlagDependent
	 || _selectedFlagDependent  != selectedFlagDependent
	 || _dependentFlagDependent != dependentFlagDependent)
	{
		_activeFlagDependent    = activeFlagDependent   ;
		_selectedFlagDependent  = selectedFlagDependent ;
		_dependentFlagDependent = dependentFlagDependent;

		if (_isFg2DPlot)
		{
			if (_fg2DPlot)
				_fg2DPlot->setMarkerPrecedence(
					_activeFlagDependent,
				      _selectedFlagDependent);
		}
		else
		{
			FgXpPlot *ptr;
			void *p;
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setFlagPrecedence(_activeFlagDependent,
						     _selectedFlagDependent,
						    _dependentFlagDependent);
		}
	}
}

void FgXpPlotLinkedList::setFlagMode(FlagMode flagMode)
{
	if (_flagMode!= flagMode)
	{
		_flagMode = flagMode;

		if (_isFg2DPlot)
		{
			if (_fg2DPlot)
				_fg2DPlot->setFlagMode(flagMode);
		}
		else
		{
			FgXpPlot *ptr;
			void *p;
			for (ptr = top(&p); ptr; ptr = next(&p))
				ptr->setFlagMode(flagMode);
		}
	}
}
