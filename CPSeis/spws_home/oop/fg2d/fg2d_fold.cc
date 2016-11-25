#include "fg2d/fg2d_fold.hh"
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
#include "fg2d/fg2d_fold_data.hh"
#include "vect/ll_seis_vect.hh"
#include "geom/field_geometry.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "sp/seis_plot.hh"

Fg2DFold::Fg2DFold(FieldGeometry *fg, FgSeisPlotList *spList, SeisPlot *sp,
	int inLine, unsigned int markerSize, unsigned int markerLineWidth)
	: Fg2DPlot(fg, spList), _sp(sp), _inLine(inLine),
	  _markerSize(markerSize), _markerLineWidth(markerLineWidth),
	  _foldData((Fg2DFoldData *) NULL), _liveFoldData((Fg2DFoldData *) NULL)
{
	_vectors = new SeisVectLinkedList();
	_vectors->addPlot(_sp);

	if (okToPlot())
		displayPlot();
}

Fg2DFold::~Fg2DFold()
{
	delete _vectors;

	if (_foldData)
	{
		assert(_liveFoldData);
		delete     _foldData;
		delete _liveFoldData;
	}
	else
	{
		assert(!_liveFoldData);
	}
}

void Fg2DFold::setMarkerColors(char *active, char *selected, char *normal)
{
	Vector *ptr;
	void   *p  ;

	switch (_vectors->count())
	{
		case 0:
			/* do nothing */
			break;
		case 2:
			for (ptr = _vectors->top(&p);
				ptr;
				ptr = _vectors->next(&p))
			{
				ptr->setAltMarkerColor(
					  _ACTIVE_COLOR_INDEX, active  );
				ptr->setAltMarkerColor(
					_SELECTED_COLOR_INDEX, selected);
				ptr->setAltMarkerColor(
					  _NORMAL_COLOR_INDEX, normal  );
			}
			break;
		default:
			assert(False);
	}
}

void Fg2DFold::setMarkerPrecedence(int active, int selected)
{
	if (_foldData)
	{
		assert(_liveFoldData);

		int activeDependent, selectedDependent;
		_foldData->getMarkerPrecedence(&activeDependent,
			&selectedDependent);

		if ((active   != activeDependent  )
		 || (selected != selectedDependent))
		{
			    _foldData->storeMarkerColorsIndices();
			_liveFoldData->storeMarkerColorsIndices();

			    _foldData->setMarkerPrecedence(active, selected);
			_liveFoldData->setMarkerPrecedence(active, selected);

			    _foldData->updateMarkerColorsIndices();
			_liveFoldData->updateMarkerColorsIndices();
			    _foldData->markerColorFlush         ();
			_liveFoldData->markerColorFlush         ();
		}
	}
}

void Fg2DFold::setLineColors(char *active, char * /*selected*/, char *normal)
{
	char *colorPtr;
	if (_spList->useActiveLine())
		colorPtr = active;
	else
		colorPtr = normal;

	_vectors->top   ()->setColor(colorPtr                   );
	_vectors->bottom()->setColor(_spList->defaultLineColor());
}

void Fg2DFold::setLinePrecedence(int active, int /*selected*/)
{
	char *colorPtr;
	if (active)
		colorPtr = _spList->activeLineColor ();
	else
		colorPtr = _spList->defaultLineColor();

	_vectors->top()->setColor(colorPtr);
}

void Fg2DFold::setFlagMode(FlagMode /*flagMode*/)
{
}

void Fg2DFold::preNewActiveCmp(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DFold::postNewActiveCmp(FieldGeometry * /*fg*/)
{
	if (isPlotted())
	{
		if (changePlot())
		{
			 removePlot();

			if (okToPlot())
				displayPlot();
		}
	}
	else if (okToPlot())
	{
		displayPlot();
	}
}

void Fg2DFold::liveFoldOutOfDate(FieldGeometry * /*fg*/)
{
	if (isPlotted())
		removePlot();
}

void Fg2DFold::preUpdateLiveFold(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DFold::postUpdateLiveFold(FieldGeometry * /*fg*/)
{
	if (!isPlotted() && okToPlot())
		displayPlot();
}

void Fg2DFold::startingChanges(FieldGeometry *fg)
{
	_newGridTransform = 0;

	Fg2DPlot::startingChanges(fg);
}

void Fg2DFold::preNewGridTransform(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DFold::postNewGridTransform(FieldGeometry * /*fg*/)
{
	_newGridTransform = 1;
}

void Fg2DFold::finishedChanges(FieldGeometry *fg)
{
	if (_newGridTransform && !_fg->liveFoldOutOfDate())
	{
		if (isPlotted())
			removePlot();

		if (okToPlot())
			displayPlot();
	}

	Fg2DPlot::finishedChanges(fg);
}

int Fg2DFold::isPlotted()
{
	return ((Fg2DFoldData *) NULL != _liveFoldData);
}

int Fg2DFold::okToPlot()
{
	return (!_fg->liveFoldOutOfDate()
		&&  (_fg->getActiveCmpIndex() != -1)
		&& ((_fg->getActiveCmpIndex() !=  0)
                        || (_fg->numUnplacedTraces() == 0)));
}

int Fg2DFold::changePlot()
{
	int retval;

	long activeCmp = _fg->getActiveCmpIndex();

	if (activeCmp == -1)
	{
		retval = 1;
	}
	else
	{
		double xgrid, ygrid;
		_fg->getCmpGridBinCenter(activeCmp, &xgrid, &ygrid);
		
		if (_inLine)
			retval = (_grid != ygrid);
		else
			retval = (_grid != xgrid);
	}
		
	return retval;
}

void Fg2DFold::displayPlot()
{
	assert(!_foldData && !_liveFoldData && !_fg->liveFoldOutOfDate()
		&& _vectors->count() == 0);

	_foldData = new Fg2DFoldData(_fg,
		_ACTIVE_COLOR_INDEX, _SELECTED_COLOR_INDEX, _NORMAL_COLOR_INDEX,
		(int) _spList->useActiveFlag  (),
		(int) _spList->useSelectedFlag());

	_liveFoldData = new Fg2DFoldData(_fg,
		_ACTIVE_COLOR_INDEX, _SELECTED_COLOR_INDEX, _NORMAL_COLOR_INDEX,
		(int) _spList->useActiveFlag  (),
		(int) _spList->useSelectedFlag());

	long activeCmp = _fg->getActiveCmpIndex();
	assert(activeCmp != -1);

	double xgrid, ygrid;
	_fg->getCmpGridBinCenter(activeCmp, &xgrid, &ygrid);

	double xGridMin, xGridMax, xGridInc, yGridMin, yGridMax, yGridInc;
	double x, y, *gridBinCenterPtr;
	if (_inLine)
	{
		xGridMin = _fg->getMinimumXgridBinCenter();
		xGridMax = _fg->getMaximumXgridBinCenter();
		xGridInc = (double) 1.0;
		_grid = yGridMin = yGridMax = ygrid;
		yGridInc = (double) 0.0;
		gridBinCenterPtr = &x;
	}
	else
	{
		_grid = xGridMin = xGridMax = xgrid;
		xGridInc = (double) 0.0;
		yGridMin = _fg->getMinimumYgridBinCenter();
		yGridMax = _fg->getMaximumYgridBinCenter();
		yGridInc = (double) 1.0;
		gridBinCenterPtr = &y;
	}

	long ixcmp, fold;
	for (x = xGridMin, y = yGridMin;
		x <= xGridMax && y <= yGridMax;
		x += xGridInc, y += yGridInc)
	{
		ixcmp = _fg->getMatchingCmpUsingGrid(x, y);
		assert(ixcmp != -1);

		fold = _fg->    foldOfStack(ixcmp);
		_foldData    ->addPoint(*gridBinCenterPtr, ixcmp, fold);

		fold = _fg->liveFoldOfStack(ixcmp);
		_liveFoldData->addPoint(*gridBinCenterPtr, ixcmp, fold);
	}

	    _foldData->adjustArray();
	_liveFoldData->adjustArray();

	char *colorPtr;
	if (_spList->useActiveLine())
		colorPtr = _spList->activeLineColor ();
	else
		colorPtr = _spList->defaultLineColor();

	Vector *foldVect = _vectors->add(_foldData, colorPtr, 2, False,
		Vector::SolidLine, Vector::CrossMarker, _markerSize,
		_markerLineWidth);

	foldVect->allowAltMarkerColors(True);
	foldVect->setAltMarkerColor(  _ACTIVE_COLOR_INDEX,
		_spList->activeFlagColor  ());
	foldVect->setAltMarkerColor(_SELECTED_COLOR_INDEX,
		_spList->selectedFlagColor());
	foldVect->setAltMarkerColor(  _NORMAL_COLOR_INDEX,
		_spList->defaultFlagColor ());

	Vector *liveFoldVect = _vectors->add(_liveFoldData,
		_spList->defaultLineColor(), 2, False,
		Vector::SolidLine, Vector::CrossMarker, _markerSize,
		_markerLineWidth);

	liveFoldVect->allowAltMarkerColors(True);
	liveFoldVect->setAltMarkerColor(  _ACTIVE_COLOR_INDEX,
		_spList->activeFlagColor  ());
	liveFoldVect->setAltMarkerColor(_SELECTED_COLOR_INDEX,
		_spList->selectedFlagColor());
	liveFoldVect->setAltMarkerColor(  _NORMAL_COLOR_INDEX,
		_spList->defaultFlagColor ());

	float xMin, xMax, yMin, yMax;

	if (_foldData->getRange(&xMin, &xMax, &yMin, &yMax))
	{
		if (xMin == xMax)
		{
			xMin -= 1.0;
			xMax += 1.0;
		}

		if (yMin == yMax)
		{
			yMin -= 1.0;
			yMax += 1.0;
		}

		_sp->setGridXYS(xMin, xMax, yMax, yMin);

		_sp->plot();
	}

	    foldVect->makeVisible(True);
	liveFoldVect->makeVisible(True);
}

void Fg2DFold::removePlot()
{
	assert(_foldData && _liveFoldData && _vectors->count() == 2);

	_vectors->remove(_vectors->top());
	_vectors->remove(_vectors->top());

	delete     _foldData;
	delete _liveFoldData;
	    _foldData = (Fg2DFoldData *) NULL;
	_liveFoldData = (Fg2DFoldData *) NULL;
}

void Fg2DFold::setActiveByIndex(int index)
{
	_foldData->setActiveByIndex(index);
}

void Fg2DFold::setSelectedByIndex(int *indices, int num, char c, int threshold)
{
	_foldData->setSelectedByIndex(indices, num, c, threshold);
}

void Fg2DFold::outputByXlatByIndex(int index, long *line_index,
	long *flag_index, long *shot_index, FgMapToFlag::SkidType *skid_type)
{
	_foldData->outputByXlatByIndex(index, line_index, flag_index,
		shot_index, skid_type);
}
