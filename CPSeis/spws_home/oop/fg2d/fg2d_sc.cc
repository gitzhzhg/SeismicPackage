#include "fg2d/fg2d_sc.hh"
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
#include "fg2d/fg2d_sc_data.hh"
#include "vect/ll_seis_vect.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "sp/seis_plot.hh"

Fg2DSc::Fg2DSc(FieldGeometry *fg, FgSeisPlotList *spList, SeisPlot *sp,
	Fg2DChartData::ActiveDriver activeDriver,
	Fg2DChartData::SelectDriver selectDriver,
	int inLine, unsigned int markerSize, unsigned int markerLineWidth)
	: Fg2DChart(fg, spList, activeDriver, selectDriver),
	  _sp(sp) ,_inLine(inLine),
	  _markerSize(markerSize), _markerLineWidth(markerLineWidth),
	  _data((Fg2DScData *) NULL)
{
	_vectors = new SeisVectLinkedList();
	_vectors->addPlot(_sp);

	if (okToPlot())
		displayPlot();
}

Fg2DSc::~Fg2DSc()
{
	delete _vectors;

	if (_data)
		delete _data;
}

void Fg2DSc::preNewActiveCmp(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DSc::postNewActiveCmp(FieldGeometry * /*fg*/)
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

void Fg2DSc::midpointGathersOutOfDate(FieldGeometry * /*fg*/)
{
	if (isPlotted())
		removePlot();
}

void Fg2DSc::preUpdateMidpointGathers(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DSc::postUpdateMidpointGathers(FieldGeometry * /*fg*/)
{
	if (!isPlotted() && okToPlot())
		displayPlot();
}

void Fg2DSc::startingChanges(FieldGeometry *fg)
{
	_newGridTransform = 0;

	Fg2DChart::startingChanges(fg);
}

void Fg2DSc::preNewGridTransform(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DSc::postNewGridTransform(FieldGeometry * /*fg*/)
{
	_newGridTransform = 1;
}

void Fg2DSc::finishedChanges(FieldGeometry *fg)
{
	if (_newGridTransform && !_fg->midpointGathersOutOfDate())
	{
		if (isPlotted())
			removePlot();

		if (okToPlot())
			displayPlot();
	}

	Fg2DChart::finishedChanges(fg);
}

int Fg2DSc::isPlotted()
{
	return ((Fg2DScData *) NULL != _data);
}

int Fg2DSc::okToPlot()
{
	return (!_fg->midpointGathersOutOfDate()
		&&  (_fg->getActiveCmpIndex() != -1)
		&& ((_fg->getActiveCmpIndex() !=  0)
			|| (_fg->numUnplacedTraces() == 0)));
}

int Fg2DSc::changePlot()
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

void Fg2DSc::displayPlot()
{
	assert(!_data && !_fg->midpointGathersOutOfDate()
		&& _vectors->count() == 0);

	_data = new Fg2DScData(_fg,
		_ACTIVE_COLOR_INDEX, _SELECTED_COLOR_INDEX, _NORMAL_COLOR_INDEX,
		(int) _spList->useActiveFlag  (),
		(int) _spList->useSelectedFlag(),
		_activeDriver, _selectDriver);

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

	long ixcmp, fold, ixfold, traceNum, groupNum;
	for (x = xGridMin, y = yGridMin;
		x <= xGridMax && y <= yGridMax;
		x += xGridInc, y += yGridInc)
	{
		ixcmp = _fg->getMatchingCmpUsingGrid(x, y);
		assert(ixcmp != -1);

		fold = _fg->foldOfStack(ixcmp);

		for (ixfold = 0; ixfold < fold; ixfold++)
		{
			/* Add 1 to go from trace index to trace number.  */
			traceNum = _fg->originalTraceIndex(ixcmp, ixfold) + 1;

			groupNum = _fg->groupNumber(traceNum);

			_data->addPoint(*gridBinCenterPtr, ixcmp,
				groupNum, traceNum);
		}
	}

	_data->determineTraceTypes();

	_data->adjustArray();

	Vector *vector = _vectors->add(_data, "white", 1, False,
		Vector::NoLine, Vector::CrossMarker, _markerSize,
		_markerLineWidth);

	vector->allowAltMarkerColors(True);
	vector->setAltMarkerColor(  _ACTIVE_COLOR_INDEX,
		_spList->activeFlagColor  ());
	vector->setAltMarkerColor(_SELECTED_COLOR_INDEX,
		_spList->selectedFlagColor());
	vector->setAltMarkerColor(  _NORMAL_COLOR_INDEX,
		_spList->defaultFlagColor ());

	float xMin, xMax, yMin, yMax;

	if (_data->getRange(&xMin, &xMax, &yMin, &yMax))
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

	vector->makeVisible(True);
}

void Fg2DSc::removePlot()
{
	assert(_data && _vectors->count() == 1);

	_vectors->remove(_vectors->top());

	delete _data;
	_data = (Fg2DScData *) NULL ;
}

void Fg2DSc::setActiveByIndex(int index)
{
	_data->setActiveByIndex(index);
}

void Fg2DSc::setSelectedByIndex(int *indices, int num, char c, int threshold)
{
	_data->setSelectedByIndex(indices, num, c, threshold);
}

Fg2DChartData *Fg2DSc::getChartData()
{
	return _data;
}

void Fg2DSc::outputByXlatByIndex(int index, long *line_index,
	long *flag_index, long *shot_index, FgMapToFlag::SkidType *skid_type)
{
	_data->outputByXlatByIndex(index, line_index, flag_index, shot_index,
		skid_type);
}
