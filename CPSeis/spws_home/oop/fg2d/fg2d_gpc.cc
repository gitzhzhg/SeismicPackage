#include "fg2d/fg2d_gpc.hh"
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
#include "fg2d/fg2d_gpc_data.hh"
#include "vect/ll_seis_vect.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "sp/seis_plot.hh"

Fg2DGpc::Fg2DGpc(FieldGeometry *fg, FgSeisPlotList *spList, SeisPlot *sp,
	Fg2DChartData::ActiveDriver activeDriver,
	Fg2DChartData::SelectDriver selectDriver,
	unsigned int markerSize, unsigned int markerLineWidth,
	Vector::VectorMarker srcMarker, Vector::VectorMarker rcvMarker)
	: Fg2DChart(fg, spList, activeDriver, selectDriver),
	  _sp(sp), _markerSize(markerSize), _markerLineWidth(markerLineWidth),
	  _srcMarker((int) srcMarker), _rcvMarker((int) rcvMarker),
	  _data((Fg2DGpcData *) NULL), _waiting_for_midpoints(0)
{
	_vectors = new SeisVectLinkedList();
	_vectors->addPlot(_sp);

	if (okToPlot())
		displayPlot();
}

Fg2DGpc::~Fg2DGpc()
{
	delete _vectors;

	if (_data)
		delete _data;
}

void Fg2DGpc::preNewActiveLine(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DGpc::postNewActiveLine(FieldGeometry * /*fg*/)
{
	if (isPlotted())
	{
		assert(!_fg->receiverGathersOutOfDate());

		if (changePlot())
		{
			removePlot();

			if (okToPlot())
				displayPlot();
		}
	}
	else
	{
		assert( _fg->receiverGathersOutOfDate());
	}
}

void Fg2DGpc::receiverGathersOutOfDate(FieldGeometry * /*fg*/)
{
	if (isPlotted())
		removePlot();
}

void Fg2DGpc::preUpdateReceiverGathers(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DGpc::postUpdateReceiverGathers(FieldGeometry * /*fg*/)
{
	if (_fg->updatingMidpointGathers())
		_waiting_for_midpoints = 1;
	else if (!isPlotted() && okToPlot())
		displayPlot();
}

void Fg2DGpc::preUpdateMidpointGathers(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DGpc::postUpdateMidpointGathers(FieldGeometry * /*fg*/)
{
	if (_waiting_for_midpoints)
	{
		if (!isPlotted() && okToPlot())
			displayPlot();

		_waiting_for_midpoints = 0;
	}
}

int Fg2DGpc::isPlotted()
{
	return ((Fg2DGpcData *) NULL != _data);
}

int Fg2DGpc::okToPlot()
{
	return (!_fg->receiverGathersOutOfDate()
		&& (_fg->getActiveLineIndex() != -1));
}

int Fg2DGpc::changePlot()
{
	return (_data->getLineIndex() != _fg->getActiveLineIndex());
}

void Fg2DGpc::displayPlot()
{
	assert(!_data && _vectors->count() == 0);

	long activeLine = _fg->getActiveLineIndex();
	assert(activeLine != -1);

	_data = new Fg2DGpcData(_fg, activeLine, _srcMarker, _rcvMarker,
		_ACTIVE_COLOR_INDEX, _SELECTED_COLOR_INDEX, _NORMAL_COLOR_INDEX,
		(int) _spList->useActiveFlag  (),
		(int) _spList->useSelectedFlag(),
		_activeDriver, _selectDriver);

	long numFlags = _fg->numFlagsOnLine(activeLine);
	long flagIndex, num, i, traceNumber, groupNumber;

	for (flagIndex = 0; flagIndex < numFlags; flagIndex++)
	{
		num = _fg->numSourcesAtFlag(activeLine, flagIndex);

		for (i = 0; i < num; i++)
		{
			groupNumber = _fg->sourceGroupNumber(activeLine,
				flagIndex, i);

			_data->addPoint(flagIndex, groupNumber, -1,
				Fg2DGpcData::Source);
		}
		
		num = _fg->numReceiversAtFlag(activeLine, flagIndex);

		for (i = 0; i < num; i++)
		{
			traceNumber = _fg->receiverTraceNumber(activeLine,
				flagIndex, i);

			groupNumber = _fg->groupNumber(traceNumber);

			_data->addPoint(flagIndex, groupNumber, traceNumber,
				Fg2DGpcData::Receiver);
		}
	}

	_data->determineTraceTypes();

	_data->adjustArray();

	Vector *vector = _vectors->add(_data, "white", 1, False,
		Vector::NoLine, Vector::DataSpecifiedMarker, _markerSize,
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

void Fg2DGpc::removePlot()
{
	assert(_data && _vectors->count() == 1);

	_vectors->remove(_vectors->top());

	delete _data;
	_data = (Fg2DGpcData *) NULL ;
}

void Fg2DGpc::setActiveByIndex(int index)
{
	_data->setActiveByIndex(index);
}

void Fg2DGpc::setSelectedByIndex(int *indices, int num, char c, int threshold)
{
	_data->setSelectedByIndex(indices, num, c, threshold);
}

Fg2DChartData *Fg2DGpc::getChartData()
{
	return _data;
}

void Fg2DGpc::outputByXlatByIndex(int index, long *line_index,
	long *flag_index, long *shot_index, FgMapToFlag::SkidType *skid_type)
{
	_data->outputByXlatByIndex(index, line_index, flag_index, shot_index,
		skid_type);
}
