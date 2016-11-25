#include "fg2d/fg2d_statics.hh"
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
#include "fg2d/fg2d_statics_data.hh"
#include "vect/ll_seis_vect.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "sp/seis_plot.hh"
#include "fgqc/statics_read_pop.hh"
#include "fgqc/statics_file.hh"

Fg2DStatics::Fg2DStatics(FieldGeometry *fg, FgSeisPlotList *spList,
	SeisPlot *sp, StaticsReadPop *pop, unsigned int width,
	unsigned int markerSize, unsigned int markerLineWidth)
	: Fg2DPlot(fg, spList), _sp(sp), _pop(pop), _width(width),
	  _markerSize(markerSize), _markerLineWidth(markerLineWidth),
	  _data((Fg2DStaticsData *) NULL)
{
	_vectors = new SeisVectLinkedList();
	_vectors->addPlot(_sp);
}

Fg2DStatics::~Fg2DStatics()
{
	delete _vectors;

	if (_data)
		delete _data;
}

void Fg2DStatics::newStaticsFile()
{
	if (isPlotted())
		removePlot ();

	if (okToPlot ())
		displayPlot();
}

void Fg2DStatics::setMarkerColors(char *active, char *selected, char *normal)
{
	Vector *v;

	switch (_vectors->count())
	{
		case 0:
			/* do nothing */
			break;
		case 1:
			v = _vectors->top();
			v->setAltMarkerColor(  _ACTIVE_COLOR_INDEX, active  );
			v->setAltMarkerColor(_SELECTED_COLOR_INDEX, selected);
			v->setAltMarkerColor(  _NORMAL_COLOR_INDEX, normal  );
			break;
		default:
			assert(False);
	}
}

void Fg2DStatics::setMarkerPrecedence(int active, int selected)
{
	if (_data)
	{
		int activeDependent, selectedDependent;
		_data->getMarkerPrecedence(&activeDependent,
			&selectedDependent);

		if ((active   != activeDependent  )
		 || (selected != selectedDependent))
		{
		    _data->storeMarkerColorsIndices();

		    _data->setMarkerPrecedence(active, selected);

		    _data->updateMarkerColorsIndices();
		    _data->markerColorFlush         ();
		}
	}
}

void Fg2DStatics::setLineColors(char *active, char * /*selected*/, char *normal)
{
	char *colorPtr;
	if (_spList->useActiveLine())
		colorPtr = active;
	else
		colorPtr = normal;

	_vectors->top()->setColor(colorPtr);
}

void Fg2DStatics::setLinePrecedence(int active, int /*selected*/)
{
	char *colorPtr;
	if (active)
		colorPtr = _spList->activeLineColor ();
	else
		colorPtr = _spList->defaultLineColor();

	_vectors->top()->setColor(colorPtr);
}

void Fg2DStatics::setFlagMode(FlagMode /*flagMode*/)
{
}

void Fg2DStatics::startingChanges(FieldGeometry *fg)
{
	_flagsRemIns = 0;

	Fg2DPlot::startingChanges(fg);
}

void Fg2DStatics::finishedChanges(FieldGeometry *fg)
{
	/*
	 * If flags are removed or inserted, just redraw the whole thing.
	 */
	if (_flagsRemIns)
	{
		if (isPlotted())
			removePlot();

		if (okToPlot())
			displayPlot();
	}

	Fg2DPlot::finishedChanges(fg);
}

void Fg2DStatics::preRemoveInsertFlags(FieldGeometry * /*fg*/,
	long /*ixl*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	_flagsRemIns = 1;
}

void Fg2DStatics::postRemoveInsertFlags(FieldGeometry * /*fg*/,
	long /*ixl*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(_flagsRemIns);
}

void Fg2DStatics::preNewActiveLine(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DStatics::postNewActiveLine(FieldGeometry * /*fg*/)
{
	assert(!_flagsRemIns);

	if (isPlotted())
	{
		if (changePlot())
		{
			removePlot();
				
			if (okToPlot())
				displayPlot();
		}
	}
	else
	{
		if (okToPlot())
			displayPlot();
	}
}

void Fg2DStatics::sourceGathersOutOfDate(FieldGeometry * /*fg*/)
{
	if (isPlotted())
	{
		assert(_pop);
		if (_pop->getFile()->getCoordinateSystem()
			== StaticsFile::GROUPSYSTEM)
		{
			removePlot();
		}
	}
}

void Fg2DStatics::preUpdateSourceGathers(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DStatics::postUpdateSourceGathers(FieldGeometry * /*fg*/)
{
	if (!isPlotted() && okToPlot())
		displayPlot();
}

int Fg2DStatics::isPlotted()
{
	return ((Fg2DStaticsData *) NULL != _data);
}

int Fg2DStatics::okToPlot()
{
	int retval;

	assert(_pop);
	StaticsFile *sf = _pop->getFile();

	if (sf->inputFileIsReadable() && (_fg->getActiveLineIndex() != -1))
	{
		switch (sf->getCoordinateSystem())
		{
			case StaticsFile::GRIDSYSTEM:
			case StaticsFile::GRIDSYSTEM2D:
			case StaticsFile::SURVEYSYSTEM:
			case StaticsFile::SURVEYSYSTEM2D:
			case StaticsFile::GROUNDPOSITION:
				retval = 1;
				break;
			case StaticsFile::GROUPSYSTEM:
				if (_fg->sourceGathersOutOfDate())
					retval = 0;
				else
					retval = 1;
				break;
			case StaticsFile::UNRECOGNIZEDSYSTEM:
				retval = 0;
				break;
			default:
				assert(False);
		}
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int Fg2DStatics::changePlot()
{
	return (_data->getLineIndex() != _fg->getActiveLineIndex());
}

void Fg2DStatics::displayPlot()
{
	assert(!_data && _vectors->count() == 0);

	_data = new Fg2DStaticsData(_fg, _fg->getActiveLineIndex(),
		_pop->getFile(),
		_ACTIVE_COLOR_INDEX, _SELECTED_COLOR_INDEX, _NORMAL_COLOR_INDEX,
		(int) _spList->useActiveFlag  (),
		(int) _spList->useSelectedFlag());

	char *colorPtr;
	if (_spList->useActiveLine())
		colorPtr = _spList->activeLineColor ();
	else
		colorPtr = _spList->defaultLineColor();

	Vector *vect = _vectors->add(_data, colorPtr, _width, False,
		Vector::SolidLine, Vector::DataSpecifiedMarker, _markerSize,
		_markerLineWidth);

	vect->allowAltMarkerColors(True);
	vect->setAltMarkerColor(  _ACTIVE_COLOR_INDEX,
		_spList->activeFlagColor  ());
	vect->setAltMarkerColor(_SELECTED_COLOR_INDEX,
		_spList->selectedFlagColor());
	vect->setAltMarkerColor(  _NORMAL_COLOR_INDEX,
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

	vect->makeVisible(True);
}

void Fg2DStatics::removePlot()
{
	assert(_data && _vectors->count() == 1);

	_vectors->remove(_vectors->top());

	delete _data;
	_data = (Fg2DStaticsData *) NULL;
}

void Fg2DStatics::setActiveByIndex(int index)
{
	_data->setActiveByIndex(index);
}

void Fg2DStatics::setSelectedByIndex(int *indices, int num,
	char c, int threshold)
{
	_data->setSelectedByIndex(indices, num, c, threshold);
}

void Fg2DStatics::outputByXlatByIndex(int index, long *line_index,
	long *flag_index, long *shot_index, FgMapToFlag::SkidType *skid_type)
{
	_data->outputByXlatByIndex(index, line_index, flag_index, shot_index,
		skid_type);
}
