#include "fg2d/fg2d_chart.hh"
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
#include "fg2d/fg2d_chart_data.hh"
#include "vect/ll_seis_vect.hh"

#include <assert.h>

Fg2DChart::Fg2DChart(FieldGeometry *fg, class FgSeisPlotList *spList,
	Fg2DChartData::ActiveDriver activeDriver,
	Fg2DChartData::SelectDriver selectDriver)
	: Fg2DPlot(fg, spList),
	  _activeDriver(activeDriver), _selectDriver(selectDriver)
{
	/* just initializers */
}

Fg2DChart::~Fg2DChart()
{
	/* do nothing */
}

void Fg2DChart::setMarkerColors(char *active, char *selected, char *normal)
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

void Fg2DChart::setMarkerPrecedence(int active, int selected)
{
	Fg2DChartData *data = getChartData();

	if (data)
	{
		int activeDependent, selectedDependent;
		data->getMarkerPrecedence(&activeDependent, &selectedDependent);

		if ((active   != activeDependent  )
		 || (selected != selectedDependent))
		{
			data->storeMarkerColorsIndices();

			data->setMarkerPrecedence(active, selected);

			data->updateMarkerColorsIndices();
			data->markerColorFlush         ();
		}
	}
}

void Fg2DChart::setFlagMode(FlagMode /*flagMode*/)
{
}

int Fg2DChart::setActive(int x, int y, PlotBase *plot)
{
	int retval;

	if (isPlotted())
		retval = getChartData()->setActiveByGroup(
			(long) floor((double) plot->yWC(y) + 0.5),
			(long) floor((double) plot->xWC(x) + 0.5));
	else
		retval = 0;

	return retval;
}

Boolean Fg2DChart::outputByXlat(SeisPlot *sp, int x, int y, long *line_index,
	long *flag_index, long *shot_index, FgMapToFlag::SkidType *skid_type)
{
	Boolean retval;

	if (isPlotted())
		retval = getChartData()->outputByXlatByGroup(
			(long) floor((double) sp->yWC(y) + 0.5),
			(long) floor((double) sp->xWC(x) + 0.5),
			line_index, flag_index, shot_index, skid_type);
	else
		retval = False;

	return retval;
}

void Fg2DChart::preZt1ValuesChanged(class FieldGeometry *fg,
	int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	preZtAnyValuesChanged();
}

void Fg2DChart::postZt1ValuesChanged(class FieldGeometry *fg,
	int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	postZtAnyValuesChanged();
}

void Fg2DChart::preZt2ValuesChanged(class FieldGeometry *fg,
	int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	preZtAnyValuesChanged();
}

void Fg2DChart::postZt2ValuesChanged(class FieldGeometry *fg,
	int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	postZtAnyValuesChanged();
}

void Fg2DChart::preZt3ValuesChanged(class FieldGeometry *fg,
	int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	preZtAnyValuesChanged();
}

void Fg2DChart::postZt3ValuesChanged(class FieldGeometry *fg,
	int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	postZtAnyValuesChanged();
}

void Fg2DChart::preZt4ValuesChanged(class FieldGeometry *fg,
	int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	preZtAnyValuesChanged();
}

void Fg2DChart::postZt4ValuesChanged(class FieldGeometry *fg,
	int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	postZtAnyValuesChanged();
}

void Fg2DChart::preTredValuesChanged(class FieldGeometry *fg)
{
	assert(fg == _fg);

	preZtAnyValuesChanged();
}

void Fg2DChart::postTredValuesChanged(class FieldGeometry *fg)
{
	assert(fg == _fg);

	postZtAnyValuesChanged();
}

void Fg2DChart::preZtAnyValuesChanged()
{
	/* do nothing */
}

void Fg2DChart::postZtAnyValuesChanged()
{
	_mustDetermineTraceTypes = 1;
}

void Fg2DChart::startingChanges(FieldGeometry *fg)
{
	_mustDetermineTraceTypes = 0;

	Fg2DPlot::startingChanges(fg);
}

void Fg2DChart::finishedChanges(FieldGeometry *fg)
{
	if (_mustDetermineTraceTypes)
	{
		Fg2DChartData *data = getChartData();

		if (data)
			data->determineTraceTypes();
	}

	Fg2DPlot::finishedChanges(fg);
}

void Fg2DChart::setActiveDriver(Fg2DChartData::ActiveDriver activeDriver)
{
	_activeDriver = activeDriver;

	Fg2DChartData *data = getChartData();

	if (data)
	{
		data->storeMarkerColorsIndices();

		data->setActiveDriver(activeDriver);

		data->updateMarkerColorsIndices();
		data->markerColorFlush         ();
	}
}

void Fg2DChart::setSelectDriver(Fg2DChartData::SelectDriver selectDriver)
{
	_selectDriver = selectDriver;

	Fg2DChartData *data = getChartData();

	if (data)
	{
		data->storeMarkerColorsIndices();

		data->setSelectDriver(selectDriver);

		data->updateMarkerColorsIndices();
		data->markerColorFlush         ();
	}
}
