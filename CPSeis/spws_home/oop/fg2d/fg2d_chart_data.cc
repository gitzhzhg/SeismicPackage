#include "fg2d/fg2d_chart_data.hh"
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
#include "geom/field_geometry.hh"

Fg2DChartData::Fg2DChartData(class FieldGeometry *fg,
	int activeColorIndex, int selectedColorIndex, int normalColorIndex,
	int activeDependent , int selectedDependent ,
	ActiveDriver activeDriver, SelectDriver selectDriver, long id)
	: Fg2DData(fg, id), 
	_activeColorIndex  (activeColorIndex  ),
	_selectedColorIndex(selectedColorIndex),
	_normalColorIndex  (normalColorIndex  ),
	_activeDependent   (activeDependent   ),
	_selectedDependent (selectedDependent ),
	_activeDriver(activeDriver),
	_selectDriver(selectDriver)
{
	/* just initializers */
}

int Fg2DChartData::getAltMarkerColor(int i, long id)
{
	assert(id == _id && i >= 0 && i < _numPts);

	int retval;

	if (_activeDependent && isActive(i))
	{
		retval = _activeColorIndex;
	}
	else if (_selectedDependent && isSelected(i))
	{
		retval = _selectedColorIndex;
	}
	else
	{
		retval = _normalColorIndex ;
	}

	return retval;
}

void Fg2DChartData::setActiveDriver(ActiveDriver activeDriver)
{
	_activeDriver = activeDriver;
}

void Fg2DChartData::setSelectDriver(SelectDriver selectDriver)
{
	_selectDriver = selectDriver;
}

void Fg2DChartData::getMarkerPrecedence(int *activeDependent,
	int *selectedDependent)
{
	  *activeDependent =   _activeDependent;
	*selectedDependent = _selectedDependent;
}

void Fg2DChartData::setMarkerPrecedence(int  activeDependent,
	int  selectedDependent)
{
	  _activeDependent =   activeDependent;
	_selectedDependent = selectedDependent;
}

int Fg2DChartData::setActiveByGroup(long group, long seqGndPos)
{
	int retval;

	int index = getIndexByGroup(group, seqGndPos);

	if (index >= 0)
	{
		setActiveByIndex(index);

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

Boolean Fg2DChartData::outputByXlatByGroup(long group, long seqGndPos,
	long *line_index, long *flag_index, long *shot_index,
	FgMapToFlag::SkidType *skid_type)
{
	Boolean retval;

	int index = getIndexByGroup(group, seqGndPos);

	if (index >= 0)
	{
		outputByXlatByIndex(index, line_index, flag_index,
			shot_index, skid_type);

		retval = True ;
	}
	else
	{
		retval = False;
	}

	return retval;
}

Fg2DChartData::TraceType Fg2DChartData::getTraceType(long trace)
{
	TraceType retval;

	int deadCode = _fg->getDeadCode((int) (trace - 1));

	if (deadCode == -1)
	{
		_fg->calculateHeaderWords(trace, 0);

		double header25 = _fg->getHeaderWordValue(25);

		if      (header25 >  (double) 0)
			retval = Normal ;
		else if (header25 == (double) 0)
			retval = Dead   ;
		else if (header25 == (double) -999)
			retval = Missing;
		else
			retval = RevPol ;
	}
	else
	{
		switch (deadCode)
		{
			case ZT_CODE_ZERO:  retval = Dead   ;  break;
			case ZT_CODE_REV :  retval = RevPol ;  break;
			case ZT_CODE_MISS:  retval = Missing;  break;
			case ZT_CODE_LIVE:  retval = Normal ;  break;
			default          :  assert(0);
		}
	}

	return retval;
}
