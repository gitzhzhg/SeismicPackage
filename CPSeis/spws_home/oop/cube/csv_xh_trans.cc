#include "cube/csv_xh_trans.hh"
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
#include "cube/cube_position.hh"
#include "cube/cube_display.hh"
#include "cube/cube.hh"
#include "cube/cube_wire_frame.hh"
#include "sp/seis_plot.hh"
#include "cprim.h"    /* for ZNIL */

#include <assert.h>

/*
 * Static data members
 * Defines ZNIL as variable because ZNIL is near the end of float range
 * comparing with #defined ZNIL does not work.
 */
      float CsvCrossHairTranslator::_generic[_NUM_GENERIC];
const float CsvCrossHairTranslator::_nil = ZNIL;

CsvCrossHairTranslator::CsvCrossHairTranslator(CubePosition *pos,
	CubeDisplay *cd)
	: CrossHairTranslator(), _pos(pos), _cd(cd)
{
	/* do nothing */
}

CsvCrossHairTranslator::~CsvCrossHairTranslator()
{
	/* do nothing */
}

const float *CsvCrossHairTranslator::nilGeneric()
{
	for (int i = 0; i < _NUM_GENERIC; i++)
		_generic[i] = _nil;

	return _generic;
}

float CsvCrossHairTranslator::getNil()
{
	return _nil;
}

InlineCrossHairTranslator::InlineCrossHairTranslator(CubePosition *pos,
	CubeDisplay *cd)
	: CsvCrossHairTranslator(pos, cd)
{
	/* do nothing */
}

InlineCrossHairTranslator::~InlineCrossHairTranslator()
{
	/* do nothing */
}

const float *InlineCrossHairTranslator::toGenericCoords(float x, float y)
{
	Cube *cube = _cd->currentDisplayedCube();

	_generic[0] = cube->convertIndexToWC(Cube::CrossLine,
		      cube->convertWCToIndex(Cube::CrossLine, x));

	assert(_pos->getCurrentInLine(&_generic[1]));

	_generic[2] = cube->convertIndexToWC(Cube::TimeSlice,
		      cube->convertWCToIndex(Cube::TimeSlice, y));

	return _generic;
}

void InlineCrossHairTranslator::fromGenericCoords(const float *generic,
	float *x, float *y)
{
	*x = generic[0];
	*y = generic[2];

	SeisPlot *sp = _cd->currentDisplayedCube()->inlineSP();

	float min = (float) sp->plottedTmin();
	float max = (float) sp->plottedTmax();

	if (min > max)
	{
		float tmp = min;
		      min = max;
		      max = tmp;
	}

	if ((*y < min) || (*y > max))
	{
		*y = _nil;
	}
}

int InlineCrossHairTranslator::getColor(const float *generic)
{
	int retval;

	float il;
	if (_pos->getCurrentInLine(&il))
	{
		if (generic[1] == il)
		{
			SeisPlot *sp = _cd->currentDisplayedCube()->inlineSP();

			float min = (float) sp->plottedTmin();
			float max = (float) sp->plottedTmax();

			if (min > max)
			{
				float tmp = min;
				      min = max;
				      max = tmp;
			}

			if ((generic[2] < min) || (generic[2] > max))
			{
				retval = 0;
			}
			else
			{
				retval = 1;
			}
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

CrosslineCrossHairTranslator::CrosslineCrossHairTranslator(CubePosition *pos,
	CubeDisplay *cd)
	: CsvCrossHairTranslator(pos, cd)
{
	/* do nothing */
}

CrosslineCrossHairTranslator::~CrosslineCrossHairTranslator()
{
	/* do nothing */
}

const float *CrosslineCrossHairTranslator::toGenericCoords(float x, float y)
{
	Cube *cube = _cd->currentDisplayedCube();

	assert(_pos->getCurrentCrossLine(&_generic[0]));

	_generic[1] = cube->convertIndexToWC(Cube::InLine,
		      cube->convertWCToIndex(Cube::InLine, x));

	_generic[2] = cube->convertIndexToWC(Cube::TimeSlice,
		      cube->convertWCToIndex(Cube::TimeSlice, y));

	return _generic;
}

void CrosslineCrossHairTranslator::fromGenericCoords(const float *generic,
	float *x, float *y)
{
	*x = generic[1];
	*y = generic[2];

	SeisPlot *sp = _cd->currentDisplayedCube()->crosslineSP();

	float min = (float) sp->plottedTmin();
	float max = (float) sp->plottedTmax();

	if (min > max)
	{
		float tmp = min;
		      min = max;
		      max = tmp;
	}

	if ((*y < min) || (*y > max))
	{
		*y = _nil;
	}
}

int CrosslineCrossHairTranslator::getColor(const float *generic)
{
	int retval;

	float xl;
	if (_pos->getCurrentCrossLine(&xl))
	{
		if (generic[0] == xl)
		{
			SeisPlot *sp =
				_cd->currentDisplayedCube()->crosslineSP();

			float min = (float) sp->plottedTmin();
			float max = (float) sp->plottedTmax();

			if (min > max)
			{
				float tmp = min;
				      min = max;
				      max = tmp;
			}

			if ((generic[2] < min) || (generic[2] > max))
			{
				retval = 0;
			}
			else
			{
				retval = 1;
			}
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

TimesliceCrossHairTranslator::TimesliceCrossHairTranslator(CubePosition *pos,
	CubeDisplay *cd)
	: CsvCrossHairTranslator(pos, cd)
{
	/* do nothing */
}

TimesliceCrossHairTranslator::~TimesliceCrossHairTranslator()
{
	/* do nothing */
}

const float *TimesliceCrossHairTranslator::toGenericCoords(float x, float y)
{
	Cube *cube = _cd->currentDisplayedCube();

	_generic[0] = cube->convertIndexToWC(Cube::CrossLine,
		      cube->convertWCToIndex(Cube::CrossLine, x));

	_generic[1] = cube->convertIndexToWC(Cube::InLine,
		      cube->convertWCToIndex(Cube::InLine, y));

	assert(_pos->getCurrentTimeSlice(&_generic[2]));

	return _generic;
}

void TimesliceCrossHairTranslator::fromGenericCoords(const float *generic,
	float *x, float *y)
{
	*x = generic[0];
	*y = generic[1];
}

int TimesliceCrossHairTranslator::getColor(const float *generic)
{
	int retval;

	float ts;
	if (_pos->getCurrentTimeSlice(&ts))
		retval = (generic[2] == ts);
	else
		retval = 0;

	return retval;
}

WireFrameCrossHairTranslator::WireFrameCrossHairTranslator(CubePosition *pos,
	CubeDisplay *cd)
	: CsvCrossHairTranslator(pos, cd)
{
	/* do nothing */
}

WireFrameCrossHairTranslator::~WireFrameCrossHairTranslator()
{
	/* do nothing */
}

const float *WireFrameCrossHairTranslator::toGenericCoords(
	float /*x*/, float /*y*/)
{
	assert(0);

	return (const float *) 0;
}

void WireFrameCrossHairTranslator::fromGenericCoords(const float *generic,
	float *x, float *y)
{
	int num_nil, i;
	for (num_nil = i = 0; i < _NUM_GENERIC; i++)
	{
		_axis[i] = generic[i];
		if (_axis[i] == _nil)
			num_nil++;
	}

	if      (num_nil == 0)
	{
		*x = *y = -1.0F * _nil;
	}
	else if (num_nil == 3)
	{
		*x = *y = _nil;
	}
	else
	{
		assert(0);
	}
}

int WireFrameCrossHairTranslator::getColor(const float * /*generic*/)
{
	return 0;
}

int WireFrameCrossHairTranslator::getAxisValues(float *xl, float *il, float *ts)
{
	int retval;

	int num_nil, i;
	for (num_nil = i = 0; i < _NUM_GENERIC; i++)
		if (_axis[i] == _nil)
			num_nil++;

	if      (num_nil == 0)
	{
		*xl = _axis[0];
		*il = _axis[1];
		*ts = _axis[2];

		retval = 1;
	}
	else if (num_nil == 3)
	{
		retval = 0;
	}
	else
	{
		assert(0);
		retval = 0;
	}

	return retval;
}

WireFrameCrossHair::WireFrameCrossHair(CubeWireFrame *wf,
	WireFrameCrossHairTranslator *trans)
	: CrossHair(), _wf(wf), _trans(trans)
{
	/* just initializers */
}

WireFrameCrossHair::~WireFrameCrossHair()
{
	/* do nothing */
}

void WireFrameCrossHair::move(float x, float y, int /*color*/)
{
	float nil = _trans->getNil();

/*	assert(!((x == nil) ^ (y == nil))); */
	if (x == nil)
		assert(y == nil);
	else
		assert(y != nil);

	if (x == nil)
	{
		_wf->clearRbn();
	}
	else
	{
		float xl, il, ts;
		assert(_trans->getAxisValues(&xl, &il, &ts));
		_wf->rbn(xl, il, ts);
	}
}

void WireFrameCrossHair::setVisibility(int /*visible*/)
{
	/* do nothing, I am always visible */
}

int WireFrameCrossHair::getVisibility()
{
	return 1;
}

void WireFrameCrossHair::expose(int /*x*/, int /*y*/, int /*width*/,
	int /*height*/, float /*x_move*/, float /*y_move*/, int /*color_move*/)
{
	/* do nothing */
}

void WireFrameCrossHair::noPlot()
{
	/* do nothing */
}

void WireFrameCrossHair::backingStoreChange()
{
	/* do nothing */
}

void WireFrameCrossHair::getParams(char *** /*colors*/, int * /*num_colors*/,
	int * /*length*/, int * /*height*/)
{
	/* do nothing */
}

void WireFrameCrossHair::setParams(const char * /*color*/, int /*length*/,
	int /*height*/)
{
	/*
	 * Don't go changing, to try and please me.
	 */
}

void WireFrameCrossHair::setParams(const char * /*color*/, int /*num_color*/,
	int /*length*/, int /*height*/)
{
	/*
	 * Don't go changing, to try and please me.
	 */
}
