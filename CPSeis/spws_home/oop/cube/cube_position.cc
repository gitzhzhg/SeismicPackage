#include "cube/cube_position.hh"
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
#include "cube/cube_display.hh"
#include "cube/cube.hh"
#include "cube/cube_world_coords.hh"

#include <assert.h>

CubePosition::CubePosition(CubeDisplay *cd)
	: _cd(cd)
{
	/* just initializer */
}

CubePosition::~CubePosition()
{
	/* do nothing */
}

void  CubePosition::getInLineRange(float *min, float *max)
{
	CubeWorldCoords cwc(_cd->currentDisplayedCube());
	*min = cwc.lineMin();
	*max = cwc.lineMax();
}

int CubePosition::getCurrentInLine(float *il)
{
	float min, max;
	getInLineRange(&min, &max);

	return getCurrentInLine(min, max, il);
}

int CubePosition::getCurrentInLine(float min, float max, float *il)
{
	int retval;

	Cube *cube = _cd->currentDisplayedCube();
	int slice = cube->inlineSlice();

	if (slice == Cube::NoLinePlotted || max <= min)
	{
		/* *il undefined */
		retval = 0;
	}
	else
	{
		CubeWorldCoords cwc(_cd->currentDisplayedCube());
		*il = cwc.lineCurrent();
		retval = 1;
	}

	return retval;
}

void  CubePosition::getCrossLineRange(float *min, float *max)
{
	CubeWorldCoords cwc(_cd->currentDisplayedCube());
	*min = cwc.crossLineMin();
	*max = cwc.crossLineMax();
}

int CubePosition::getCurrentCrossLine(float *xl)
{
	float min, max;
	getCrossLineRange(&min, &max);

	return getCurrentCrossLine(min, max, xl);
}

int CubePosition::getCurrentCrossLine(float min, float max, float *xl)
{
	int retval;

	Cube *cube = _cd->currentDisplayedCube();
	int slice = cube->crosslineSlice();

	if (slice == Cube::NoLinePlotted || max <= min)
	{
		/* *xl undefined */
		retval = 0;
	}
	else
	{
		CubeWorldCoords cwc(_cd->currentDisplayedCube());
		*xl = cwc.crossLineCurrent();
		retval = 1;
	}
		
	return retval;
}

void  CubePosition::getTimeSliceRange(float *min, float *max)
{
	CubeWorldCoords cwc(_cd->currentDisplayedCube());
	*min = cwc.sliceMin();
	*max = cwc.sliceMax();
}

int CubePosition::getCurrentTimeSlice(float *ts)
{
	float min, max;
	getTimeSliceRange(&min, &max);

	return getCurrentTimeSlice(min, max, ts);
}

int CubePosition::getCurrentTimeSlice(float min, float max, float *ts)
{
	int retval;

	Cube *cube = _cd->currentDisplayedCube();
	int slice = cube->timeSlice();

	if (slice == Cube::NoLinePlotted || max <= min)
	{
		/* *ts undefined */
		retval = 0;
	}
	else
	{
		CubeWorldCoords cwc(_cd->currentDisplayedCube());
		*ts = cwc.sliceCurrent();
		retval = 1;
	}

	return retval;
}
