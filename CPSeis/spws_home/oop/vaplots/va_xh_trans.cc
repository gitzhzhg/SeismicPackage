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
// $Id: va_xh_trans.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include "vaplots/va_xh_trans.hh"
#include "vaplots/va_picks.hh"
#include "vaplots/va_gvs_picks.hh"
#include "vaplots/va_iso_picks.hh"
#include "vaplots/va_iso_plot.hh"
#include "cprim.h"    /* for ZNIL */

/*
 * Static data members
 * Defines ZNIL as variable because ZNIL is near the end of float range
 * comparing with #defined ZNIL does not work.
 */
      float VaCrossHairTranslator::_generic[_NUM_GENERIC];
const float VaCrossHairTranslator::_nil = ZNIL;

VaCrossHairTranslator::VaCrossHairTranslator(VaPicks *picks)
	: CrossHairTranslator(), _picks(picks)
{
	/* do nothing */
}

VaCrossHairTranslator::~VaCrossHairTranslator()
{
	/* do nothing */
}

const float *VaCrossHairTranslator::nilGeneric()
{
	for (int i = 0; i < _NUM_GENERIC; i++)
		_generic[i] = _nil;

	return _generic;
}

float VaCrossHairTranslator::getNil()
{
	return _nil;
}

CmpCrossHairTranslator::CmpCrossHairTranslator(VaPicks *picks)
	: VaCrossHairTranslator(picks)
{
	/* do nothing */
}

CmpCrossHairTranslator::~CmpCrossHairTranslator()
{
	/* do nothing */
}

const float *CmpCrossHairTranslator::toGenericCoords(float x, float y)
{
	_generic[0] = _nil; 
	_generic[1] = _nil; 
	_generic[2] = y; 
	_generic[3] = _nil; 
	_generic[4] = x; 

	return _generic;
}

void CmpCrossHairTranslator::fromGenericCoords(const float *generic,
	float *x, float *y)
{
	*x = generic[4];
	*y = generic[2];
}

GvsCrossHairTranslator::GvsCrossHairTranslator(VaPicks *picks)
	: VaCrossHairTranslator(picks)
{
	/* do nothing */
}

GvsCrossHairTranslator::~GvsCrossHairTranslator()
{
	/* do nothing */
}

const float *GvsCrossHairTranslator::toGenericCoords(float x, float y)
{
	int is_x;
	float value;
	((VaGvsPicks *) _picks)->traceToXorY(x, &is_x, &value);

	if (is_x)
	{
		_generic[0] = value;
		_generic[1] = _nil; 
	}
	else
	{
		_generic[0] = _nil; 
		_generic[1] = value;
	}

	_generic[2] = y; 
	_generic[3] = _nil; 
	_generic[4] = _nil; 

	return _generic;
}

void GvsCrossHairTranslator::fromGenericCoords(const float *generic,
	float *x, float *y)
{
	*x = ((VaGvsPicks *) _picks)->XorYtoTrace(_generic[0], generic[1],
		_nil);
	*y = generic[2];
}

IsoCrossHairTranslator::IsoCrossHairTranslator(VaPicks *picks)
	: VaCrossHairTranslator(picks)
{
	/* do nothing */
}

IsoCrossHairTranslator::~IsoCrossHairTranslator()
{
	/* do nothing */
}

const float *IsoCrossHairTranslator::toGenericCoords(float x, float y)
{
	switch (((VaIsoPlot *) (_picks->getPlot()))->getPlottedLineType())
	{
		case VaIsoPlot::TIMESLICE:
			_generic[0] = x; 
			_generic[1] = y; 
			_generic[2] = _nil;
			break;
		case VaIsoPlot::INLINE:
			_generic[0] = x; 
			_generic[1] = _nil; 
			_generic[2] = y; 
			break;
		case VaIsoPlot::CROSSLINE:
			_generic[0] = _nil; 
			_generic[1] = x;
			_generic[2] = y; 
			break;
		default:
			assert(0);
	}

	_generic[3] = _nil; 
	_generic[4] = _nil; 

	return _generic;
}

void IsoCrossHairTranslator::fromGenericCoords(const float *generic,
	float *x, float *y)
{
	switch (((VaIsoPlot *) (_picks->getPlot()))->getPlottedLineType())
	{
		case VaIsoPlot::TIMESLICE:
			*x = generic[0];
			*y = generic[1];
			break;
		case VaIsoPlot::INLINE:
			*x = generic[0];
			*y = generic[2];
			break;
		case VaIsoPlot::CROSSLINE:
			*x = generic[1];
			*y = generic[2];
			break;
		default:
			assert(0);
	}
}

SemblanceCrossHairTranslator::SemblanceCrossHairTranslator(VaPicks *picks)
	: VaCrossHairTranslator(picks)
{
	/* do nothing */
}

SemblanceCrossHairTranslator::~SemblanceCrossHairTranslator()
{
	/* do nothing */
}

const float *SemblanceCrossHairTranslator::toGenericCoords(float x, float y)
{
	_generic[0] = _nil; 
	_generic[1] = _nil; 
	_generic[2] = y; 
	_generic[3] = x; 
	_generic[4] = _nil; 

	return _generic;
}

void SemblanceCrossHairTranslator::fromGenericCoords(const float *generic,
	float *x, float *y)
{
	*x = generic[3];
	*y = generic[2];
}

GridCrossHairTranslator::GridCrossHairTranslator(VaPicks *picks)
	: VaCrossHairTranslator(picks)
{
	/* do nothing */
}

GridCrossHairTranslator::~GridCrossHairTranslator()
{
	/* do nothing */
}

const float *GridCrossHairTranslator::toGenericCoords(float x, float y)
{
	_generic[0] = x; 
	_generic[1] = y; 
	_generic[2] = _nil; 
	_generic[3] = _nil; 
	_generic[4] = _nil; 

	return _generic;
}

void GridCrossHairTranslator::fromGenericCoords(const float *generic,
	float *x, float *y)
{
	*x = generic[0];
	*y = generic[1];
}
