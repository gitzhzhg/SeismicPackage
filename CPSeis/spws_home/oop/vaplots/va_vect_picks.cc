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
// $Id: va_vect_picks.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_vect_picks.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"

#include <assert.h>

VaVectPickData::VaVectPickData(VfManager *manager, VaPicks *picks, SeisPlot *sp,
	VectorLinkedList *vectors, VaVectColors *colors)
	: VaPickData(manager, picks),
	  _sp(sp), _vectors(vectors), _colors(colors),
	  _numVectAllocated(0), _numVectUsed(0)
{
	/* just initializers */
}

VaVectPickData::~VaVectPickData()
{
	if (_numVectAllocated)
	{
		delete [] _vect  ;
		delete [] _data  ;
	}
}

#define _ALLOC_INC 100

void VaVectPickData::checkAllocation(int new_numVectUsed)
{
	if (_numVectAllocated < new_numVectUsed)
	{
		int new_numVectAllocated;

		for (   new_numVectAllocated = _numVectAllocated;
			new_numVectAllocated < new_numVectUsed;
			new_numVectAllocated += _ALLOC_INC);

		Vector   **new_vect = new Vector   *[new_numVectAllocated];
		BaseData **new_data = new BaseData *[new_numVectAllocated];
		
		if (_numVectUsed)
		{
			memcpy((void *) new_vect, (void *) _vect,
				(size_t) _numVectUsed * sizeof(Vector   *));
			memcpy((void *) new_data, (void *) _data,
				(size_t) _numVectUsed * sizeof(BaseData *));
		}
		
		if (_numVectAllocated)
		{
			delete [] _vect;
			delete [] _data;
		}

		_numVectAllocated = new_numVectAllocated;
		_vect             = new_vect            ;
		_data             = new_data            ;
	}

	_numVectUsed = new_numVectUsed;
}

int VaVectPickData::getIndex(Vector *vector)
{
	for (int retval = 0; retval < _numVectUsed; retval++)
		if (vector == _vect[retval])
			return retval;

	assert(0);
	return 0;	/* silence compiler warning */
}

const char *VaVectPickData::getLineColor(int ifunc,
	const char * activeFuncColor,
	const char *defaultFuncColor,
	int show_active_line)
{
	const char *retval;

	VfDataset *ds = manager()->activeDataset();

	if (_colors->getShowActiveFunc())
	{
		if	( show_active_line
		      && (ifunc == (int) ds->getActiveVelocityFunction()))
		{
			retval = activeFuncColor;
		}
		else if (_colors->getEnableShowFuncs())
		{
			if      ( _picks->getShowFunc(VaVectColors::REF)
			      && (ifunc == (int)
					ds->getReferenceVelocityFunction()))
			{
				retval=_colors->getFuncColor(VaVectColors::REF);
			}
			else if ( _picks->getShowFunc(VaVectColors::PIL)
			      && (ifunc == (int) ds->findPrevXloc()))
			{
				retval=_colors->getFuncColor(VaVectColors::PIL);
			}
			else if ( _picks->getShowFunc(VaVectColors::NIL)
			      && (ifunc == (int) ds->findNextXloc()))
			{
				retval=_colors->getFuncColor(VaVectColors::NIL);
			}
			else if ( _picks->getShowFunc(VaVectColors::PXL)
			      && (ifunc == (int) ds->findPrevYloc()))
			{
				retval=_colors->getFuncColor(VaVectColors::PXL);
			}
			else if ( _picks->getShowFunc(VaVectColors::NXL)
			      && (ifunc == (int) ds->findNextYloc()))
			{
				retval=_colors->getFuncColor(VaVectColors::NXL);
			}
			else if ( _picks->getShowFunc(VaVectColors::SEL)
			      && (_colors->getSelectType() ==
					VaVectColors::SEL_SEL)
			      &&  ds->velocityFunctionIsSelected((long) ifunc))
			{
				retval=_colors->getFuncColor(VaVectColors::SEL);
			}
			else
			{
				retval=defaultFuncColor;
			}
		}
		else
		{
			retval = defaultFuncColor;
		}
	}
	else if (_colors->getEnableShowFuncs())
	{
		if      ( _picks->getShowFunc(VaVectColors::REF)
		      && (ifunc == (int) ds->getReferenceVelocityFunction()))
		{
			retval = _colors->getFuncColor(VaVectColors::REF);
		}
		else if ( _picks->getShowFunc(VaVectColors::SEL)
		      && (_colors->getSelectType() == VaVectColors::SEL_SEL)
		      &&  ds->velocityFunctionIsSelected((long) ifunc))
		{
			retval = _colors->getFuncColor(VaVectColors::SEL);
		}
		else
		{
			retval = defaultFuncColor;
		}
	}
	else
	{
		retval = defaultFuncColor;
	}

	return retval;
}
