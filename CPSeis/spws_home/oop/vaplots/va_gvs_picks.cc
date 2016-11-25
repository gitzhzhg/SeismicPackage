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
// $Id: va_gvs_picks.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_gvs_picks.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_xh_trans.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_utilities.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"
#include "sl/sl_error_pop.hh"
#include "cprim.h"	/* for ZNIL */


#include <stdio.h>
#include <math.h>
#include <assert.h>

VaGvsFuncData::VaGvsFuncData(VaGvsPickData *data, int ifun)
	: BaseData(),
	  _data(data), _ifun(ifun), _remove_cntr(0), _need_mod_done(0),
	  _hiding_active_pick(0)
{
	/* just initializers */
}

VaGvsFuncData::~VaGvsFuncData()
{
	/* do nothing */
}

void VaGvsFuncData::setFunc(int ifun)
{
	_ifun = ifun;
}

void VaGvsFuncData::updatePicks(int *indices, int numIndices)
{
	if (drawLine())
	{
		modAttributesByIndices(indices, numIndices, 1);
	}
	else
	{
		int min, max, i;
		for (min = max = indices[0], i = 1; i < numIndices; i++)
		{
			if (indices[i] < min)
				min = indices[i];

			if (indices[i] > max)
				max = indices[i];
		}

		modAttributesNeedingRepair(min, max - min + 1);
	}
}

void VaGvsFuncData::hideActivePick()
{
	VfDataset *ds = _data->manager()->activeDataset();
	assert(_ifun == (int) ds->getActiveVelocityFunction());
	int act_pck = (int) ds->getActivePick((long) _ifun) + 1;
		
	if (act_pck != 0)
	{
		_hiding_active_pick = 1;
		modAttributesByIndices(&act_pck, 1);
		_hiding_active_pick = 0;
	}
}

#define ABS_DIFF(x, y) (x < y) ? (y - x) : (x - y)

int VaGvsFuncData::closestToVel(float vel)
{
	int retval;

	VfDataset *ds = _data->manager()->activeDataset();
	int num_picks = (int) ds->numPicks((long) _ifun);

	if (num_picks > 0)
	{
		float delta, min_delta;
		int i;

		for (min_delta = ABS_DIFF(vel,
			ds->getOrdinate((long) _ifun, 0L, VTNM)),
			retval = 0, i = 1;
		     (i < num_picks) && (min_delta > 0.0F);
		     i++)
		{
			delta = ABS_DIFF(vel,
				ds->getOrdinate((long) _ifun, (long) i, VTNM));

			if (delta < min_delta)
			{
				retval    = i;
				min_delta = delta;
			}
		}
	}
	else
	{
		retval = -1;
	}

	return retval;
}

int VaGvsFuncData::getNumPts(long /*id*/)
{
	int retval;

	if (_data->isDisplayed(_ifun))
		retval = (int) _data->manager()->activeDataset()->numPicks(
			(long) _ifun) + 2;
	else
		retval = 0;

	return retval;
}

float VaGvsFuncData::getX(int /*i*/, long /*id*/)
{
	return (float) _data->traceNumber(_ifun);
}

float VaGvsFuncData::getY(int i, long /*id*/)
{
	float retval;

	int numPts = getNumPts();

	if      (i ==          0)
		retval = _data->_sp->memTmin();
	else if (i == numPts - 1)
		retval = _data->_sp->memTmax();
	else
		retval = _data->manager()->activeDataset()->getAbscissa(
			(long) _ifun, (long) (i - 1), VTNM);

	return retval;
}

int VaGvsFuncData::getMarkerType(int i, long /*id*/)
{
	int retval;

	if      (i == 0)
	{
		retval = (int) Vector::NoMarker;
	}
	else if (i == getNumPts() - 1)
	{
		retval = (int) Vector::NoMarker;
	}
	else if (drawLine())
	{
		retval = (int) _data->_colors->gvsFuncMarker();
	}
	else if (_data->_colors->getShowActiveFunc())
	{
		VfDataset *ds = _data->manager()->activeDataset();
		long act_fun = ds->getActiveVelocityFunction();
		assert(act_fun != -1L);
		long act_pck = ds->getActivePick(act_fun);

		if (act_pck != -1L)
		{
			float vel = ds->getOrdinate(act_fun, act_pck, VTNM);

			if (i - 1 == closestToVel(vel))
				 retval = (int) _data->_colors->gvsFuncMarker();
			else
				 retval = (int) Vector::NoMarker;
		}
		else
		{
			retval = (int) Vector::NoMarker;
		}
	}
	else
	{
		retval = (int) Vector::NoMarker;
	}

	return retval;
}

int VaGvsFuncData::getAltMarkerColor(int i, long /*id*/)
{
	int retval;

	if (_data->_colors->getShowActiveFunc())
	{
		VfDataset *ds = _data->manager()->activeDataset();
		long act_fun = ds->getActiveVelocityFunction();
		assert(act_fun != -1L);
		long act_pck = ds->getActivePick(act_fun);

		if ((long) _ifun == act_fun)
		{
			if (_hiding_active_pick)
			{
				retval = VaVectColors::DEF_COL;
			}
			else
			{
				/*
				 * If there are no picks, this makes index zero
				 * ACT_COL.  Fortunately, index zero has no
				 * marker.
				 */
				if (i - 1 == (int) act_pck)
					 retval = VaVectColors::ACT_COL;
				else
					 retval = VaVectColors::DEF_COL;
			}
		}
		else
		{
			if (act_pck != -1L)
			{
				float vel = ds->getOrdinate(act_fun, act_pck,
					VTNM);

				if (i - 1 == closestToVel(vel))
					 retval = VaVectColors::ACT_COL;
				else
					 retval = 0;
			}
			else
			{
				retval = 0;
			}
		}
	}
	else
	{
		retval = 0;
	}

	return retval;
}

void VaGvsFuncData::pointsRemoved(int index, int num)
{
	_remove_cntr++;

	if (_data->isDisplayed(_ifun))
	{
		modIndicesBefore(index, num);
		_need_mod_done = 1;
	}
}

void VaGvsFuncData::pointsInserted(int index, int num)
{
	if (_data->isDisplayed(_ifun))
	{
		modIndicesAfter(index, num);
		_need_mod_done = 1;
	}

	if(--_remove_cntr == 0)
	{
		if (_need_mod_done)
		{
			modDone();
			_need_mod_done = 0;
		}
	}
}

int VaGvsFuncData::drawLine()
{
	int retval;

	if      (_data->_colors->getEnableShowFuncs())
	{
		retval = 1;
	}
	else if (_data->_colors->getShowActiveFunc ())
	{
		VfDataset *ds = _data->manager()->activeDataset();
		long act_fun = ds->getActiveVelocityFunction();
		assert(act_fun != -1L);

		if ((long) _ifun == act_fun)
			retval = 1;
		else
			retval = 0;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

VaGvsPickData::VaGvsPickData(VfManager *manager, VaPicks *picks, SeisPlot *sp,
	VectorLinkedList *vectors, VaVectColors *colors)
	: VaVectPickData(manager, picks, sp, vectors, colors)
{
        _error_in_headers_shown = 0;
        strcpy(_filename,"");
	/* just initializers */
}

VaGvsPickData::~VaGvsPickData()
{
	assert(_numVectUsed ==
		(int) manager()->activeDataset()->numVelocityFunctions());

	for (int i = 0; i < _numVectUsed; i++)
		delete _data[i];

	/*
	 * Vectors in _vect are removed and deleted when
	 * their VectorLinkedList is deleted.
	 */
}

int VaGvsPickData::isDisplayed(int ifun)
{
	int retval;

	if (_sp->isPlotDisplayed() && (_sp->originalTraces() > 0))
	{
		int x_fast, fast, slow;
		float fast_first, fast_last, slow_val;
		retval = getRange(&x_fast, &fast, &fast_first, &fast_last,
			             &slow, &slow_val);
                //X and Y headers may not be set correctly
                if(!retval)
                  return retval;
                
		float (VfDataset  ::*fast_loc      )(long ) const;
		float (VfDataset  ::*slow_loc      )(long ) const;
		long  (VfUtilities::*fast_binNumber)(float) const;
		long  (VfUtilities::*slow_binNumber)(float) const;

		if (x_fast)
		{
			fast_loc       = &VfDataset  ::getXloc   ;
			slow_loc       = &VfDataset  ::getYloc   ;
			fast_binNumber = &VfUtilities::xbinNumber;
			slow_binNumber = &VfUtilities::ybinNumber;
		}
		else
		{
			fast_loc       = &VfDataset  ::getYloc   ;
			slow_loc       = &VfDataset  ::getXloc   ;
			fast_binNumber = &VfUtilities::ybinNumber;
			slow_binNumber = &VfUtilities::xbinNumber;
		}

		VfDataset   *ds = manager()->activeDataset();
		VfUtilities *ut = manager()->utilities    ();

		if ((ut->*slow_binNumber)(slow_val                    )
		 == (ut->*slow_binNumber)((ds->*slow_loc)((long) ifun)))
		{
			float x   = (ds->*fast_loc)((long) ifun);

			if      ((x >= fast_first) && (x <= fast_last ))
			{
				retval = 1;
			}
			/*
			 * first and last could be either order
			 */
			else if ((x >= fast_last ) && (x <= fast_first))
			{
				retval = 1;
			}
			else
			{
				/*
				 * Maybe value is just outside range,
				 * but in the same bin as one of the range
				 * endpoints.
				 */
				float xbin = (ut->*fast_binNumber)(x);

				if      (xbin == (ut->*fast_binNumber)
						(fast_first))
				{
					retval = 1;
				}
				else if (xbin == (ut->*fast_binNumber)
						(fast_last ))
				{
					retval = 1;
				}
				else 
				{
					retval = 0;
				}
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


int VaGvsPickData::traceNumber(int ifun)
{
	int retval;

	int x_fast, fast, slow;
	float fast_first, fast_last, slow_val;
        long offset = _sp->originalTraces() * _sp->getCurrentPanel();

	retval = getRange(&x_fast, &fast, &fast_first, &fast_last, 
                          &slow, &slow_val);
        //X and Y headers may not be set correctly
        if(!retval)
          return -1;

	VfDataset   *ds = manager()->activeDataset();
	VfUtilities *ut = manager()->utilities    ();
	float value;
	long  (VfUtilities::*fast_binNumber)(float) const;

	if (x_fast)
	{
		value = ds->getXloc((long) ifun);
		fast_binNumber = &VfUtilities::xbinNumber;
	}
	else
	{
		value = ds->getYloc((long) ifun);
		fast_binNumber = &VfUtilities::ybinNumber;
	}

	int num = (int) _sp->displayedTraces(_sp->getCurrentPanel());

	if (num == 1)
	{
		assert((ut->*fast_binNumber)(value)
		    == (ut->*fast_binNumber)(_sp->getHeaderFromTrace
				(1 + offset, fast)));
		retval = 1;
		return retval;
	}

	assert(fast_last != fast_first);
	retval = 1 + (int) ((float) (num - 1)
		* (value - fast_first) / (fast_last - fast_first) + 0.5F);

	if      (retval < 1)
	{
		assert((ut->*fast_binNumber)(value)
		    == (ut->*fast_binNumber)(_sp->getHeaderFromTrace
				(  1 + offset, fast)));
		retval =   1;
	}
	else if (retval > num)
	{
		assert((ut->*fast_binNumber)(value)
		    == (ut->*fast_binNumber)(_sp->getHeaderFromTrace
				(num + offset, fast)));
		retval = num;
	}

	float retval_value = _sp->getHeaderFromTrace(retval + offset, fast);

	if      (retval_value < value)
	{
		int inc   = (fast_last > fast_first) ?       1 :  -1;
		int limit = (fast_last > fast_first) ? num + 1 :   0;

		for (retval += inc;
			(retval != limit) &&
				(_sp->getHeaderFromTrace(retval + offset, fast)
                                                                       < value);
			retval += inc);

		if (retval == limit)
		{
			retval -= inc;
		}
		/*
		 * I want to round up if exactly between traces.
		 * Since inc direction is to larger, use > in this test.
		 */
		else if ((_sp->getHeaderFromTrace(retval+offset, fast) - value)
		       >  (value - _sp->getHeaderFromTrace(retval+offset - inc,
                                                                         fast)))
		{
			retval -= inc;
		}
	}
	else if (retval_value > value)
	{
		int inc   = (fast_last > fast_first) ?  -1 :       1;
		int limit = (fast_last > fast_first) ?   0 : num + 1;

		for (retval += inc;
			(retval != limit) &&
				(_sp->getHeaderFromTrace(retval + offset, fast)
                                                                       > value);
			retval += inc);

		if (retval == limit)
		{
			retval -= inc;
		}
		/*
		 * I want to round up if exactly between traces.
		 * Since inc direction is to smaller, use >= in this test.
		 */
		else if ((value - _sp->getHeaderFromTrace(retval+offset, fast))
		       >= (_sp->getHeaderFromTrace(retval - inc, fast) - value))
		{
			retval -= inc;
		}
	}

	return retval;
}

/*
 * If the nearest function (index) is in the same bin as xtrace,
 * insertOrSelectFunc will select it.  If the nearest function is
 * not in the same bin, but there is a function in the same bin as
 * xtrace, insertOrSelectFunc will still select the nearest.  Only
 * in the case where the nearest function is not in the same bin
 * and there is no function in the same bin as xtrace will a new
 * function be inserted at xtrace.
 * If a function is within the picking tolerance of xtrace, that
 * function is selected in the VaGvsPicker class and insertOrSelectFunc
 * is never called.
 */
void VaGvsPickData::insertOrSelectFunc(int index, float xtrace)
{
	int x_fast, fast, slow, ok;
	float fast_first, fast_last, slow_val;

	ok= getRange(&x_fast, &fast, &fast_first, &fast_last,&slow, &slow_val);
        //X and Y headers may not be set correctly
        if(!ok) return;

	int itrace    = (int) (xtrace + 0.5F);
	int numTraces = (int) _sp->originalTraces();
	assert((numTraces > 1) && (itrace >= 1) && (itrace <= numTraces));

	float fast_val = fast_first + (float) (itrace - 1)
		/ (float) (numTraces - 1) * (fast_last - fast_first);

	float xloc, yloc;
	if (x_fast)
	{
		xloc = fast_val;
		yloc = slow_val;
	}
	else
	{
		xloc = slow_val;
		yloc = fast_val;
	}

	VfDataset   *ds = manager()->activeDataset();
	VfUtilities *ut = manager()->utilities    ();

	if (ds->numVelocityFunctions())
	{
		/*
		 * findOrInsertVelfun would usually work for
		 * all of these if/else branches, except in the
		 * case that the nearest function is a different
		 * bin and there is a function in the bin.
		 */
		/*
		 * If nearest function is in same bin, select it.
		 */
		if ((ut->xbinNumber(ds->getXloc((long) index))
		  == ut->xbinNumber(xloc))
		 && (ut->ybinNumber(ds->getYloc((long) index))
		  == ut->ybinNumber(yloc)))
		{
			ds->setActiveVelocityFunction((long) index);
		}
		/*
		 * If there is a function in same bin, select nearest.
		 */
		else if (-1 != ds->findMatchingVelfun(xloc, yloc))
		{
			ds->setActiveVelocityFunction((long) index);
		}
		/*
		 * If no function in bin, insert.
		 */
		else
		{
			ds->findOrInsertVelfun(xloc, yloc);
		}
	}
	/*
	 * If there are no functions, we must be inserting.
	 */
	else
	{
		assert(index == 0);
		ds->findOrInsertVelfun(xloc, yloc);
	}
}

int VaGvsPickData::updateLineColors(int check_picks)
{
	/*
	 * check_picks == 0:  just check line color
	 * check_picks == 1:  also check active pick due to active pick
	 * 		      changing
	 * check_picks == 2:  also check active pick due to ShowActiveFunc
	 * 		      changing
	 */
	int retval;

	VfDataset *ds = manager()->activeDataset();
	long act_fun  = ds->getActiveVelocityFunction();
	int i;

	/*
	 * Since Vector method setColor only redraws if the color
	 * changes, calling setColor for every vector is not
	 * inefficient.
	 */
	for (retval = i = 0; i < _numVectUsed; i++)
		if (((VaGvsFuncData *) _data[i])->drawLine()
		 &&  _vect[i]->setColor(getLineColor(i,
			_colors-> gvsActiveFuncColor(),
			_colors->gvsDefaultFuncColor())))
		{
			retval++;
		}
		else if ((check_picks == 1) && isDisplayed(i)
		      && (i != (int) act_fun))
		{
			int old_closest_pck, new_closest_pck;

			if (_old_act_pick_vel > 0.0F)
				old_closest_pck = ((VaGvsFuncData *)_data[i])->
					closestToVel(_old_act_pick_vel);
			else
				old_closest_pck = -1;

			long act_pck = ds->getActivePick(act_fun);

			if (act_pck != -1L)
			{
				float vel = ds->getOrdinate(act_fun, act_pck,
					VTNM);

				new_closest_pck = ((VaGvsFuncData *)_data[i])->
					closestToVel(vel);
			}
			else
			{
				new_closest_pck = -1;
			}

			if (old_closest_pck != new_closest_pck)
			{
				int indices[2];
				int nIndices = 0;

				if (old_closest_pck != -1)
					indices[nIndices++] = old_closest_pck+1;

				if (new_closest_pck != -1)
					indices[nIndices++] = new_closest_pck+1;

				if (nIndices)
					((VaGvsFuncData *) _data[i])->
						updatePicks(indices, nIndices);
			}
		}
		else if ((check_picks == 2) && isDisplayed(i)
		      && (i != (int) act_fun))
		{
			int closest_pck;

			long act_pck = ds->getActivePick(act_fun);

			if (act_pck != -1L)
			{
				float vel = ds->getOrdinate(act_fun, act_pck,
					VTNM);

				closest_pck = ((VaGvsFuncData *)_data[i])->
					closestToVel(vel);
			}
			else
			{
				closest_pck = -1;
			}

			if (closest_pck != -1)
			{
				int index = closest_pck + 1;

				((VaGvsFuncData *) _data[i])->
					updatePicks(&index, 1);
			}
		}

	return retval;
}

void VaGvsPickData::updateLineDrawing()
{
	for (int i = 0; i < _numVectUsed; i++)
		_vect[i]->setStyle(
			(((VaGvsFuncData *) _data[i])->drawLine())
				? _colors->gvsFuncLineStyle()
				: Vector::NoLine);
}

void VaGvsPickData::hideActivePick()
{
	assert(!vaIsHoldingVectors());

	int act_fun = (int) manager()->activeDataset()->
		getActiveVelocityFunction();

	if (act_fun != -1)
		((VaGvsFuncData *) _data[act_fun])->hideActivePick();
}

void VaGvsPickData::traceToXorY(float trace, int *is_x, float *value)
{
	assert(_sp->isPlotDisplayed() && (_sp->originalTraces() > 0));

	int fast, slow, ok;
	float fast_first, fast_last, slow_val;

	ok = getRange(is_x, &fast, &fast_first, &fast_last, &slow, &slow_val);
        //X and Y headers may not be set correctly
        if(!ok) return;

	*value = fast_first + (trace - 1.0F)
		/ (float) (_sp->originalTraces() - 1L)
		* (fast_last - fast_first);
}

float VaGvsPickData::XorYtoTrace(float x, float y, float nil)
{
	float retval;

	if (_sp->isPlotDisplayed() && (_sp->originalTraces() > 0))
	{
		int is_x, fast, slow, ok;
		float fast_first, fast_last, slow_val;

		ok = getRange(&is_x, &fast, &fast_first, &fast_last,
			      &slow, &slow_val);
                //X and Y headers may not be set correctly
                if(!ok) return 0.0F;

		float value;
		if (is_x)
			value = x;
		else
			value = y;

		if (value == nil)
			retval = nil;
		else
			retval = 1.0F + (value - fast_first)
				/ (fast_last - fast_first)
				* (float) (_sp->originalTraces() - 1L);
	}
	else
	{
		retval = nil;
	}

	return retval;
}


int VaGvsPickData::getRange(int *x_fast,
	int *fast, float *fast_first, float *fast_last,
	int *slow, float *slow_val)
{
        int ok = 1;
        SLErrorPop *errpop;
	VfDataset *ds = manager()->activeDataset();
	int nhx = ds->getNhx();
	int nhy = ds->getNhy();
        long offset = _sp->originalTraces() * _sp->getCurrentPanel();
	int num = (int) _sp->displayedTraces(_sp->getCurrentPanel());
	assert(num > 1);

/*
 * need to round them off
 *	float x1 = _sp->getHeaderFromTrace(  1, nhx);
 *	float y1 = _sp->getHeaderFromTrace(  1, nhy);
 *	float xn = _sp->getHeaderFromTrace(num, nhx);
 *	float yn = _sp->getHeaderFromTrace(num, nhy);
 */
	int x1 = (int) floor((double) _sp->getHeaderFromTrace(1+offset,   nhx)
                                                                        + 0.5);
	int y1 = (int) floor((double) _sp->getHeaderFromTrace(1+offset,   nhy)
                                                                        + 0.5);
	int xn = (int) floor((double) _sp->getHeaderFromTrace(num+offset, nhx)
                                                                        + 0.5);
	int yn = (int) floor((double) _sp->getHeaderFromTrace(num+offset, nhy) 
                                                                        + 0.5);

	switch (2 * (x1 == xn) + (y1 == yn))
	{
		case 0:
		case 3:
	/*
	 * I am making the assumption that one header varies
	 * across the gvs display and the other does not.
	 * This should handle either inlines or
	 * crosslines in a 3D survey.  If the user reads
	 * in multiple inlines or crosslines, the program
	 * will bomb here and I will have some work to do.
	 */

        /*
        fprintf(stderr, "\n\nProgram bug:  ");
 	fprintf(stderr,
	  "First check that you used the correct headers for your gvs.\n");
	fprintf(stderr,
		"If headers OK, please contact Bill Menger bill.menger@gmail.com\n\n\n");
        assert(0);
        */
                  
                  //This is an attempt to warn the user that one header
                  //needs to be constant and one needs to vary instead
                  //of aborting as was done in the above code. The
                  //_error_in_headers_shown is a flag to prevent this
                  //error from popping up more than once with the
                  //same file. MLS 06/2002

                  if(strcmp(_filename, _sp->filename()) &&
                     !_sp->fileHasNeverBeenPlotted())
                       _error_in_headers_shown = 0;

                  if(!_error_in_headers_shown)
                    {
                      errpop = new SLErrorPop(_sp->imageGraphic(), "Error",
                       "Please check your gvs/cvst headers.\n\
                       VA expects that one header remains constant and\n\
                       the other header vary. This does not appear to\n\
                       be the case in the gvs/cvst file you have selected.\n\
                       Results will be unpredictable if you continue.");
                     
                      _error_in_headers_shown = 1;
                      strcpy(_filename,_sp->filename());
                    }
                  return (ok = 0);                       
		break;


		case 1:
			*x_fast     = 1;
			*fast       = nhx;
			*fast_first = x1;
			*fast_last  = xn;
			*slow       = nhy;
			*slow_val   = y1;
			break;
		case 2:
			*x_fast     = 0;
			*fast       = nhy;
			*fast_first = y1;
			*fast_last  = yn;
			*slow       = nhx;
			*slow_val   = x1;
			break;
		default:
			assert(0);
	}

        return ok;
}

void VaGvsPickData::beforeChanges()
{
	assert(!vaIsHoldingVectors());

	_change_act_pick = _rem_ins_pick = 0;

	/*
	 * Get _old_act_pick_vel to update non-active line marker
	 * colors in updateLineColors.
	 */
	VfDataset *ds = manager()->activeDataset();
	long act_fun = ds->getActiveVelocityFunction();

	if (act_fun != -1L)
	{
		long act_pck = ds->getActivePick(act_fun);

		if (act_pck != -1L)
			_old_act_pick_vel = ds->getOrdinate(act_fun, act_pck,
				VTNM);
		else
			_old_act_pick_vel = -1.0F;
	}
	else
	{
		_old_act_pick_vel = -1.0F;
	}
}

void VaGvsPickData::afterChanges()
{
	assert(!vaIsHoldingVectors());

	int num_changes = updateLineColors(1);
	updateLineDrawing();

	if (_change_act_pick)
	{
		assert(num_changes == 0);
		assert(_old_act_func == (int) manager()->activeDataset()->
			getActiveVelocityFunction());

		int indices[2];
		int numIndices = 0;

		if (_old_act_pick != 0)
		{
			if (_rem_ins_pick)
			{
				_old_act_pick = adjustIndex(_old_act_pick,
					_pick_index, _pick_nrem, _pick_nins);

				if (_old_act_pick != -1)
					indices[numIndices++] = _old_act_pick;
			}
			else
			{
				indices[numIndices++] = _old_act_pick;
			}
		}

		int new_act_pick = (int) manager()->activeDataset()->
			getActivePick((long) _old_act_func) + 1;

		if (new_act_pick != 0)
		{
			if (_rem_ins_pick)
			{
				/*
				 * Only recolor if not redrawn by
				 * remove/insert
				 */
				if (!newIndexEffected(new_act_pick,
					_pick_index, _pick_nrem, _pick_nins))
				{
					indices[numIndices++] = new_act_pick;
				}
			}
			else
			{
				indices[numIndices++] = new_act_pick;
			}
		}

		if ((numIndices == 2) && (indices[0] == indices[1]))
			numIndices = 0;

		if (numIndices)
			((VaGvsFuncData *) _data[_old_act_func])->updatePicks(
				indices, numIndices);
	}
}

void VaGvsPickData::preTotalChanges(VfDataset *dataset)
{
	if (manager()->activeDataset() == dataset)
	{
		int numFunc = (int) dataset->numVelocityFunctions();
		assert(_numVectUsed == numFunc);

		if (numFunc > 1)
			vaHoldVectors();

		for (int i = 0; i < _numVectUsed; i++)
		{
			_vectors->remove(_vect[i]);
			delete           _data[i] ;
		}

		if (numFunc > 1)
			vaFlushVectors();

		_numVectUsed = 0;
	}
}

void VaGvsPickData::postTotalChanges(VfDataset *dataset)
{
	assert(!vaIsHoldingVectors());

	if (manager()->activeDataset() == dataset)
	{
		int numFunc = (int) dataset->numVelocityFunctions();
		assert(_numVectUsed == 0);

		checkAllocation(numFunc);

		for (int i = 0; i < _numVectUsed; i++)
		{
			_data[i] = new VaGvsFuncData(this, i);

			_vect[i] = _vectors->add(_data[i],
				getLineColor(i, _colors-> gvsActiveFuncColor(),
						_colors->gvsDefaultFuncColor()),
				_colors->gvsFuncWidth          (),
				False,
				(((VaGvsFuncData *) _data[i])->drawLine())
					? _colors->gvsFuncLineStyle()
					: Vector::NoLine,
				Vector::DataSpecifiedMarker      ,
				_colors->gvsFuncMarkerSize     (),
				_colors->gvsFuncMarkerLineWidth());

			_vect[i]->allowAltMarkerColors(True);
			_vect[i]->setAltMarkerColor(VaVectColors::DEF_COL,
				_colors->gvsDefaultPickColor());
			_vect[i]->setAltMarkerColor(VaVectColors::ACT_COL,
				_colors->gvsActivePickColor ());

			_vect[i]->makeVisible(vaIsHoldingVectors());
		}
	}
}


void VaGvsPickData::preNewActiveVelocityFunction(VfDataset * /*dataset*/)
{
	/* afterChanges will handle */
}

void VaGvsPickData::postNewActiveVelocityFunction(VfDataset * /*dataset*/)
{
	/* afterChanges will handle */
}

void VaGvsPickData::preModifyPicks(VfDataset *dataset, long ifun, int /*type*/,
	long ipick, long nrem)
{
	assert(manager()->activeDataset() == dataset);
	assert(dataset->getActiveVelocityFunction() == ifun);

	((VaGvsFuncData *) _data[ifun])->pointsRemoved((int) ipick + 1,
		(int) nrem);
}

void VaGvsPickData::postModifyPicks(VfDataset *dataset, long ifun, int /*type*/,
	long ipick, long nrem, long nins)
{
	assert(manager()->activeDataset() == dataset);
	assert(dataset->getActiveVelocityFunction() == ifun);

	((VaGvsFuncData *) _data[ifun])->pointsInserted((int) ipick + 1,
		(int) nins);

	/*
	 * For recoloring picks in afterChanges
	 */
	_rem_ins_pick = 1;
	_pick_index = (int) ipick + 1;
	_pick_nrem  = (int) nrem;
	_pick_nins  = (int) nins;
}

void VaGvsPickData::preRemoveInsertVelocityFunctions(VfDataset *dataset,
	long /*ifun*/, long /*nrem*/, long /*nins*/)
{
	assert(manager()->activeDataset() == dataset);
	assert(_numVectUsed == (int) dataset->numVelocityFunctions());

	/*
	 * Since all we need to know about old stuff is indices,
	 * everything can wait until postRemoveInsertVelocityFunctions.
	 */
}

void VaGvsPickData::postRemoveInsertVelocityFunctions(VfDataset *dataset,
	long ifun, long nrem, long nins)
{
	assert(manager()->activeDataset() == dataset);

	if (nrem > 1)
		vaHoldVectors();

	int num_move = (nrem == nins) ? 0 : _numVectUsed - (int) (ifun + nrem);
	assert(num_move >= 0);
	checkAllocation(_numVectUsed + (int) (nins - nrem));
	assert(_numVectUsed == (int) dataset->numVelocityFunctions());

	int i;

	/*
	 * Must reset ifun before you delete any vectors, or else
	 * you will have problems when vectors redraw because of
	 * deletions.  Not resetting ifun on data classes to be deleted;
	 * since we holdVectors if nrem > 1 this is not a problem.
	 */
	if (num_move)
	{
		for (	i = (int) (ifun + nrem);
			i < (int) (ifun + nrem) + num_move;
			i++)
		{
			((VaGvsFuncData *) _data[i])->setFunc(
				i + (int) (nins - nrem));
		}
	}

	for (i = (int) ifun; i < (int) (ifun + nrem); i++)
	{
		_vectors->remove(_vect[i]);
		delete           _data[i];
	}

	if (nrem > 1)
		vaFlushVectors();
	assert(!vaIsHoldingVectors());

	if (num_move)
	{
		memmove((void *) &_vect[ifun + nins],
			(void *) &_vect[ifun + nrem],
			(size_t) num_move * sizeof(Vector        *));

		memmove((void *) &_data[ifun + nins],
			(void *) &_data[ifun + nrem],
			(size_t) num_move * sizeof(VaGvsFuncData *));
	}

	for (i = (int) ifun; i < (int) (ifun + nins); i++)
	{
		_data[i] = new VaGvsFuncData(this, i);

		_vect[i] = _vectors->add(_data[i],
			getLineColor(i, _colors-> gvsActiveFuncColor(),
					_colors->gvsDefaultFuncColor()),
			_colors->gvsFuncWidth          (),
			False,
			(((VaGvsFuncData *) _data[i])->drawLine())
				? _colors->gvsFuncLineStyle()
				: Vector::NoLine,
			Vector::DataSpecifiedMarker      ,
			_colors->gvsFuncMarkerSize     (),
			_colors->gvsFuncMarkerLineWidth());

		_vect[i]->allowAltMarkerColors(True);
		_vect[i]->setAltMarkerColor(VaVectColors::DEF_COL,
			_colors->gvsDefaultPickColor());
		_vect[i]->setAltMarkerColor(VaVectColors::ACT_COL,
			_colors->gvsActivePickColor ());

		_vect[i]->makeVisible(nrem > 1);
	}
}

void VaGvsPickData::preChangeCoords(VfDataset *dataset, long ifun, long nchng)
{
	assert(manager()->activeDataset() == dataset);

	for (int i = (int) ifun; i < (int) (ifun + nchng); i++)
		((VaGvsFuncData *) _data[i])->
			pointsRemoved(0, _data[i]->getNumPts());
}

void VaGvsPickData::postChangeCoords(VfDataset *dataset, long ifun, long nchng)
{
	assert(manager()->activeDataset() == dataset);

	for (int i = (int) ifun; i < (int) (ifun + nchng); i++)
		((VaGvsFuncData *) _data[i])->
			pointsInserted(0, _data[i]->getNumPts());
}

void VaGvsPickData::preNewActiveDataset()
{
	preTotalChanges(manager()->activeDataset());
}

void VaGvsPickData::postNewActiveDataset()
{
	postTotalChanges(manager()->activeDataset());
}

void VaGvsPickData::preNewActivePicks(VfDataset *dataset, long ifun, long nchng)
{
	if (manager()->activeDataset() == dataset)
	{
		_old_act_func = (int) dataset->getActiveVelocityFunction();

		if (((long) _old_act_func >= ifun        )
		 && ((long) _old_act_func <  ifun + nchng))
		{
			_change_act_pick = 1;
			_old_act_pick = (int) dataset->getActivePick(
				(long) _old_act_func) + 1;
		}
	}
}

void VaGvsPickData::postNewActivePicks(VfDataset * /*dataset*/,
	long /*ifun*/, long /*nchng*/)
{
	/* do it in afterChanges */
}

void VaGvsPickData::preChangeBinTolerances()
{
	_picks->makeEditableInvisible();
}

void VaGvsPickData::postChangeBinTolerances()
{
	_picks->makeEditableVisible();
}



//SeisInform. This may help prevent the user from continuing with
//a gvs/cvst file that does not have at least on constant header.
//See the getRange method. MLS 06/2002
void VaGvsPickData::newPlot(SeisPlot *sp)
{
  if(sp == _sp)
    _error_in_headers_shown = 0;
}








#define FALLBACK "mouse*VA_GVS_PICK: BTN#1: Edit or Insert Pick, BTN#2: Delete Pick\\nShift-BTN#1: Activate or Insert Velocity Function, Shift-BTN#2: Delete Velocity Function"

VaGvsPicker::VaGvsPicker(PlotBase *plot, VfManager *manager,
	VaPicks *picks, VectorLinkedList *vectors)
	: VaPicker(plot, "", "VA_GVS_PICK", FALLBACK,
		manager, picks, vectors)
{
	/* just initializers */
}

VaGvsPicker::~VaGvsPicker()
{
	/* do nothing */
}

void VaGvsPicker::noModButtonOnePress(int x, int y)
{
	float pick_tol = _manager->utilities()->getPickingTolerance();

	float dist_mm;
	Vector *vector = _vectors->closest(x, y, getPlot(), &dist_mm);

	VfDataset *ds   = _manager->activeDataset();
	long       ifun = ds->getActiveVelocityFunction();

	if ((vector) && (dist_mm <= pick_tol)
	 && (((VaGvsPicks *) _picks)->getIndex(vector) == (int) ifun))
	{
		float xWC = getPlot()->xWC(x);
		float yWC = getPlot()->yWC(y);

		bracketTime(yWC);

		if (_use_t_min && (yWC < _t_min))
			yWC = _t_min;
		
		if (_use_t_max && (yWC > _t_max))
			yWC = _t_max;

		_start_frame=(int) ((SeisPlot *) getPlot())->getCurrentPanel();
		_max_frame   = (int) ((SeisPlot *) getPlot())->frames() - 1;
		_trace_number = ((VaGvsPicks *)_picks)->traceNumber((int)ifun);

		float vel = ((VaGvsPicks *) _picks)->getVelFromPick(xWC, yWC,
			_start_frame, _max_frame, _trace_number);

		if (_insert)
		{
			if (canEdit())
			{
				_picks->broadcastInsStrt(_index, vel, yWC);
			}
			else
			{
				doBeep();
				ignoreActions();
			}
		}
		else
		{
			/*
			 * Remember PickWatch is a PickBase.
			 * Do not want PickBase changing in middle
			 * of action.
			 */
			Bool watch_ok = PlotBase::watchOK();
			if (watch_ok)
				PlotBase::setWatchOK(False);

			if ((int) ds->getActivePick(ifun) != _index)
				ds->setActivePick(ifun, (long) _index);

			if (watch_ok)
				PlotBase::setWatchOK(True );

			if (canEdit())
			{
				_picks->broadcastRepStrt(_index, vel, yWC);
			}
			else
			{
				ignoreActions();
			}
		}
	}
	else
	{
		doBeep();
		ignoreActions();
	}
}

void VaGvsPicker::noModButtonOneMotion(int /*x1*/, int x2,
				       int /*y1*/, int y2)
{
	float xWC = getPlot()->xWC(x2);
	float yWC = getPlot()->yWC(y2);
	
	if (_use_t_min && (yWC < _t_min))
		yWC = _t_min;

	if (_use_t_max && (yWC > _t_max))
		yWC = _t_max;

	float vel = ((VaGvsPicks *) _picks)->getVelFromPick(xWC, yWC,
		_start_frame, _max_frame, _trace_number);

	if (_insert)
		_picks->broadcastInsDrag(_index, vel, yWC);
	else
		_picks->broadcastRepDrag(_index, vel, yWC);
}

void VaGvsPicker::noModButtonOneRelease(int /*x1*/, int x2,
					int /*y1*/, int y2)
{
	float xWC = getPlot()->xWC(x2);
	float yWC = getPlot()->yWC(y2);
	
	if (_use_t_min && (yWC < _t_min))
		yWC = _t_min;

	if (_use_t_max && (yWC > _t_max))
		yWC = _t_max;

	float vel = ((VaGvsPicks *) _picks)->getVelFromPick(xWC, yWC,
		_start_frame, _max_frame, _trace_number);

	VfDataset *ds = _manager->activeDataset();
	long ifun = ds->getActiveVelocityFunction();

	if (_insert)
	{
		_picks->broadcastInsDone(   _index, vel, yWC);
		ds->insertPick(ifun, (long) _index, yWC, vel, VTNM);
	}
	else
	{
		_picks->broadcastRepDone(             _index, vel, yWC);
		ds->replacePick         (ifun, (long) _index, yWC, vel, VTNM);
	}
}

void VaGvsPicker::noModButtonTwoPress(int x, int y)
{
	float pick_tol = _manager->utilities()->getPickingTolerance();

	float dist_mm;
	Vector *vector = _vectors->closest(x, y, getPlot(), &dist_mm);

	VfDataset *ds   = _manager->activeDataset();
	long       ifun = ds->getActiveVelocityFunction();

	if ((vector) && (dist_mm <= pick_tol)
	 && (((VaGvsPicks *) _picks)->getIndex(vector) == (int) ifun)
	 && canEdit())
	{
		/*
		 * Use bracketTime to set _index and _insert
		 */
		float yWC = getPlot()->yWC(y);
		bracketTime(yWC);

		if (_insert)
		{
			doBeep();
			ignoreActions();
		}
	}
	else
	{
		doBeep();
		ignoreActions();
	}
}

void VaGvsPicker::noModButtonTwoMotion(int /*x1*/, int /*x2*/,
				       int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void VaGvsPicker::noModButtonTwoRelease(int /*x1*/, int /*x2*/,
					int /*y1*/, int /*y2*/)
{
	VfDataset *ds = _manager->activeDataset();
	long ifun = ds->getActiveVelocityFunction();
	assert(ifun != -1);
	ds->removePick(ifun, (long) _index);
}

void VaGvsPicker::shiftButtonOnePress(int x, int y)
{
	float dist_mm;
	Vector *vector = _vectors->closest(x, y, getPlot(), &dist_mm);

	if (vector)
	{
		float pick_tol = _manager->utilities()->getPickingTolerance();
		int new_ifun = ((VaGvsPicks *) _picks)->getIndex(vector); 

		/*
		 * If within picking tolerance, select it.
		 */
		if (dist_mm <= pick_tol)
		{
			VfDataset *ds = _manager->activeDataset();
			int cur_ifun = (int) ds->getActiveVelocityFunction();
			_insert = 0;
			_index  = (int) new_ifun;

			if (cur_ifun == new_ifun)
				ignoreActions();  /* already there */
		}
		else
		{
			_insert = 1;
			_index  = new_ifun;
		}
	}
	else
	{
		_insert = 1;
		_index  = 0;
	}

	if (_insert && !canEdit())
	{
		doBeep();
		ignoreActions();
	}
}

void VaGvsPicker::shiftButtonOneMotion(int /*x1*/, int /*x2*/,
				       int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void VaGvsPicker::shiftButtonOneRelease(int   x1  , int /*x2*/,
					int /*y1*/, int /*y2*/)
{
	/*
	 * _insert = 1 will not necessarily insert,
	 * calls insertOrSelectFunc which uses the bin information to
	 * determine whether to insert or select.
	 */
	if (_insert)
		((VaGvsPicks *) _picks)->
			insertOrSelectFunc(_index, getPlot()->xWC(x1));
	else
		_manager->activeDataset()->
			setActiveVelocityFunction((long) _index);
}

void VaGvsPicker::shiftButtonTwoPress(int x, int y)
{
	float pick_tol = _manager->utilities()->getPickingTolerance();

	float dist_mm;
	Vector *vector = _vectors->closest(x, y, getPlot(), &dist_mm);

	if ((vector) && (dist_mm <= pick_tol) && canEdit())
	{
		_index = (int) ((VaGvsPicks *) _picks)->getIndex(vector); 
	}
	else
	{
		doBeep();
		ignoreActions();
	}
}

void VaGvsPicker::shiftButtonTwoMotion(int /*x1*/, int /*x2*/,
				       int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void VaGvsPicker::shiftButtonTwoRelease(int /*x1*/, int /*x2*/,
					int /*y1*/, int /*y2*/)
{
	_manager->activeDataset()->removeVelocityFunction((long) _index);
}

VaGvsPicks::VaGvsPicks(class VfManager *manager, class VfHorizons *horizons,
	class VaPlot *plot, SeisPlot *sp, class VaVectColors *colors)
	: VaPicks(manager, horizons, plot, sp, colors, 1)
{
	_data = new VaGvsPickData(_manager, this, sp, _editable_vectors,
		_colors);

	_va_gvs_horizons_data = new VaGvsHorizonsData(manager, _colors,
		(VaGvsPlot *) _plot, this);

	_va_horizons          = new VaHorizons       (manager, horizons,
		_va_gvs_horizons_data, _colors,
		_constant_vectors[0], _editable_vectors);

	/*
	 * Add coupled cursor after all vector linked lists since
	 * if cursor is in rbn mode, it must repair exposes last.
	 */
	_xh_trans = new GvsCrossHairTranslator(this);

	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());
}

VaGvsPicks::~VaGvsPicks()
{
	/*
	 * _data is deleted in base class destructor.
	 */
	if (_picker)
		delete _picker;

	_cross_hairs.remove(top());

	delete _xh_trans;

	delete _va_horizons         ;
	delete _va_gvs_horizons_data;
}

void VaGvsPicks::init(SeisPlot *sp)
{
	_picker = new VaGvsPicker(sp, _manager, this, _editable_vectors);
}

void VaGvsPicks::insStrt(int /*index*/, float x, float y)
{
	((VaGvsPickData *) _data)->hideActivePick();

	strtRbn(x, y);
}

void VaGvsPicks::insDrag(int /*index*/, float x, float y)
{
	dragRbn(x, y);
}

void VaGvsPicks::insDone(int /*index*/, float x, float y)
{
	doneRbn(x, y);
}

void VaGvsPicks::repStrt(int /*index*/, float x, float y)
{
	strtRbn(x, y);
}

void VaGvsPicks::repDrag(int /*index*/, float x, float y)
{
	dragRbn(x, y);
}

void VaGvsPicks::repDone(int /*index*/, float x, float y)
{
	doneRbn(x, y);
}

void VaGvsPicks::setShowFunc(int which, int set)
{
	if (set)
		assert(!_show_func[which]);
	else
		assert( _show_func[which]);

	_show_func[which] = set;

	((VaGvsPickData *) _data)->updateLineColors(0);
}

void VaGvsPicks::setSelectType(int /*set*/)
{
	((VaGvsPickData *) _data)->updateLineColors(0);
}

void VaGvsPicks::setEnableShowFuncs(int set)
{
	if (set)
	{
		((VaGvsPickData *) _data)->updateLineColors (0);
		((VaGvsPickData *) _data)->updateLineDrawing( );
	}
	else
	{
		((VaGvsPickData *) _data)->updateLineDrawing( );
		((VaGvsPickData *) _data)->updateLineColors (0);
	}
}

void VaGvsPicks::setShowActiveFunc(int /*set*/)
{
	((VaGvsPickData *) _data)->updateLineColors (2);
	((VaGvsPickData *) _data)->updateLineDrawing( );
}

char *VaGvsPicks::getClassName()
{
	static char *retval = VA_GVS_PICKS;

	return retval;
}

int VaGvsPicks::getIndex(Vector *vector)
{
	return ((VaGvsPickData *) _data)->getIndex(vector);
}

void VaGvsPicks::strtRbn(float v, float t)
{
	VfDataset *ds = _manager->activeDataset();
	long ifun     = ds->getActiveVelocityFunction();

	if ((ifun > -1) && ((VaGvsPickData *) _data)->isDisplayed((int) ifun))
	{
		float xloc = ds->getXloc(ifun);
		float yloc = ds->getYloc(ifun);
		float tn   = ((VaGvsPickData *) _data)->traceNumber((int) ifun);

		_rbn_data = new VectData(1, &tn, &t);

		_rbn_vector = _rbn_vectors->add(_rbn_data,
			_colors->gvsRbnColor          (),
			_colors->gvsFuncWidth         (),
			False                           ,
			_colors->gvsFuncLineStyle     (),
			_colors->gvsRbnMarker         (),
			_colors->gvsRbnMarkerSize     (),
			_colors->gvsRbnMarkerLineWidth());
			
		_rbn_vector->makeVisible();

		((VaGvsPlot *) _plot)->changePanel(xloc, yloc, t, v);
	}
	else
	{
		_rbn_data = (VectData *) NULL;
	}
}

void VaGvsPicks::dragRbn(float v, float t)
{
	if (_rbn_data)
	{
		VfDataset *ds = _manager->activeDataset();
		long ifun     = ds->getActiveVelocityFunction();

		float xloc = ds->getXloc(ifun);
		float yloc = ds->getYloc(ifun);
		float tn   = ((VaGvsPickData *) _data)->traceNumber((int) ifun);

		_rbn_data->replace(0, 1, &tn, &t);

		((VaGvsPlot *) _plot)->changePanel(xloc, yloc, t, v);
	}
}

void VaGvsPicks::doneRbn(float /*v*/, float /*t*/)
{
	if (_rbn_data)
	{
		_rbn_vectors->remove(_rbn_vector);
		delete _rbn_data;
	}
}

int VaGvsPicks::traceNumber(int ifun)
{
	return ((VaGvsPickData *) _data)->traceNumber(ifun);
}

#define TRACES_PER_FRAME 2.0F

float VaGvsPicks::getVelFromPick(float x, float y,
	int start_frame, int max_frame, int trace_number)
{
	int frame = start_frame + (int) floor((double)
		((x - (float) trace_number) / TRACES_PER_FRAME + 0.5F));

	frame = (frame <         0) ?         0 : frame;
	frame = (frame > max_frame) ? max_frame : frame;

	return ((VaGvsPlot *) _plot)->getVelocityFromPanel((long) frame, y, 
                                                           trace_number);
}

void VaGvsPicks::insertOrSelectFunc(int index, float xtrace)
{
	((VaGvsPickData *) _data)->insertOrSelectFunc(index, xtrace);
}

int VaGvsPicks::withinDisplay(long ifun, int /*use_getPlottedLineType*/)
{
	return ((VaGvsPickData *) _data)->isDisplayed((int) ifun);
}

void VaGvsPicks::traceToXorY(float trace, int *is_x, float *value)
{
	((VaGvsPickData *) _data)->traceToXorY(trace, is_x, value);
}

float VaGvsPicks::XorYtoTrace(float x, float y, float nil)
{
	return ((VaGvsPickData *) _data)->XorYtoTrace(x, y, nil);
}

VaPicker *VaGvsPicks::newSeparateWindow(SeisPlot *sp)
{
	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());

	return new VaGvsPicker(sp, _manager, this, _editable_vectors);
}

VaGvsHorizonsData::VaGvsHorizonsData(VfManager *vf_manager,
	VaVectColors *colors, VaGvsPlot *va_gvs_plot, VaGvsPicks *va_gvs_picks)
	: VaHorizonsData(vf_manager, colors),
	  _va_gvs_plot(va_gvs_plot), _va_gvs_picks(va_gvs_picks)
{
	/* just initializers */
}

VaGvsHorizonsData::~VaGvsHorizonsData()
{
	/* do nothing */
}

int VaGvsHorizonsData::getNumPts(long id)
{
	int retval;
	SeisPlot *sp = _va_gvs_plot->SP();
        long offset = sp->originalTraces() * sp->getCurrentPanel();
	int num = (int) sp->displayedTraces(sp->getCurrentPanel());

	if (sp->isPlotDisplayed() && (sp->originalTraces() > 1L))
	{
		VfDataset *ds = _vf_manager->activeDataset();
		int nhx = ds->getNhx();
		int nhy = ds->getNhy();
		int num = (int) sp->displayedTraces(sp->getCurrentPanel());

		int x1 = (int) floor((double)
			sp->getHeaderFromTrace(1+offset, nhx) + 0.5);
		int y1 = (int) floor((double)
			sp->getHeaderFromTrace(1+offset, nhy) + 0.5);
		int xn = (int) floor((double)
			sp->getHeaderFromTrace(num+offset, nhx) + 0.5);
		int yn = (int) floor((double)
			sp->getHeaderFromTrace(num+offset, nhy) + 0.5);

		SortedHorizon *sh = _va_horizons_manager->getSortedHorizon(id);
		int bin;

		VfUtilities *ut = _vf_manager->utilities();

		switch (2 * (x1 == xn) + (y1 == yn))
		{
			case 0:
			case 3:
				assert(0);
				break;
			case 1:
				bin = (int) ut->ybinNumber(
					sp->getHeaderFromTrace(1+offset, nhy));

				retval = sh->getInlineIndex(bin,
					&_starting_index);

				_is_inline = 1;
				break;
			case 2:
				bin = (int) ut->xbinNumber(
					sp->getHeaderFromTrace(1+offset, nhx));
//
// Original code had sp->getHeaderFromTrace(1, nhy)) with
// sp->getHeaderFromTrace(1, nhx)) commented out.  Using nhx seems logic, but
// the original form suggests that I had tried this, it hadn't worked, so I
// commented it out and used nhy.  On 12jul00 Greg Loumos reported trouble
// displaying horizons on a gvs crossline and I had to go back to nhx to get
// it to work.  Perhaps what we mean by a crossline is not altogether standard??
// ehs - 13jul00
//					sp->getHeaderFromTrace(1, nhy));

				retval = sh->getCrosslineIndex(bin,
					&_starting_index);

				_is_inline = 0;
				break;
			default:
				assert(0);
		}
	}
	else
	{
		retval = 0;
	}

	return retval;
}

float VaGvsHorizonsData::getX(int i, long id)
{
	static const float nil = ZNIL;

	float retval;
	float x, y;

	SortedHorizon *sh = _va_horizons_manager->getSortedHorizon(id);
//
// Original code used sh->getSorted(SortedHorizon::   INLINE) whether _is_line
// or not.  Either form works with a 2D horizon.  We need a 3D horizon and a
// crossline to test which is right.
// ehs - 13jul00
//
	if (_is_inline)
	{
		x = (sh->getSorted(SortedHorizon::   INLINE))
			[_starting_index + i]->x;
		y = (sh->getSorted(SortedHorizon::   INLINE))
			[_starting_index + i]->y;
	}
	else
	{
		x = (sh->getSorted(SortedHorizon::CROSSLINE))
			[_starting_index + i]->x;
		y = (sh->getSorted(SortedHorizon::CROSSLINE))
			[_starting_index + i]->y;
	}

	retval = _va_gvs_picks->XorYtoTrace(x, y, nil);
	assert(retval != nil);

	return retval;
}

float VaGvsHorizonsData::getY(int i, long id)
{
	float retval;

	SortedHorizon *sh = _va_horizons_manager->getSortedHorizon(id);

	if (_is_inline)
	{
		retval = (sh->getSorted(SortedHorizon::   INLINE))
			[_starting_index + i]->t;
	}
	else
	{
		retval = (sh->getSorted(SortedHorizon::CROSSLINE))
			[_starting_index + i]->t;
	}

	return retval;
}

int VaGvsHorizonsData::getMarkerType(int /*i*/, long /*id*/)
{
	int retval;

	switch (_show_horizon_markers)
	{
		case VaVectColors::OVER_LINE:
			retval = (int) Vector::NoMarker;
			break;
		case VaVectColors::OVER_BOTH:
		case VaVectColors::OVER_MARK:
			retval = (int) Vector::FilledSquareMarker;
			break;
		default:
			assert(0);
	}

	return retval;
}

int VaGvsHorizonsData::getLineStyle(long /*id*/)
{
	int retval;

	switch (_show_horizon_markers)
	{
		case VaVectColors::OVER_LINE:
		case VaVectColors::OVER_BOTH:
			retval = (int) Vector::SolidLine;
			break;
		case VaVectColors::OVER_MARK:
			retval = (int) Vector::NoLine;
			break;
		default:
			assert(0);
	}

	return retval;
}
