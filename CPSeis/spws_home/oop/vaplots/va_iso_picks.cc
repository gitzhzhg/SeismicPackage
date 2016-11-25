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
// $Id: va_iso_picks.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_iso_picks.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_xh_trans.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "vect/vector.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"

#include <assert.h>

VaIsoPickData::VaIsoPickData(VfManager *manager, VaPicks *picks,
	SeisPlot *sp, VaVectColors *colors)
	: VaPickData(manager, picks), _sp(sp), _colors(colors),
	  _hiding_active_pick(0)
{
	/* just initializers */
}

VaIsoPickData::~VaIsoPickData()
{
	/* do nothing */
}

/*
 * Only check if getActiveVelocityFunction == -1 in getNumPts.
 * getActiveVelocityFunction is used in getX, getY, and getAltMarkerColor,
 * but these methods are called by Vector only if getNumPts > 0.
 */
int VaIsoPickData::getNumPts(long /*id*/)
{
	int retval;

	VfDataset *ds = manager()->activeDataset();
	long ifun = ds->getActiveVelocityFunction();

	if ((ifun >= 0) && _sp->isPlotDisplayed()
		&& (_sp->originalTraces() > 0L))
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

		if (line_type == VaIsoPlot::TIMESLICE)
		{
			if (_colors->getIsoTSOverlay())
				retval = (int) ds->numVelocityFunctions();
			else
				retval = 0;
		}
		else if (isDisplayed(line_type, ds, ifun)
		      && ((VaIsoPicks *) _picks)->getShowActiveFunc())
		{
			retval = (int) ds->numPicks(ifun) + 2;
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

	_numPts = retval;	/* for getY and getMarkerType */
	return retval;
}

/*
 * getX assumes that if the active velocity function is in the range of
 * the display for inline and crossline displays.  Others numPts would
 * have returned 0 and getX would never get called.
 */
float VaIsoPickData::getX(int i, long /*id*/)
{
	float retval;

	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();
	VfDataset *ds = manager()->activeDataset();
	long ifun = ds->getActiveVelocityFunction();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			retval = ds->getXloc((long) i);
			break;
		case VaIsoPlot::INLINE:
			retval = ds->getXloc(ifun);
			break;
		case VaIsoPlot::CROSSLINE:
			retval = ds->getYloc(ifun);
			break;
		default:
			assert(0);
	}

	return retval;
}

/*
 * getY assumes that if the active velocity function is in the range of
 * the display for inline and crossline displays.  Others numPts would
 * have returned 0 and getY would never get called.
 */
float VaIsoPickData::getY(int i, long /*id*/)
{
	float retval;

	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();
	VfDataset *ds = manager()->activeDataset();
	long ifun = ds->getActiveVelocityFunction();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			retval = ds->getYloc((long) i);
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			if      (i ==  0)
				retval = _sp->memTmin();
			else if (i == _numPts - 1)
				retval = _sp->memTmax();
			else
				retval = ds->getAbscissa(
					ifun, (long) (i - 1),
					((VaIsoPlot *) _picks->getPlot())->
					getPlottedVelocityType());
			break;
		default:
			assert(0);
	}

	return retval;
}

int VaIsoPickData::getMarkerType(int i, long /*id*/)
{
	int retval;

	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			retval = (int) _colors->isoGridMarker();
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			if      (i ==  0)
				retval = Vector::NoMarker;
			else if (i == _numPts - 1)
				retval = Vector::NoMarker;
			else
				retval = _colors->isoProfileMarker();
			break;
		default:
			assert(0);
	}

	return retval;
}

int VaIsoPickData::getAltMarkerColor(int i, long /*id*/)
{
	int retval;

	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();
	VfDataset *ds = manager()->activeDataset();
	long ifun = ds->getActiveVelocityFunction();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			if ((i == (int) ifun)
			 && ((VaIsoPicks *) _picks)->getShowActiveFunc())
				retval = 4;
			else
				retval = 3;
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			/*
			 * Don't bother checking getShowActiveFunc,
			 * if 0, numPts will be 0 for inline and crossline.
			 */
			if ((i - 1 == (int) ds->getActivePick(ifun))
			 && !_hiding_active_pick)
			{
				retval = 2;
			}
			else
			{
				retval = 1;
			}
			break;
		default:
			assert(0);
	}

	return retval;
}

int VaIsoPickData::getLineStyle(long /*id*/)
{
	int retval;

	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			retval = (int) Vector::NoLine;
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			retval = (int) _colors->isoProfileLineStyle();
			break;
		default:
			assert(0);
	}

	return retval;
}

int VaIsoPickData::isDisplayed(int line_type, VfDataset *ds, long ifun)
{
	int retval;

	if (_sp->isPlotDisplayed() && (_sp->originalTraces() > 0L))
	{
		int trace_offset = (int)
			(_sp->currentFrame() * _sp->originalTraces());
		float (VfDataset  ::*getloc   )(long ) const;
		long  (VfUtilities::*binNumber)(float) const;
		float loc;

		switch (line_type)
		{
			case VaIsoPlot::INLINE:
				getloc    = &VfDataset  ::getYloc   ;
				binNumber = &VfUtilities::ybinNumber;
				loc = _sp->getHeaderFromTrace(
					trace_offset + 1, ds->getNhy());
				break;
			case VaIsoPlot::CROSSLINE:
				getloc    = &VfDataset  ::getXloc   ;
				binNumber = &VfUtilities::xbinNumber;
				loc = _sp->getHeaderFromTrace(
//					trace_offset + 1, ds->getNhx());
					trace_offset + 1, ds->getNhy());
				break;
			default:
				assert(0);
		}

		VfUtilities *ut = manager()->utilities();

		retval = (ut->*binNumber)(loc)
		      == (ut->*binNumber)((ds->*getloc)(ifun));
	}
	else
	{
		retval = 0;
	}

	return retval;
}

void VaIsoPickData::hideActivePick()
{
	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();

	if (line_type != VaIsoPlot::TIMESLICE)
	{
		VfDataset *ds = manager()->activeDataset();
		long act_fun  = ds->getActiveVelocityFunction();

		if (act_fun != -1)
		{
			int act_pck = (int) ds->getActivePick(act_fun) + 1;

			if (act_pck != 0)
			{
				_hiding_active_pick = 1;
				modAttributesByIndices(&act_pck, 1);
				_hiding_active_pick = 0;
			}
		}
	}

}

void VaIsoPickData::beforeChanges()
{
	_need_mod_done = _change_act_pick = _change_act_func
		       = _rem_ins_pick    = _rem_ins_func    = 0;
}

void VaIsoPickData::afterChanges()
{
	if (_change_act_func)
	{
		/* one way or the other */
		assert(!_change_act_pick && !_rem_ins_pick);

		assert(((VaIsoPlot *) _picks->getPlot())->getPlottedLineType()
			== VaIsoPlot::TIMESLICE);

		int indices[2];
		int numIndices = 0;

		if (_old_act_func != -1)
		{
			if (_rem_ins_func)
				_old_act_func = adjustIndex(_old_act_func,
					_func_index, _func_nrem, _func_nins);

			if (_old_act_func != -1)
				indices[numIndices++] = _old_act_func;
		}

		int new_act_func = (int) manager()->activeDataset()->
			getActiveVelocityFunction();

		if (new_act_func != -1)
		{
			if (_rem_ins_func)
			{
				/*
				 * Only recolor if not redrawn by
				 * remove/insert
				 */
				if (!newIndexEffected(new_act_func,
					_func_index, _func_nrem, _func_nins))
				{
					indices[numIndices++] = new_act_func;
				}
			}
			else
			{
				indices[numIndices++] = new_act_func;
			}
		}

		if ((numIndices == 2) && (indices[0] == indices[1]))
			numIndices = 0;

		if (numIndices)
			modAttributesByIndices(indices, numIndices);
	}
	else if (_change_act_pick)
	{
		assert(!_rem_ins_func);

		assert(((VaIsoPlot *) _picks->getPlot())->getPlottedLineType()
			!= VaIsoPlot::TIMESLICE);

		assert(_old_act_func == (int) manager()->activeDataset()->
			getActiveVelocityFunction());

		int indices[2];
		int numIndices = 0;

		if (_old_act_pick != 0)
		{
			if (_rem_ins_pick)
				_old_act_pick = adjustIndex(_old_act_pick,
					_pick_index, _pick_nrem, _pick_nins);

			if (_old_act_pick != -1)
				indices[numIndices++] = _old_act_pick;
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
			modAttributesByIndices(indices, numIndices);
	}
}

void VaIsoPickData::preTotalChanges(VfDataset *dataset)
{
	if (manager()->activeDataset() == dataset)
		modIndicesBefore(0, getNumPts());
}

void VaIsoPickData::postTotalChanges(VfDataset *dataset)
{
	if (manager()->activeDataset() == dataset)
	{
		modIndicesAfter(0, getNumPts());
		modDone();
	}
}

void VaIsoPickData::preNewActiveVelocityFunction(VfDataset *dataset)
{
	if (manager()->activeDataset() == dataset)
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

		switch (line_type)
		{
			/*
			 * For timeslice, new active function only
			 * needs recoloring.
			 */
			case VaIsoPlot::TIMESLICE:
				_change_act_func = 1;
				_old_act_func = (int) dataset->
					getActiveVelocityFunction();
				break;
			/*
			 * For inline or crossline, new active function
			 * needs remove/insert.
			 */
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				preTotalChanges(dataset);
				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::postNewActiveVelocityFunction(VfDataset *dataset)
{
	if (manager()->activeDataset() == dataset)
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

		switch (line_type)
		{
			/*
			 * For timeslice, new active function only
			 * needs recoloring.
			 */
			case VaIsoPlot::TIMESLICE:
				/* do it in afterChanges */
				break;
			/*
			 * For inline or crossline, new active function
			 * needs remove/insert.
			 */
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				postTotalChanges(dataset);
				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::preNewActivePicks(VfDataset *dataset, long ifun,
	long nchng)
{
	if (manager()->activeDataset() == dataset)
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				/*
				 * do nothing
				 * timeslice does not show active pick
				 */
				break;
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				_old_act_func = (int) dataset->
					getActiveVelocityFunction();
				if (isDisplayed(line_type, dataset,
				     (long) _old_act_func)
				 && ((long) _old_act_func >= ifun        )
				 && ((long) _old_act_func <  ifun + nchng))
				{
					_change_act_pick = 1;
					_old_act_pick = (int) dataset->
						getActivePick(
						(long) _old_act_func) + 1;
				}
				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::postNewActivePicks(VfDataset *dataset, long /*ifun*/,
	long /*nchng*/)
{
	if (manager()->activeDataset() == dataset)
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				/*
				 * do nothing
				 * timeslice does not show active pick
				 */
				break;
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				/* do it in afterChanges */
				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::preModifyPicks(VfDataset *dataset,
	long ifun, int /*type*/, long ipick, long nrem)
{
	if (manager()->activeDataset() == dataset)
	{
		assert(dataset->getActiveVelocityFunction() == ifun);

		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				/*
				 * do nothing
				 * timeslice does not show picks
				 */
				break;
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				if (isDisplayed(line_type, dataset, ifun))
				{
					modIndicesBefore((int) ipick + 1,
						(int) nrem);

					_need_mod_done = 1;
				}
				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::postModifyPicks(VfDataset *dataset,
	long ifun, int /*type*/, long ipick, long nrem, long nins)
{
	if (manager()->activeDataset() == dataset)
	{
		assert(dataset->getActiveVelocityFunction() == ifun);

		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				/*
				 * do nothing
				 * timeslice does not show picks
				 */
				break;
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				if (isDisplayed(line_type, dataset, ifun))
				{
					/*
					 * isDisplayed should not change
					 * when modifying picks.
					 */
					assert(_need_mod_done);

					modIndicesAfter((int) ipick + 1,
						(int) nins);

					modDone();

					/*
					 * For recoloring in afterChanges
					 */
					_rem_ins_pick = 1;
					_pick_index = (int) ipick + 1;
					_pick_nrem  = (int) nrem;
					_pick_nins  = (int) nins;
				}
				else
				{
					assert(!_need_mod_done);
				}

				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::preRemoveInsertVelocityFunctions(VfDataset *dataset,
	long ifun, long nrem, long /*nins*/)
{
	if (manager()->activeDataset() == dataset)
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				modIndicesBefore((int) ifun, (int) nrem);
				break;
			/*
			 * For inline or crossline,
			 * preNewActiveVelocityFunction will
			 * handle anything that needs to be done.
			 */
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				/* do nothing */
				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::postRemoveInsertVelocityFunctions(VfDataset *dataset,
	long ifun, long nrem, long nins)
{
	if (manager()->activeDataset() == dataset)
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				modIndicesAfter((int) ifun, (int) nins);
				modDone();

				/*
				 * For recoloring in afterChanges
				 */
				_rem_ins_func = 1;
				_func_index = (int) ifun;
				_func_nrem  = (int) nrem;
				_func_nins  = (int) nins;

				break;
			/*
			 * For inline or crossline,
			 * postNewActiveVelocityFunction will
			 * handle anything that needs to be done.
			 */
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				/* do nothing */
				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::preChangeCoords(VfDataset *dataset, long ifun, long nchng)
{
	if (manager()->activeDataset() == dataset)
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();
		long act_func;

		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				modIndicesBefore((int) ifun, (int) nchng);
				break;
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				act_func = (int)
					dataset->getActiveVelocityFunction();
				/*
				 * If I did not check isDisplayed, getNumPts
				 * would be 0 anyway for not displayed.
				 * But this is more efficient.
				 */
				if (isDisplayed(line_type, dataset, act_func)
				 && (act_func >= ifun        )
				 && (act_func <  ifun + nchng))
				{
					modIndicesBefore(0, getNumPts());

					_need_mod_done = 1;
				}
				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::postChangeCoords(VfDataset *dataset, long ifun, long nchng)
{
	if (manager()->activeDataset() == dataset)
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();
		long act_func;

		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				modIndicesAfter((int) ifun, (int) nchng);
				modDone();
				break;
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				act_func = (int)
					dataset->getActiveVelocityFunction();
				if (isDisplayed(line_type, dataset, act_func)
				 && (act_func >= ifun        )
				 && (act_func <  ifun + nchng))
				{
					modIndicesAfter(0, getNumPts());

					_need_mod_done = 1;
				}

				if (_need_mod_done)
					modDone();

				break;
			default:
				assert(0);
		}
	}
}

void VaIsoPickData::preNewActiveDataset()
{
	preTotalChanges(manager()->activeDataset());
}

void VaIsoPickData::postNewActiveDataset()
{
	postTotalChanges(manager()->activeDataset());
}

void VaIsoPickData::preChangeBinTolerances()
{
	int line_type = ((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			/* do nothing */
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			_picks->makeEditableInvisible();
			break;
		default:
			assert(0);
	}
}

void VaIsoPickData::postChangeBinTolerances()
{
	int line_type = ((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			/* do nothing */
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			_picks->makeEditableVisible();
			break;
		default:
			assert(0);
	}
}

void VaIsoPickData::preChangeShowActiveFunc()
{
	int line_type = ((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			/* do nothing */
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			modIndicesBefore(0, getNumPts());
			break;
		default:
			assert(0);
	}
}

void VaIsoPickData::postChangeShowActiveFunc()
{
	int line_type = ((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();
	int act_func;

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			act_func = (int) manager()->activeDataset()->
				getActiveVelocityFunction();
			if (act_func != -1)
				modAttributesByIndices(&act_func, 1);
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			modIndicesAfter(0, getNumPts());
			modDone();
			break;
		default:
			assert(0);
	}
}

#define FALLBACK "mouse*VA_ISO_PICK: BTN#1: Edit or Insert Pick, BTN#2: Delete Pick\\nShift-BTN#1: Activate or Insert Velocity Function, Shift-BTN#2: Delete Velocity Function, Control-BTN#1: Edit or Insert Pick modifying velocity"

VaIsoPicker::VaIsoPicker(PlotBase *plot, VfManager *manager,
	VaPicks *picks, VectorLinkedList *vectors)
	: VaPicker(plot, "", "VA_ISO_PICK", FALLBACK,
		manager, picks, vectors)
{
	/* just initializers */
}

VaIsoPicker::~VaIsoPicker()
{
	/* do nothing */
}

void VaIsoPicker::noModButtonOnePress(int x, int y)
{
	if (initPick(x, y))
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();
		bracketTime(_t, line_type != VaIsoPlot::TIMESLICE,
			((VaIsoPlot *) _picks->getPlot())->
				getPlottedVelocityType());

		if (_use_t_min && (_t < _t_min))
			_t = _t_min;
		
		if (_use_t_max && (_t > _t_max))
			_t = _t_max;

		_v = ((SeisPlot *) getPlot())->getZvalueFromPixelXY(x, y);

		if (_insert)
		{
			if (canEdit())
			{
				_picks->broadcastInsStrt(_index, _v, _t);
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

			VfDataset *ds = _manager->activeDataset();
			long act_fun = ds->getActiveVelocityFunction();

			if ((int) ds->getActivePick(act_fun) != _index)
				ds->setActivePick(act_fun, (long) _index);

			if (watch_ok)
				PlotBase::setWatchOK(True );

			if (canEdit())
				_picks->broadcastRepStrt(_index, _v, _t);
			else
				ignoreActions();
		}
	}
	else
	{
		doBeep();
		ignoreActions();
	}
}

void VaIsoPicker::noModButtonOneMotion(int /*x1*/, int /*x2*/,
				       int /*y1*/, int   y2  )
{
	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			/* do nothing */
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			_t = getPlot()->yWC(y2);

			if (_use_t_min && (_t < _t_min))
				_t = _t_min;
			
			if (_use_t_max && (_t > _t_max))
				_t = _t_max;

			if (_insert)
				_picks->broadcastInsDrag(_index, _v, _t);
			else
				_picks->broadcastRepDrag(_index, _v, _t);
			break;
		default:
			assert(0);
	}
}

void VaIsoPicker::noModButtonOneRelease(int /*x1*/, int /*x2*/,
					int /*y1*/, int   y2  )
{
	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			/* do nothing */
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			_t = getPlot()->yWC(y2);

			if (_use_t_min && (_t < _t_min))
				_t = _t_min;
			
			if (_use_t_max && (_t > _t_max))
				_t = _t_max;

			break;
		default:
			assert(0);
	}

	VfDataset *ds = _manager->activeDataset();
	long ifun = ds->getActiveVelocityFunction();

	if (_insert)
	{
		_picks->broadcastInsDone(   _index, _v, _t);
		ds->insertPick(ifun, (long) _index, _t, _v,
			((VaIsoPlot *) _picks->getPlot())->
				getPlottedVelocityType());
	}
	else
	{
		_picks->broadcastRepDone(             _index, _v, _t);
		ds->replacePick         (ifun, (long) _index, _t, _v,
			((VaIsoPlot *) _picks->getPlot())->
				getPlottedVelocityType());
	}
}

void VaIsoPicker::noModButtonTwoPress(int x, int y)
{
	if (initPick(x, y) && canEdit())
	{
		/*
		 * Use bracketTime to set _index and _insert
		 */
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();
		bracketTime(_t, line_type != VaIsoPlot::TIMESLICE,
			((VaIsoPlot *) _picks->getPlot())->
				getPlottedVelocityType());

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

void VaIsoPicker::noModButtonTwoMotion(int /*x1*/, int /*x2*/,
				       int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void VaIsoPicker::noModButtonTwoRelease(int /*x1*/, int /*x2*/,
					int /*y1*/, int /*y2*/)
{
	VfDataset *ds = _manager->activeDataset();
	long ifun = ds->getActiveVelocityFunction();
	assert(ifun != -1);
	ds->removePick(ifun, (long) _index);
}

void VaIsoPicker::shiftButtonOnePress(int x, int y)
{
	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();
	Vector *vector;
	float dist_mm;

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			vector = _vectors->closestVertex(x, y, &_index,
				getPlot(), &dist_mm);
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			vector = _vectors->closest(x, y, getPlot(), &dist_mm);
			break;
		default:
			assert(0);
	}

	float pick_tol = _manager->utilities()->getPickingTolerance();

	if (vector)
	{
		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				_insert = (dist_mm <= pick_tol) ? 0 : 1;
				break;
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				if (dist_mm <= pick_tol)
				{
					_insert = 0;
					ignoreActions();  /* already there */
				}
				else
				{
					_insert = 1;
				}
				break;
			default:
				assert(0);
		}
	}
	else
	{
		_index  = -1;
		_insert =  1;
	}

	if (_insert && !canEdit())
	{
		VfDataset   *ds = _manager->activeDataset();
		VfUtilities *ut = _manager->utilities    ();
		float xloc, yloc;
		long closest = closestFunc(x, y, &xloc, &yloc);

		if (closest == -1)
		{
			doBeep();
		}
		else if ((ut->xbinNumber(ds->getXloc(closest))
		       == ut->xbinNumber(xloc                ))
		      && (ut->ybinNumber(ds->getYloc(closest))
		       == ut->ybinNumber(yloc                )))
		{
			 ds->setActiveVelocityFunction(closest);
		}
		else if (-1 != ds->findMatchingVelfun(xloc, yloc))
		{
			ds->setActiveVelocityFunction(closest);
		}
		else
		{
			doBeep();
		}

		ignoreActions();
	}
}

void VaIsoPicker::shiftButtonOneMotion(int /*x1*/, int /*x2*/,
				       int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void VaIsoPicker::shiftButtonOneRelease(int x1, int /*x2*/,
					int y1, int /*y2*/)
{
	/*
	 * _insert = 1 will not necessarily insert,
	 * uses the bin information to determine whether to insert or select.
	 */
	if (_insert)
	{
		VfDataset   *ds = _manager->activeDataset();
		VfUtilities *ut = _manager->utilities    ();
		float xloc, yloc;
		long closest = closestFunc(x1, y1, &xloc, &yloc);

		if (closest == -1)
		{
			ds->findOrInsertVelfun(xloc, yloc);
		}
		else if ((ut->xbinNumber(ds->getXloc(closest))
		       == ut->xbinNumber(xloc                ))
		      && (ut->ybinNumber(ds->getYloc(closest))
		       == ut->ybinNumber(yloc                )))
		{
			 ds->setActiveVelocityFunction(closest);
		}
		else if (-1 != ds->findMatchingVelfun(xloc, yloc))
		{
			ds->setActiveVelocityFunction(closest);
		}
		else
		{
			ds->findOrInsertVelfun(xloc, yloc);
		}
	}
	else
	{
		_manager->activeDataset()->
			setActiveVelocityFunction((long) _index);
	}
}

void VaIsoPicker::shiftButtonTwoPress(int x, int y)
{
	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();
	Vector *vector;
	float dist_mm;

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			vector = _vectors->closestVertex(x, y, &_index,
				getPlot(), &dist_mm);
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			vector = _vectors->closest(x, y, getPlot(), &dist_mm);
			break;
		default:
			assert(0);
	}

	float pick_tol = _manager->utilities()->getPickingTolerance();

	if (vector && (dist_mm <= pick_tol) && canEdit())
	{
		switch (line_type)
		{
			case VaIsoPlot::TIMESLICE:
				/* _index already set */
				break;
			case VaIsoPlot::INLINE:
			case VaIsoPlot::CROSSLINE:
				_index = (int) _manager->activeDataset()->
					getActiveVelocityFunction();
				break;
			default:
				assert(0);
		}
	}
	else
	{
		doBeep();
		ignoreActions();
	}
}

void VaIsoPicker::shiftButtonTwoMotion(int /*x1*/, int /*x2*/,
				       int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void VaIsoPicker::shiftButtonTwoRelease(int /*x1*/, int /*x2*/,
					int /*y1*/, int /*y2*/)
{
	_manager->activeDataset()->removeVelocityFunction((long) _index);
}

void VaIsoPicker::cntlButtonOnePress(int x, int y)
{
	if (initPick(x, y))
	{
		int line_type =
			((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();
		bracketTime(_t, line_type != VaIsoPlot::TIMESLICE,
			((VaIsoPlot *) _picks->getPlot())->
				getPlottedVelocityType());

		if (_use_t_min && (_t < _t_min))
			_t = _t_min;
		
		if (_use_t_max && (_t > _t_max))
			_t = _t_max;

		/*
		 * extraRbnStrt must be called before broadcastIns/RepStrt
		 * so _doNormalRbn is turned off.  Updating an rbn and
		 * a normal vector on the same PlotBase is very slow.
		 */
		((VaIsoPicks *) _picks)->extraRbnStrt(getPlot()->xWC(x),
						      getPlot()->yWC(y));

		_v = ((SeisPlot *) getPlot())->getZvalueFromPixelXY(x, y);

		if (_insert)
		{
			if (canEdit())
			{
				_picks->broadcastInsStrt(_index, _v, _t);
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

			VfDataset *ds = _manager->activeDataset();
			long act_fun = ds->getActiveVelocityFunction();

			if ((int) ds->getActivePick(act_fun) != _index)
				ds->setActivePick(act_fun, (long) _index);

			if (watch_ok)
				PlotBase::setWatchOK(True );

			if (canEdit())
				_picks->broadcastRepStrt(_index, _v, _t);
			else
				ignoreActions();
		}
	}
	else
	{
		doBeep();
		ignoreActions();
	}
}

void VaIsoPicker::cntlButtonOneMotion(int /*x1*/, int x2,
				      int /*y1*/, int y2)
{
	((VaIsoPicks *) _picks)->extraRbnDrag(getPlot()->xWC(x2),
					      getPlot()->yWC(y2));

	_v = ((SeisPlot *) getPlot())->getZvalueFromPixelXY(x2, y2);

	if (_insert)
		_picks->broadcastInsDrag(_index, _v, _t);
	else
		_picks->broadcastRepDrag(_index, _v, _t);
}

void VaIsoPicker::cntlButtonOneRelease(int /*x1*/, int x2,
				       int /*y1*/, int y2)
{
	((VaIsoPicks *) _picks)->extraRbnDone();

	_v = ((SeisPlot *) getPlot())->getZvalueFromPixelXY(x2, y2);

	VfDataset *ds = _manager->activeDataset();
	long ifun = ds->getActiveVelocityFunction();

	if (_insert)
	{
		_picks->broadcastInsDone(   _index, _v, _t);
		ds->insertPick(ifun, (long) _index, _t, _v,
			((VaIsoPlot *) _picks->getPlot())->
				getPlottedVelocityType());
	}
	else
	{
		_picks->broadcastRepDone(             _index, _v, _t);
		ds->replacePick         (ifun, (long) _index, _t, _v,
			((VaIsoPlot *) _picks->getPlot())->
				getPlottedVelocityType());
	}
}

/*
 * initPick returns if pick is within tolerance and sets _t
 */
int VaIsoPicker::initPick(int x, int y)
{
	int line_type = ((VaIsoPlot *) _picks->getPlot())->getPlottedLineType();
	float pick_tol = _manager->utilities()->getPickingTolerance();
	Vector *vector;
	float dist_mm;
	int use_closest_func, closest_func;

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			use_closest_func = 1;
			vector = _vectors->closestVertex(x, y, &closest_func,
				getPlot(), &dist_mm);
			_t = ((VaIsoPlot *) _picks->getPlot())->getTimeOfFrame(
				((SeisPlot *) getPlot())->currentFrame());
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			use_closest_func = 0;
			vector = _vectors->closest(x, y, getPlot(), &dist_mm);
			_t = getPlot()->yWC(y);
			break;
		default:
			assert(0);
	}

	return vector && (dist_mm <= pick_tol)
	     && (!use_closest_func || (closest_func ==
		(int) _manager->activeDataset()->getActiveVelocityFunction()));
}

/*
 * closestFunc both finds the index of the velocity function closest to
 * pixels x_dc & y_dc; and converts the pixels to location coords x_wc & y_wc
 */
long VaIsoPicker::closestFunc(int x_dc, int y_dc, float *x_wc, float *y_wc)
{
	long retval;
	int line_type = ((VaIsoPlot *)_picks->getPlot())->getPlottedLineType();
	VfDataset   *ds = _manager->activeDataset();
	VfUtilities *ut = _manager->utilities    ();
	SeisPlot *sp = (SeisPlot *) getPlot();
	int trace_offset = (int) (sp->currentFrame() * sp->originalTraces());
	float (VfDataset  ::*get_line_loc  )(long ) const;
	long  (VfUtilities::*line_binNumber)(float) const;
	float (VfDataset  ::*get_dist_loc  )(long ) const;
	long line_bin;
	float dist_loc;
	int need_search;

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			*x_wc = getPlot()->xWC(x_dc);
			*y_wc = getPlot()->yWC(y_dc);

			/*
			 * _index was set by closestVertex in
			 * shiftButtonOnePress.  Was set to -1 if vector
			 * was NULL.
			 */
			retval = (long) _index;
			need_search = 0;
			break;
		case VaIsoPlot::INLINE:
			dist_loc = *x_wc = getPlot()->xWC(x_dc);
			*y_wc = sp->getHeaderFromTrace(
				trace_offset + 1, ds->getNhy());
			get_line_loc   = &VfDataset  ::getYloc   ;
			line_binNumber = &VfUtilities::ybinNumber;
			line_bin = (ut->*line_binNumber)(*y_wc);
			get_dist_loc = &VfDataset::getXloc;
			need_search = 1;
			break;
		case VaIsoPlot::CROSSLINE:
			*x_wc = sp->getHeaderFromTrace(
//				trace_offset + 1, ds->getNhx());
				trace_offset + 1, ds->getNhy());
			dist_loc = *y_wc = getPlot()->xWC(x_dc);
			get_line_loc   = &VfDataset  ::getXloc   ;
			line_binNumber = &VfUtilities::xbinNumber;
			line_bin = (ut->*line_binNumber)(*x_wc);
			get_dist_loc = &VfDataset::getYloc;
			need_search = 1;
			break;
		default:
			assert(0);
	}

	/*
	 * This searching is necessary for inline and crossline
	 * because only the active velocity function is displayed
	 * so vectors can not tell us the closest.
	 * For timeslice, we display all velocity functions
	 * so the vectors have already told us the closest.
	 */
	if (need_search)
	{
		float dist;
		float min_dist = 0.0F;	/* Warn me not, fair compiler.  */
		long i;

		for (retval = -1, i = 0; i < ds->numVelocityFunctions(); i++)
		{
			if ((ut->*line_binNumber)((ds->*get_line_loc)(i))
			 == line_bin)
			{
				dist = (ds->*get_dist_loc)(i) - dist_loc;
				dist = (dist < 0.0F) ? -dist : dist;

				if ((retval == -1) || (dist < min_dist))
				{
					retval   = i   ;
					min_dist = dist;
				}
			}
		}
	}

	return retval;
}

VaIsoPicks::VaIsoPicks(class VfManager *manager, class VfHorizons *horizons,
	class VaPlot *plot, SeisPlot *sp, class VaVectColors *colors)
	: VaPicks(manager, horizons, plot, sp, colors, 1), _doNormalRbn(1)
{
	_show_active_func = _colors->getShowActiveFunc();

	_data = new VaIsoPickData(_manager, this, sp, colors);

	Vector *vect = _editable_vectors->add(_data, BaseData::defaultId,
                _colors->isoProfileColor   (),
                _colors->isoProfileWidth   (),
                False                        ,
                Vector::DataSpecifiedStyle   ,
                Vector::DataSpecifiedMarker  ,
                _colors->isoMarkerSize     (),
                _colors->isoMarkerLineWidth());

	vect->allowAltMarkerColors(True);
	vect->setAltMarkerColor(1, _colors->isoProfileDefaultPickColor());
	vect->setAltMarkerColor(2, _colors->isoProfileActivePickColor ());
	vect->setAltMarkerColor(3, _colors->isoGridDefaultColor       ());
	vect->setAltMarkerColor(4, _colors->isoGridActiveColor        ());
	vect->makeVisible();

	_va_iso_horizons_data = new VaIsoHorizonsData(manager, _colors,
		(VaIsoPlot *) _plot);

	_va_horizons          = new VaHorizons       (manager, horizons,
		_va_iso_horizons_data, _colors,
		_constant_vectors[0], _editable_vectors);

	/*
	 * Add coupled cursor after all vector linked lists since
	 * if cursor is in rbn mode, it must repair exposes last.
	 */
	_xh_trans = new IsoCrossHairTranslator(this);

	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());
}

VaIsoPicks::~VaIsoPicks()
{
	/*
	 * _data is deleted in base class destructor because it
	 * must outlast vectors.
	 */

	if (_picker)
		delete _picker;

	_cross_hairs.remove(top());

	delete _xh_trans;

	delete _va_horizons         ;
	delete _va_iso_horizons_data;
}

void VaIsoPicks::init(SeisPlot *sp)
{
	_picker = new VaIsoPicker(sp, _manager, this, _editable_vectors);
}

void VaIsoPicks::insStrt(int /*index*/, float /*x*/, float y)
{
	((VaIsoPickData *) _data)->hideActivePick();

	strtRbn(y);
}

void VaIsoPicks::insDrag(int /*index*/, float /*x*/, float y)
{
	dragRbn(y);
}

void VaIsoPicks::insDone(int /*index*/, float /*x*/, float y)
{
	doneRbn(y);
}

void VaIsoPicks::repStrt(int /*index*/, float /*x*/, float y)
{
	strtRbn(y);
}

void VaIsoPicks::repDrag(int /*index*/, float /*x*/, float y)
{
	dragRbn(y);
}

void VaIsoPicks::repDone(int /*index*/, float /*x*/, float y)
{
	doneRbn(y);
}

void VaIsoPicks::setIsoTSOverlay(int /*set*/)
{
	int line_type = ((VaIsoPlot *) getPlot())->getPlottedLineType();

	if (line_type == VaIsoPlot::TIMESLICE)
		_editable_vectors->redisplay();
}

void VaIsoPicks::setShowActiveFunc(int set)
{
	((VaIsoPickData *) _data)-> preChangeShowActiveFunc();
	_show_active_func = set;
	((VaIsoPickData *) _data)->postChangeShowActiveFunc();
}

char *VaIsoPicks::getClassName()
{
	static char *retval = VA_ISO_PICKS;

	return retval;
}

void VaIsoPicks::strtRbn(float t)
{
	int line_type = ((VaIsoPlot *) getPlot())->getPlottedLineType();
	VfDataset *ds = _manager->activeDataset();
	long ifun = ds->getActiveVelocityFunction();

	switch (line_type)
	{
		case VaIsoPlot::TIMESLICE:
			_rbn_data = (VectData *) NULL;
			break;
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			if ((ifun > -1) && _doNormalRbn
			 && ((VaIsoPickData *) _data)->isDisplayed(
				line_type, ds, ifun))
			{
				float loc;

				if (line_type == VaIsoPlot::INLINE)
					loc = ds->getXloc(ifun);
				else
					loc = ds->getYloc(ifun);

				_rbn_data = new VectData(1, &loc, &t);

				_rbn_vector = _rbn_vectors->add(_rbn_data,
					_colors->isoRbnColor          (),
					_colors->isoProfileWidth      (),
					False                           ,
					_colors->isoProfileLineStyle  (),
					_colors->isoRbnMarker         (),
					_colors->isoRbnMarkerSize     (),
					_colors->isoRbnMarkerLineWidth());
					
				_rbn_vector->makeVisible();
			}
			else
			{
				_rbn_data = (VectData *) NULL;
			}
			break;
		default:
			assert(0);
	}
}

void VaIsoPicks::dragRbn(float t)
{
	if (_rbn_data)
	{
		float loc = _rbn_data->getX(0);

		_rbn_data->replace(0, 1, &loc, &t);
	}
}

void VaIsoPicks::doneRbn(float /*t*/)
{
	if (_rbn_data)
	{
		_rbn_vectors->remove(_rbn_vector);
		delete _rbn_data;
	}
}

void VaIsoPicks::extraRbnStrt(float x1, float y1)
{
	_doNormalRbn = 0;

	float x[2], y[2];
	x[0] = x[1] = x1;
	y[0] = y[1] = y1;

	_extra_rbn_data = new VectData(2, x, y);

	_extra_rbn_vector = _rbn_vectors->add(_extra_rbn_data,
		_colors->isoRbnColor    (),
		_colors->isoProfileWidth(),
		True                      );
}

void VaIsoPicks::extraRbnDrag(float x2, float y2)
{
	_extra_rbn_data->replace(1, 1, &x2, &y2);
}

void VaIsoPicks::extraRbnDone()
{
	_rbn_vectors->remove(_extra_rbn_vector);
	delete _extra_rbn_data;

	_doNormalRbn = 1;
}

int VaIsoPicks::withinDisplay(long ifun, int use_getPlottedLineType)
{
	int retval;

	int line_type;
	if (use_getPlottedLineType)
		line_type = ((VaIsoPlot *) getPlot())->getPlottedLineType();
	else
		line_type = ((VaIsoPlot *) getPlot())->getLineType       ();

	VfDataset *ds = _manager->activeDataset();
//	assert(count() == 1);	/* zoom separate window allows > 1 */
	SeisPlot *sp = top();	/* I am a SeisInform, 1st SeisPlot is main */

	switch (line_type)
	{
		case -1:
			retval = 0;
			break;
		case VaIsoPlot::TIMESLICE:
		{
			float xloc = ds->getXloc(ifun);
			float yloc = ds->getYloc(ifun);
			float x1, x2, y1, y2;

			if (use_getPlottedLineType)
			{
				x1 = sp->plottedGridX1();
				x2 = sp->plottedGridX2();
				y1 = sp->plottedGridY1();
				y2 = sp->plottedGridY2();
			}
			else
			{
				x1 = sp->       gridX1();
				x2 = sp->       gridX2();
				y1 = sp->       gridY1();
				y2 = sp->       gridY2();
			}

			/*
			 * This will work if x2 > x1 or x2 < x1.
			 * (Or for that matter if x2 == x1.)
			 */
			int x_in_range = ((xloc - x1) * (x2 - xloc)) >= 0.0F;
			int y_in_range = ((yloc - y1) * (y2 - yloc)) >= 0.0F;

			retval = x_in_range && y_in_range;

			break;
		}
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			if (((VaIsoPickData *) _data)->isDisplayed(
				line_type, ds, ifun))
			{
				int t1 = (int)  (sp->currentFrame()
					* sp->originalTraces()) + 1;
				int t2 = (int) ((sp->currentFrame() + 1L)
					* sp->originalTraces());

				int   nh ;
				float loc;

				if (line_type == VaIsoPlot::INLINE)
				{
					nh  = ds->getNhx();
					loc = ds->getXloc(ifun);
				}
				else
				{
//					nh  = ds->getNhy();
					nh  = ds->getNhx();
					loc = ds->getYloc(ifun);
				}

				retval = (loc - sp->getHeaderFromTrace(t1, nh))
				       * (sp->getHeaderFromTrace(t2, nh) - loc)
				       >= 0.0F;
			}
			else
			{
				retval = 0;
			}
			break;
		default:
			assert(0);
	}

	return retval;
}

int VaIsoPicks::getShowActiveFunc()
{
	return _show_active_func;
}

VaPicker *VaIsoPicks::newSeparateWindow(SeisPlot *sp)
{
	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());

	return new VaIsoPicker(sp, _manager, this, _editable_vectors);
}

int VaIsoPicks::getVelocityType()
{
	return ((VaIsoPlot *) _plot)->getPlottedVelocityType();
}

VaIsoHorizonsData::VaIsoHorizonsData(VfManager *vf_manager,
	VaVectColors *colors, VaIsoPlot *va_iso_plot)
	: VaHorizonsData(vf_manager, colors), _va_iso_plot(va_iso_plot)
{
	/* just initializers */
}

VaIsoHorizonsData::~VaIsoHorizonsData()
{
	/* do nothing */
}

#define TIME_SLICE_PICK_WINDOW 0.1F

int VaIsoHorizonsData::getNumPts(long id)
{
	int retval;

	SeisPlot *sp = _va_iso_plot->SP();

	if (sp->isPlotDisplayed() && (sp->originalTraces() > 0L))
	{
		int trace_offset = (int)
			(sp->currentFrame() * sp->originalTraces());

		VfDataset   *ds = _vf_manager->activeDataset();
		VfUtilities *ut = _vf_manager->utilities    ();

		SortedHorizon *sh = _va_horizons_manager->getSortedHorizon(id);
		int bin;
		float time;

		switch (_va_iso_plot->getPlottedLineType())
		{
			case VaIsoPlot::INLINE:
				bin = (int) ut->ybinNumber(
					sp->getHeaderFromTrace(
					trace_offset + 1, ds->getNhy()));
				retval = sh->getInlineIndex(bin,
					&_starting_index);
				break;
			case VaIsoPlot::CROSSLINE:
				bin = (int) ut->xbinNumber(
					sp->getHeaderFromTrace(
//					trace_offset + 1, ds->getNhx()));
					trace_offset + 1, ds->getNhy()));
				retval = sh->getCrosslineIndex(bin,
					&_starting_index);
				break;
			case VaIsoPlot::TIMESLICE:
				time = _va_iso_plot->getTimeOfFrame(
					sp->currentFrame());
				retval = sh->getTimeIndex(
					time - 0.5F * TIME_SLICE_PICK_WINDOW,
					time + 0.5F * TIME_SLICE_PICK_WINDOW,
					&_starting_index);
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

float VaIsoHorizonsData::getX(int i, long id)
{
	float retval;

	switch (_va_iso_plot->getPlottedLineType())
	{
		case VaIsoPlot::INLINE:
		case VaIsoPlot::TIMESLICE:
			retval = getHorizonStuct(i, id)->x;
			break;
		case VaIsoPlot::CROSSLINE:
			retval = getHorizonStuct(i, id)->y;
			break;
		default:
			assert(0);
	}

	return retval;
}

float VaIsoHorizonsData::getY(int i, long id)
{
	float retval;

	switch (_va_iso_plot->getPlottedLineType())
	{
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			retval = getHorizonStuct(i, id)->t;
			break;
		case VaIsoPlot::TIMESLICE:
			retval = getHorizonStuct(i, id)->y;
			break;
		default:
			assert(0);
	}

	return retval;
}

SortedHorizon::HorizonStruct *VaIsoHorizonsData::getHorizonStuct(int i, long id)
{
	SortedHorizon *sh = _va_horizons_manager->getSortedHorizon(id);
	SortedHorizon::HorizonStruct **hs_ptr;

	switch (_va_iso_plot->getPlottedLineType())
	{
		case VaIsoPlot::INLINE:
			hs_ptr = sh->getSorted(SortedHorizon::INLINE);
			break;
		case VaIsoPlot::CROSSLINE:
			hs_ptr = sh->getSorted(SortedHorizon::CROSSLINE);
			break;
		case VaIsoPlot::TIMESLICE:
			hs_ptr = sh->getSorted(SortedHorizon::TIME);
			break;
		default:
			assert(0);
	}

	return hs_ptr[_starting_index + i];
}

int VaIsoHorizonsData::getMarkerType(int /*i*/, long /*id*/)
{
	int retval;

	switch (_va_iso_plot->getPlottedLineType())
	{
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
			switch (_show_horizon_markers)
			{
				case VaVectColors::OVER_LINE:
					retval = (int) Vector::NoMarker;
					break;
				case VaVectColors::OVER_BOTH:
				case VaVectColors::OVER_MARK:
					retval = (int)
						Vector::FilledSquareMarker;
					break;
				default:
					assert(0);
			}
			break;
		case VaIsoPlot::TIMESLICE:
			retval = (int) Vector::FilledSquareMarker;
			break;
		default:
			assert(0);
	}

	return retval;
}

int VaIsoHorizonsData::getLineStyle(long /*id*/)
{
	int retval;

	switch (_va_iso_plot->getPlottedLineType())
	{
		case VaIsoPlot::INLINE:
		case VaIsoPlot::CROSSLINE:
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
			break;
		case VaIsoPlot::TIMESLICE:
			retval = (int) Vector::NoLine;
			break;
		default:
			assert(0);
	}

	return retval;
}
