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
// $Id: va_crossplot_picks.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_crossplot_picks.hh"
#include "vaplots/va_xh_trans.hh"
#include "vaplots/va_vect_colors.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_utilities.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"
#include "plot/pick_watch.hh"
#include "sl/shell_watch.hh"

#include <assert.h>
#include <math.h>

VaCrossFuncData::VaCrossFuncData(VaCrossplotPickData *data, int ifunc)
	: BaseData(), _data(data), _ifunc(ifunc)
{
	/* just initializers */
}

VaCrossFuncData::~VaCrossFuncData()
{
	/* do nothing */
}

void VaCrossFuncData::setFunc(int ifunc)
{
	_ifunc = ifunc;
}

int VaCrossFuncData::getFunc()
{
	return _ifunc;
}

void VaCrossFuncData::redrawActivePick()
{
	int act_pck = (int)
		_data->manager()->activeDataset()->getActivePick((long) _ifunc);

	if (act_pck != -1)
	{
		int overlay_type = ((VaCrossplotPicks *) _data->getPicks())->
			getOverlayType();
		int num = 1;
		xlateRange   (&act_pck, &num, overlay_type);

		if (num > 0)
			modAttributes( act_pck,  num);
	}
}

void VaCrossFuncData::redrawAllPicks()
{
	int numPts = getNumPts(BaseData::defaultId);

	if (numPts > 0)
		modAttributes(0, numPts);
}

void VaCrossFuncData::redrawPicks(int *indices, int numIndices)
{
	assert(numIndices > 0);

	modAttributesByIndices(indices, numIndices);
}

int VaCrossFuncData::getNumPts(long /*id*/)
{
	int numPts = (int)
		_data->manager()->activeDataset()->numPicks((int) _ifunc);

	int overlay_type = ((VaCrossplotPicks *) _data->getPicks())
		->getOverlayType();

	return xlateNumPts(numPts, overlay_type);
}

float VaCrossFuncData::getX(int i, long /*id*/)
{
	int overlay_type = ((VaCrossplotPicks *) _data->getPicks())
		->getOverlayType();

	int index = xlateXindex(i, overlay_type);

	return _data->manager()->activeDataset()->getOrdinate(
		(long) _ifunc, (long) index, overlay_type);
}

float VaCrossFuncData::getY(int i, long /*id*/)
{
	int overlay_type = ((VaCrossplotPicks *) _data->getPicks())
		->getOverlayType();

	int index = xlateYindex(i, overlay_type);

	return _data->manager()->activeDataset()->getAbscissa(
		(long) _ifunc, (long) index, overlay_type);
}

int VaCrossFuncData::getAltMarkerColor(int i, long /*id*/)
{
	int retval;

	int overlay_type, index, act_pck;

	if (_data->hidingActivePick())
	{
		retval = 0;	/* not DEF_COL */
	}
	else
	{
		int is_act_fun = ((long) _ifunc
			== _data->manager()->activeDataset()->
				getActiveVelocityFunction());

		switch (((VaCrossplotPicks *) _data->getPicks())->
			getShowOverlayActivePick())
		{
			case VaVectColors::MARK_ACT:
				if (!(is_act_fun
				 &&   _data->_colors->getShowActiveFunc()))
				{
					retval = 0;	/* not DEF_COL */
					break;
				}
				/* no break */
			case VaVectColors::MARK_ALL:
				overlay_type = ((VaCrossplotPicks *)
					_data->getPicks())->getOverlayType();

				index   = xlateXindex(i, overlay_type);

				act_pck = (int) _data->manager()->
					activeDataset()->
					getActivePick((long) _ifunc);

				if      (index == act_pck)
				{
					retval =  VaVectColors::ACT_COL;
				}
				else if (is_act_fun
				 && _data->_colors->getShowOverlayActiveFunc()
				 && _data->_colors->getShowActiveFunc())
				{
					retval =  VaVectColors::DEF_COL;
				}
				else
				{
					retval = 0;	/* not DEF_COL */
				}

				break;
			case VaVectColors::MARK_NONE:
				retval = 0;	/* not DEF_COL */
				break;
			default:
				assert(0);
		}
	}

	return retval;
}

int VaCrossFuncData::xlateNumPts(int num, int overlay_type)
{
	int retval;

	if (overlay_type == VTIN)
	{
		retval = (num < 2) ? 0 : 2 * num - 2;
	}
	else
	{
		retval = num;
	}

	return retval;
}

int VaCrossFuncData::xlateXindex(int i, int overlay_type)
{
	int retval;

	if (overlay_type == VTIN)
	{
		retval = (i + 2) / 2;
	}
	else
	{
		retval = i;
	}

	return retval;
}

int VaCrossFuncData::xlateYindex(int i, int overlay_type)
{
	int retval;

	if (overlay_type == VTIN)
	{
		retval = (i + 1) / 2;
	}
	else
	{
		retval = i;
	}

	return retval;
}

void VaCrossFuncData::xlateRange(int *i, int *num, int overlay_type)
{
	if (overlay_type == VTIN)
	{
		if (*i == 0)
		{
			(*i  )++;
			(*num)--;
		}

		*i    = 2 * (*i - 1);
		*num *= 2;
	}
	else
	{
		/* i and num unchanged */
	}
}

void VaCrossFuncData::checkRange(int *i, int *num, int overlay_type, int type)
{
	if (overlay_type != type)
		*num = (int) _data->manager()->activeDataset()->
			numPicks((long) _ifunc) - *i;
}

void VaCrossFuncData::pointsRemoved(int ipick, int nrem, int type)
{
	int overlay_type = ((VaCrossplotPicks *) _data->getPicks())->
		getOverlayType();

	checkRange(&ipick, &nrem, overlay_type, type);
	xlateRange(&ipick, &nrem, overlay_type      );

	modIndicesBefore(ipick, nrem);
}

void VaCrossFuncData::pointsInserted(int ipick, int nins, int type)
{
	int overlay_type = ((VaCrossplotPicks *) _data->getPicks())->
		getOverlayType();

	checkRange(&ipick, &nins, overlay_type, type);
	xlateRange(&ipick, &nins, overlay_type      );

	modIndicesAfter(ipick, nins);
	modDone();
}

VaCrossplotPickData::VaCrossplotPickData(VfManager *manager, VaPicks *picks,
	SeisPlot *sp, VectorLinkedList *vectors, VaVectColors *colors)
	: VaVectPickData(manager, picks, sp, vectors, colors),
	  _hiding_active_pick(0)
{
	/* just initializers */
}

VaCrossplotPickData::~VaCrossplotPickData()
{
	for (int i = 0; i < _numVectUsed; i++)
		delete _data[i];
}

void VaCrossplotPickData::hideActivePick()
{
	if ((((VaCrossplotPicks *) _picks)->getShowOverlayActivePick()
		!= VaVectColors::MARK_NONE)
	 && (((VaCrossplotPicks *) _picks)->getShowOverlayMarkers   ()
		!= VaVectColors::OVER_LINE))
	{
		int ack_fun = (int)
			manager()->activeDataset()->getActiveVelocityFunction();

		if (_vect[ack_fun]->isVisible())
		{
			_hiding_active_pick = 1;
			((VaCrossFuncData*) _data[ack_fun])->redrawActivePick();
			_hiding_active_pick = 0;
		}
	}
}

int VaCrossplotPickData::hidingActivePick()
{
	return _hiding_active_pick;
}

int VaCrossplotPickData::updateLineColors()
{
	int retval, i;

	/*
	 * Since Vector method setColor only redraws if the color
	 * changes, calling setColor for every vector is not
	 * inefficient.
	 */
	for (retval = i = 0; i < _numVectUsed; i++)
		if (_vect[i]->isVisible()
		 && _vect[i]->setColor(getLineColor(i,
			_colors->crossplotActiveFuncColor(),
			"black",	/* will be no defaults */
			_colors->getShowOverlayActiveFunc())))
		{
			retval++;
		}
		else
		{
			/*
			 * Not always necessary, but since its just one point
			 * I don't bother figuring out if its needed.
			 */
			((VaCrossFuncData*) _data[i])->redrawActivePick();
		}

	return retval;
}

#define WATCH_THRESH 10L

void VaCrossplotPickData::on()
{
	int vis_ok = _picks->getShowFunc(VaVectColors::SEL)
		 &&  _colors->getEnableShowFuncs()
		 && (_colors->getSelectType() == VaVectColors::SEL_SEL);

	if (vis_ok)
	{
		VfDataset *ds = manager()->activeDataset();

		 PickWatch * pick_watch = ( PickWatch *) 0;
		ShellWatch *shell_watch = (ShellWatch *) 0;

		if (ds->numSelectedVelocityFunctions() >= WATCH_THRESH)
		{
			_picks->setWatch(1);
			 pick_watch = new  PickWatch();
			shell_watch = new ShellWatch();
		}

		for (int i = 0; i < _numVectUsed; i++)
			if (ds->velocityFunctionIsSelected((long) i))
			{
				/*
				 * Make sure the color is right.
				 */
				_vect[i]->setColor(getLineColor(i,
					_colors->crossplotActiveFuncColor(),
					"black",  /* will be no defaults */
					_colors->getShowOverlayActiveFunc()));

				_vect[i]->makeVisible();
			}

		if (_picks->getWatch())
		{
			delete  pick_watch;
			delete shell_watch;
			_picks->setWatch(0);
		}
	}
}

void VaCrossplotPickData::off()
{
	if (manager()->activeDataset()->numSelectedVelocityFunctions()
		>= WATCH_THRESH)
	{
		_picks->setWatch(1);
	}

	_vectors->makeInvisible();

	if (_picks->getWatch())
		_picks->setWatch(0);
}

void VaCrossplotPickData::setMarkers(int old_set, int new_set)
{
	assert((new_set >= VaVectColors::OVER_LINE)
	    && (new_set <= VaVectColors::OVER_MARK));
	/* range check for old_set is handled by switch */

	int i;

	switch (3 * old_set + new_set)
	{
	   case 0:	/* no change */
	   case 4:
	   case 8:
		assert(0);
		break;	/* not really needed */
	   case 1:	/* OVER_LINE to OVER_BOTH */
		for (i = 0; i < _numVectUsed; i++)
			_vect[i]->setMarker(
				_colors->crossplotMarker         (),
				_colors->crossplotMarkerSize     (),
				_colors->crossplotMarkerLineWidth());
		break;
	   case 2:	/* OVER_LINE to OVER_MARK */
		vaHoldVectors();

		for (i = 0; i < _numVectUsed; i++)
		{
			_vect[i]->setMarker(
				_colors->crossplotMarker         (),
				_colors->crossplotMarkerSize     (),
				_colors->crossplotMarkerLineWidth());

			_vect[i]->setStyle(Vector::NoLine);
		}

		vaFlushVectors();
		break;
	   case 3:	/* OVER_BOTH to OVER_LINE */
		vaHoldVectors();

		for (i = 0; i < _numVectUsed; i++)
			_vect[i]->setMarker(
				Vector::NoMarker,
				_colors->crossplotMarkerSize     (),
				_colors->crossplotMarkerLineWidth());

		vaFlushVectors();
		break;
	   case 5:	/* OVER_BOTH to OVER_MARK */
		vaHoldVectors();

		for (i = 0; i < _numVectUsed; i++)
			_vect[i]->setStyle(Vector::NoLine);

		vaFlushVectors();
		break;
	   case 6:	/* OVER_MARK to OVER_LINE */
		vaHoldVectors();

		for (i = 0; i < _numVectUsed; i++)
		{
			_vect[i]->setMarker(
				Vector::NoMarker,
				_colors->crossplotMarkerSize     (),
				_colors->crossplotMarkerLineWidth());

			_vect[i]->setStyle(Vector::SolidLine);
		}

		vaFlushVectors();
		break;
	   case 7:	/* OVER_MARK to OVER_BOTH */
		for (i = 0; i < _numVectUsed; i++)
			_vect[i]->setStyle(Vector::SolidLine);
		break;
	   default:
		assert(0);
		break;	/* not really needed */
	}
}

void VaCrossplotPickData::setActivePicks(int old_set, int new_set)
{
	assert((new_set >= VaVectColors::MARK_ALL )
	    && (new_set <= VaVectColors::MARK_NONE));
	/* range check for old_set is handled by switch */

	int ack_fun =
		(int) manager()->activeDataset()->getActiveVelocityFunction();
	int i;

	switch (3 * old_set + new_set)
	{
	   case 0:	/* no change */
	   case 4:
	   case 8:
		assert(0);
		break;	/* not really needed */
	   case 1:	/* MARK_ALL  to MARK_ACT  */
	   case 3:	/* MARK_ACT  to MARK_ALL  */

		for (i = 0; i < _numVectUsed; i++)
			if (_vect[i]->isVisible()
			 && ((i != ack_fun) || !_colors->getShowActiveFunc()))
			{
				((VaCrossFuncData *) _data[i])->
					redrawActivePick();
			}
		break;
	   case 2:	/* MARK_ALL  to MARK_NONE */
	   case 6:	/* MARK_NONE to MARK_ALL  */
		for (i = 0; i < _numVectUsed; i++)
			if (_vect[i]->isVisible())
				if ((i == ack_fun)
				 && _colors->getShowOverlayActiveFunc())
				{
					((VaCrossFuncData *) _data[i])->
						redrawAllPicks  ();
				}
				else
				{
					((VaCrossFuncData *) _data[i])->
						redrawActivePick();
				}
		break;
	   case 5:	/* MARK_ACT  to MARK_NONE */
	   case 7:	/* MARK_NONE to MARK_ACT  */
		if ((ack_fun != -1) && _vect[ack_fun]->isVisible())
			if (_colors->getShowOverlayActiveFunc())
			{
				((VaCrossFuncData *) _data[ack_fun])->
					redrawAllPicks  ();
			}
			else
			{
				((VaCrossFuncData *) _data[ack_fun])->
					redrawActivePick();
			}
		break;
	   default:
		assert(0);
		break;	/* not really needed */
	}
}

int VaCrossplotPickData::activeFuncVisible()
{
	int ack_fun =
		(int) manager()->activeDataset()->getActiveVelocityFunction();

	return _vect[ack_fun]->isVisible();
}

int VaCrossplotPickData::getIndex(class Vector *vector)
{
	int retval = -1;	/* init just to avoid compiler warning */
        int i;
	for (i = 0; i < _numVectUsed; i++)
		if (_vect[i] == vector)
		{
			retval = i;
			break;
		}

	assert(i < _numVectUsed);

	return retval;
}

void VaCrossplotPickData::beforeChanges()
{
	assert(!vaIsHoldingVectors());

	_change_act_pick = _rem_ins_pick = 0;
}

void VaCrossplotPickData::afterChanges()
{
	assert(!vaIsHoldingVectors());
	assert(!_picks->getWatch  ());

	int num_changes = updateLineColors();

	if (_change_act_pick)
	{
	    assert(num_changes == 0);
	    assert(_old_act_func == (int) manager()->activeDataset()->
		getActiveVelocityFunction());

	    if ( _vect[_old_act_func]->isVisible()
	     && (((VaCrossplotPicks *) getPicks())->getShowOverlayMarkers   ()
		!= VaVectColors::OVER_LINE)
	     && (((VaCrossplotPicks *) getPicks())->getShowOverlayActivePick()
		!= VaVectColors::MARK_NONE))
	    {
		int indices[4];
		int numIndices = 0;
		int overlay_type = ((VaCrossplotPicks *) getPicks())->
			getOverlayType();
		int extra = (overlay_type == VTIN) ? 0 : 1;
		int num, i;

		if (_old_act_pick != -1)
		{
			if (_rem_ins_pick)
				_old_act_pick = adjustIndex(_old_act_pick,
					_pick_index, _pick_nrem, _pick_nins,
					extra);

			if (_old_act_pick != -1)
			{
				num = 1;
				VaCrossFuncData::xlateRange(&_old_act_pick,
					&num, overlay_type);

				for (i = 0; i < num; i++)
					indices[numIndices++] =
						_old_act_pick + i;
			}
		}

		int new_act_pick = (int) manager()->activeDataset()->
			getActivePick((long) _old_act_func);

		if (new_act_pick != -1)
		{
			if (_rem_ins_pick)
			{
				if (newIndexEffected(new_act_pick, _pick_index,
					_pick_nrem, _pick_nins, extra))
				{
					new_act_pick = -1;
				}
			}

			if (new_act_pick != -1)
			{
				num = 1;
				VaCrossFuncData::xlateRange(&new_act_pick,
					&num, overlay_type);

				for (i = 0; i < num; i++)
					indices[numIndices++] =
						new_act_pick + i;
			}
		}

		if      ((numIndices == 2) && (indices[0] == indices[1]))
		{
			numIndices = 0;
		}
		else if ((numIndices == 4) && (indices[0] == indices[2]))
		{
			assert(indices[1] == indices[3]);
			numIndices = 0;
		}

		if (numIndices)
			((VaCrossFuncData *) _data[_old_act_func])->
				redrawPicks(indices, numIndices);
	    }
	}
}

void VaCrossplotPickData::preTotalChanges(VfDataset *dataset)
{
	if (manager()->activeDataset() == dataset)
	{
		int numFunc = (int) dataset->numVelocityFunctions();
		assert(_numVectUsed == numFunc);
		
		int vis_ok = _picks->getShowFunc(VaVectColors::SEL)
			 &&  _colors->getEnableShowFuncs()
			 && (_colors->getSelectType() == VaVectColors::SEL_SEL);

		if ( vis_ok
		 && (dataset->numSelectedVelocityFunctions() >= WATCH_THRESH))
		{
			_picks->setWatch(1);
		}

		vaHoldVectors();
		
		for (int i = 0; i < _numVectUsed; i++)
		{
			_vectors->remove(_vect[i]);
			delete           _data[i] ;
		}
		
		vaFlushVectors();

		if (_picks->getWatch())
			_picks->setWatch(0);
		
		_numVectUsed = 0;
	}
}

void VaCrossplotPickData::postTotalChanges(VfDataset *dataset)
{
	assert(!vaIsHoldingVectors());
		
	if (manager()->activeDataset() == dataset)
	{
		int numFunc = (int) dataset->numVelocityFunctions();
		assert(_numVectUsed == 0);
		
		checkAllocation(numFunc);
		
		Vector::VectorStyle  style ;
		Vector::VectorMarker marker;

		switch (((VaCrossplotPicks *) _picks)->getShowOverlayMarkers())
		{
			case VaVectColors::OVER_LINE:
				style  = Vector::SolidLine;
				marker = Vector::NoMarker ;
				break;
			case VaVectColors::OVER_BOTH:
				style  = Vector::SolidLine;
				marker = _colors->crossplotMarker();
				break;
			case VaVectColors::OVER_MARK:
				style  = Vector::NoLine;
				marker =_colors->crossplotMarker();
				break;
			default:
				assert(0);
		}

		int vis_ok = _picks->getShowFunc(VaVectColors::SEL)
			 &&  _colors->getEnableShowFuncs()
			 && (_colors->getSelectType() == VaVectColors::SEL_SEL);

		 PickWatch * pick_watch = ( PickWatch *) 0;
		ShellWatch *shell_watch = (ShellWatch *) 0;

		if ( vis_ok
		 && (dataset->numSelectedVelocityFunctions() >= WATCH_THRESH))
		{
			_picks->setWatch(1);
			 pick_watch = new  PickWatch();
			shell_watch = new ShellWatch();
		}

		for (int i = 0; i < _numVectUsed; i++)
		{
			_data[i] = new VaCrossFuncData(this, i);
		
			_vect[i] = _vectors->add(_data[i],
				getLineColor(i,
					_colors->crossplotActiveFuncColor(),
					"black",  /* will be no defaults */
					_colors->getShowOverlayActiveFunc()),
				_colors->crossplotWidth          (),
				False, style, marker,
				_colors->crossplotMarkerSize     (),
				_colors->crossplotMarkerLineWidth());
		
			_vect[i]->allowAltMarkerColors(True);
			_vect[i]->setAltMarkerColor(VaVectColors::DEF_COL,
				_colors->crossplotDefaultPickColor());
			_vect[i]->setAltMarkerColor(VaVectColors::ACT_COL,
				_colors->crossplotActivePickColor ());
		
			if (vis_ok
			 && dataset->velocityFunctionIsSelected((long) i))
			{
				_vect[i]->makeVisible();
			}
		}
		
		if (_picks->getWatch())
		{
			delete  pick_watch;
			delete shell_watch;
			_picks->setWatch(0);
		}
	}
}

void VaCrossplotPickData::preModifyPicks(VfDataset *dataset, long ifun,
	int type, long ipick, long nrem)
{
	assert(manager()->activeDataset() == dataset);
	assert(dataset->getActiveVelocityFunction() == ifun);

	if (_vect[ifun]->isVisible())
		((VaCrossFuncData *) _data[ifun])->pointsRemoved((int) ipick,
			(int) nrem, type);
}

void VaCrossplotPickData::postModifyPicks(VfDataset *dataset, long ifun,
	int type, long ipick, long nrem, long nins)
{
	assert(manager()->activeDataset() == dataset);
	assert(dataset->getActiveVelocityFunction() == ifun);

	if (_vect[ifun]->isVisible())
	{
		if (dataset->numSelectedVelocityFunctions() >= WATCH_THRESH)
			_picks->setWatch(1);

		((VaCrossFuncData *) _data[ifun])->pointsInserted((int) ipick,
			(int) nins, type);
		
		if (_picks->getWatch())
			_picks->setWatch(0);

		/*
		 * For recoloring picks in afterChanges
		 */
		_rem_ins_pick = 1;
		_pick_index = (int) ipick;
		_pick_nrem  = (int) nrem;
		_pick_nins  = (int) nins;
	}
}

void VaCrossplotPickData::preNewActiveDataset()
{
	preTotalChanges(manager()->activeDataset());
}

void VaCrossplotPickData::postNewActiveDataset()
{
	postTotalChanges(manager()->activeDataset());
}

void VaCrossplotPickData::preNewActivePicks(VfDataset *dataset, long ifun,
	long nchng)
{
	assert(manager()->activeDataset() == dataset);
	assert(dataset->getActiveVelocityFunction() == ifun);
	assert(nchng == 1L);

	_old_act_func = (int) ifun;
	_change_act_pick = 1;
	_old_act_pick = (int) dataset->getActivePick((long) _old_act_func);
}

void VaCrossplotPickData::postNewActivePicks(VfDataset * /*dataset*/,
	long /*ifun*/, long /*nchng*/)
{
	/* do it in afterChanges */
}

void VaCrossplotPickData::preRemoveInsertVelocityFunctions(VfDataset *dataset,
	long /*ifun*/, long /*nrem*/, long /*nins*/)
{
	assert(manager()->activeDataset() == dataset);
	assert(_numVectUsed == (int) dataset->numVelocityFunctions());

	/*
	 * Since all we need to know about old stuff is indices,
	 * everything can wait until postRemoveInsertVelocityFunctions.
	 */
}

void VaCrossplotPickData::postRemoveInsertVelocityFunctions(VfDataset *dataset,
	long ifun, long nrem, long nins)
{
	assert(manager()->activeDataset() == dataset);

	int vis_ok = _picks->getShowFunc(VaVectColors::SEL)
		 &&  _colors->getEnableShowFuncs()
		 && (_colors->getSelectType() == VaVectColors::SEL_SEL);

	if (vis_ok)
		_picks->setWatch(1);

	int num_move = (nrem == nins) ? 0 : _numVectUsed - (int) (ifun + nrem);
	assert(num_move >= 0);
	checkAllocation(_numVectUsed + (int) (nins - nrem));
	assert(_numVectUsed == (int) dataset->numVelocityFunctions());

	int i;

	/*
	 * Must reset ifun before you delete any vectors, or else
	 * you will have problems when vectors redraw because of
	 * deletions.  Not resetting ifun on data classes to be deleted;
	 * since we holdVectors this is not a problem.
	 */
	if (num_move)
	{
		for (	i = (int) (ifun + nrem);
			i < (int) (ifun + nrem) + num_move;
			i++)
		{
			((VaCrossFuncData *) _data[i])->setFunc(
				i + (int) (nins - nrem));
		}
	}

	vaHoldVectors();

	for (i = (int) ifun; i < (int) (ifun + nrem); i++)
	{
		_vectors->remove(_vect[i]);
		delete           _data[i];
	}

	vaFlushVectors();
	assert(!vaIsHoldingVectors());

	if (num_move)
	{
		memmove((void *) &_vect[ifun + nins],
			(void *) &_vect[ifun + nrem],
			(size_t) num_move * sizeof(Vector        *));

		memmove((void *) &_data[ifun + nins],
			(void *) &_data[ifun + nrem],
			(size_t) num_move * sizeof(VaCrossFuncData *));
	}

	if (nins > 0L)
	{
		Vector::VectorStyle  style ;
		Vector::VectorMarker marker;

		switch (((VaCrossplotPicks *) _picks)->getShowOverlayMarkers())
		{
			case VaVectColors::OVER_LINE:
				style  = Vector::SolidLine;
				marker = Vector::NoMarker ;
				break;
			case VaVectColors::OVER_BOTH:
				style  = Vector::SolidLine;
				marker = _colors->crossplotMarker();
				break;
			case VaVectColors::OVER_MARK:
				style  = Vector::NoLine;
				marker =_colors->crossplotMarker();
				break;
			default:
				assert(0);
		}

		for (i = (int) ifun; i < (int) (ifun + nins); i++)
		{
			_data[i] = new VaCrossFuncData(this, i);
		
			_vect[i] = _vectors->add(_data[i],
				getLineColor(i,
					_colors->crossplotActiveFuncColor(),
					"black",  /* will be no defaults */
					_colors->getShowOverlayActiveFunc()),
				_colors->crossplotWidth          (),
				False, style, marker,
				_colors->crossplotMarkerSize     (),
				_colors->crossplotMarkerLineWidth());
		
			_vect[i]->allowAltMarkerColors(True);
			_vect[i]->setAltMarkerColor(VaVectColors::DEF_COL,
				_colors->crossplotDefaultPickColor());
			_vect[i]->setAltMarkerColor(VaVectColors::ACT_COL,
				_colors->crossplotActivePickColor ());
		
			if (vis_ok
			 && dataset->velocityFunctionIsSelected((long) i))
			{
				_vect[i]->makeVisible();
			}
		}
	}

	if (vis_ok)
		_picks->setWatch(0);
}

void VaCrossplotPickData::preChangeSelections(VfDataset *dataset, long ifun,
	long nchng)
{
	if ((manager()->activeDataset() == dataset)
	 &&  _picks->getShowFunc(VaVectColors::SEL)
	 &&  _colors->getEnableShowFuncs()
	 && (_colors->getSelectType() == VaVectColors::SEL_SEL))
	{
		_old_selections = new int[(int) nchng];

		for (int i = 0; i < (int) nchng; i++)
			_old_selections[i] = dataset->
				velocityFunctionIsSelected(ifun + (long) i);
	}
}

void VaCrossplotPickData::postChangeSelections(VfDataset *dataset, long ifun,
	long nchng)
{
	assert(!vaIsHoldingVectors());
		
	if ((manager()->activeDataset() == dataset)
	 &&  _picks->getShowFunc(VaVectColors::SEL)
	 &&  _colors->getEnableShowFuncs()
	 && (_colors->getSelectType() == VaVectColors::SEL_SEL))
	{
		_picks->setWatch(1);

		int *new_selections = new int[(int) nchng];
		int i;

		for (i = 0; i < (int) nchng; i++)
			new_selections[i] = dataset->
				velocityFunctionIsSelected(ifun + (long) i);

		vaHoldVectors();

		for (i = 0; i < (int) nchng; i++)
			if (_old_selections[i] && !new_selections[i])
				_vect[(int) ifun + i]->makeInvisible();

		vaFlushVectors();

		 PickWatch * pick_watch = ( PickWatch *) 0;
		ShellWatch *shell_watch = (ShellWatch *) 0;

		if (dataset->numSelectedVelocityFunctions() >= WATCH_THRESH)
		{
			 pick_watch = new  PickWatch();
			shell_watch = new ShellWatch();
		}

		for (i = 0; i < (int) nchng; i++)
			if (!_old_selections[i] && new_selections[i])
			{
				_vect[(int) ifun + i]->setColor(
					getLineColor((int) ifun + i,
					_colors->crossplotActiveFuncColor(),
					"black",  /* will be no defaults */
					_colors->getShowOverlayActiveFunc()));

				_vect[(int) ifun + i]->makeVisible();
			}

		if (pick_watch)
		{
			delete  pick_watch;
			delete shell_watch;
		}

		delete [] _old_selections;
		delete []  new_selections;

		_picks->setWatch(0);
	}
}

#define FALLBACK "mouse*VA_XP_PICK: BTN#1: Activate Pick, Shift-BTN#1: Activate Velocity Function"

VaCrossplotPicker::VaCrossplotPicker(PlotBase *plot, VfManager *manager,
	VaPicks *picks, VectorLinkedList *vectors)
	: VaPicker(plot, "", "VA_XP_PICK", FALLBACK,
		manager, picks, vectors)
{
	/* just initializers */
}

VaCrossplotPicker::~VaCrossplotPicker()
{
	/* do nothing */
}

void VaCrossplotPicker::noModButtonOnePress(int x, int y)
{
	int index;
	float dist_mm;
	Vector *vector = _vectors->closestVertex(x, y, &index, getPlot(),
		&dist_mm);
	
	if (vector)
	{
		float pick_tol = _manager->utilities()->getPickingTolerance();

		if (dist_mm <= pick_tol)
		{
			_picked_fun = ((VaCrossplotPicks *) _picks)->
				getIndex(vector);
			_picked_pck = ((VaCrossplotPicks *) _picks)->
				getPick (index );
		}
		else
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

void VaCrossplotPicker::noModButtonOneMotion(int /*x1*/, int /*x2*/,
					     int /*y1*/, int /*y2*/)
{
	/* wait for release */
}

void VaCrossplotPicker::noModButtonOneRelease(int /*x1*/, int /*x2*/,
					      int /*y1*/, int /*y2*/)
{
	VfDataset *ds = _manager->activeDataset();
	int act_fun = (int) ds->getActiveVelocityFunction();
	assert(act_fun != -1);

	if (act_fun != _picked_fun)
		ds->setActiveVelocityFunction((long) _picked_fun);

	int act_pck = (int) ds->getActivePick((long) _picked_fun);
	assert(act_pck != -1);

	if (act_pck != _picked_pck)
		ds->setActivePick((long) _picked_fun, (long) _picked_pck);
}

void VaCrossplotPicker::shiftButtonOnePress(int x, int y)
{
	float dist_mm;
	Vector *vector = _vectors->closest(x, y, getPlot(), &dist_mm);
	
	if (vector)
	{
		float pick_tol = _manager->utilities()->getPickingTolerance();

		if (dist_mm <= pick_tol)
		{
			_picked_fun = ((VaCrossplotPicks *) _picks)->
				getIndex(vector);
		}
		else
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

void VaCrossplotPicker::shiftButtonOneMotion(int /*x1*/, int /*x2*/,
					     int /*y1*/, int /*y2*/)
{
	/* wait for release */
}

void VaCrossplotPicker::shiftButtonOneRelease(int /*x1*/, int /*x2*/,
					      int /*y1*/, int /*y2*/)
{
	VfDataset *ds = _manager->activeDataset();
	int act_fun = (int) ds->getActiveVelocityFunction();
	assert(act_fun != -1);

	if (act_fun != _picked_fun)
		ds->setActiveVelocityFunction((long) _picked_fun);
}

VaCrossplotPicks::VaCrossplotPicks(VfManager *manager,
	class VfHorizons *horizons, VaPlot *plot,
	SeisPlot *sp, VaVectColors *colors)
	: VaPicks(manager, horizons, plot, sp, colors), _rbn_convert_init(0)
{
	/*
	 * _select_type is just used in method setSelectType.
	 */
	_select_type          = _colors->getSelectType        ();

	/*
	 * _overlay_type & _show_overlay_markers are accessed
	 * by VaCrossplotPickData thru methods.
	 */
	_overlay_type             = _colors->getOverlayType          ();
	_show_overlay_markers     = _colors->getShowOverlayMarkers   ();
	_show_overlay_active_pick = _colors->getShowOverlayActivePick();

	_data = new VaCrossplotPickData(_manager, this, sp, _editable_vectors,
		_colors);

	/*
	 * Add coupled cursor after all vector linked lists since
	 * if cursor is in rbn mode, it must repair exposes last.
	 */
	_xh_trans = new SemblanceCrossHairTranslator(this);
	
	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());
}

VaCrossplotPicks::~VaCrossplotPicks()
{
	/*
	 * _data is deleted in base class destructor.
	 */
	if (_picker)
		delete _picker;
	
	_cross_hairs.remove(top());
	
	delete _xh_trans;
}

int VaCrossplotPicks::getIndex(class Vector *vector)
{
	return ((VaCrossplotPickData *) _data)->getIndex(vector);
}

int VaCrossplotPicks::getPick(int pick)
{
	return VaCrossFuncData::xlateXindex(pick, _overlay_type);
}

int VaCrossplotPicks::getOverlayType()
{
	return _overlay_type;
}

int VaCrossplotPicks::getShowOverlayMarkers()
{
	return _show_overlay_markers;
}

int VaCrossplotPicks::getShowOverlayActivePick()
{
	return _show_overlay_active_pick;
}

void VaCrossplotPicks::init(SeisPlot *sp)
{
	_picker = new VaCrossplotPicker(sp, _manager, this, _editable_vectors);
}

/*
 * Need 6 points for VTNM with before and after.
 */
#define MIN_RBN 6

void VaCrossplotPicks::insStrt(int index, float x, float y)
{
	if (((VaCrossplotPickData *) _data)->activeFuncVisible())
	{
		((VaCrossplotPickData *) _data)->hideActivePick();

		initConvertFromVTNM(index, y, 0);

		if (_overlay_type == VTAV)
		{
			int numPts = getActiveFuncNumPts();

			_xs = new float[numPts + 1];
			_ys = new float[numPts + 1];
		}
		else
		{
			_xs = new float[MIN_RBN];
			_ys = new float[MIN_RBN];
		}

		int numRbn;

		_xs[0]  = x;
		_ys[0]  = y;
		numRbn = 1;
		convertFromVTNM(_xs, _ys, &numRbn);

		_rbn_data = new VectData(numRbn, _xs, _ys);

		if (numRbn == 1)
		{
			_rbn_vector = _rbn_vectors->add(_rbn_data,
				_colors->crossplotRbnColor       (),
				_colors->crossplotWidth          (),
				False                              ,
				Vector::SolidLine                  ,
				_colors->crossplotMarker         (),
				_colors->crossplotMarkerSize     (),
				_colors->crossplotMarkerLineWidth());

			_rbn_vector->makeVisible();
		}
		else
		{
			_rbn_vector = _rbn_vectors->add(_rbn_data,
				_colors->crossplotRbnColor(),
				_colors->crossplotWidth   (), True);
		}
	}
	else
	{
		_rbn_data = (VectData *) NULL;
	}
}

void VaCrossplotPicks::insDrag(int /*index*/, float x, float y)
{
	if (_rbn_data)
	{
		int numRbn;

		_xs[0]  = x;
		_ys[0]  = y;
		numRbn = 1;
		convertFromVTNM(_xs, _ys, &numRbn);

		_rbn_data->replace(0, numRbn, _xs, _ys);
	}
}

void VaCrossplotPicks::insDone(int /*index*/, float /*x*/, float /*y*/)
{
	if (_rbn_data)
	{
		_rbn_vectors->remove(_rbn_vector);
		delete _rbn_data;

		doneConvertFromVTNM();

		delete [] _xs;
		delete [] _ys;
	}
}

void VaCrossplotPicks::repStrt(int index, float x, float y)
{
	if (((VaCrossplotPickData *) _data)->activeFuncVisible())
	{
		int numPts = getActiveFuncNumPts();

		if (numPts)
		{
			initConvertFromVTNM(index, y, 1);

			if (_overlay_type == VTAV)
			{
				_xs = new float[numPts + 1];
				_ys = new float[numPts + 1];
			}
			else
			{
				_xs = new float[MIN_RBN];
				_ys = new float[MIN_RBN];
			}

			int numRbn;

			_xs[0]  = x;
			_ys[0]  = y;
			numRbn = 1;
			convertFromVTNM(_xs, _ys, &numRbn);

			_rbn_data = new VectData(numRbn, _xs, _ys);

			_rbn_vector = _rbn_vectors->add(_rbn_data,
				_colors->crossplotRbnColor(),
				_colors->crossplotWidth   (), True);
		}
		else
		{
			assert(0);
		}
	}
	else
	{
		_rbn_data = (VectData *) NULL;
	}
}

void VaCrossplotPicks::repDrag(int /*index*/, float x, float y)
{
	if (_rbn_data)
	{
		int numRbn;

		_xs[0]  = x;
		_ys[0]  = y;
		numRbn = 1;
		convertFromVTNM(_xs, _ys, &numRbn);

		_rbn_data->replace(0, numRbn, _xs, _ys);
	}
}

void VaCrossplotPicks::repDone(int /*index*/, float /*x*/, float /*y*/)
{
	if (_rbn_data)
	{
		_rbn_vectors->remove(_rbn_vector);
		delete _rbn_data;

		doneConvertFromVTNM();

		delete [] _xs;
		delete [] _ys;
	}
}

int VaCrossplotPicks::getActiveFuncNumPts()
{
	int retval;

	VfDataset *ds = _manager->activeDataset();
	long ack_fun  = ds->getActiveVelocityFunction();

	if (ack_fun != -1L)
		retval = (int) ds->numPicks(ack_fun);
	else
		retval = 0;

	return retval;
}

void VaCrossplotPicks::initConvertFromVTNM(int index, float y,
		int doing_replace)
{
	assert(!_rbn_convert_init);

	VfDataset *ds = _manager->activeDataset();
	long ack_fun  = ds->getActiveVelocityFunction();
	assert(ack_fun != -1L);
	int numPts = (int) ds->numPicks(ack_fun);

	if (index == -1)
	{
		for (_rbn_index = 0;
			(_rbn_index < numPts) &&
			(ds->getAbscissa(ack_fun, (long)_rbn_index, VTNM) <= y);
			_rbn_index++);

		_rbn_add_prev = _rbn_add_next = 0;
	}
	else
	{
		_rbn_index    =  index                          ;
		_rbn_add_prev = (index > 0                     );
		_rbn_add_next = (index < numPts - doing_replace);
	}

	_rbn_replacing = doing_replace;

	_rbn_convert_init = 1;
}

void VaCrossplotPicks::convertFromVTNM(float *xs, float *ys, int *num)
{
	assert(_rbn_convert_init);

	VfDataset *ds = _manager->activeDataset();
	long ack_fun  = ds->getActiveVelocityFunction();
	assert(ack_fun != -1L);

	float last_time, last_vtnm, last_vtav, term, vint;
	float this_time = ys[0];
	float this_vtnm = xs[0];

	switch (_overlay_type)
	{
		case VTNM:
			/* do nothing */
			break;
		case VTRM:
			/* do nothing
			 * For rubberbanding assume VTNM & VTRM
			 * are the same.  They are different only
			 * if ray tracing is on.
			 */
			break;
		case VTAV:
			/*
			 * We are assuming VTNM and VTRM to be equal
			 * in this calculation.
			 */
			if (_rbn_index == 0)
			{
				/* do nothing */
			}
			else
			{
				last_time = ds->getAbscissa(ack_fun,
					(long) (_rbn_index - 1), VTNM);

				last_vtnm = ds->getOrdinate(ack_fun,
					(long) (_rbn_index - 1), VTNM);

				last_vtav = ds->getOrdinate(ack_fun,
					(long) (_rbn_index - 1), VTAV);

				if (this_time > last_time)
				{
					term =
					  (this_vtnm * this_vtnm * this_time
					 - last_vtnm * last_vtnm * last_time)
					 / (this_time - last_time);

					if (term > 0.0F)
					{
					   vint = (float) sqrt((double) term);

					   xs[0] = (last_vtav * last_time
					      + vint * (this_time - last_time))
					      / this_time;
					}
					else
					{
					   xs[0] = -999.0F;
					}
				}
				else
				{
					xs[0] = -999.0F;
				}
			}
			break;
		case VTIN:
			/*
			 * We are assuming VTNM and VTRM to be equal
			 * in this calculation.
			 */
			if (_rbn_index == 0)
			{
				xs[1] = xs[0];
				ys[1] = ys[0] ;
				ys[0] = 0.0F;
			}
			else
			{
				last_time = ds->getAbscissa(ack_fun,
					(long) (_rbn_index - 1), VTNM);

				last_vtnm = ds->getOrdinate(ack_fun,
					(long) (_rbn_index - 1), VTNM);

				if (this_time > last_time)
				{
					term =
					  (this_vtnm * this_vtnm * this_time
					 - last_vtnm * last_vtnm * last_time)
					 / (this_time - last_time);

					if (term > 0.0F)
					   xs[0] = xs[1] =
						(float) sqrt((double) term);
					else
					   xs[0] = xs[1] = -999.0F;
				}
				else
				{
					xs[0] = xs[1] = -999.0F;
				}

				ys[0] = last_time;
				ys[1] = this_time;
			}

			*num = 2;
			break;
		default:
			assert(0);
	}

	int numPts = (int) ds->numPicks(ack_fun);
	int i;

	if (_rbn_add_prev)
	{
		assert(_rbn_index > 0);

		for (i = *num - 1; i >= 0; i--)
		{
			xs[i + 1] = xs[i];
			ys[i + 1] = ys[i];
		}

		xs[0] = ds->getOrdinate(ack_fun, (long) (_rbn_index - 1),
			_overlay_type);
		ys[0] = ds->getAbscissa(ack_fun, (long) (_rbn_index - 1),
			_overlay_type);

		(*num)++;
	}

	if (_rbn_add_next)
	{
		assert(_rbn_index + _rbn_replacing < numPts);

		float next_time, next_vtnm; 

		switch (_overlay_type)
		{
			case VTNM:
			case VTRM:
				xs[*num] = ds->getOrdinate(ack_fun,
					(long) (_rbn_index + _rbn_replacing),
					_overlay_type);
				ys[*num] = ds->getAbscissa(ack_fun,
					(long) (_rbn_index + _rbn_replacing),
					_overlay_type);
				(*num)++;
				break;
			case VTAV:
				for (i = _rbn_index + _rbn_replacing;
					i < numPts;
					this_time = next_time,
					this_vtnm = next_vtnm,
					(*num)++, i++)
				{
				   next_time = ds->getAbscissa(ack_fun,
					(long) i, VTNM);

				   next_vtnm = ds->getOrdinate(ack_fun,
					(long) i, VTNM);

				   if ((next_time  > this_time)
				    && (xs[*num-1] > 0.0F     ))
				   {
					term =
					  (next_vtnm * next_vtnm * next_time
					 - this_vtnm * this_vtnm * this_time)
					 / (next_time - this_time);

					if (term > 0.0F)
					{
					   vint = (float) sqrt((double) term);

					   xs[*num] = (xs[*num-1] * this_time
					      + vint * (next_time - this_time))
					      / next_time;
				   	}
				   	else
				   	{
					   xs[*num] = -999.0F;
				   	}
				   }
				   else
				   {
					xs[*num] = -999.0F;
				   }

				   ys[*num] = next_time;
				}

				break;
			case VTIN:
				next_time = ds->getAbscissa(ack_fun,
					(long) (_rbn_index + _rbn_replacing),
					VTNM);

				next_vtnm = ds->getOrdinate(ack_fun,
					(long) (_rbn_index +_rbn_replacing),
					VTNM);

				if (next_time > this_time)
				{
					term =
					  (next_vtnm * next_vtnm * next_time
					 - this_vtnm * this_vtnm * this_time)
					 / (next_time - this_time);

					if (term > 0.0F)
					   xs[*num] = xs[*num + 1] =
						(float) sqrt((double) term);
					else
					   xs[*num] = xs[*num + 1] = -999.0F;
				}
				else
				{
					xs[*num] = xs[*num + 1] = -999.0F;
				}

				ys[*num    ] = this_time;
				ys[*num + 1] = next_time;

				*num += 2;

				if (_rbn_index + _rbn_replacing + 1 < numPts)
				{
					xs[*num] = ds->getOrdinate(ack_fun,
						(long) (_rbn_index
							+ _rbn_replacing + 1),
						_overlay_type);
					ys[*num] = next_time;
					(*num)++;
				}

				break;
			default:
				assert(0);
		}
	}
}

void VaCrossplotPicks::doneConvertFromVTNM()
{
	assert(_rbn_convert_init);

	_rbn_convert_init = 0;
}

void VaCrossplotPicks::setShowFunc(int which, int set)
{
	_show_func[which] = set;

	if (which == VaVectColors::SEL)
	{
		if ((_colors->getSelectType() == VaVectColors::SEL_SEL)
		 &&  _colors->getEnableShowFuncs())
		{
			if (set)
				((VaCrossplotPickData *) _data)->on ();
			else
				((VaCrossplotPickData *) _data)->off();
		}
	}
	else
	{
		((VaCrossplotPickData *) _data)->updateLineColors();
	}
}

void VaCrossplotPicks::setSelectType(int set)
{
	assert(set != _select_type);

	if (_show_func[VaVectColors::SEL] && _colors->getEnableShowFuncs())
	{
		if (set == VaVectColors::SEL_SEL)
		{
			((VaCrossplotPickData *) _data)->on ();
		}
		else if (_select_type == VaVectColors::SEL_SEL)
		{
			((VaCrossplotPickData *) _data)->off();
		}
	}

	_select_type = set;
}

void VaCrossplotPicks::setEnableShowFuncs(int set)
{
	if ( _show_func[VaVectColors::SEL]
	 && (_colors->getSelectType() == VaVectColors::SEL_SEL))
	{
		if (set)
			((VaCrossplotPickData *) _data)->on ();
		else
			((VaCrossplotPickData *) _data)->off();
	}
}

void VaCrossplotPicks::setShowActiveFunc(int /*set*/)
{
	((VaCrossplotPickData *) _data)->updateLineColors();
}

void VaCrossplotPicks::setShowOverlayMarkers(int set)
{
	assert(set != _show_overlay_markers);

	int old_show_overlay_markers = _show_overlay_markers;
	_show_overlay_markers = set;

	 PickWatch * pick_watch = ( PickWatch *) 0;
	ShellWatch *shell_watch = (ShellWatch *) 0;

	if (_manager->activeDataset()->numSelectedVelocityFunctions()
		>= WATCH_THRESH)
	{
		setWatch(1);
		 pick_watch = new  PickWatch();
		shell_watch = new ShellWatch();
	}

	((VaCrossplotPickData *) _data)->setMarkers(
		old_show_overlay_markers, _show_overlay_markers);

	if (getWatch())
	{
		delete  pick_watch;
		delete shell_watch;
		setWatch(0);
	}
}

void VaCrossplotPicks::setOverlayType(int set)
{
	assert(set != _overlay_type);

	((VaCrossplotPickData *) _data)->off();

	_overlay_type = set;

	((VaCrossplotPickData *) _data)->on ();
}

void VaCrossplotPicks::setShowOverlayActiveFunc(int /*set*/)
{
	((VaCrossplotPickData *) _data)->updateLineColors();
}

void VaCrossplotPicks::setShowOverlayActivePick(int set)
{
	assert(set != _show_overlay_active_pick);

	int old_show_overlay_active_pick = _show_overlay_active_pick;
	_show_overlay_active_pick = set;

	if (_show_overlay_markers != VaVectColors::OVER_LINE)
		((VaCrossplotPickData *) _data)->setActivePicks(
		   old_show_overlay_active_pick, _show_overlay_active_pick);
}

char *VaCrossplotPicks::getClassName()
{
	static char *retval = VA_CROSSPLOT_PICKS;

	return retval;
}

VaPicker *VaCrossplotPicks::newSeparateWindow(SeisPlot *sp)
{
	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());
	
	return new VaCrossplotPicker(sp, _manager, this, _editable_vectors);
}
