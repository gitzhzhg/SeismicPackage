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
// $Id: va_cmp_picks.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_cmp_picks.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_xh_trans.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_moveout.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"

#include <assert.h>
#include <string.h>
#include <math.h>

VaHyperData::VaHyperData(VaCmpPickData *data, int ipick, int is_doppler_mute)
	: BaseData(),
	  _data(data), _ipick(ipick), _is_doppler_mute(is_doppler_mute)
{
	/* just initializers */
}

VaHyperData::~VaHyperData()
{
	/* do nothing */
}

void VaHyperData::setPick(int ipick)
{
	_ipick = ipick;
}

int VaHyperData::getNumPts(long /*id*/)
{
	SeisPlot *sp = _data->_sp;
	int mem_traces       = (int) sp->originalTraces();
        int displayed_traces = (int)sp->displayedTraces(sp->getCurrentPanel());
	_trace_offset        = (int) sp->currentFrame() * mem_traces;

	if (_is_doppler_mute)
	{
		if (!_data->displayDopplerMute())
			displayed_traces = 0;
	}
	else
	{
		VfDataset *ds = _data->manager()->activeDataset();
		long ifun = ds->getActiveVelocityFunction();
		float vel = ds->getOrdinate(ifun, (long) _ipick, VTNM);

		if (vel < 0.0F)
			displayed_traces = 0;
	}

	return displayed_traces;
}

float VaHyperData::getX(int i, long /*id*/)
{
	return (float) (i + 1);
}

float VaHyperData::getY(int i, long /*id*/)
{
	float retval;
	VfMoveout mo;
	VfDataset *ds = _data->manager()->activeDataset();
	long ifun = ds->getActiveVelocityFunction();
	float offset;

	if (_is_doppler_mute)
	{
		assert(_data->displayDopplerMute());

/*********************************
           ////////////////////////////////////////////////////////////////
           //--> This works except is not immediate when the doppler mute
           //--> parameter is changed:

		retval = _data->_sp->getHeaderFromTrace(
			_trace_offset + i + 1, 31);
           ////////////////////////////////////////////////////////////////
*********************************/

           ////////////////////////////////////////////////////////////////
           //--> Therefore we will stick with this for now:

		offset = _data->_sp->getHeaderFromTrace(
			_trace_offset + i + 1, 6);

		mo.getDopplerMute(ds, ifun, (float) _data->_sp->plottedTmax(),
		  0.016F,
		  ((VaCmpPicks *) _data->_picks)->getDopplerMuteParameter(),
		  1, &offset, &retval);
           ////////////////////////////////////////////////////////////////

	}
	else
	{
		if (((VaCmpPlot *) _data->_picks->getPlot())->nmcApplied())
		{
			offset = 0.0F;
		}
		else
		{
			offset = _data->_sp->getHeaderFromTrace(
				_trace_offset + i + 1, 6);
		}

		mo.getMoveoutTimes(ds, ifun, (long)_ipick, 1, &offset, &retval);
	}

	return retval;
}

VaCmpPickData::VaCmpPickData(VfManager *manager, VaPicks *picks, SeisPlot *sp,
	VectorLinkedList *vectors, VectorLinkedList *constant_vectors,
	VaVectColors *colors)
	: VaVectPickData(manager, picks, sp, vectors, colors),
	  _constant_vectors(constant_vectors), _dop_data((VaHyperData *) 0)
{
	/* just initializers */
}

VaCmpPickData::~VaCmpPickData()
{
	VfDataset *ds = manager()->activeDataset();
	long ifun = ds->getActiveVelocityFunction();

	if (ifun == -1)
		assert(_numVectUsed == 0);
	else
		assert(_numVectUsed == ds->numPicks(ifun));

	for (int i = 0; i < _numVectUsed; i++)
		delete _data[i];

	if (_dop_data)
		delete _dop_data;

	/*
	 * Vectors in _vect and _dop_vect are removed and deleted when
	 * their VectorLinkedList is deleted.
	 */
}

void VaCmpPickData::beforeChanges()
{
	assert(!vaIsHoldingVectors());

	_change_act_pick = _rem_ins_pick = 0;
}

void VaCmpPickData::afterChanges()
{
	assert(!vaIsHoldingVectors());

	if (_change_act_pick)
	{
		assert(_old_act_func == (int) manager()->activeDataset()->
			getActiveVelocityFunction());

		if ((_old_act_pick != -1)  && _rem_ins_pick)
			_old_act_pick = adjustIndex(_old_act_pick,
				_pick_index, _pick_nrem, _pick_nins, 0);

		int new_act_pick = (int) manager()->activeDataset()->
			getActivePick((long) _old_act_func);

		/*
		 * Only recolor if not redrawn by
		 * remove/insert
		 */
		if ((new_act_pick != -1) && _rem_ins_pick
		  && newIndexEffected(new_act_pick, _pick_index,
			_pick_nrem, _pick_nins, 0))
		{
			new_act_pick = -1;
		}

		if (_old_act_pick != new_act_pick)
		{
			if (_old_act_pick != -1)
				_vect[_old_act_pick]->setColor(
					_colors->cmpDefaultPickColor());

			if ( new_act_pick != -1)
				_vect[ new_act_pick]->setColor(
					_colors->cmpActivePickColor ());
		}
	}
}

void VaCmpPickData::preTotalChanges(VfDataset *dataset)
{
	if (manager()->activeDataset() == dataset)
	{
		long ifun = dataset->getActiveVelocityFunction();

		if (ifun >= 0)
		{
			assert(_numVectUsed == (int) dataset->numPicks(ifun));

			int need_hold =
				(_numVectUsed + displayDopplerMute()) > 1;

			if (need_hold)
				vaHoldVectors();

			for (int i = 0; i < _numVectUsed; i++)
			{
				_vectors->remove(_vect[i]);
				delete           _data[i] ;
			}

			if (_dop_data)
			{
				_constant_vectors->remove(_dop_vect);
				delete                    _dop_data ;
			}

			if (need_hold)
				vaFlushVectors();

			_numVectUsed = 0;
		}
	}
}

void VaCmpPickData::postTotalChanges(VfDataset *dataset)
{
	if (manager()->activeDataset() == dataset)
	{
		long ifun = dataset->getActiveVelocityFunction();

		if (ifun >= 0)
		{
			assert(_numVectUsed == 0);
			checkAllocation((int) dataset->numPicks(ifun));

			int active_pick = (int) dataset->getActivePick(ifun);
			const char *act_col = _colors->cmpActivePickColor ();
			const char *def_col = _colors->cmpDefaultPickColor();
			const char *color;

			for (int i = 0; i < _numVectUsed; i++)
			{
				_data[i] = new VaHyperData(this, i);

				if (i == active_pick)
					color = act_col;
				else
					color = def_col;

				_vect[i] = _vectors->add(_data[i], color,
					_colors->cmpWidth    (), False,
					_colors->cmpLineStyle());

				if (_colors->getShowActiveFunc()
				 && _colors->getShowHyper     ())
				{
					_vect[i]->makeVisible();
				}
			}

			_dop_data = new VaHyperData(this, 0, 1);
			_dop_vect = _constant_vectors->add(_dop_data,
				_colors->cmpDopplerMuteColor(),
				_colors->cmpWidth    (), False,
				_colors->cmpLineStyle());

			if (_colors->getShowActiveFunc())
				_dop_vect->makeVisible();
		}
	}
}

void VaCmpPickData::preNewActiveVelocityFunction(VfDataset *dataset)
{
	preTotalChanges(dataset);
}

void VaCmpPickData::postNewActiveVelocityFunction(VfDataset *dataset)
{
	postTotalChanges(dataset);
}

void VaCmpPickData::preModifyPicks(VfDataset *dataset,
	long ifun, int /*type*/, long /*ipick*/, long /*nrem*/)
{
	assert(manager()->activeDataset() == dataset);
	assert(dataset->getActiveVelocityFunction() == ifun);
	assert(_numVectUsed == (int) dataset->numPicks(ifun));

	/*
	 * Since all we need to know about old stuff is indices,
	 * everything can wait until postModifyPicks.
	 */
}

void VaCmpPickData::postModifyPicks(VfDataset *dataset,
	long ifun, int type, long ipick, long nrem, long nins)
{
	assert(manager()->activeDataset() == dataset);
	assert(dataset->getActiveVelocityFunction() == ifun);

	if (type != VTNM)
	{
		long new_num_picks = dataset->numPicks(ifun);
		long old_num_picks = new_num_picks - nins + nrem;
                //Following typo fixed by MLS on 03/26/01
		//assert(old_num_picks = _numVectUsed);
                assert(old_num_picks == _numVectUsed);
		nrem = old_num_picks - ipick;
		nins = new_num_picks - ipick;
	}

	int num_move = (nrem == nins) ? 0 : _numVectUsed - (int) (ipick + nrem);
	assert(num_move >= 0);
	checkAllocation(_numVectUsed + (int) (nins - nrem));
	assert(_numVectUsed == (int) dataset->numPicks(ifun));

	int i;

	/*
	 * Must reset ipick before you delete any vectors, or else
	 * you will have problems when vectors redraw because of
	 * deletions.  Not resetting ipick on data classes to be deleted;
	 * since we holdVectors if nrem > 1 this is not a problem.
	 */
	if (num_move)
	{
		for (	i = (int) (ipick + nrem);
			i < (int) (ipick + nrem) + num_move;
			i++)
		{
			((VaHyperData *) _data[i])->setPick(
				i + (int) (nins - nrem));
		}
	}

	int hold_em = (nrem + displayDopplerMute()) > 1;

	if (hold_em)
		vaHoldVectors();

	for (i = (int) ipick; i < (int) (ipick + nrem); i++)
	{
		_vectors->remove(_vect[i]);
		delete           _data[i];
	}

	assert(_dop_data);
	_constant_vectors->remove(_dop_vect);
	delete                    _dop_data ;

	if (hold_em)
		vaFlushVectors();
	assert(!vaIsHoldingVectors());

	if (num_move)
	{
		memmove((void *) &_vect[ipick + nins],
			(void *) &_vect[ipick + nrem],
			(size_t) num_move * sizeof(Vector      *));

		memmove((void *) &_data[ipick + nins],
			(void *) &_data[ipick + nrem],
			(size_t) num_move * sizeof(VaHyperData *));
	}

	int active_pick = (int) dataset->getActivePick(ifun);
	const char *act_col = _colors->cmpActivePickColor ();
	const char *def_col = _colors->cmpDefaultPickColor();
	const char *color;

	for (i = (int) ipick; i < (int) (ipick + nins); i++)
	{
		_data[i] = new VaHyperData(this, i);

		if (i == active_pick)
			color = act_col;
		else
			color = def_col;

		_vect[i] = _vectors->add(_data[i], color,
			_colors->cmpWidth(), False, _colors->cmpLineStyle(),
			Vector::NoMarker, 5U, 1U, (char *) NULL, "fixed",
			BaseData::defaultId, True);

		if (_colors->getShowActiveFunc() && _colors->getShowHyper())
			_vect[i]->makeVisible();
	}

	_dop_data = new VaHyperData(this, 0, 1);
	_dop_vect = _constant_vectors->add(_dop_data,
		_colors->cmpDopplerMuteColor(),
		_colors->cmpWidth    (), False,
		_colors->cmpLineStyle());

	if (_colors->getShowActiveFunc())
		_dop_vect->makeVisible();

	/*
	 * For recoloring in afterChanges
	 */
	_rem_ins_pick = 1;
	_pick_index = (int) ipick;
	_pick_nrem  = (int) nrem;
	_pick_nins  = (int) nins;
}

void VaCmpPickData::preNewActiveDataset()
{
	preTotalChanges(manager()->activeDataset());
}

void VaCmpPickData::postNewActiveDataset()
{
	postTotalChanges(manager()->activeDataset());
}

void VaCmpPickData::preNewActivePicks(VfDataset *dataset,
	long ifun, long nchng)
{
	if (manager()->activeDataset() == dataset)
	{
		_old_act_func = (int) dataset->getActiveVelocityFunction();

		if (((long) _old_act_func >= ifun        )
		 && ((long) _old_act_func <  ifun + nchng))
		{
			_change_act_pick = 1;
			_old_act_pick = (int) dataset->getActivePick(
				(long) _old_act_func);
		}
	}
}

void VaCmpPickData::postNewActivePicks(VfDataset * /*dataset*/,
	long /*ifun*/, long /*nchng*/)
{
	/* do it in afterChanges */
}

void VaCmpPickData::hideActivePick()
{
	assert(!vaIsHoldingVectors());
	VfDataset *ds = manager()->activeDataset();
	long act_fun = ds->getActiveVelocityFunction();

	if (act_fun != -1)
	{
		int act_pck = (int) ds->getActivePick(act_fun);

		if (act_pck != -1)
		   _vect[act_pck]->setColor(_colors->cmpDefaultPickColor());
	}
}

int  VaCmpPickData::displayDopplerMute()
{
	return ((VaCmpPlot  *) _picks->getPlot())->nmcApplied        ()
	    && ((VaCmpPicks *) _picks           )->getShowDopplerMute();
}

#define FALLBACK "mouse*VA_CMP_PICK: BTN#1: Edit or Insert Pick, BTN#2: Delete Pick"

VaCmpPicker::VaCmpPicker(PlotBase *plot, VfManager *manager,
	VaPicks *picks, VectorLinkedList *vectors)
	: VaPicker(plot, "", "VA_CMP_PICK", FALLBACK,
		manager, picks, vectors)
{
	/* just initializers */
}

VaCmpPicker::~VaCmpPicker()
{
	/* do nothing */
}

void VaCmpPicker::noModButtonOnePress(int x, int y)
{
	_started = 0;

	VfDataset *ds   = _manager->activeDataset();
	long       ifun = ds->getActiveVelocityFunction();
	float pick_tol  = _manager->utilities()->getPickingTolerance();

	float dist_mm;
	Vector *vector = _vectors->closest(x, y, getPlot(), &dist_mm);

	if (vector)
	{
		if (dist_mm <= pick_tol)
		{
			_insert = 0;
			_index  = ((VaCmpPicks *) _picks)->getIndex(vector);

			float mm2x, mm2y;
			getPlot()->MM2WC(&mm2x, &mm2y);
			float time_tol = pick_tol * mm2y;

			if (_index == 0)
			{
				_use_t_min = 0;
			}
			else
			{
				_use_t_min = 1;
				    _t_min = ds->getAbscissa(ifun,
					(long) (_index - 1), VTNM) + time_tol;
			}

			if (_index == (int) ds->numPicks(ifun) - 1)
			{
				_use_t_max = 0;
			}
			else
			{
				_use_t_max = 1;
				    _t_max = ds->getAbscissa(ifun,
					(long) (_index + 1), VTNM) - time_tol;
			}
		}
		else
		{
			_insert =  1;
			_index  = -1;	/* new point, can go anywhere */
			_use_t_min = 0;
			_use_t_max = 0;
		}
	}
	else
	{
		_insert    =  1;
		_index     = -1;
		_use_t_min =  0;
		_use_t_max =  0;
	}

	if (!canEdit())
	{
		if (_insert)
		{
			doBeep();
		}
		else
		{
			Bool watch_ok = PlotBase::watchOK();
			if (watch_ok)
				PlotBase::setWatchOK(False);

			ds->setActivePick(ifun, (long) _index);

			if (watch_ok)
				PlotBase::setWatchOK(True );
		}

		ignoreActions();
	}
}

void VaCmpPicker::noModButtonOneMotion(int x1, int x2,
				       int y1, int y2)
{
	if (!canEdit())
	{
		assert(!_insert);
		return;
	}

	SeisPlot *sp = (SeisPlot *) getPlot();
	int trace_offset = (int) (sp->currentFrame() * sp->originalTraces());

	float off1 = sp->getHeaderFromTrace(
		trace_offset + (int) (getPlot()->xWC(x1) + 0.5F), 6);
	float tim1 = getPlot()->yWC(y1);
	
	float off2 = sp->getHeaderFromTrace(
		trace_offset + (int) (getPlot()->xWC(x2) + 0.5F), 6);
	float tim2 = getPlot()->yWC(y2);
	
	VfMoveout mo;
	mo.getPickFromTwoPoints(_manager->activeDataset(),
		off1, tim1, off2, tim2, &_t, &_v);

	if (_use_t_min && (_t < _t_min))
		_t = _t_min;

	if (_use_t_max && (_t > _t_max))
		_t = _t_max;

	if (_started)
	{
		if (_insert)
			_picks->broadcastInsDrag(_index, _v, _t);
		else
			_picks->broadcastRepDrag(_index, _v, _t);
	}
	else
	{
		if (_insert)
		{
			_picks->broadcastInsStrt(_index, _v, _t);
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

			_picks->broadcastRepStrt(_index, _v, _t);
		}

		_started = 1;
	}
}

void VaCmpPicker::noModButtonOneRelease(int /*x1*/, int /*x2*/,
					int /*y1*/, int /*y2*/)
{
	if (_started)
	{
		VfDataset *ds = _manager->activeDataset();

		long ifun = ds->getActiveVelocityFunction();

		if (_insert)
		{
			assert(_index == -1);
			_picks->broadcastInsDone(_index, _v, _t);

			int numPicks = (int) ds->numPicks(ifun);

			float mm2x, mm2y;
			getPlot()->MM2WC(&mm2x, &mm2y);
			float time_tol = _manager->utilities()
				->getPickingTolerance() * mm2y;

			/*
			 * Init i_first_bigger to numPicks, then if none are
			 * bigger we are left with the correct result.
			 */
			int i_first_bigger = numPicks;
			int index;
			float pick_time;

			for (index = 0; index < numPicks; index++)
			{
				pick_time = ds->getAbscissa(ifun, (long) index,
					VTNM);

				if (fabs((double) (pick_time - _t)) <= time_tol)
				{
					ds->replacePick(ifun, (long) index,
						_t, _v, VTNM);
					return;
				}
				else if ((i_first_bigger == numPicks)
				      && (pick_time      >        _t))
				{
					i_first_bigger = index;
				}
			}

			/*
			 * If we get this far, we are inserting a new pick.
			 */
				ds->insertPick(ifun, (long) i_first_bigger,
					_t, _v, VTNM);
		}
		else
		{
			_picks->broadcastRepDone(             _index, _v, _t);
			ds->replacePick         (ifun, (long) _index, _t, _v,
				VTNM);
		}
	}
}

void VaCmpPicker::noModButtonTwoPress(int x, int y)
{
	float dist_mm;
	Vector *vector = _vectors->closest(x, y, getPlot(), &dist_mm);

	if (!vector || (dist_mm > _manager->utilities()->getPickingTolerance())
		|| !canEdit())
	{
		doBeep();
		ignoreActions();
	}
	else
	{
		_index = ((VaCmpPicks *) _picks)->getIndex(vector);
	}
}

void VaCmpPicker::noModButtonTwoMotion(int /*x1*/, int /*x2*/,
				       int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void VaCmpPicker::noModButtonTwoRelease(int /*x1*/, int /*x2*/,
					int /*y1*/, int /*y2*/)
{
	VfDataset *ds = _manager->activeDataset();
	long ifun = ds->getActiveVelocityFunction();
	assert(ifun != -1);
	ds->removePick(ifun, (long) _index);
}

VaCmpPicks::VaCmpPicks(VfManager *manager, class  VfHorizons *horizons,
	VaPlot *plot, SeisPlot *sp, VaVectColors *colors)
	: VaPicks(manager, horizons, plot, sp, colors, 1),
	  _numTracesAllocated(0) 
{
	_show_doppler_mute  = _colors->getShowDopplerMute     ();
	_doppler_mute_param = _colors->getDopplerMuteParameter();

	_data = new VaCmpPickData(_manager, this, sp, _editable_vectors,
		*_constant_vectors, _colors);

	/*
	 * Add coupled cursor after all vector linked lists since
	 * if cursor is in rbn mode, it must repair exposes last.
	 */
	_xh_trans = new CmpCrossHairTranslator(this);

	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());
}

VaCmpPicks::~VaCmpPicks()
{
	/*
	 * _data is deleted in base class destructor.
	 */
	if (_picker)
		delete _picker;
	
	if (_numTracesAllocated)
	{
		delete [] _offset    ;
		delete [] _x         ;
		delete [] _y         ;
		delete [] _x_in_range;
		delete [] _y_in_range;
	}

	_cross_hairs.remove(top());

	delete _xh_trans;
}

void VaCmpPicks::init(SeisPlot *sp)
{
	_picker = new VaCmpPicker(sp, _manager, this, _editable_vectors);
}

void VaCmpPicks::insStrt(int /*index*/, float x, float y)
{
	((VaCmpPickData *) _data)->hideActivePick();

	strtRbn(x, y);
}

void VaCmpPicks::insDrag(int /*index*/, float x, float y)
{
	dragRbn(x, y);
}

void VaCmpPicks::insDone(int /*index*/, float x, float y)
{
	doneRbn(x, y);
}

void VaCmpPicks::repStrt(int /*index*/, float x, float y)
{
	strtRbn(x, y);
}

void VaCmpPicks::repDrag(int /*index*/, float x, float y)
{
	dragRbn(x, y);
}

void VaCmpPicks::repDone(int /*index*/, float x, float y)
{
	doneRbn(x, y);
}

void VaCmpPicks::setShowActiveFunc(int set)
{
	if (!set)
		_data->vaHoldVectors();

	/*
	 * If ShowHyper is false, changing ShowActiveFunc
	 * will not change visibility.
	 */
	if (_colors->getShowHyper())
	{
		if (set)
			_editable_vectors->makeVisible  ();
		else
			_editable_vectors->makeInvisible();
	}

	if (set)
		(*_constant_vectors)->makeVisible  ();
	else
		(*_constant_vectors)->makeInvisible();

	if (!set)
		_data->vaFlushVectors();
}

void VaCmpPicks::setShowHyper(int set)
{
	/*
	 * If ShowActiveFunc is false, changing ShowHyper 
	 * will not change visibility.
	 */
	if (_colors->getShowActiveFunc())
	{
		if (set)
			_editable_vectors->makeVisible  ();
		else
			_editable_vectors->makeInvisible();
	}
}

void VaCmpPicks::setShowDopplerMute(int set)
{
	if (_colors->getShowActiveFunc())
		(*_constant_vectors)->makeInvisible();

	_show_doppler_mute = set;

	if (_colors->getShowActiveFunc())
		(*_constant_vectors)->makeVisible  ();
}

void VaCmpPicks::setDopplerMuteParameter(float set)
{
	if (_colors->getShowActiveFunc())
		(*_constant_vectors)->makeInvisible();

	_doppler_mute_param = set;

	if (_colors->getShowActiveFunc())
		(*_constant_vectors)->makeVisible  ();
}

char *VaCmpPicks::getClassName()
{
	static char *retval = VA_CMP_PICKS;

	return retval;
}

int VaCmpPicks::getIndex(Vector *vector)
{
	return ((VaCmpPickData *) _data)->getIndex(vector);
}

void VaCmpPicks::strtRbn(float v, float t)
{
	checkAllocation();

	if (_numTracesUsed)
	{
		VfMoveout mo;

		mo.getMoveoutTimes(_manager->activeDataset(), t, v,
			_numTracesUsed, _offset, _y);

		_num_in_range = checkHyperRange();

		_rbn_data = new VectData(_num_in_range,
			_x_in_range, _y_in_range);

		_rbn_vector = _rbn_vectors->add(_rbn_data,
			_colors->cmpRbnColor(), _colors->cmpWidth(), True);
	}
}

void VaCmpPicks::dragRbn(float v, float t)
{
	if (_numTracesUsed)
	{
		VfMoveout mo;

		mo.getMoveoutTimes(_manager->activeDataset(), t, v,
			_numTracesUsed, _offset, _y);

		int num_in_range = checkHyperRange();

		_rbn_data->replace(0, _num_in_range, num_in_range,
			_x_in_range, _y_in_range);

		_num_in_range = num_in_range;
	}
}

void VaCmpPicks::doneRbn(float /*v*/, float /*t*/)
{
	if (_numTracesUsed)
	{
		_rbn_vectors->remove(_rbn_vector);
		delete _rbn_data;
	}
}

#define _ALLOC_INC 100

void VaCmpPicks::checkAllocation()
{
//	assert(count() == 1);	/* zoom separate window allows > 1 */
	SeisPlot *sp = top();	/* I am a SeisInform, 1st SeisPlot is main */
	_numTracesUsed = (int) sp->originalTraces();
	int trace_offset = (int) sp->currentFrame() * _numTracesUsed;

	if (_numTracesAllocated < _numTracesUsed)
	{
		if (_numTracesAllocated)
		{
			delete [] _offset    ;
			delete [] _x         ;
			delete [] _x         ;
			delete [] _y_in_range;
			delete [] _y_in_range;
		}

		for ( ; _numTracesAllocated < _numTracesUsed;
			_numTracesAllocated += _ALLOC_INC);

		_offset     = new float[_numTracesAllocated];
		_x          = new float[_numTracesAllocated];
		_y          = new float[_numTracesAllocated];
		_x_in_range = new float[_numTracesAllocated];
		_y_in_range = new float[_numTracesAllocated];
	}

	for (int i = 0; i < _numTracesUsed; i++)
	{
		_x[i] = (float) (i + 1);

		/*
		 * Downcasting --- shame, shame, shame
		 */
		if (((VaCmpPlot *) _plot)->nmcApplied())
			_offset[i] = 0.0F;
		else
			_offset[i] = sp->getHeaderFromTrace(
				trace_offset + i + 1, 6);
	}
}

int VaCmpPicks::getShowDopplerMute()
{
	return _show_doppler_mute;
}

float VaCmpPicks::getDopplerMuteParameter()
{
	return _doppler_mute_param;
}

VaPicker *VaCmpPicks::newSeparateWindow(SeisPlot *sp)
{
	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());

	return new VaCmpPicker(sp, _manager, this, _editable_vectors);
}

/*
 * SGI display goes funky with rubberbanding hyperbola that is way
 * off scale due to zero or very low velocity.
 * Returns number in y-range, but does not set _num_in_range.
 */
int VaCmpPicks::checkHyperRange()
{
	int retval;

	SeisPlot *sp = top();	/* I am a SeisInform, 1st SeisPlot is main */
	float min = (float) sp->plottedTmin();
	float max = (float) sp->plottedTmax();
	if (min > max)
	{
		float tmp = min;
		      min = max;
		      max = tmp;
	}

	int i;
	for (retval = i = 0; i < _numTracesUsed; i++)
		if ((_y[i] >= min) && (_y[i] <= max))
		{
			_x_in_range[retval] = _x[i];
			_y_in_range[retval] = _y[i];
			retval++;
		}

	return retval;
}
