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
// $Id: va_picks.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_picks.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/cross_hairs.hh"
#include "plot/plot_base.hh"
#include "oprim/static_utils.hh"
#include "sl/prim_support.hh"

#include <assert.h>

int VaPickData::_hold_count = 0;

VaPickData::VaPickData(class VfManager *manager, VaPicks *picks)
	: BaseData(), VfInform(manager), _picks(picks)
{
	/* just initializers */
}

VaPickData::~VaPickData()
{
	/* do nothing in base class destructor */
}

/*
 * adjustIndex is input an index value before an remove/insert
 * and returns the index value for the same object after the
 * remove/insert.  If the object was removed, returns -1.
 * Also returns -1 if the object was redrawn because of the
 * remove/insert if extra point is redrawn to each side
 * of the remove/insert region.
 */
int VaPickData::adjustIndex(int old_index, int rem_ins_index,
	int nrem, int nins, int extra)
{
	int retval;

	if (old_index < rem_ins_index - extra)
		retval = old_index;
	else if (old_index > rem_ins_index + nrem + extra - 1)
		retval = old_index - nrem + nins;
	else
		retval = -1;

	return retval;
}

/*
 * newIndexEffected returns 1 if if new_index was redrawn due to
 * the remove/insert, 0 if not.
 */
int VaPickData::newIndexEffected(int new_index, int rem_ins_index,
	int /*nrem*/, int nins, int extra)
{
	return (new_index >= rem_ins_index - extra           )
	    && (new_index <= rem_ins_index + nins + extra - 1);
}

int VaPickData::vaHoldVectors()
{
	int retval;

	if (_hold_count++ == 0)
	{
		assert(!SU::isHoldingVectors());
		SU::holdVectors();
		retval = 1;
	}
	else
	{
		assert( SU::isHoldingVectors());
		retval = 0;
		assert(0);
	}

	return retval;
}

int VaPickData::vaFlushVectors()
{
	assert(_hold_count && SU::isHoldingVectors());

	int retval;

	if (--_hold_count == 0)
	{
		SU::flushVectors();
		retval = 1;
	}
	else
	{
		retval = 0;
		assert(0);
	}

	return retval;
}

int VaPickData::vaIsHoldingVectors()
{
	if (_hold_count > 0)
		assert( SU::isHoldingVectors());
	else
		assert(!SU::isHoldingVectors());

	return _hold_count;
}

VaPicks *VaPickData::getPicks()
{
	return _picks;
}

VaPicker::VaPicker(PlotBase *plot, char *mode,
	const char * const helpToken, const char * const helpFallback,
	VfManager *manager, VaPicks *picks, VectorLinkedList *vectors)
	: PickBase(plot, mode, helpToken, helpFallback, XC_tcross, beep, True,
		&PrimSupport::updateEverything),
	  _manager(manager), _picks(picks), _vectors(vectors)
{
	/* just initializers */
}

VaPicker::~VaPicker()
{
	/* do nothing in base class destructor */
}

/*
 * bracketTime sets _insert, _index, _use_t_min, _use_t_max, _t_min, & _t_max
 */
void VaPicker::bracketTime(float time, int calc_time_tol, int vel_type)
{
	VfDataset *ds       = _manager->activeDataset();
  	long       ifun     = ds->getActiveVelocityFunction();
	int        numPicks = (int) ds->numPicks(ifun);

	float time_tol;
	if (calc_time_tol)
	{
		float mm2x, mm2y;
		getPlot()->MM2WC(&mm2x, &mm2y);
		time_tol = _manager->utilities()->getPickingTolerance() * mm2y;
	}
	else
	{
		/*
		 * If y-axis is depth instead of time, I should do
		 * some conversion here.  But form now I'll let it go.
		 */
		time_tol = _manager->utilities()->getTimeTolerance();
	}

	float pick_time;

	/*
	 * Init i_first_bigger to numPicks, then if none are
	 * bigger we are left with the correct result.
	 */
	int i_first_bigger = numPicks;

	for (_index = 0; _index < numPicks; _index++)
	{
		pick_time = ds->getAbscissa(ifun, (long) _index, vel_type);

		if (fabs((double) (pick_time - time)) <= time_tol)
		{
			_insert = 0;

			if (_index == 0)
			{
				_use_t_min = 0;
			}
			else
			{
				_use_t_min = 1;
				    _t_min = ds->getAbscissa(ifun,
					(long) (_index - 1), vel_type) +
					time_tol;
			}

			if (_index == numPicks - 1)
			{
				_use_t_max = 0;
			}
			else
			{
				_use_t_max = 1;
				    _t_max = ds->getAbscissa(ifun,
					(long) (_index + 1), vel_type) -
					time_tol;
			}

			return;
		}
		else if ((i_first_bigger == numPicks)
		      && (pick_time      >      time))
		{
			i_first_bigger = _index;
		}
	}

	/*
	 * If we get this far, we are inserting a new pick.
	 */
	_insert = 1;
	_index = i_first_bigger;

	if (_index == 0)
	{
		_use_t_min = 0;
	}
	else
	{
		_use_t_min = 1;
		    _t_min = ds->getAbscissa(ifun,
			(long) (_index - 1), vel_type) +
			time_tol;
	}

	if (_index == numPicks)
	{
		_use_t_max = 0;
	}
	else
	{
		_use_t_max = 1;
		    _t_max = ds->getAbscissa(ifun, (long) _index, vel_type)
			- time_tol;
	}
}

int VaPicker::canEdit()
{
	VfDataset *ds = _manager->activeDataset();

	return ds->isEditable() && ds->notLocked();
}

void VaPicker::buttonAny(int /*x1*/, int /*x2*/, int /*y1*/, int /*y2*/,
	int /*button*/, Action /*action*/, Modifier /*modifier*/)
{
	doBeep();
	ignoreActions();
}

VaSepWinElement::VaSepWinElement(SeisPlot *plot, VaPicker *pick)
	: _plot(plot), _pick(pick)
{
	/* just initializers */
}

VaSepWinElement::~VaSepWinElement()
{
	/* do nothing */
}

int VaSepWinElement::operator ==(void * const plot) const
{
	return((SeisPlot *) plot == _plot);
}

void VaSepWinElement::print() const
{
	cout << " " << _plot;
}

VaSepWinLinkedList::VaSepWinLinkedList()
{
	/* do nothing */
}

VaSepWinLinkedList::~VaSepWinLinkedList()
{
	/* do nothing */
}

void VaSepWinLinkedList::add(SeisPlot *plot, VaPicker *pick)
{
	VaSepWinElement *theElement = new VaSepWinElement(plot, pick);
	BaseLinkedList::add(theElement);
}

void VaSepWinLinkedList::remove(SeisPlot *plot)
{
	BaseLinkedList::remove((void *) plot);
}

SeisPlot *VaSepWinLinkedList::find(SeisPlot *plot, void **p)
{
	VaSepWinElement *ptr = (VaSepWinElement *)
		BaseLinkedList::find((void *) plot, p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *VaSepWinLinkedList::top    (void **p)
{
	VaSepWinElement *ptr = (VaSepWinElement *)
		BaseLinkedList::top    (p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *VaSepWinLinkedList::bottom (void **p)
{
	VaSepWinElement *ptr = (VaSepWinElement *)
		BaseLinkedList::bottom (p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *VaSepWinLinkedList::next   (void **p)
{
	VaSepWinElement *ptr = (VaSepWinElement *)
		BaseLinkedList::next   (p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *VaSepWinLinkedList::prev   (void **p)
{
	VaSepWinElement *ptr = (VaSepWinElement *)
		BaseLinkedList::prev   (p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *VaSepWinLinkedList::current(void **p)
{
	VaSepWinElement *ptr = (VaSepWinElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

VaPicker *VaSepWinLinkedList::picker(void **p)
{
	VaSepWinElement *ptr = (VaSepWinElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_pick : (VaPicker *) NULL);
}

/*
 * static variables
 */
VaPicks *VaPicks::_va_picks[_NUM_VA_DISPLAYS];
int VaPicks::_num_va_picks = 0;
CoupledCrossHairs VaPicks::_cross_hairs;

VaPicks::VaPicks(class VfManager *manager, class VfHorizons *horizons,
	class VaPlot *plot, SeisPlot *sp, VaVectColors *colors,
	int num_const_vect_ll)
	: SeisInform(),
	  _manager(manager), _horizons(horizons), _plot(plot), _colors(colors),
	  _picker((VaPicker *) NULL), _is_inited(0),
	  _num_const_vect_ll(num_const_vect_ll), _watch_on(0)
{
	int i;

	if (_num_const_vect_ll > 0)
	{
		_constant_vectors =
			new SeisVectLinkedList *[_num_const_vect_ll];

		for (i = 0; i < _num_const_vect_ll; i++)
		{
			_constant_vectors[i] = new SeisVectLinkedList();
			_constant_vectors[i]->addPlot(sp);
		}
	}

	/*
	 * Add _editable_vectors last so they will be on top of constant.
	 */
	_editable_vectors = new SeisVectLinkedList();
	_editable_vectors->addPlot(sp);

	     _rbn_vectors = new SeisVectLinkedList();
	     _rbn_vectors->addPlot(sp);

	/*
	 * addSeisPlot after vector addPlot will give vector
	 * postZoomSeparateWindow before VaPicks.
	 * This will make the vectors be redrawn on exposes
	 * before the rbn coupled cursor in the separate window.
	 */
	addSeisPlot(sp);

	_va_picks[_num_va_picks++] = this;
	assert(_num_va_picks <= _NUM_VA_DISPLAYS);

	for (i = 0; i < VaVectColors::NUM_SHOW_FUNC; i++)
		_show_func[i] = _colors->getShowFunc(i);

	if (PlotBase::watchOK())
		PlotBase::setWatchOK(False);

	_sep_wins = new VaSepWinLinkedList();
}

VaPicks::~VaPicks()
{
	SeisPlot *ptr;
	void *p;
	for (ptr = _sep_wins->top(&p); ptr; ptr = _sep_wins->next(&p))
	{
		delete _sep_wins->picker(&p);
		_cross_hairs.remove(ptr);
	}

	delete _sep_wins;

	if (_num_const_vect_ll > 0)
	{
		for (int i = 0; i < _num_const_vect_ll; i++)
			delete _constant_vectors[i];

		delete [] _constant_vectors;
	}

	delete _editable_vectors;
	delete      _rbn_vectors;

	/*
	 * _data is newed in derived class, so we would normally
	 * delete it there; however, _data must be around when
	 * vectors are deleted and derived destructors execute
	 * before base class.
	 */
	delete _data;
}

void VaPicks::expose(SeisPlot *sp, int /*x*/, int /*y*/,
	int /*width*/, int /*height*/)
{
	if (!_is_inited)
	{
		init(sp);
		_is_inited = 1;
	}
}

void VaPicks::preDataChange(SeisPlot * /*sp*/)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i] != this)
			_va_picks[i]->otherPreDataChange(this);
}

void VaPicks::prePlot(SeisPlot * /*sp*/)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i] != this)
			_va_picks[i]->otherPrePlot(this);
}

void VaPicks::newPlot(SeisPlot * /*sp*/)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i] != this)
			_va_picks[i]->otherNewPlot(this);
}

void VaPicks::noPlotDisplayed(SeisPlot * /*sp*/)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i] != this)
			_va_picks[i]->otherNoPlotDisplayed(this);
}

void VaPicks::preMovie(SeisPlot * /*sp*/, SeisPlot::MovieDir /*dir*/)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i] != this)
			_va_picks[i]->otherPreMovie(this);
}

void VaPicks::postMovie(SeisPlot * /*sp*/, SeisPlot::MovieDir /*dir*/)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i] != this)
			_va_picks[i]->otherPostMovie(this);
}

void VaPicks::postZoomSeparateWindow(SeisPlot * /*sp*/, SeisPlot *zoomsp)
{
	VaPicker *pick = newSeparateWindow(zoomsp);

	_sep_wins->add(zoomsp, pick);

	addSeisPlot(zoomsp);
}

void VaPicks::destroyed(SeisPlot *sp)
{
	void *p;
	assert(_sep_wins->find(sp, &p));
	delete _sep_wins->picker(&p);
	_sep_wins->remove(sp);
	_cross_hairs.remove(sp, 1);
}

void VaPicks::init(SeisPlot * /*sp*/)
{
	/* do nothing in base class */
}

void VaPicks::broadcastInsStrt(int index, float x, float y)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i]->getVelocityType() == getVelocityType())
			_va_picks[i]->insStrt(index, x, y);
}

void VaPicks::broadcastInsDrag(int index, float x, float y)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i]->getVelocityType() == getVelocityType())
			_va_picks[i]->insDrag(index, x, y);
}

void VaPicks::broadcastInsDone(int index, float x, float y)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i]->getVelocityType() == getVelocityType())
			_va_picks[i]->insDone(index, x, y);
}

void VaPicks::broadcastRepStrt(int index, float x, float y)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i]->getVelocityType() == getVelocityType())
			_va_picks[i]->repStrt(index, x, y);
}

void VaPicks::broadcastRepDrag(int index, float x, float y)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i]->getVelocityType() == getVelocityType())
			_va_picks[i]->repDrag(index, x, y);
}

void VaPicks::broadcastRepDone(int index, float x, float y)
{
	for (int i = 0; i < _num_va_picks; i++)
		if (_va_picks[i]->getVelocityType() == getVelocityType())
			_va_picks[i]->repDone(index, x, y);
}

void VaPicks::insStrt(int /*index*/, float /*x*/, float /*y*/)
{
	/* do nothing in base class */
}

void VaPicks::insDrag(int /*index*/, float /*x*/, float /*y*/)
{
	/* do nothing in base class */
}

void VaPicks::insDone(int /*index*/, float /*x*/, float /*y*/)
{
	/* do nothing in base class */
}

void VaPicks::repStrt(int /*index*/, float /*x*/, float /*y*/)
{
	/* do nothing in base class */
}

void VaPicks::repDrag(int /*index*/, float /*x*/, float /*y*/)
{
	/* do nothing in base class */
}

void VaPicks::repDone(int /*index*/, float /*x*/, float /*y*/)
{
	/* do nothing in base class */
}

void VaPicks::broadcastUpdateVectColors()
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->updateVectColors();
}

void VaPicks::broadcastSetShowFunc(int which, int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setShowFunc(which, set);
}

void VaPicks::broadcastSetSelectType(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setSelectType(set);
}

void VaPicks::broadcastSetIsoTSOverlay(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setIsoTSOverlay(set);
}

void VaPicks::broadcastSetEnableShowFuncs(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setEnableShowFuncs(set);
}

void VaPicks::broadcastSetCrossHairSize(int length, int width)
{
	CrossHairLinkedList *list = _cross_hairs.getList();
	PlotBase *ptr;
	void *p;
	for (ptr = list->top(&p); ptr; ptr = list->next(&p))
		list->crossHair(&p)->setParams((const char *) 0, length, width);
}

void VaPicks::broadcastSetCrossHairVisibility(int set)
{
	CrossHairLinkedList *list = _cross_hairs.getList();
	PlotBase *ptr;
	void *p;
	for (ptr = list->top(&p); ptr; ptr = list->next(&p))
		list->crossHair(&p)->setVisibility(set);
}

void VaPicks::broadcastSetShowActiveFunc(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setShowActiveFunc(set);
}

void VaPicks::broadcastSetShowHyper(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setShowHyper(set);
}

void VaPicks::broadcastSetShowOverlayMarkers(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setShowOverlayMarkers(set);
}

void VaPicks::broadcastSetOverlayType(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setOverlayType(set);
}

void VaPicks::broadcastSetShowDopplerMute(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setShowDopplerMute(set);
}

void VaPicks::broadcastSetDopplerMuteParameter(float set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setDopplerMuteParameter(set);
}

void VaPicks::broadcastSetShowOverlayActiveFunc(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setShowOverlayActiveFunc(set);
}

void VaPicks::broadcastSetShowOverlayActivePick(int set)
{
	for (int i = 0; i < _num_va_picks; i++)
		_va_picks[i]->setShowOverlayActivePick(set);
}

void VaPicks::updateVectColors()
{
	/* do nothing in base class */
}

void VaPicks::setShowFunc(int /*which*/, int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setSelectType(int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setIsoTSOverlay(int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setEnableShowFuncs(int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setShowActiveFunc(int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setShowHyper(int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setShowOverlayMarkers(int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setOverlayType(int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setShowDopplerMute(int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setDopplerMuteParameter(float /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setShowOverlayActiveFunc(int /*set*/)
{
	/* do nothing in base class */
}

void VaPicks::setShowOverlayActivePick(int /*set*/)
{
	/* do nothing in base class */
}

class VaPlot *VaPicks::getPlot()
{
	return _plot;
}

VaVectColors* VaPicks::getColors()
{
	return _colors;
}

void VaPicks::otherPreDataChange(VaPicks * /*other_class*/)
{
	/* do nothing in base class */
}

void VaPicks::otherPrePlot(VaPicks * /*other_class*/)
{
	/* do nothing in base class */
}

void VaPicks::otherNewPlot(VaPicks * /*other_class*/)
{
	/* do nothing in base class */
}

void VaPicks::otherNoPlotDisplayed(VaPicks * /*other_class*/)
{
	/* do nothing in base class */
}

void VaPicks::otherPreMovie(VaPicks * /*other_class*/)
{
	/* do nothing in base class */
}

void VaPicks::otherPostMovie(VaPicks * /*other_class*/)
{
	/* do nothing in base class */
}

VaPicks *VaPicks::getOtherClass(char *other_class_name)
{
	VaPicks *retval;

	int i;
	for (i = 0; i < _num_va_picks; i++)
		if (!strcmp(other_class_name, _va_picks[i]->getClassName()))
		{
			retval = _va_picks[i];
			break;
		}

	assert(i < _num_va_picks);

	return retval;
}

int VaPicks::getShowFunc(int which)
{
	return _show_func[which];
}

void VaPicks::setWatch(int set)
{
	assert(set != _watch_on);

	if (set)
	{
		_watch_was_ok = (int) PlotBase::watchOK();
		PlotBase::setWatchOK(True);
	}
	else
	{
		if (!_watch_was_ok)
			PlotBase::setWatchOK(False);
	}

	_watch_on = set;
}

int VaPicks::getWatch()
{
	return _watch_on;
}

void VaPicks::makeEditableVisible()
{
	_editable_vectors->makeVisible();
}

void VaPicks::makeEditableInvisible()
{
	_editable_vectors->makeInvisible();
}

int VaPicks::getVelocityType()
{
	return VTNM;
}
