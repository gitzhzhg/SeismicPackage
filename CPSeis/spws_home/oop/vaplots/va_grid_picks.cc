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
// $Id: va_grid_picks.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_grid_picks.hh"
#include "vaplots/va_iso_picks.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_gvs_picks.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_xh_trans.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"

#include <assert.h>

VaGridPickData::VaGridPickData(VfManager *manager, VaPicks *picks,
	VaVectColors *colors)
	: VaPickData(manager, picks),
	  _colors(colors), _changing(0), _alloc4change(0), _doing_post(0),
	  _doing_total_changes(0), _doing_rem_ins_func(0),
          _coord_change_without_remove_or_insert(0)
{
	/* just initializers */
}

VaGridPickData::~VaGridPickData()
{
	if (_alloc4change)
	{
		delete [] _before ;
		delete [] _after  ;
		delete [] _changed;
	}
}

int VaGridPickData::getNumPts(long /*id*/)
{
	return (int) manager()->activeDataset()->numVelocityFunctions();
}

float VaGridPickData::getX(int i, long /*id*/)
{
	return manager()->activeDataset()->getXloc((long) i);
}

float VaGridPickData::getY(int i, long /*id*/)
{
	return manager()->activeDataset()->getYloc((long) i);
}

int VaGridPickData::getAltMarkerColor(int i, long /*id*/)
{
	int retval;

	VfDataset *ds = manager()->activeDataset();

	if (((VaGridPicks *) _picks)->getShowActiveFunc())
	{
		if (i == (int) ds->getActiveVelocityFunction())
		{
			retval = VaVectColors::ACT_COL;
		}
		else if (((VaGridPicks *) _picks)->getEnableShowFuncs())
		{
			if ( _picks->getShowFunc(VaVectColors::REF)
			  && (i == (int) ds->getReferenceVelocityFunction()))
			{
				retval = VaVectColors::REF_COL;
			}
			else if ( _picks->getShowFunc(VaVectColors::PIL)
			      && (i == (int) ds->findPrevXloc()))
			{
				retval = VaVectColors::PIL_COL;
			}
			else if ( _picks->getShowFunc(VaVectColors::NIL)
			      && (i == (int) ds->findNextXloc()))
			{
				retval = VaVectColors::NIL_COL;
			}
			else if ( _picks->getShowFunc(VaVectColors::PXL)
			      && (i == (int) ds->findPrevYloc()))
			{
				retval = VaVectColors::PXL_COL;
			}
			else if ( _picks->getShowFunc(VaVectColors::NXL)
			      && (i == (int) ds->findNextYloc()))
			{
				retval = VaVectColors::NXL_COL;
			}
			else if ( _picks->getShowFunc(VaVectColors::SEL)
			      && ((VaGridPicks *) _picks)->isSelected(i,
					_doing_post))
			{
				retval = VaVectColors::SEL_COL;
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
	}
	else if (((VaGridPicks *) _picks)->getEnableShowFuncs())
	{
		if ( _picks->getShowFunc(VaVectColors::REF)
		  && (i == (int) ds->getReferenceVelocityFunction()))
		{
			retval = VaVectColors::REF_COL;
		}
		else if ( _picks->getShowFunc(VaVectColors::SEL)
		      && ((VaGridPicks *) _picks)->isSelected(i,
				_doing_post))
		{
			retval = VaVectColors::SEL_COL;
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

int VaGridPickData::isChanging()
{
	return _changing;
}

int VaGridPickData::ignoreOther()
{
	return _doing_total_changes || _doing_rem_ins_func;
}

void VaGridPickData::beforeChanges()
{
	assert(!_changing && !((VaGridPicks *) _picks)->isChanging());
	_changing = 1;

	_using_mod = _rem_ins_func = _coord_change_without_remove_or_insert = 0;

	preCheckMarkersChanged();
}

void VaGridPickData::afterChanges()
{
	assert( _changing && !((VaGridPicks *) _picks)->isChanging());

	/*
	 * This if takes away a purify UMR if calling with
	 * _rem_ins_func = 0 and _func_index, _func_nrem,  &_func_nins unset.
	 */
	if (_rem_ins_func || _coord_change_without_remove_or_insert)
		postCheckMarkersChanged(0, 
                        _rem_ins_func | _coord_change_without_remove_or_insert,
                        _func_index, _func_nrem, _func_nins, _using_mod);
	else
		postCheckMarkersChanged(0);

	_doing_rem_ins_func = _doing_total_changes = 0;
	_changing = 0;
}

void VaGridPickData::preTotalChanges(VfDataset *dataset)
{
	assert(!_rem_ins_func);

	if (manager()->activeDataset() == dataset)
	{
		int numPts = getNumPts();
		modIndicesBefore(0, numPts);
		_using_mod = 1;

		/*
		 * For recoloring in afterChanges
		 */
		_rem_ins_func = 1     ;
		_func_index   = 0     ;
		_func_nrem    = numPts;

		_doing_total_changes = 1;
	}
}

void VaGridPickData::postTotalChanges(VfDataset *dataset)
{
	if (manager()->activeDataset() == dataset)
	{
		int numPts = getNumPts();
		modIndicesAfter(0, numPts);
		modDone();

		/*
		 * For recoloring in afterChanges
		 */
		_func_nins = numPts;
	}
}

void VaGridPickData::preNewActiveVelocityFunction(VfDataset * /*dataset*/)
{
	/* do it in afterChanges */
}

void VaGridPickData::postNewActiveVelocityFunction(VfDataset * /*dataset*/)
{
	/* do it in afterChanges */
}

void VaGridPickData::preRemoveInsertVelocityFunctions(VfDataset *dataset,
	long ifun, long nrem, long /*nins*/)
{
	assert(!_rem_ins_func);

	if (manager()->activeDataset() == dataset)
	{
		if (nrem)
		{
			modIndicesBefore((int) ifun, (int) nrem);
			_using_mod = 1;
		}

		/*
		 * For recoloring in afterChanges
		 */
		_rem_ins_func = 1;
		_func_index = (int) ifun;
		_func_nrem  = (int) nrem;

		_doing_rem_ins_func = 1;
	}
}

void VaGridPickData::postRemoveInsertVelocityFunctions(VfDataset *dataset,
	long ifun, long nrem, long nins)
{
	if (manager()->activeDataset() == dataset)
	{
		if (nrem)
		{
			modIndicesAfter((int) ifun, (int) nins);
			modDone();
		}
		else
		{
			/*
			 * Since no line, no need to erase anything if
			 * only inserting functions.
			 */
			modAttributes((int) ifun, (int) nins);
		}

		/*
		 * For recoloring in afterChanges
		 */
		_func_nins = (int) nins;
	}
}

void VaGridPickData::preChangeCoords(VfDataset *dataset, long ifun, long nchng)
{

        if(!_rem_ins_func)
          {
	  if (manager()->activeDataset() == dataset)
	    {
		modIndicesBefore((int) ifun, (int) nchng);
		_using_mod = 1;

		/*
		 * For recoloring in afterChanges
		 */
                _coord_change_without_remove_or_insert = 1;
		_rem_ins_func = 1;
		_func_index = (int) ifun;
		_func_nrem  = (int) nchng;
	    }
          }
}

void VaGridPickData::postChangeCoords(VfDataset *dataset, long ifun, long nchng)
{
        if(_coord_change_without_remove_or_insert)
          {
	  if (manager()->activeDataset() == dataset)
	   {
		modIndicesAfter((int) ifun, (int) nchng);
		modDone();

		/*
		 * For recoloring in afterChanges
		 */
		_func_nins = (int) nchng;
	   }
          }
}

void VaGridPickData::preNewActiveDataset()
{
	preTotalChanges(manager()->activeDataset());
}

void VaGridPickData::postNewActiveDataset()
{
	postTotalChanges(manager()->activeDataset());
}

void VaGridPickData::preCheckMarkersChanged()
{
	check_alloc4change();

	int num = (int) manager()->activeDataset()->numVelocityFunctions();

	for (int i = 0; i < num; i++)
		_before[i] = getAltMarkerColor(i);
}

void VaGridPickData::postCheckMarkersChanged(int doing_post,
	int rem_ins_or_coordchange, int func_index, int func_nrem, int func_nins,
	int using_mod)
{
	if (rem_ins_or_coordchange && (func_nins > func_nrem))
		check_alloc4change(1);

	int num = (int) manager()->activeDataset()->numVelocityFunctions();

	int i, num_changed;

	assert(!_doing_post);
	_doing_post = doing_post;

	for (i = 0; i < num; i++)
		_after[i] = getAltMarkerColor(i);

	/*
	 * If remove/insert func was done, adjust _before with _after.
	 */
	if (rem_ins_or_coordchange)
	{
		int old_num = num - func_nins + func_nrem;

		/*
		 * 1st adjust _before for remove/insert
		 */
		if ((func_nrem != func_nins)
		 && (old_num - func_index - func_nrem > 0))
		{
			memmove(_before + func_index + func_nins,
				_before + func_index + func_nrem,
				(size_t) (old_num - func_index - func_nrem)
					* sizeof(int));
		}

		/*
		 * Copy inserted stuff to _before from _after
		 */
		if (func_nins > 0)
			memcpy (_before + func_index,
				_after  + func_index,
				(size_t) func_nins * sizeof(int));

		/*
		 * Vector mods update one point to each side
		 */
		if (using_mod)
		{
			if (func_index > 0)
				_before[func_index-1] = _after[func_index-1];

			if (func_index + func_nins < num)
				_before[func_index + func_nins] =
				_after [func_index + func_nins];
		}
	}

	for (i = num_changed = 0; i < num; i++)
		if( _before[i] != _after[i])
			_changed[num_changed++] = i;

	if (num_changed)
		modAttributesByIndices(_changed, num_changed);

	_doing_post = 0;
}

void VaGridPickData::check_alloc4change(int save_before)
{
	int needed = (int) manager()->activeDataset()->numVelocityFunctions();

	if (needed > _alloc4change)
	{
		/*
		 * Alloc new _before before deleting old because
		 * you may have to save old stuff.
		 */
		int *new_before  = new int[needed];

		if (_alloc4change)
		{
			if (save_before)
				memcpy(new_before, _before,
					(size_t) _alloc4change * sizeof(int));

			delete [] _before ;
			delete [] _after  ;
			delete [] _changed;
		}

		_before  = new_before;
		_after   = new int[needed];
		_changed = new int[needed];

		_alloc4change = needed;
	}
}

#define FALLBACK "mouse*VA_GRID_PICK: BTN#1: Select inline, BTN#2: Select crossline\\nShift-BTN#1: Activate or Insert Velocity Function, Shift-BTN#2: Delete Velocity Function, Control-BTN#1: Select timeslice"

VaGridPicker::VaGridPicker(PlotBase *plot, VfManager *manager,
	VaPicks *picks, VectorLinkedList *vectors)
	: VaPicker(plot, "", "VA_GRID_PICK", FALLBACK,
		manager, picks, vectors)
{
	/* just initializers */
}

VaGridPicker::~VaGridPicker()
{
	/* do nothing */
}

void VaGridPicker::noModButtonOnePress(int x, int y)
{
	int index;
	Vector *vector      = _vectors->closestVertex(x, y, &index, getPlot());
	int     select_type = ((VaGridPicks *) _picks)->getSelectType();

	if (vector
	 && ((select_type == VaVectColors::ISO_SEL)
	  || (select_type == VaVectColors::SEL_SEL)))
	{
		VfDataset   *ds = _manager->activeDataset();
		VfUtilities *ut = _manager->utilities    ();
		float xloc = getPlot()->xWC(x);
		float yloc = ut->ybinCenter(ds->getYloc((long) index));

		((VaGridPicks *) _picks)->inlineStrt(xloc, yloc);
	}
	else
	{
		doBeep();
		ignoreActions();
	}
}

void VaGridPicker::noModButtonOneMotion(int /*x1*/, int   x2  ,
				        int /*y1*/, int /*y2*/)
{
	((VaGridPicks *) _picks)->inlineDrag(getPlot()->xWC(x2));
}

void VaGridPicker::noModButtonOneRelease(int /*x1*/, int   x2  ,
					 int /*y1*/, int /*y2*/)
{
	((VaGridPicks *) _picks)->inlineDone(getPlot()->xWC(x2));
}

void VaGridPicker::noModButtonTwoPress(int x, int y)
{
	int index;
	Vector *vector      = _vectors->closestVertex(x, y, &index, getPlot());
	int     select_type = ((VaGridPicks *) _picks)->getSelectType();

	if (vector
	 && ((select_type == VaVectColors::ISO_SEL)
	  || (select_type == VaVectColors::SEL_SEL)))
	{
		VfDataset   *ds = _manager->activeDataset();
		VfUtilities *ut = _manager->utilities    ();
		float xloc = ut->xbinCenter(ds->getXloc((long) index));
		float yloc = getPlot()->yWC(y);

		((VaGridPicks *) _picks)->crosslineStrt(xloc, yloc);
	}
	else
	{
		doBeep();
		ignoreActions();
	}
}

void VaGridPicker::noModButtonTwoMotion(int /*x1*/, int /*x2*/,
				        int /*y1*/, int   y2  )
{
	((VaGridPicks *) _picks)->crosslineDrag(getPlot()->yWC(y2));
}

void VaGridPicker::noModButtonTwoRelease(int /*x1*/, int /*x2*/,
					 int /*y1*/, int   y2  )
{
	((VaGridPicks *) _picks)->crosslineDone(getPlot()->yWC(y2));
}

void VaGridPicker::shiftButtonOnePress(int x, int y)
{
	float dist_mm;
	Vector *vector = _vectors->closestVertex(x, y, &_index, getPlot(),
		&dist_mm);

	if (vector)
	{
		float pick_tol = _manager->utilities()->getPickingTolerance();
		_insert = (dist_mm <= pick_tol) ? 0 : 1;
	}
	else
	{
		_index  = -1;
		_insert =  1;
	}

	if (_insert && !canEdit())
	{
		doBeep();
		ignoreActions();
	}
}

void VaGridPicker::shiftButtonOneMotion(int /*x1*/, int /*x2*/,
				        int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void VaGridPicker::shiftButtonOneRelease(int x1, int /*x2*/,
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
		float xloc = getPlot()->xWC(x1);
		float yloc = getPlot()->yWC(y1);

		if (_index == -1)
		{
			ds->findOrInsertVelfun(xloc, yloc);
		}
		else if ((ut->xbinNumber(ds->getXloc((long) _index))
		       == ut->xbinNumber(xloc                      ))
		      && (ut->ybinNumber(ds->getYloc((long) _index))
		       == ut->ybinNumber(yloc                      )))
		{
			 ds->setActiveVelocityFunction((long) _index);
		}
		else if (-1 != ds->findMatchingVelfun(xloc, yloc))
		{
			ds->setActiveVelocityFunction((long) _index);
		}
		else
		{
			ds->findOrInsertVelfun(xloc, yloc);
		}
	}
	else
	{
		_manager->activeDataset()->
			setActiveVelocityFunction((long) (long) _index);
	}
}

void VaGridPicker::shiftButtonTwoPress(int x, int y)
{
	float dist_mm;
	Vector *vector = _vectors->closestVertex(x, y, &_index, getPlot(),
		&dist_mm);
	float pick_tol = _manager->utilities()->getPickingTolerance();

	if (!vector || (dist_mm > pick_tol) || !canEdit())
	{
		doBeep();
		ignoreActions();
	}
}

void VaGridPicker::shiftButtonTwoMotion(int /*x1*/, int /*x2*/,
				        int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void VaGridPicker::shiftButtonTwoRelease(int /*x1*/, int /*x2*/,
					 int /*y1*/, int /*y2*/)
{
	_manager->activeDataset()->removeVelocityFunction((long) _index);
}

void VaGridPicker::cntlButtonOnePress(int x, int y)
{
	int index;
	Vector *vector      = _vectors->closestVertex(x, y, &index, getPlot());
	int     select_type = ((VaGridPicks *) _picks)->getSelectType();

	if (vector
	 && ((select_type == VaVectColors::ISO_SEL)
	  || (select_type == VaVectColors::SEL_SEL)))
	{
		float xloc = getPlot()->xWC(x);
		float yloc = getPlot()->yWC(y);

		((VaGridPicks *) _picks)->timesliceStrt(xloc, yloc);
	}
	else
	{
		doBeep();
		ignoreActions();
	}
}

void VaGridPicker::cntlButtonOneMotion(int /*x1*/, int x2,
				       int /*y1*/, int y2)
{
	((VaGridPicks *) _picks)->timesliceDrag(getPlot()->xWC(x2),
						getPlot()->yWC(y2));
}

void VaGridPicker::cntlButtonOneRelease(int /*x1*/, int x2,
				        int /*y1*/, int y2)
{
	((VaGridPicks *) _picks)->timesliceDone(getPlot()->xWC(x2),
						getPlot()->yWC(y2));
}

VaGridPicks::VaGridPicks(class VfManager *manager, class VfHorizons *horizons,
	class VaPlot *plot, SeisPlot *sp, class VaVectColors *colors)
	: VaPicks(manager, horizons, plot, sp, colors, 1), _changing(0),
	  _vaIsoPicks((VaIsoPicks *) 0), _vaGvsPicks((VaGvsPicks *) 0)
{
	_select_type       = _colors->getSelectType     ();
	_enable_show_funcs = _colors->getEnableShowFuncs();
	_show_active_func  = _colors->getShowActiveFunc ();

	_data = new VaGridPickData(_manager, this, _colors);

	Vector *vect = _editable_vectors->add(_data, BaseData::defaultId,
		_colors->gridDefaultColor   (),
		(unsigned int) 1, False, Vector::NoLine,
		_colors->gridMarker         (),
		_colors->gridMarkerSize     (),
		_colors->gridMarkerLineWidth());

	vect->allowAltMarkerColors(True);

	vect->setAltMarkerColor(VaVectColors::ACT_COL,
		_colors->gridActiveColor());
	vect->setAltMarkerColor(VaVectColors::REF_COL,
		_colors->getFuncColor(VaVectColors::REF));
	vect->setAltMarkerColor(VaVectColors::SEL_COL,
		_colors->getFuncColor(VaVectColors::SEL));
	vect->setAltMarkerColor(VaVectColors::PIL_COL,
		_colors->getFuncColor(VaVectColors::PIL));
	vect->setAltMarkerColor(VaVectColors::NIL_COL,
		_colors->getFuncColor(VaVectColors::NIL));
	vect->setAltMarkerColor(VaVectColors::PXL_COL,
		_colors->getFuncColor(VaVectColors::PXL));
	vect->setAltMarkerColor(VaVectColors::NXL_COL,
		_colors->getFuncColor(VaVectColors::NXL));

	vect->makeVisible();

	_va_grid_horizons_data = new VaGridHorizonsData(manager, _colors);

	_va_horizons           = new VaHorizons        (manager, horizons,
		_va_grid_horizons_data, _colors,
		_constant_vectors[0], _editable_vectors, 0, 0, 1);

	/*
	 * Add coupled cursor after all vector linked lists since
	 * if cursor is in rbn mode, it must repair exposes last.
	 */
	_xh_trans = new GridCrossHairTranslator(this);

	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());
}

VaGridPicks::~VaGridPicks()
{
	/*
	 * _data is deleted in base class destructor because it
	 * must outlast vectors.
	 */
	
	if (_picker)
		delete _picker;

	_cross_hairs.remove(top());

	delete _xh_trans;

	delete _va_horizons          ;
	delete _va_grid_horizons_data;
}

int VaGridPicks::isChanging()
{
	return _changing > 0;
}

void VaGridPicks::init(SeisPlot *sp)
{
	_picker = new VaGridPicker(sp, _manager, this, _editable_vectors);
}

void VaGridPicks::setShowFunc(int which, int set)
{
	((VaGridPickData *) _data)-> preCheckMarkersChanged( );

	_show_func[which] = set;

	((VaGridPickData *) _data)->postCheckMarkersChanged(0);
}

void VaGridPicks::setSelectType(int set)
{
	((VaGridPickData *) _data)-> preCheckMarkersChanged( );

	_select_type = set;

	((VaGridPickData *) _data)->postCheckMarkersChanged(0);
}

void VaGridPicks::setEnableShowFuncs(int set)
{
	((VaGridPickData *) _data)-> preCheckMarkersChanged( );

	_enable_show_funcs = set;

	((VaGridPickData *) _data)->postCheckMarkersChanged(0);
}

void VaGridPicks::setShowActiveFunc(int set)
{
	((VaGridPickData *) _data)-> preCheckMarkersChanged( );

	_show_active_func = set;

	((VaGridPickData *) _data)->postCheckMarkersChanged(0);
}

char *VaGridPicks::getClassName()
{
	static char *retval = VA_GRID_PICKS;

	return retval;
}

void VaGridPicks::otherPreDataChange(VaPicks *other_class)
{
	if (((VaGridPickData *) _data)->ignoreOther())
		return;

	if      ((other_class  == getVaIsoPicks()      )
	      && (_select_type == VaVectColors::ISO_SEL))
	{
		assert((_changing == 0)
			&& !((VaGridPickData *) _data)->isChanging());
		_changing = 1;

		((VaGridPickData *) _data)->preCheckMarkersChanged();
	}
	else if ((other_class  == getVaGvsPicks()      )
	      && (_select_type == VaVectColors::GVS_SEL))
	{
		assert((_changing == 0)
			&& !((VaGridPickData *) _data)->isChanging());
		_changing = 1;

		((VaGridPickData *) _data)->preCheckMarkersChanged();
	}
}

void VaGridPicks::otherPrePlot(VaPicks *other_class)
{
	if (((VaGridPickData *) _data)->ignoreOther())
		return;

	if (((other_class  == getVaIsoPicks()      )
	  && (_select_type == VaVectColors::ISO_SEL))
	 || ((other_class  == getVaGvsPicks()      )
	  && (_select_type == VaVectColors::GVS_SEL)))
	{
		assert((_changing == 1)
			&& !((VaGridPickData *) _data)->isChanging());
		_changing = 2;
	}
}

void VaGridPicks::otherNewPlot(VaPicks *other_class)
{
	if (((VaGridPickData *) _data)->ignoreOther())
		return;

	if      ((other_class  == getVaIsoPicks()      )
	      && (_select_type == VaVectColors::ISO_SEL))
	{
		assert((_changing == 2)
			&& !((VaGridPickData *) _data)->isChanging());

		((VaGridPickData *) _data)->postCheckMarkersChanged(1);

		_changing = 0;
	}
	else if ((other_class  == getVaGvsPicks()      )
	      && (_select_type == VaVectColors::GVS_SEL))
	{
		assert((_changing == 2)
			&& !((VaGridPickData *) _data)->isChanging());

		((VaGridPickData *) _data)->postCheckMarkersChanged(0);

		_changing = 0;
	}
}

void VaGridPicks::otherNoPlotDisplayed(VaPicks *other_class)
{
	if (((VaGridPickData *) _data)->ignoreOther())
		return;

	if (((other_class  == getVaIsoPicks()      )
	  && (_select_type == VaVectColors::ISO_SEL))
	 || ((other_class  == getVaGvsPicks()      )
	  && (_select_type == VaVectColors::GVS_SEL)))
	{
		assert(!((VaGridPickData *) _data)->isChanging());
		_changing = 0;
	}
}

void VaGridPicks::otherPreMovie(VaPicks *other_class)
{
	if (((VaGridPickData *) _data)->ignoreOther())
		return;

	/*
	 * VaGridPickData might be changing if new active func caused
	 * this movie frame change, so do not assert on _data)->isChanging(),
	assert(!_changing && !((VaGridPickData *) _data)->isChanging());
	 */
	assert(!_changing);
	_changing = 1;

	if ((other_class  == getVaIsoPicks()      )
	 && (_select_type == VaVectColors::ISO_SEL))
	{
		if (((VaIsoPlot *) getVaIsoPicks()->getPlot())->
			getPlottedLineType() != VaIsoPlot::TIMESLICE)
		{
			((VaGridPickData *) _data)->preCheckMarkersChanged();
		}
	}
}

void VaGridPicks::otherPostMovie(VaPicks *other_class)
{
	if (((VaGridPickData *) _data)->ignoreOther())
		return;

	if ((other_class  == getVaIsoPicks()      )
	 && (_select_type == VaVectColors::ISO_SEL))
	{
		if (((VaIsoPlot *) getVaIsoPicks()->getPlot())->
			getPlottedLineType() != VaIsoPlot::TIMESLICE)
		{
			((VaGridPickData *) _data)->postCheckMarkersChanged(0);
		}
	}

	_changing = 0;
}

VaIsoPicks *VaGridPicks::getVaIsoPicks()
{
	if (!_vaIsoPicks)
		_vaIsoPicks = (VaIsoPicks *) getOtherClass(VA_ISO_PICKS);

	return _vaIsoPicks;
}

VaGvsPicks *VaGridPicks::getVaGvsPicks()
{
	if (!_vaGvsPicks)
		_vaGvsPicks = (VaGvsPicks *) getOtherClass(VA_GVS_PICKS);

	return _vaGvsPicks;
}

int VaGridPicks::isSelected(int ifun, int doing_post)
{
	int retval;

	switch (_select_type)
	{
		case VaVectColors::ISO_SEL:
			retval = getVaIsoPicks()->withinDisplay((long) ifun,
				!doing_post);
			break;
		case VaVectColors::GVS_SEL:
			retval = getVaGvsPicks()->withinDisplay((long) ifun,
				!doing_post);
			break;
		case VaVectColors::SEL_SEL:
			retval = _manager->activeDataset()->
				velocityFunctionIsSelected((long) ifun);
			break;
		case VaVectColors::GEO_SEL:
			retval = 0;
			assert(0);
			break;
		default:
			retval = 0;
			assert(0);
	}

	return retval;
}

int VaGridPicks::getSelectType()
{
	return _select_type;
}

int VaGridPicks::getEnableShowFuncs()
{
	return _enable_show_funcs;
}

int VaGridPicks::getShowActiveFunc()
{
	return _show_active_func;
}

void VaGridPicks::inlineStrt(float x, float y)
{
	float xs[2], ys[2];
	xs[0] = xs[1] = x;
	ys[0] = ys[1] = y;

	_extra_rbn_data = new VectData(2, xs, ys);

	_extra_rbn_vector = _rbn_vectors->add(_extra_rbn_data,
		_colors->gridRbnColor(),
		_colors->gridRbnWidth(),
		True                   );
}

void VaGridPicks::inlineDrag(float x2)
{
	float y2 = _extra_rbn_data->getY(1);
	_extra_rbn_data->replace(1, 1, &x2, &y2);
}

void VaGridPicks::inlineDone(float x2)
{
	float x1 = _extra_rbn_data->getX(0);
	float y1 = _extra_rbn_data->getY(0);
	float y2 = _extra_rbn_data->getY(1);
	float tmp;

	_rbn_vectors->remove(_extra_rbn_vector);
	delete _extra_rbn_data;

	switch (_select_type)
	{
		case VaVectColors::ISO_SEL:
			if (x2 < x1)
			{
				tmp = x2 ;
				x2  = x1 ;
				x1  = tmp;
			}

			((VaIsoPlot *) getVaIsoPicks()->getPlot())->
				changePlot(x1, x2, y1, y2);
			break;
		case VaVectColors::GVS_SEL:
			assert(0);
			break;
		case VaVectColors::SEL_SEL:
			selectFuncs(x1, x2, y1, y2);
			break;
		case VaVectColors::GEO_SEL:
			assert(0);
			break;
		default:
			assert(0);
	}
}

void VaGridPicks::crosslineStrt(float x, float y)
{
	float xs[2], ys[2];
	xs[0] = xs[1] = x;
	ys[0] = ys[1] = y;

	_extra_rbn_data = new VectData(2, xs, ys);

	_extra_rbn_vector = _rbn_vectors->add(_extra_rbn_data,
		_colors->gridRbnColor(),
		_colors->gridRbnWidth(),
		True                   );
}

void VaGridPicks::crosslineDrag(float y2)
{
	float x2 = _extra_rbn_data->getX(1);
	_extra_rbn_data->replace(1, 1, &x2, &y2);
}

void VaGridPicks::crosslineDone(float y2)
{
	float x1 = _extra_rbn_data->getX(0);
	float y1 = _extra_rbn_data->getY(0);
	float x2 = _extra_rbn_data->getX(1);
	float tmp;

	_rbn_vectors->remove(_extra_rbn_vector);
	delete _extra_rbn_data;

	switch (_select_type)
	{
		case VaVectColors::ISO_SEL:
			if (y2 < y1)
			{
				tmp = y2 ;
				y2  = y1 ;
				y1  = tmp;
			}

			((VaIsoPlot *) getVaIsoPicks()->getPlot())->
				changePlot(x1, x2, y1, y2);
			break;
		case VaVectColors::GVS_SEL:
			assert(0);
			break;
		case VaVectColors::SEL_SEL:
			selectFuncs(x1, x2, y1, y2);
			break;
		case VaVectColors::GEO_SEL:
			assert(0);
			break;
		default:
			assert(0);
	}
}

void VaGridPicks::timesliceStrt(float x, float y)
{
	float xs[5], ys[5];
	xs[0] = xs[1] = xs[2] = xs[3] = xs[4] = x;
	ys[0] = ys[1] = ys[2] = ys[3] = ys[4] = y;

	_extra_rbn_data = new VectData(5, xs, ys);

	_extra_rbn_vector = _rbn_vectors->add(_extra_rbn_data,
		_colors->gridRbnColor(),
		_colors->gridRbnWidth(),
		True                   );
}

void VaGridPicks::timesliceDrag(float x2, float y2)
{
	float xs[3], ys[3];
	xs[0] = _extra_rbn_data->getX(0);
	xs[1] = xs[2] = x2;
	ys[0] = ys[1] = y2;
	ys[2] = _extra_rbn_data->getY(0);

	_extra_rbn_data->replace(1, 3, xs, ys);
}

void VaGridPicks::timesliceDone(float x2, float y2)
{
	float x1 = _extra_rbn_data->getX(0);
	float y1 = _extra_rbn_data->getY(0);
	float tmp;

	_rbn_vectors->remove(_extra_rbn_vector);
	delete _extra_rbn_data;

	switch (_select_type)
	{
		case VaVectColors::ISO_SEL:
			if (x2 < x1)
			{
				tmp = x2 ;
				x2  = x1 ;
				x1  = tmp;
			}

			if (y2 < y1)
			{
				tmp = y2 ;
				y2  = y1 ;
				y1  = tmp;
			}

			((VaIsoPlot *) getVaIsoPicks()->getPlot())->
				changePlot(x1, x2, y1, y2);
			break;
		case VaVectColors::GVS_SEL:
			assert(0);
			break;
		case VaVectColors::SEL_SEL:
			selectFuncs(x1, x2, y1, y2);
			break;
		case VaVectColors::GEO_SEL:
			assert(0);
			break;
		default:
			assert(0);
	}
}

void VaGridPicks::selectFuncs(float x1, float x2, float y1, float y2)
{
	VfDataset   *ds = _manager->activeDataset();
	VfUtilities *ut = _manager->utilities    ();
	int   num = (int) ds->numVelocityFunctions();
	long *sel_funs = new long[num];
	int   num_sels = 0;
	int   i;

	/*
	 * Select single func
	 */
	if      ((x1 == x2) && (y1 == y2))
	{
		int index;
		Vector *vect = _editable_vectors->closestVertex(x1, y1, &index);
		assert(vect);

		sel_funs[num_sels++] = (long) index;
	}
	/*
	 * Select along inline
	 */
	else if (               y1 == y2 )
	{
		if (x1 > x2)
		{
			float temp = x1;
			x1 = x2;
			x2 = temp;
		}

		long line_bin = ut->ybinNumber(y1);
		long bin;
		float x;

		for (i = 0; i < num; i++)
		{
			bin = ut->ybinNumber(ds->getYloc((long) i));
			x   =                ds->getXloc((long) i) ;

			if ((bin == line_bin) && (x >= x1) && (x <= x2))
				sel_funs[num_sels++] = (long) i;
		}
	}
	/*
	 * Select along crossline
	 */
	else if ( x1 == x2               )
	{
		if (y1 > y2)
		{
			float temp = y1;
			y1 = y2;
			y2 = temp;
		}

		long line_bin = ut->xbinNumber(x1);
		long bin;
		float y;

		for (i = 0; i < num; i++)
		{
			bin = ut->xbinNumber(ds->getXloc((long) i));
			y   =                ds->getYloc((long) i) ;

			if ((bin == line_bin) && (y >= y1) && (y <= y2))
				sel_funs[num_sels++] = (long) i;
		}
	}
	/*
	 * Select area
	 */
	else
	{
		if (x1 > x2)
		{
			float temp = x1;
			x1 = x2;
			x2 = temp;
		}

		if (y1 > y2)
		{
			float temp = y1;
			y1 = y2;
			y2 = temp;
		}

		float x, y;

		for (i = 0; i < num; i++)
		{
			x = ds->getXloc((long) i);
			y = ds->getYloc((long) i);

			if ((x >= x1) && (x <= x2) && (y >= y1) && (y <= y2))
				sel_funs[num_sels++] = (long) i;
		}
	}

	if ((num_sels > 1) || _colors->getDeselectFirst())
		ds->beforeSettingSeveralSelectFlags();

	if (_colors->getDeselectFirst())
		ds->clearSelectFlags();

	for (i = 0; i < num_sels; i++)
		ds->setSelectFlag(sel_funs[i], SELECT_YES);

	if ((num_sels > 1) || _colors->getDeselectFirst())
		ds-> afterSettingSeveralSelectFlags();

	delete [] sel_funs;
}

VaPicker *VaGridPicks::newSeparateWindow(SeisPlot *sp)
{
	int length, width;
	_colors->getCrossHairSize(&length, &width);
	_cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
		length, width, _colors->getCrossHairVisibility());

	return new VaGridPicker(sp, _manager, this, _editable_vectors);
}

VaGridHorizonsData::VaGridHorizonsData(VfManager *vf_manager,
	VaVectColors *colors)
	: VaHorizonsData(vf_manager, colors)
{
	/* just initializers */
}

VaGridHorizonsData::~VaGridHorizonsData()
{
	/* do nothing */
}

int VaGridHorizonsData::getNumPts(long id)
{
	SortedHorizon *sh = _va_horizons_manager->getSortedHorizon(id);

	return sh->getNumPicks();
}

float VaGridHorizonsData::getX(int i, long id)
{
	return getHorizonStuct(i, id)->x;
}

float VaGridHorizonsData::getY(int i, long id)
{
	return getHorizonStuct(i, id)->y;
}

SortedHorizon::HorizonStruct *VaGridHorizonsData::getHorizonStuct(int i,
	long id)
{
	SortedHorizon *sh = _va_horizons_manager->getSortedHorizon(id);
	SortedHorizon::HorizonStruct *hs = sh->getUnsorted();

	return hs + i;
}

int VaGridHorizonsData::getMarkerType(int /*i*/, long /*id*/)
{
	int retval;

	switch (_show_horizon_markers)
	{
		case VaVectColors::OVER_LINE:
			retval = (int) Vector::NoMarker;
			break;
		case VaVectColors::OVER_BOTH:
		case VaVectColors::OVER_MARK:
			retval = (int) Vector::CrossMarker;
			break;
		default:
			assert(0);
	}

	return retval;
}

int VaGridHorizonsData::getLineStyle(long /*id*/)
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

int VaGridHorizonsData::getPenLift(int i, long id)
{
	int retval;

	if (i == 0)
	{
		retval = 0;
	}
	else
	{
		SortedHorizon *sh = _va_horizons_manager->getSortedHorizon(id);

		retval = !sh->onSameLine(i, i - 1);
	}

	return retval;
}
