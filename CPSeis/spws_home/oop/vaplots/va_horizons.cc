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
// $Id: va_horizons.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_horizons.hh"
#include "vaplots/va_vect_colors.hh"
#include "vf/vf_horizons.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_informer.hh"
#include "vect/ll_seis_vect.hh"
#include "plot/plot_base.hh"
#include "plot/pick_watch.hh"
#include "sl/shell_watch.hh"
#include "oprim/static_utils.hh"
#include "named_constants.h"
/*
#include "cprim.h"	/ * for ZNIL * /
*/

#include <stdlib.h>
#include <assert.h>

/*
 * Static Variables.
 */
VfManager *SortedHorizon::_vf_manager = (VfManager *) 0;

SortedHorizon::SortedHorizon(VfManager *vf_manager, VfHorizons *vf_horizons,
	long ihorizon)
	: _vf_horizons(vf_horizons), _ihorizon(ihorizon), _sorted_lines(0)
{
	if (_vf_manager)
		assert(_vf_manager == vf_manager);
	else
		       _vf_manager =  vf_manager ;

	_num_picks = (int) vf_horizons->numPicks(ihorizon);

	if (_num_picks == 0)
		return;

	_hs = new HorizonStruct[_num_picks];

	_sorted_hs[   INLINE] = new HorizonStruct * [_num_picks];
	_sorted_hs[CROSSLINE] = new HorizonStruct * [_num_picks];
	_sorted_hs[     TIME] = new HorizonStruct * [_num_picks];

	loadData ();
	sortLines();

	setGotLineNumbers();
}

SortedHorizon::~SortedHorizon()
{
	if (_num_picks)
	{
		assert(_sorted_lines);

		delete [] _x_bin_index;
		delete [] _y_bin_index;

		delete []  _sorted_hs[   INLINE];
		delete []  _sorted_hs[CROSSLINE];
		delete []  _sorted_hs[     TIME];

		delete [] _hs;
	}
}

void SortedHorizon::loadData()
{
	if (_num_picks == 0)
		return;

	_vf_manager->informer()->showMessage("loading horizon for displays...");

	for (int i = 0; i < _num_picks; i++)
	{
	    switch (_vf_manager->activeDataset()->getNhx())
	    {
		case  7:
		    _hs[i].x = _vf_horizons->getXgrid     (_ihorizon, (long) i);
		    break;
		case  8:
		    _hs[i].x = _vf_horizons->getYgrid     (_ihorizon, (long) i);
		    break;
		case 17:
		    _hs[i].x = _vf_horizons->getXloc      (_ihorizon, (long) i);
		    break;
		case 18:
		    _hs[i].x = _vf_horizons->getYloc      (_ihorizon, (long) i);
		    break;
		case 37:
		    _hs[i].x = _vf_horizons->getShotpoint (_ihorizon, (long) i);
		    break;
		case 38:
		    _hs[i].x = _vf_horizons->getLineNumber(_ihorizon, (long) i);
		    break;
		default:
		    assert(0);
	    }

	    switch (_vf_manager->activeDataset()->getNhy())
	    {
		case  7:
		    _hs[i].y = _vf_horizons->getXgrid     (_ihorizon, (long) i);
		    break;
		case  8:
		    _hs[i].y = _vf_horizons->getYgrid     (_ihorizon, (long) i);
		    break;
		case 17:
		    _hs[i].y = _vf_horizons->getXloc      (_ihorizon, (long) i);
		    break;
		case 18:
		    _hs[i].y = _vf_horizons->getYloc      (_ihorizon, (long) i);
		    break;
		case 37:
		    _hs[i].y = _vf_horizons->getShotpoint (_ihorizon, (long) i);
		    break;
		case 38:
		    _hs[i].y = _vf_horizons->getLineNumber(_ihorizon, (long) i);
		    break;
		default:
		    assert(0);
	    }

	    _hs[i].t = _vf_horizons->getTime (_ihorizon, (long) i) / 1000.F;

	    _sorted_hs[   INLINE][i] =
	    _sorted_hs[CROSSLINE][i] =
	    _sorted_hs[     TIME][i] = &_hs[i];
	}

	_vf_manager->informer()->showMessage("sorting horizon by time...");

	qsort((void *) _sorted_hs[TIME], (size_t) _num_picks,
		sizeof(HorizonStruct *), timeComparFunc);
}

void SortedHorizon::sortLines()
{
	if (_num_picks == 0)
		return;

	_vf_manager->informer()->showMessage("sorting horizon into inlines...");

	qsort((void *) _sorted_hs[   INLINE], (size_t) _num_picks,
		sizeof(HorizonStruct *),    inlineComparFunc);

	_vf_manager->informer()->showMessage(
		"sorting horizon into crosslines...");

	qsort((void *) _sorted_hs[CROSSLINE], (size_t) _num_picks,
		sizeof(HorizonStruct *), crosslineComparFunc);

	VfUtilities *ut = _vf_manager->utilities();

	_min_x_bin = (int) ut->xbinNumber(
		_sorted_hs[CROSSLINE][             0]->x);

	_max_x_bin = (int) ut->xbinNumber(
		_sorted_hs[CROSSLINE][_num_picks - 1]->x);

	_min_y_bin = (int) ut->ybinNumber(
		_sorted_hs[   INLINE][             0]->y);

	_max_y_bin = (int) ut->ybinNumber(
		_sorted_hs[   INLINE][_num_picks - 1]->y);

	if (_sorted_lines)
	{
		delete [] _x_bin_index;
		delete [] _y_bin_index;
	}
	else
	{
		_sorted_lines = 1;
	}

	_x_bin_index = new int[_max_x_bin - _min_x_bin + 1];
	_y_bin_index = new int[_max_y_bin - _min_y_bin + 1];

	int bin_value, bin_index, i;

	for (_x_bin_index[0] = 0, bin_value = _min_x_bin, bin_index = i = 1;
		bin_value < _max_x_bin; )
	{
		if ((int) ut->xbinNumber(_sorted_hs[CROSSLINE][i]->x)
			> bin_value)
		{
			_x_bin_index[bin_index++] = i;
			bin_value++;
		}
		else
		{
			i++;
		}
	}

	for (_y_bin_index[0] = 0, bin_value = _min_y_bin, bin_index = i = 1;
		bin_value < _max_y_bin; )
	{
		if ((int) ut->ybinNumber(_sorted_hs[INLINE][i]->y) > bin_value)
		{
			_y_bin_index[bin_index++] = i;
			bin_value++;
		}
		else
		{
			i++;
		}
	}
}

SortedHorizon::HorizonStruct **SortedHorizon::getSorted(SortBy sort_by)
{
	assert(_num_picks > 0);

	return _sorted_hs[sort_by];
}

SortedHorizon::HorizonStruct *SortedHorizon::getUnsorted()
{
	assert(_num_picks > 0);

	return _hs;
}

int SortedHorizon::getNumPicks()
{
	return _num_picks;
}

int SortedHorizon::inlineComparFunc(const void *element1, const void *element2)
{
	int retval;

	VfUtilities *ut = _vf_manager->utilities();

	int ybin1 = (int) ut->ybinNumber((*((HorizonStruct **) element1))->y);
	int ybin2 = (int) ut->ybinNumber((*((HorizonStruct **) element2))->y);

	if      (ybin1 < ybin2)
	{
		retval = -1;
	}
	else if (ybin1 > ybin2)
	{
		retval =  1;
	}
	else
	{
		float x1 = (*((HorizonStruct **) element1))->x;
		float x2 = (*((HorizonStruct **) element2))->x;

		if      (x1 < x2)
			retval = -1;
		else if (x1 > x2)
			retval =  1;
		else
			retval =  0;
	}

	return retval;
}

int SortedHorizon::crosslineComparFunc(const void *element1,
	const void *element2)
{
	int retval;

	VfUtilities *ut = _vf_manager->utilities();

	int xbin1 = (int) ut->xbinNumber((*((HorizonStruct **) element1))->x);
	int xbin2 = (int) ut->xbinNumber((*((HorizonStruct **) element2))->x);

	if      (xbin1 < xbin2)
	{
		retval = -1;
	}
	else if (xbin1 > xbin2)
	{
		retval =  1;
	}
	else
	{
		float y1 = (*((HorizonStruct **) element1))->y;
		float y2 = (*((HorizonStruct **) element2))->y;

		if      (y1 < y2)
			retval = -1;
		else if (y1 > y2)
			retval =  1;
		else
			retval =  0;
	}

	return retval;
}

int SortedHorizon::timeComparFunc(const void *element1, const void *element2)
{
	int retval;

	float time1 = (*((HorizonStruct **) element1))->t;
	float time2 = (*((HorizonStruct **) element2))->t;

	if      (time1 < time2)
		retval = -1;
	else if (time1 > time2)
		retval =  1;
	else
		retval =  0;

	return retval;
}

int SortedHorizon::getInlineIndex(int ybin, int *index)
{
	int retval;

	if ((_num_picks == 0) || (ybin < _min_y_bin) || (ybin > _max_y_bin))
	{
		retval = 0;
	}
	else
	{
		*index = _y_bin_index[ybin - _min_y_bin];

		if (ybin == _max_y_bin)
		{
			retval = _num_picks - _y_bin_index[ybin - _min_y_bin];
		}
		else
		{
			retval = _y_bin_index[ybin - _min_y_bin + 1]
			       - _y_bin_index[ybin - _min_y_bin    ];
		}
	}

	return retval;
}

int SortedHorizon::getCrosslineIndex(int xbin, int *index)
{
	int retval;

	if ((_num_picks == 0) || (xbin < _min_x_bin) || (xbin > _max_x_bin))
	{
		retval = 0;
	}
	else
	{
		*index = _x_bin_index[xbin - _min_x_bin];

		if (xbin == _max_x_bin)
		{
			retval = _num_picks - _x_bin_index[xbin - _min_x_bin];
		}
		else
		{
			retval = _x_bin_index[xbin - _min_x_bin + 1]
			       - _x_bin_index[xbin - _min_x_bin    ];
		}
	}

	return retval;
}

int SortedHorizon::getTimeIndex(float tmin, float tmax, int *index)
{
	int retval;

	if (_num_picks == 0)
	{
		retval = 0;
	}
	else
	{
		*index   = time_bs(tmin, _sorted_hs[TIME], _num_picks, -1);
		int imax = time_bs(tmax, _sorted_hs[TIME], _num_picks,  1);

		retval = imax - *index;
	}

	return retval;
}

int SortedHorizon::getNumPicksInBin(float xloc, float yloc, float **pick_time,
	int *interpolated)
{
	assert(_num_picks > 0);

	int retval;

	VfUtilities *ut = _vf_manager->utilities();

	int xbin = (int) ut->xbinNumber(xloc);
	int ybin = (int) ut->ybinNumber(yloc);

	/*
	 * index1 is the beginning of the inline/crossline.
	 * index2 is the closest point in the inline/crossline to
	 * the xloc/yloc.
	 */
	int il_index1, xl_index1, il_index2, xl_index2;
	int il_num = getInlineIndex   (ybin, &il_index1);
	int xl_num = getCrosslineIndex(xbin, &xl_index1);

	if (il_num && xl_num)
	{
		/*
		 * il_index3 is first in inline that is in the right
		 * xbin.  il_index4 is first in inline that is in the
		 * next xbin.
		 */
		int il_index3, il_index4;

		/*
		 * Find in bin or closest
		 */
		il_index2 = il_index1 + insert_loc_bs(INLINE, xloc,
			&_sorted_hs[INLINE][il_index1], il_num);

		/*
		 * Find 1st in bin
		 */
		for (il_index3 = il_index2; ; il_index3--)
		{
			if (il_index3 == il_index1)
				break;

		  	if ((int) ut->xbinNumber(
				_sorted_hs[INLINE][il_index3 - 1]->x) != xbin)
			{
				break;
			}
		}

		/*
		 * Find 1st not in bin
		 */
		for ( il_index4 = il_index2; ; il_index4++)
		{
			if (il_index4 == il_index1 + il_num)
				break;

		  	if ((int) ut->xbinNumber(
				_sorted_hs[INLINE][il_index4]->x) != xbin)
			{
				break;
			}
		}

		/*
		 * Any in bin?
		 */
		if (il_index4 > il_index3)
		{
			retval = il_index4 - il_index3;

			*pick_time = new float[retval];

			for (int i = 0; i < retval; i++)
				(*pick_time)[i] =
					_sorted_hs[INLINE][il_index3 + i]->t;

			*interpolated = 0;
		}
		else
		{
			xl_index2 = xl_index1 + insert_loc_bs(CROSSLINE, yloc,
				&_sorted_hs[CROSSLINE][xl_index1], xl_num);

			if (xl_index2 < xl_num)
			   assert((int) ut->ybinNumber(
				_sorted_hs[CROSSLINE][xl_index2]->y) != ybin);

			*interpolated = 1;
		}
	}
	else
	{
		if (il_num)
			il_index2 = il_index1 + insert_loc_bs(   INLINE, xloc,
				&_sorted_hs[   INLINE][il_index1], il_num);

		if (xl_num)
			xl_index2 = xl_index1 + insert_loc_bs(CROSSLINE, yloc,
				&_sorted_hs[CROSSLINE][xl_index1], xl_num);

		*interpolated = 1;
	}

	/*
	 * Really interpolated or extrapolated
	 */
	if (*interpolated)
	{
		retval = 0;

		/*
		 * Max of 2
		 */
		if (il_num || xl_num)
			*pick_time = new float[2];

		if (il_num)
			(*pick_time)[retval++] = interpOrExtrap(   INLINE, xloc,
				&_sorted_hs[   INLINE][il_index1],
				il_index2 - il_index1, il_num);

		if (xl_num)
			(*pick_time)[retval++] = interpOrExtrap(CROSSLINE, yloc,
				&_sorted_hs[CROSSLINE][xl_index1],
				xl_index2 - xl_index1, xl_num);
	}

	return retval;
}

/*
 * Binary search with a twist.
 * If value not found, returns where it would be inserted.
 * If value found multiple times, returns 1st instance if inc == -1,
 *   and after last instance if inc == 1.
 * If value < 1st  element, returns 0.
 * If value > last element, returns num.
 * If array is empty returns 0.
 */
int SortedHorizon::time_bs(float value, HorizonStruct **hs, int num, int inc)
{
	assert(num >= 0);

	int retval;

	if (num)
	{
		int lower, upper, mid;

		for (lower = 0, upper = num - 1; lower <= upper;)
		{
			mid = (lower + upper) / 2;

			if      (hs[mid]->t < value)
			{
				lower = mid + 1;
			}
			else if (hs[mid]->t > value)
			{
				upper = mid - 1;
			}
			else
			{
				if (inc == 1)
				{
					for (; (mid < num)
						&& (hs[mid  ]->t == value);
					       mid++);
				}
				else if (inc == -1)
				{
					for (; (mid > 0)
						&& (hs[mid-1]->t == value);
					       mid--);
				}
				else
				{
					assert(0);
				}

				assert(lower <= mid);
				break;
			}
		}

		retval = (lower > mid) ? mid + 1 : mid;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

/*
 * Returns index of element which exactly matches value,
 * or if no match index before which value would be inserted.
 * If value < 1st  element, returns 0.
 * If value > last element, returns num
 * If array is empty returns 0.
 */
int SortedHorizon::insert_loc_bs(SortBy sort_by, float value,
	HorizonStruct **hs, int num)
{
	assert(num >= 0);

	int retval;

	if (num)
	{
		int lower, upper, mid;
		float test_val;

		for (lower = 0, upper = num - 1; lower <= upper;)
		{
			mid = (lower + upper) / 2;
			test_val = getValue(sort_by, hs[mid]);

			if      (test_val < value)
			{
				lower = mid + 1;
			}
			else if (test_val > value)
			{
				upper = mid - 1;
			}
			else
			{
				break;
			}
		}

		retval = (lower > mid) ? mid + 1 : mid;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

float SortedHorizon::interpOrExtrap(SortBy sort_by, float loc,
	HorizonStruct **hs, int insert_index, int num)
{
	float retval;

	if (num == 1)
	{
	    /*
	     * All you can do is use the one you got
	     */
	    retval = hs[0]->t;
	}
	else if (insert_index ==   0)
	{
	    retval = doInterpOrExtrap(loc,
		getValue(sort_by, hs[0]), hs[0]->t,
		getValue(sort_by, hs[1]), hs[1]->t);
	}
	else if (insert_index == num)
	{
	    retval = doInterpOrExtrap(loc,
		getValue(sort_by, hs[num - 1]), hs[num - 1]->t,
		getValue(sort_by, hs[num - 2]), hs[num - 2]->t);
	}
	else if (loc < getValue(sort_by, hs[insert_index]))
	{
	    /*
	     * Interpolate left
	     */
	    retval = doInterpOrExtrap(loc,
		getValue(sort_by, hs[insert_index  ]), hs[insert_index  ]->t,
		getValue(sort_by, hs[insert_index-1]), hs[insert_index-1]->t);
	}
	else
	{
	    /*
	     * Interpolate right
	     */
	    assert(0);	/* should not get here */
//	    retval = doInterpOrExtrap(loc,
//		getValue(sort_by, hs[insert_index  ]), hs[insert_index  ]->t,
//		getValue(sort_by, hs[insert_index+1]), hs[insert_index+1]->t);
	}

	return retval;
}

float SortedHorizon::doInterpOrExtrap(float x,
	float x_closest     , float y_closest     ,
	float x_next_closest, float y_next_closest)
{
	float retval;

	if (x_closest == x_next_closest)
	{

		retval = y_closest;
	}
	else
	{

		float slope = (y_next_closest - y_closest)
			    / (x_next_closest - x_closest);

		retval =  y_closest + slope * (x - x_closest);
	}

	return retval;
}

float SortedHorizon::getValue(SortBy sort_by, HorizonStruct *hs)
{
	float retval;

	switch (sort_by)
	{
		case    INLINE:	retval = hs->x;	break;
		case CROSSLINE:	retval = hs->y;	break;
		case TIME     :	retval = hs->t;	break;
		default       :	assert(0)     ;	break;
	}

	return retval;
}

int SortedHorizon::gotLineNumbers()
{
	return _got_line_numbers;
}

void SortedHorizon::setGotLineNumbers()
{
	int i;
	for (	_got_line_numbers = 1, i = 0;
		_got_line_numbers && (i < _num_picks);
		_got_line_numbers =
		   (_vf_horizons->getLineNumber(_ihorizon, (long) i) != FNIL),
		  i++);
}

int SortedHorizon::onSameLine(int i1, int i2)
{
	assert(_got_line_numbers);

	return (_vf_horizons->getLineNumber(_ihorizon, (long) i1)
	     == _vf_horizons->getLineNumber(_ihorizon, (long) i2));
}

VaHorizonsData::VaHorizonsData(VfManager *vf_manager, VaVectColors *colors)
	: BaseData(), _vf_manager(vf_manager), _colors(colors)
{
	/* just initializers */
}

VaHorizonsData::~VaHorizonsData()
{
	/* do nothing */
}

void VaHorizonsData::setVaHorizonsManager(
	VaHorizonsManager *va_horizons_manager)
{
	_va_horizons_manager = va_horizons_manager;
}

int VaHorizonsData::usesMarkers()
{
	/*
	 * Default is yes, override in derived class for no.
	 */
	return 1;
}

void VaHorizonsData::setShowHorizonMarkers(int set)
{
	_show_horizon_markers = set;
}

int VaHorizonsData::getShowHorizonMarkers()
{
	return _show_horizon_markers;
}

/*
 * Static Variables.
 */
int                VaHorizons::_num_va_horizons     = 0;
VaHorizonsManager *VaHorizons::_va_horizons_manager    ;

VaHorizons::VaHorizons(VfManager *vf_manager, VfHorizons *vf_horizons,
	VaHorizonsData *va_horizons_data, VaVectColors *colors,
	SeisVectLinkedList *hor_vectors, SeisVectLinkedList *top_vectors,
	int hiding, int line_width_factor, int pen_lift)
	: _vf_horizons(vf_horizons), _va_horizons_data(va_horizons_data),
	  _colors(colors), _hor_vectors(hor_vectors), _top_vectors(top_vectors),
	  _hiding(hiding), _line_width_factor(line_width_factor),
	  _pen_lift(pen_lift)
{
	/*
	 * Set this here instead of in VaHorizonsData constructor
	 * in case value changes between VaHorizonsData constructor
	 * and VaHorizons constructor.
	 */
	_va_horizons_data->setShowHorizonMarkers(
		_colors->getShowHorizonMarkers());

	if (_num_va_horizons++ == 0)
		_va_horizons_manager = new VaHorizonsManager(vf_manager,
			vf_horizons);

	_va_horizons_manager->addVaHorizons(this);

	_va_horizons_data->setVaHorizonsManager(_va_horizons_manager);

	for (long i = 0; i < _vf_horizons->numHorizons(); i++)
		addHorizon(i);
}

VaHorizons::~VaHorizons()
{
	if (--_num_va_horizons == 0)
		delete _va_horizons_manager;
}

void VaHorizons::addHorizon(long ihorizon)
{
	/* assume always adds at end */
	assert((int) ihorizon == _hor_vectors->count());

	unsigned int lw = (_colors->getShowActiveHorizon()
		&& (ihorizon == _vf_horizons->getActiveHorizonIndex()))
		? _colors-> activeHorizonWidth()
		: _colors->defaultHorizonWidth();

	lw *= (unsigned int) _line_width_factor;

	Vector *vector = _hor_vectors->add(_va_horizons_data, ihorizon,
		_vf_horizons->getColor(ihorizon),
		lw, False,
		Vector::DataSpecifiedStyle ,
		Vector::DataSpecifiedMarker,
		_colors->horizonMarkerSize     (),
		_colors->horizonMarkerLineWidth());

	if (_pen_lift)
	{
		if (_va_horizons_manager->getSortedHorizon(ihorizon)->
			gotLineNumbers())
		{
			vector->setDataPenLift(1);
		}
		else
		{
			vector->setAutoPenLift(1);
		}
	}

	if (_vf_horizons->isSelected(ihorizon) && !_hiding)
	{
		vector->makeVisible();
		putTopVectorsOnTop();
	}
}

void VaHorizons::preRemHorizon(long ihorizon)
{
	Vector *vect;
	void   *p   ;

	for (vect = _hor_vectors->top(&p); vect; vect = _hor_vectors->next(&p))
		if (vect->getId() == ihorizon)
		{
			assert(!SU::isHoldingVectors());
			_hor_vectors->remove(vect);
			break;
		}
}

void VaHorizons::postRemHorizon(long ihorizon)
{
	Vector *vect;
	void   *p   ;

	for (vect = _hor_vectors->top(&p); vect; vect = _hor_vectors->next(&p))
	{
		assert(vect->getId() != ihorizon);

		if (vect->getId() > ihorizon)
			vect->setId(vect->getId() - 1);
	}
}

void VaHorizons::preNewActiveHorizon()
{
	if (_colors->getShowActiveHorizon() && _line_width_factor)
	{
		Vector *vect;
		void   *p;
		for (vect = _hor_vectors->top(&p);
			vect;
			vect = _hor_vectors->next(&p))
		{
			if (vect->getId() ==
				_vf_horizons->getActiveHorizonIndex())
			{
				vect->setWidth(_colors->defaultHorizonWidth());
				break;
			}
		}
	}
}

void VaHorizons::postNewActiveHorizon()
{
	if (_colors->getShowActiveHorizon() && _line_width_factor)
	{
		Vector *vect;
		void   *p;
		for (vect = _hor_vectors->top(&p);
			vect;
			vect = _hor_vectors->next(&p))
		{
			if (vect->getId() ==
				_vf_horizons->getActiveHorizonIndex())
			{
				vect->setWidth(_colors->activeHorizonWidth());
				break;
			}
		}

		putTopVectorsOnTop();
	}
}

void VaHorizons::preRemoveInsertHorizons(long index, long nrem, long /*nins*/)
{
	if (nrem)
	{
		assert(index == _vf_horizons->getActiveHorizonIndex());
		preRemHorizon(index);
	}
}

void VaHorizons::postRemoveInsertHorizons(long index, long nrem, long /*nins*/)
{
	if (nrem)
	{
		postRemHorizon(index);
		postNewActiveHorizon();
	}
	else
	{
		addHorizon(index);
	}
}

void VaHorizons::setColor(long ihorizon)
{
	Vector *vect;
	void   *p;
	for (vect = _hor_vectors->top(&p); vect; vect = _hor_vectors->next(&p))
		if (vect->getId() == ihorizon)
		{
			vect->setColor(_vf_horizons->getColor(ihorizon));
			break;
		}
}

void VaHorizons::makeVisible(long ihorizon)
{
	Vector *vect;
	void   *p;

	if (_hiding)
	{
		for (vect = _hor_vectors->top(&p);
			vect;
			vect = _hor_vectors->next(&p))
		{
			assert(!vect->isVisible());
		}
	}
	else
	{
		for (vect = _hor_vectors->top(&p);
			vect;
			vect = _hor_vectors->next(&p))
		{
			if (vect->getId() == ihorizon)
			{
				vect->makeVisible();
				break;
			}
		}
	}
}

void VaHorizons::makeInvisible(long ihorizon)
{
	Vector *vect;
	void   *p;

	if (_hiding)
	{
		for (vect = _hor_vectors->top(&p);
			vect;
			vect = _hor_vectors->next(&p))
		{
			assert(!vect->isVisible());
		}
	}
	else
	{
		for (vect = _hor_vectors->top(&p);
			vect;
			vect = _hor_vectors->next(&p))
		{
			if (vect->getId() == ihorizon)
			{
				vect->makeInvisible();
				break;
			}
		}
	}
}

void VaHorizons::putTopVectorsOnTop()
{
	Vector *vect;
	void   *p;
	for (vect = _top_vectors->top(&p); vect; vect = _top_vectors->next(&p))
		vect->redraw();
}

void VaHorizons::hide()
{
	assert(!_hiding);

	Vector *vect;
	void   *p;
	for (vect = _hor_vectors->top(&p); vect; vect = _hor_vectors->next(&p))
		if (_vf_horizons->isSelected(vect->getId()))
			vect->makeInvisible();
		else
			assert(!vect->isVisible());

	_hiding = 1;
}

void VaHorizons::reveal()
{
	assert( _hiding);

	Vector *vect;
	void   *p;
	for (vect = _hor_vectors->top(&p); vect; vect = _hor_vectors->next(&p))
		if (_vf_horizons->isSelected(vect->getId()))
			vect->makeVisible();
		else
			assert(!vect->isVisible());

	_hiding = 0;
}

void VaHorizons::broadcastSetShowHorizonMarkers(int set)
{
	if (_num_va_horizons)
		_va_horizons_manager->setShowHorizonMarkers(set);
}

void VaHorizons::broadcastSetShowActiveHorizon (int set)
{
	if (_num_va_horizons)
		_va_horizons_manager->setShowActiveHorizon (set);
}

void VaHorizons::setShowHorizonMarkers(int set)
{
	if (_va_horizons_data->usesMarkers())
	{
		Vector *vect;
		void   *p;

		switch (3 * _va_horizons_data->getShowHorizonMarkers() + set)
		{
			case 0:	/* no change */
			case 4:
			case 8:
				assert(0);
			case 1:	/* OVER_LINE to OVER_BOTH */
			case 7:	/* OVER_MARK to OVER_BOTH */
				_va_horizons_data->setShowHorizonMarkers(set);

				for (vect = _hor_vectors->top(&p);
					vect;
					vect = _hor_vectors->next(&p))
				{
					if (vect->isVisible())
						vect->redraw();
				}

				break;
			case 2:	/* OVER_LINE to OVER_MARK */
			case 3:	/* OVER_BOTH to OVER_LINE */
			case 5:	/* OVER_BOTH to OVER_MARK */
			case 6:	/* OVER_MARK to OVER_LINE */
				assert(!SU::isHoldingVectors());
				SU::holdVectors();

				for (vect = _hor_vectors->top(&p);
					vect;
					vect = _hor_vectors->next(&p))
				{
					if (_vf_horizons->
						isSelected(vect->getId()))
					{
						vect->makeInvisible();
					}
					else
					{
						assert(!vect->isVisible());
					}
				}

				SU::flushVectors();

				_va_horizons_data->setShowHorizonMarkers(set);

				for (vect = _hor_vectors->top(&p);
					vect;
					vect = _hor_vectors->next(&p))
				{
					if (_vf_horizons->
						isSelected(vect->getId()))
					{
						vect->makeVisible();
					}
					else
					{
						assert(!vect->isVisible());
					}
				}

				break;
			default:
				assert(0);
		}

		putTopVectorsOnTop();
	}
}

void VaHorizons::setShowActiveHorizon (int set)
{
	if (_line_width_factor)
	{
		Vector *vect;
		void   *p;
		for (vect = _hor_vectors->top(&p);
			vect;
			vect = _hor_vectors->next(&p))
		{
			if (vect->getId() ==
				_vf_horizons->getActiveHorizonIndex())
			{
				vect->setWidth((set)
					? _colors-> activeHorizonWidth()
					: _colors->defaultHorizonWidth());

				break;
			}
		}

		putTopVectorsOnTop();
	}
}

VaHorizonsElement::VaHorizonsElement(VaHorizons *va_horizons)
	: _va_horizons(va_horizons)
{
	/* just initializers */
}

VaHorizonsElement::~VaHorizonsElement()
{
	/* do nothing */
}

int VaHorizonsElement::operator ==(void * const va_horizons) const
{
	return((VaHorizons *) va_horizons == _va_horizons);
}

void VaHorizonsElement::print() const
{
	cout << " " << _va_horizons;
}

VaHorizonsLinkedList::VaHorizonsLinkedList()
{
	/* do nothing */
}

VaHorizonsLinkedList::~VaHorizonsLinkedList()
{
	/* do nothing */
}

void VaHorizonsLinkedList::add(VaHorizons *va_horizons)
{
	VaHorizonsElement *theElement = new VaHorizonsElement(va_horizons);
	BaseLinkedList::add(theElement);
}

void VaHorizonsLinkedList::remove(VaHorizons *va_horizons)
{
	BaseLinkedList::remove((void *) va_horizons);
}

VaHorizons *VaHorizonsLinkedList::find(VaHorizons *va_horizons, void **p)
{
	VaHorizonsElement *ptr = (VaHorizonsElement *)
		BaseLinkedList::find((void *) va_horizons, p);
	return (ptr ? ptr->_va_horizons : (VaHorizons *) NULL);
}

VaHorizons *VaHorizonsLinkedList::top    (void **p)
{
	VaHorizonsElement *ptr = (VaHorizonsElement *)
		BaseLinkedList::top    (p);
	return (ptr ? ptr->_va_horizons : (VaHorizons *) NULL);
}

VaHorizons *VaHorizonsLinkedList::bottom (void **p)
{
	VaHorizonsElement *ptr = (VaHorizonsElement *)
		BaseLinkedList::bottom (p);
	return (ptr ? ptr->_va_horizons : (VaHorizons *) NULL);
}

VaHorizons *VaHorizonsLinkedList::next   (void **p)
{
	VaHorizonsElement *ptr = (VaHorizonsElement *)
		BaseLinkedList::next   (p);
	return (ptr ? ptr->_va_horizons : (VaHorizons *) NULL);
}

VaHorizons *VaHorizonsLinkedList::prev   (void **p)
{
	VaHorizonsElement *ptr = (VaHorizonsElement *)
		BaseLinkedList::prev   (p);
	return (ptr ? ptr->_va_horizons : (VaHorizons *) NULL);
}

VaHorizons *VaHorizonsLinkedList::current(void **p)
{
	VaHorizonsElement *ptr = (VaHorizonsElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_va_horizons : (VaHorizons *) NULL);
}

VaHorizonsManager::VaHorizonsManager(VfManager *vf_manager,
	VfHorizons *vf_horizons)
	: VfInform(vf_manager),
	  _vf_horizons(vf_horizons), _num_sorted_horizons(0),
	  _watch((PickWatch *) 0)
{
	_va_horizons_list = new VaHorizonsLinkedList();

	for (long i = 0; i < _vf_horizons->numHorizons(); i++)
		addVfHorizon(i);
}

VaHorizonsManager::~VaHorizonsManager()
{
	delete _va_horizons_list;

	for (int i = 0; i < _num_sorted_horizons; i++)
		delete _sorted_horizons[i];

	if (_num_sorted_horizons)
		delete _sorted_horizons;

	assert(_watch == (PickWatch *) 0);
}

void VaHorizonsManager::addVaHorizons(VaHorizons *va_horizons)
{
	_va_horizons_list->add(va_horizons);
}

SortedHorizon *VaHorizonsManager::getSortedHorizon(long ihorizon)
{
	assert((int) ihorizon < _num_sorted_horizons);

	return _sorted_horizons[(int) ihorizon];
}

void VaHorizonsManager::addVfHorizon(long ihorizon)
{
	assert((int) ihorizon == _num_sorted_horizons);

	if (_num_sorted_horizons++)
	{
		_sorted_horizons = (SortedHorizon **) realloc(
			(void *) _sorted_horizons,
			(size_t) _num_sorted_horizons * sizeof(SortedHorizon*));
	}
	else
	{
		_sorted_horizons = (SortedHorizon **)  malloc(
			(size_t) _num_sorted_horizons * sizeof(SortedHorizon*));
	}

	assert(_sorted_horizons);

	_sorted_horizons[(int) ihorizon]
		= new SortedHorizon(manager(), _vf_horizons, ihorizon);
}

void VaHorizonsManager::remVfHorizon(long ihorizon)
{
	assert((int) ihorizon < _num_sorted_horizons);

	delete _sorted_horizons[(int) ihorizon];

	if ((int) ihorizon < _num_sorted_horizons - 1)
	{
		memmove((void *) &_sorted_horizons[(int) ihorizon    ],
			(void *) &_sorted_horizons[(int) ihorizon + 1],
			(size_t) (_num_sorted_horizons - (int) ihorizon - 1)
			* sizeof(SortedHorizon *));
	}

	if (--_num_sorted_horizons)
	{
		_sorted_horizons = (SortedHorizon **) realloc(
			(void *) _sorted_horizons,
			(size_t) _num_sorted_horizons * sizeof(SortedHorizon*));
	}
	else
	{
		free((void *) _sorted_horizons);
	}
}

void VaHorizonsManager::preNewActiveHorizon()
{
	VaHorizons *va_horizons;
	void       *p          ;

	for (	va_horizons = _va_horizons_list->top (&p);
		va_horizons;
		va_horizons = _va_horizons_list->next(&p))
	{
		va_horizons->preNewActiveHorizon();
	}
}

void VaHorizonsManager::postNewActiveHorizon()
{
	VaHorizons *va_horizons;
	void       *p          ;

	for (	va_horizons = _va_horizons_list->top (&p);
		va_horizons;
		va_horizons = _va_horizons_list->next(&p))
	{
		va_horizons->postNewActiveHorizon();
	}
}

void VaHorizonsManager::preRemoveInsertHorizons(long index, long nrem,
	long nins)
{
	assert(((nrem == 1) && (nins == 0))
	    || ((nrem == 0) && (nins == 1)));

	if (nins)
		watchOn();

	VaHorizons *va_horizons;
	void       *p          ;

	for (	va_horizons = _va_horizons_list->top (&p);
		va_horizons;
		va_horizons = _va_horizons_list->next(&p))
	{
		va_horizons->preRemoveInsertHorizons(index, nrem, nins);
	}
}

void VaHorizonsManager::postRemoveInsertHorizons(long index, long nrem,
	long nins)
{
	if (nrem)
		remVfHorizon(index);
	else
		addVfHorizon(index);

	VaHorizons *va_horizons;
	void       *p          ;

	for (	va_horizons = _va_horizons_list->top (&p);
		va_horizons;
		va_horizons = _va_horizons_list->next(&p))
	{
		va_horizons->postRemoveInsertHorizons(index, nrem, nins);
	}

	if (nins)
		watchOff();
}

void VaHorizonsManager::preNewSelectedHorizons()
{
	int num_horizons = (int) _vf_horizons->numHorizons();
	assert(num_horizons == _num_sorted_horizons);

	_selected_before = new int[num_horizons];

	int i;
	for (_num_selected_before = i = 0; i < num_horizons; i++)
		if (_vf_horizons->isSelected((long) i))
			_selected_before[_num_selected_before++] = i;
}

void VaHorizonsManager::postNewSelectedHorizons()
{
	watchOn();

	int num_horizons = (int) _vf_horizons->numHorizons();
	assert(num_horizons == _num_sorted_horizons);

	int *selected_after = new int[num_horizons];

	int num_selected_after, i;
	for (num_selected_after = i = 0; i < num_horizons; i++)
		if (_vf_horizons->isSelected((long) i))
			selected_after[num_selected_after++] = i;

	int *make_visible   = new int[num_horizons];
	int *make_invisible = new int[num_horizons];
	int num_make_visible, num_make_invisible;
	int before_idx, after_idx;

	for (num_make_visible = num_make_invisible = before_idx = after_idx = 0;
		   (before_idx < _num_selected_before)
		|| ( after_idx <  num_selected_after ); )
	{
		if      ((before_idx < _num_selected_before)
		 &&      ( after_idx <  num_selected_after ))
		{
			if      (_selected_before[before_idx]
			       <  selected_after [ after_idx])
			{
				make_invisible[num_make_invisible++]
					= _selected_before[before_idx++];
			}
			else if (_selected_before[before_idx]
			       >  selected_after [ after_idx])
			{
				make_visible[num_make_visible++]
					=  selected_after[after_idx++];
			}
			else
			{
				assert(_selected_before[before_idx]
				    ==  selected_after [ after_idx]);

				before_idx++;
				 after_idx++;
			}
		}
		else if ( before_idx < _num_selected_before )
		{
			make_invisible[num_make_invisible++]
				= _selected_before[before_idx++];
		}
		else
		{
			assert(after_idx < num_selected_after);

			make_visible[num_make_visible++]
				=  selected_after[after_idx++];
		}
	}

	assert(!SU::isHoldingVectors());
	if (num_make_invisible > 1)
		SU::holdVectors();

	VaHorizons *va_horizons;
	void       *p          ;

	for (i = 0; i < num_make_invisible; i++)
		for (	va_horizons = _va_horizons_list->top (&p);
			va_horizons;
			va_horizons = _va_horizons_list->next(&p))
		{
			va_horizons->makeInvisible((long) make_invisible[i]);
		}

	if (num_make_invisible > 1)
		SU::flushVectors();

	for (i = 0; i < num_make_visible; i++)
		for (	va_horizons = _va_horizons_list->top (&p);
			va_horizons;
			va_horizons = _va_horizons_list->next(&p))
		{
			va_horizons->makeVisible((long) make_visible[i]);
		}

	if (num_make_visible)
		for (	va_horizons = _va_horizons_list->top (&p);
			va_horizons;
			va_horizons = _va_horizons_list->next(&p))
		{
			va_horizons->putTopVectorsOnTop();
		}

	delete [] _selected_before;
	delete []  selected_after ;
	delete [] make_visible    ;
	delete [] make_invisible  ;

	watchOff();
}

void VaHorizonsManager::preNewHorizonColor(long /*ihorizon*/)
{
	/* wait for post */
}

void VaHorizonsManager::postNewHorizonColor(long ihorizon)
{
	watchOn();

	VaHorizons *va_horizons;
	void       *p          ;

	for (	va_horizons = _va_horizons_list->top (&p);
		va_horizons;
		va_horizons = _va_horizons_list->next(&p))
	{
		va_horizons->setColor(ihorizon);
		va_horizons->putTopVectorsOnTop();
	}

	watchOff();
}

void VaHorizonsManager::preChangeBinTolerances()
{
	preDataChange();
}

void VaHorizonsManager::postChangeBinTolerances()
{
	for (int i = 0; i < _num_sorted_horizons; i++)
		_sorted_horizons[i]->sortLines();

	postDataChange();
}

void VaHorizonsManager::preNewHorizonTransform()
{
	preDataChange();
}

void VaHorizonsManager::postNewHorizonTransform()
{
	for (int i = 0; i < _num_sorted_horizons; i++)
	{
		_sorted_horizons[i]->loadData ();
		_sorted_horizons[i]->sortLines();
	}

	postDataChange();
}

void VaHorizonsManager::preDataChange()
{
	watchOn();

	int num_horizons = (int) _vf_horizons->numHorizons();
	assert(num_horizons == _num_sorted_horizons);

	int num_vis, i;
	for (num_vis = i = 0; i < num_horizons; i++)
		if (_vf_horizons->isSelected((long) i))
			num_vis++;

	assert(!SU::isHoldingVectors());
	if (num_vis > 1)
		SU::holdVectors();

	VaHorizons *va_horizons;
	void       *p          ;

	for (i = 0; (num_vis > 0) && (i < num_horizons); i++)
		if (_vf_horizons->isSelected((long) i))
		{
			for (	va_horizons = _va_horizons_list->top (&p);
				va_horizons;
				va_horizons = _va_horizons_list->next(&p))
			{
				va_horizons->makeInvisible((long) i);
			}

			num_vis--;
		}

	if (SU::isHoldingVectors())	/* I counted num_vis down */
		SU::flushVectors();
}

void VaHorizonsManager::postDataChange()
{
	int num_horizons = (int) _vf_horizons->numHorizons();
	assert(num_horizons == _num_sorted_horizons);

	int num_vis, i;
	for (num_vis = i = 0; i < num_horizons; i++)
		if (_vf_horizons->isSelected((long) i))
			num_vis++;

	VaHorizons *va_horizons;
	void       *p          ;

	for (i = 0; (num_vis > 0) && (i < num_horizons); i++)
		if (_vf_horizons->isSelected((long) i))
		{
			for (	va_horizons = _va_horizons_list->top (&p);
				va_horizons;
				va_horizons = _va_horizons_list->next(&p))
			{
				va_horizons->makeVisible((long) i);
			}

			num_vis--;

			if (num_vis == 0)
				for (va_horizons = _va_horizons_list->top (&p);
				     va_horizons;
				     va_horizons = _va_horizons_list->next(&p))
				{
					va_horizons->putTopVectorsOnTop();
				}
		}

	watchOff();
}

void VaHorizonsManager::watchOn()
{
	assert(_watch == (PickWatch *) 0);

	_watch_was_ok = (int) PlotBase::watchOK();

	if (!_watch_was_ok)
		PlotBase::setWatchOK(True);

	_watch = new PickWatch();
}

void VaHorizonsManager::watchOff()
{
	assert(_watch != (PickWatch *) 0);

	delete _watch;
	_watch = (PickWatch *) 0;

	if (!_watch_was_ok)
		PlotBase::setWatchOK(False);
}

void VaHorizonsManager::setShowHorizonMarkers(int set)
{
	ShellWatch sw;
	watchOn();

	VaHorizons *va_horizons;
	void       *p          ;

	for (	va_horizons = _va_horizons_list->top (&p);
		va_horizons;
		va_horizons = _va_horizons_list->next(&p))
	{
		va_horizons->setShowHorizonMarkers(set);
	}

	/* sw goes out of scope */
	watchOff();
}

void VaHorizonsManager::setShowActiveHorizon (int set)
{
	ShellWatch sw;
	watchOn();

	VaHorizons *va_horizons;
	void       *p          ;

	for (	va_horizons = _va_horizons_list->top (&p);
		va_horizons;
		va_horizons = _va_horizons_list->next(&p))
	{
		va_horizons->setShowActiveHorizon (set);
	}

	/* sw goes out of scope */
	watchOff();
}
