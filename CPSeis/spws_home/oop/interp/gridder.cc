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
#include "interp/gridder.hh"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>


Index::Index(int ndims, int *dim)
	: _ndims(ndims)
{
	assert(_ndims > 0);

	_dim  = new int[_ndims];
	_span = new int[_ndims];

	memcpy(_dim, dim, (size_t) _ndims * sizeof(int));

	int i;
	for (i = _ndims - 1, _span[i--] = 1; i >= 0; i--)
		_span[i] = _span[i+1] * dim[i+1];

	_total_size = _span[0] * dim[0];
}

Index::~Index()
{
	delete [] _dim ;
	delete [] _span;
}

int Index::numDims() const
{
	return _ndims;
}

int Index::totalSize() const
{
	return _total_size;
}

int Index::to1D(int *i_multiD) const
{
	int retval, i;

	for (retval = i = 0; (retval != -1) && (i < _ndims); i++)
		if ((i_multiD[i] >= 0) && (i_multiD[i] < _dim[i]))
			retval += _span[i] * i_multiD[i];
		else
			retval = -1;

	return retval;
}

void Index::from1D(int i_1d, int *i_multiD) const
{
	int i;

	if ((i_1d >= 0) && (i_1d < _total_size))
	{
		for (i = 0; i < _ndims; i++)
		{
			i_multiD[i] = i_1d / _span[i];
			i_1d        = i_1d % _span[i];
		}
	}
	else
	{
		for (i = 0; i < _ndims; i++)
			i_multiD[i] = -1;
	}
}

Points::Points(int ndims)
	: _ndims(ndims), _num(0), _num_alloc(0)
{
	assert(_ndims > 0);
}

Points::~Points()
{
	if (_num_alloc)
	{
		for (int i = 0; i < _num; i++)
			delete _point[i];

		delete [] _point;
	}
}

int Points::numDims() const
{
	return _ndims;
}

long Points::add (const float *loc, float val)
{
	long retval = 1;
	if (_num == _num_alloc)
	{
		_num_alloc += ALLOC_INC;
		Point **tmp_point;
		try {
		  tmp_point = new Point *[_num_alloc];
		}
		catch (...) {
		  retval = 0;
		}

		if (_num && retval)
		{
			memcpy(tmp_point, _point,
				(size_t) _num * sizeof(Point *));

			delete [] _point;
		}

		if (retval) _point = tmp_point;
	}

	if (retval) {
	  try {
	    _point[_num] = new Point(this, loc, val);

            retval = (long)_point[_num];
	    _num++;
	  }
	  catch (...) {
	    retval = 0;
	  }
	}
	return retval;
}

long Points::add (const Points *points, int i)
{
	long retval;
	assert(_ndims == points->numDims());

	float *loc = new float[_ndims];
	points->getLoc(i, loc);

	float val =  points->getVal(i);

	retval = add(loc, val);

	delete [] loc;
	return retval;
}

int Points::num() const
{
	return _num;
}

void Points::getLoc(int i, float *loc) const
{
	_point[i]->getLoc(loc);
}

float Points::getVal(int i) const
{
	return _point[i]->getVal();
}

Points::Point::Point(Points *container, const float *loc, float val)
	: _container(container), _val(val)
{
	_loc = new float[_container->numDims()];
	memcpy(_loc, loc, (size_t) _container->numDims() * sizeof(float));
}

Points::Point::~Point()
{
	delete [] _loc;
}

void Points::Point::getLoc(float *loc) const
{
	memcpy(loc, _loc, (size_t) _container->numDims() * sizeof(float));
}

float Points::Point::getVal() const
{
	return _val;
}


Gridder::Gridder(int ndims, float *min, float *max, float *reach,
	float nil_val, int stype)
	: _ndims(ndims), _nil_val(nil_val), _stype(stype)
{
	assert(_ndims > 0);
	assert(_stype == WTD || _stype == NN);

	// Make sure inputs make sense.
	//
	int i;
	for (i = 0; i < _ndims; i++)
	{
		assert(reach[i] > 0.0F);
		assert(max[i] >= min[i]);
	}

	_min   = new float[_ndims];
	_max   = new float[_ndims];
	_reach = new float[_ndims];

	memcpy(_min  , min  , (size_t) _ndims * sizeof(float));
	memcpy(_max  , max  , (size_t) _ndims * sizeof(float));
	memcpy(_reach, reach, (size_t) _ndims * sizeof(float));

	_grid_size = new int[_ndims];

	for (i = 0; i < _ndims; i++)
		_grid_size[i] = gridIndex(_min[i], _reach[i], _max[i]) + 1;

	_index = new Index(_ndims, _grid_size);

	_points = new Points *[_index->totalSize()];

	for (i = 0; i < _index->totalSize(); i++)
		_points[i] = new Points(_ndims);
}

Gridder::~Gridder()
{
	delete [] _min      ;
	delete [] _max      ;
	delete [] _reach    ;
	delete [] _grid_size;

	for (int i = 0; i < _index->totalSize(); i++)
		delete _points[i];

	delete [] _points;
	delete _index;
}

long Gridder::addPoint(const float *loc, float val)
{
	long retval;
	int *i_multiD = new int[_ndims];

	int i;
	for (i = 0; i < _ndims; i++)
	{
		assert((loc[i] >= _min[i]) && (loc[i] <= _max[i]));
		i_multiD[i] = gridIndex(_min[i], _reach[i], loc[i]);
	}

	int i_1D = _index->to1D(i_multiD);
	assert(i_1D > -1);
	delete [] i_multiD;

	retval = _points[i_1D]->add(loc, val);
	return retval;
}

void Gridder::getSpecs (int *ndims, float *min, float *max,
  float *reach, float *nil_val, int *stype) const
{
  *ndims = _ndims;
  int k2;
  for (k2 = 0; k2 < _ndims; k2++) {
    min[k2]   = _min[k2];
    max[k2]   = _max[k2];
    reach[k2] = _reach[k2];
  }
  if (nil_val) *nil_val = _nil_val;
  if (stype  ) *stype   = _stype  ;
}

float Gridder::getVal(const float *loc) const
{
	float retval;

	int num_same_pts;
	float sum, weighted_sum, factor_sum;

	if (num_same_pts = sameLocation(loc, &sum))	// = not ==
	{
		retval = sum / (float) num_same_pts;
	}
	else if (withinReach(loc, &weighted_sum, &factor_sum))
	{
		retval = weighted_sum / factor_sum;
	}
	else
	{
		retval = _nil_val;
	}

	return retval;
}

void Gridder::setResampleType (int stype)
{
	assert (stype == WTD || stype == NN);
	_stype = stype;
}

// create an additional copy of the Points array and return the pointer to it
int Gridder::getPoints (Points ***points) const
{
  int retval, k2, k3;
  if (_index->totalSize()) {
    retval = _index->totalSize ();
    
    Points **tmp = new Points*[_index->totalSize()];
    *points = tmp;
    for (k2 = 0; k2 < retval; k2++) {
      tmp[k2] = new Points (_ndims);
      for (k3 = 0; k3 < _points[k2]->num(); k3++) {
	tmp[k2]->add (_points[k2], k3);
      }
    }
    *points = tmp;
  }
  else {
    *points = 0;
    retval = 0;
  }
  return retval;
}

int Gridder::findRangeOfPossibleChange (const Points **points, int total_size,
  float *mins, float *maxs) const
{
  assert (points && total_size > 0 && total_size == _index->totalSize() &&
    points[0]->numDims() == _points[0]->numDims() &&
    mins && maxs);

  int k2, k3, retval = 0;
  int   *indices = new int[_ndims];
  int   *i_min   = new int[_ndims];
  int   *i_max   = new int[_ndims];
  int   *i_cur   = new int[_ndims];

  for (k2 = 0; k2 < total_size; k2++) {
    if (compar(k2,points[k2])) {
      _index->from1D (k2, indices);
// establish the range of neighbors for each dimension
      for (k3 = 0; k3 < _ndims; k3++) {
	i_min[k3] = indices[k3] - 1;
	i_max[k3] = indices[k3] + 1;
      }
// recursively find the location range
      findLocationRange (&retval, i_min, i_max, i_cur, 0, mins, maxs);
    }
  }
  delete [] indices;
  delete [] i_min;
  delete [] i_max;
  delete [] i_cur;
  return retval;
}

int Gridder::extractValues (float *values, float *mins, float *maxs,
  float *incs) const
{
  assert (values);

  int   *i_max = new int  [_ndims];
  int   *i_cur = new int  [_ndims];
  float *locs  = new float[_ndims];
  int k2;
  for (k2 = 0; k2 < _ndims; k2++) {
    i_max[k2] = gridIndex(mins[k2], incs[k2], maxs[k2]);
  }

  int retval = 0;
// extract the values
  extractValues (&retval, values, mins, i_max, incs, i_cur, locs, 0);
  delete [] i_max;
  delete [] i_cur;
  delete [] locs ;
  return retval;
}

int Gridder::gridIndex(float min, float reach, float loc) const
{
	return (int) floor((double) ((loc - min) / reach));
}

float Gridder::gridLocation (float min, float reach, int index,
  int just) const
{
  float extra;  
  switch (just) {
    case MIN:
      extra = 0;
      break;
    case MAX:
      extra = reach;
      break;
    case CENTER:
      extra = reach / (float)2;
      break;
    default:
      assert (0);
      break;
  }
  return reach * (float)index + min + extra;
}

int Gridder::sameLocation(const float *loc, float *sum) const
{
	int retval = 0;
	*sum = 0.0F;

	int *i_multiD = new int[_ndims];
	int i;
	for (i = 0; i < _ndims; i++)
		i_multiD[i] = gridIndex(_min[i], _reach[i], loc[i]);

	int i_1D = _index->to1D(i_multiD);

	if (i_1D != -1)
	{
		Points *points = _points[i_1D];
		float *loc_chk = new float[_ndims];
		float normalized_diff;
		int j;

		// Use normalized_diff >= eps for not equal instead of !=
		// to avoid undeflow leading to divide by zero later.
		//
		static const float eps = 1.0e-6;

		for (i = 0; i < points->num(); i++)
		{
			points->getLoc(i, loc_chk);

			for (j = 0; j < _ndims; j++)
			{
				normalized_diff =
					(loc[j] - loc_chk[j]) / _reach[j];

				if ((normalized_diff >  eps)
				 || (normalized_diff < -eps))
				{
					break;
				}
			}

			if (j == _ndims)
			{
				*sum += points->getVal(i);
				retval++;
			}
		}

		delete [] loc_chk;
	}

	delete [] i_multiD;

	return retval;
}

int Gridder::withinReach(const float *loc,
	float *weighted_sum, float *factor_sum) const
{
	int retval;
	*weighted_sum = *factor_sum = 0.0F;

	int *i_min = new int[_ndims];
	int *i_max = new int[_ndims];
	int *i_cur = new int[_ndims];
	int  i_multiD;
	float min_dist2;

	for (int i = 0; i <_ndims; i++)
	{
		i_multiD = gridIndex(_min[i], _reach[i], loc[i]);
		i_min[i] = i_multiD - 1;
		i_max[i] = i_multiD + 1;
	}

	retval = withinReach(
		i_min, i_max, i_cur, 0, loc, weighted_sum, factor_sum,
		&min_dist2);

	delete [] i_min;
	delete [] i_max;
	delete [] i_cur;

	return retval;
}

int Gridder::withinReach(const int *i_min, const int *i_max, int *i_cur,
	int cur_dim, const float *loc,
	float *weighted_sum, float *factor_sum, float *min_dist2) const
{
	int retval = 0;

	if (cur_dim == _ndims)
	{
		int i_1D = _index->to1D(i_cur);

		if (i_1D != -1)
		{
			Points *points = _points[i_1D];
			float *loc_chk = new float[_ndims];
			float dist2, factor;
			int i;

			for (i = 0; i < points->num(); i++)
			{
				points->getLoc(i, loc_chk);

				dist2 = normalizedDistanceSquared(loc, loc_chk);

				if (dist2 <= 1.0F)
				{
					assert (dist2 > 0.0F);
					if (_stype == NN) {
					  if (!retval || dist2 < *min_dist2) {
					    *min_dist2 = dist2;
					    *weighted_sum = points->getVal(i);
					    *factor_sum = 1;
					    retval++;
                                          }
					}
					else /* if (_stype == WTD) */ {
					  factor = 1.0F /
						(float) sqrt((double) dist2);

					  retval++;
					  *weighted_sum += factor
						* points->getVal(i);
					  *factor_sum += factor;
					}
				}
			}

			delete [] loc_chk;
		}
	}
	else
	{
		for (	i_cur[cur_dim]  = i_min[cur_dim];
			i_cur[cur_dim] <= i_max[cur_dim];
			i_cur[cur_dim]++)
		{
			retval += withinReach(
				i_min, i_max, i_cur, cur_dim + 1, loc,
				weighted_sum, factor_sum, min_dist2);
		}
	}

	return retval;
}

float Gridder::normalizedDistanceSquared(
	const float *loc1, const float *loc2) const
{
	float retval;
	int i;

	for (retval = 0.0F, i = 0; i < _ndims; i++)
		retval += (loc2[i] - loc1[i]) / _reach[i]
			* (loc2[i] - loc1[i]) / _reach[i];

	return retval;
}

int Gridder::compar (int i_1D, const Points *point) const
{
  assert (i_1D > -1 && point->numDims() == _ndims);

  int retval = 0;

  if (_points[i_1D]->num() == point->num()) {

    float *given_loc = new float[_ndims];
    float *  cur_loc = new float[_ndims];
    float given_val, cur_val, ave_val, dif;
    float eps = 1e-6; // same if within 1 pt per million
    int k2, k3;
    for (k2 = 0; k2 < _points[i_1D]->num(); k2++) {
      point        ->getLoc (k2, given_loc);
      _points[i_1D]->getLoc (k2,   cur_loc);
// compare locations normalized to reach
      for (k3 = 0; !retval && k3 < _ndims; k3++) {
	dif = (float)fabs ((double)((given_loc[k3]-cur_loc[k3])/_reach[k3]));
	if (dif > eps) {
	  retval = 1;
	  break;
	}
      }
      if (retval) break;
// compare value normalized to average (unless average is very small!)
      given_val = point        ->getVal (k2);
        cur_val = _points[i_1D]->getVal (k2);
        ave_val = (float)fabs ((double)((given_val+cur_val)/(float)2));
      if (ave_val < eps) {
	dif = (float)fabs ((double)(given_val-cur_val));
      }
      else {
	dif = (float)fabs ((double)((given_val-cur_val)/ave_val));
      }
      if (dif > eps) {
	retval = 1;
	break;
      }
    }
    delete [] given_loc;
    delete []   cur_loc;
  }
  else {
    retval = 1;
  }
  return retval;
}

void Gridder::findLocationRange (int *count, const int *i_min,
  const int *i_max, int *i_cur, int cur_dim, float *mins, float *maxs) const
{
  if (cur_dim == _ndims) {
    int k2, index, in_bounds;
    float *min_loc = new float[_ndims];
    float *max_loc = new float[_ndims];
    for (k2 = 0, in_bounds = 1; in_bounds && k2 < _ndims; k2++) {
      index = i_cur[k2];
      if (index > -1 && index < _grid_size[k2]) {
	min_loc[k2] = gridLocation (mins[k2], _reach[k2], index, MIN);
	max_loc[k2] = gridLocation (mins[k2], _reach[k2], index, MAX);
      }
      else {
	in_bounds = 0;
      }
    }
    if (in_bounds) {
      if (!(*count)) {
	mins[k2] = min_loc[k2];
        maxs[k2] = max_loc[k2];
      }
      else {
	if (mins[k2] > min_loc[k2]) mins[k2] = min_loc[k2];
	if (maxs[k2] < max_loc[k2]) maxs[k2] = max_loc[k2];
      }
      (*count)++;
    }
    delete [] min_loc;
    delete [] max_loc;
  }
  else {
    for (i_cur[cur_dim]  = i_min[cur_dim];
	 i_cur[cur_dim] <= i_max[cur_dim];
         i_cur[cur_dim]++                 ) {
      findLocationRange (count, i_min, i_max, i_cur, cur_dim+1, mins, maxs);
    }
  }
}

void Gridder::extractValues (int *index, float *values, float *mins,
  int *i_max, float *incs, int *i_cur, float *locs, int dim) const
{
  if (dim == _ndims) {
    
    if (values) {
      int k2;
      for (k2 = 0; k2 < _ndims; k2++) {
	locs[k2] = mins[k2] + i_cur[k2] * incs[k2];
      }
      values[*index] = getVal (locs);
    }
    (*index)++; // the order of the output array coincides with the order
                // of the extrema and location arrays (beginning with the 0th
                // direction, then 1st, then 2nd, and so on)
  }
  else {
    for (i_cur[dim]  = 0         ;
         i_cur[dim] <= i_max[dim];
         i_cur[dim]++             ) {
      extractValues (index, values, mins, i_max, incs, i_cur, locs, dim+1);
    }
  }
}
