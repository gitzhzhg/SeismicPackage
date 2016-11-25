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
#include "interp/gridder_3d.hh"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>

Index3D::Index3D (int *dim)
{
  memcpy (_dim, dim, (size_t)3*sizeof(int));

  int k2;
  _span[2] = 1;
  _span[1] = _dim[2];
  _span[0] = _dim[2] * _dim[1];
  _total_size = _span[0] * _dim[0];
}

Index3D::~Index3D ()
{
}

int Index3D::totalSize () const
{
  return _total_size;
}

int Index3D::to1D (int *i_3D) const
{
  int k2, retval;

  for (k2 = retval = 0; (retval != -1) && (k2 < 3); k2++) {
    if ((i_3D[k2] >= 0) && (i_3D[k2] < _dim[k2])) {
      retval += _span[k2] * i_3D[k2];
    }
    else {
      retval = -1;
    }
  }
  return retval;
}

void Index3D::from1D (int i_1D, int *i_3D) const
{
  int k2;

  if ((i_1D >= 0) && (i_1D < _total_size)) {
    for (k2 = 0; k2 < 3; k2++) {
      i_3D[k2] = i_1D / _span[k2];
      i_1D     = i_1D % _span[k2];
    }
  }
  else {
    for (k2 = 0; k2 < 3; k2++) {
      i_3D[k2] = -1;
    }
  }
}


Points3D::Points3D () :
  _num        (0),
  _num_alloc  (0),
  _loc        (0),
  _val        (0)
{
}

Points3D::~Points3D ()
{
  if (_num_alloc) {
    delete [] _loc_block;
    delete [] _loc;
    delete [] _val;
  }
}

int Points3D::add (const float *loc, float val)
{
  int k2, k3, retval = 1;
  if (_num == _num_alloc) {
    _num_alloc += ALLOC_INC;
    float *tmp_loc_block, *tmp_val;
    try {
      if (_loc) delete [] _loc;
      tmp_loc_block = new float [_num_alloc*3];
      _loc          = new float*[_num_alloc];
      tmp_val       = new float [_num_alloc  ];
    }
    catch (...) {
      retval = 0;
    }

    if (_num && retval) {
      memcpy (tmp_loc_block, _loc_block, sizeof(float )*(size_t)(_num*3));
      memcpy (tmp_val      , _val      , sizeof(float )*(size_t) _num   );
      delete [] _loc_block;
      delete [] _val      ;
    }
    if (retval) {
      _loc_block = tmp_loc_block;
      k3 = 0;
      for (k2 = 0; k2 < _num_alloc; k2++) {
	_loc[k2] = &(_loc_block[k3]);
	k3 += 3;
      }
      _val = tmp_val;
    }
  }

  if (retval) {
    memcpy (_loc[_num], loc, sizeof(float)*(size_t)3);
    _val[_num] = val;
    _num++;
  }
  return retval;
}

int Points3D::add (const Points3D *points, int index)
{
  int retval;

  float loc[3];
  points->getLoc (index, loc);

  float val = points->getVal (index);

  retval = add (loc, val);

  return retval;
}

int Points3D::num () const
{
  return _num;
}

void Points3D::getLoc (int index, float *loc) const
{
  memcpy (loc, _loc[index], (size_t)3*sizeof(float));
}

float Points3D::getVal(int index) const
{
  return _val[index];
}


Gridder3D::Gridder3D (float *min, float *max, float *reach, float nil_val,
  int stype) :
  _nil_val  (nil_val),
  _stype    (stype)
{
  assert (_stype == WTD || _stype == NN);

  // Make sure inputs make sense.
  int k2;
  for (k2 = 0; k2 < 3; k2++) {
    assert (reach[k2] >  0.0F   );
    assert (max  [k2] >= min[k2]);
  }

  memcpy (_min  , min  , (size_t)3*sizeof(float));
  memcpy (_max  , max  , (size_t)3*sizeof(float));
  memcpy (_reach, reach, (size_t)3*sizeof(float));

  for (k2 = 0; k2 < 3; k2++) {
    _grid_size[k2] = gridIndex (_min[k2], _reach[k2], _max[k2]) + 1;
  }

// create index object for selecting a Points3D pointer
  _index = new Index3D (_grid_size);

// create an array of Points3D pointers to accomodate every grid cell
  _points = new Points3D*[_index->totalSize()];

// nullify all of the Points3D pointers (create them as needed)
  for (k2 = 0; k2 < _index->totalSize(); k2++) {
    _points[k2] = 0;
  }
}

Gridder3D::~Gridder3D ()
{
// delete the existing Points3D objects
  int k2;
  for (k2 = 0; k2 < _index->totalSize(); k2++) {
    if (_points[k2]) delete _points[k2];
  }
  delete [] _points;
  delete _index;
}

int Gridder3D::addPoint (const float *loc, float val)
{
  int retval = 1;
  int i_3D[3];

  int k2;
  for (k2 = 0; k2 < 3; k2++) {
    assert ((loc[k2] >= _min[k2]) && (loc[k2] <= _max[k2]));
    i_3D[k2] = gridIndex (_min[k2], _reach[k2], loc[k2]);
  }

  int i_1D = _index->to1D (i_3D);
  assert (i_1D > -1);
  if (!_points[i_1D]) {
// first time this cell has been needed
    try {
      _points[i_1D] = new Points3D ();
    }
    catch (...) {
      retval = 0;
    }
  }
  if (retval) retval = _points[i_1D]->add (loc, val);
  return retval;
}

void Gridder3D::getSpecs (float *min, float *max, float *reach,
  float *nil_val, int *stype) const
{
  int k2;
  for (k2 = 0; k2 < 3; k2++) {
    min[k2]   = _min[k2];
    max[k2]   = _max[k2];
    reach[k2] = _reach[k2];
  }
  if (nil_val) *nil_val = _nil_val;
  if (stype)   *stype   = _stype;
}

float Gridder3D::getVal (const float *loc) const
{
  float retval;

  int num_same_pts;
  float sum, weighted_sum, factor_sum;

  if (num_same_pts = sameLocation(loc, &sum)) {  // = not ==
    retval = sum / (float) num_same_pts;
  }
  else if (withinReach (loc,&weighted_sum,&factor_sum)) {
    retval = weighted_sum / factor_sum;
  }
  else {
    retval = _nil_val;
  }

  return retval;
}

void Gridder3D::setResampleType (int stype)
{
  assert (stype == WTD || stype == NN);
  _stype = stype;
}

// create an additional copy of the Points3D array and return the pointr to it
void Gridder3D::getPoints (Points3D ***points, int *grid_size) const
{
  int k2, ok = 1;

  if (_grid_size[0] && _grid_size[1] && _grid_size[2]) {
    for (k2 = 0; k2 < 3; k2++) {
      grid_size[k2] = _grid_size[k2];
    }
    
    Points3D *tmp_point;
    try {
      (*points) = new Points3D*[_index->totalSize()];
    }
    catch (...) {
      ok = 0;
      *points = 0;
    }
    if (ok) {
      int k3;
      for (k2 = 0; ok && k2 < _index->totalSize(); k2++) {
	(*points)[k2] = 0;
	if (_points[k2]) {
	  try {
	    tmp_point = new Points3D ();
	    for (k3 = 0; ok && k3 < _points[k2]->num(); k3++) {
	      ok = tmp_point->add (_points[k2], k3);
            }
	    if (!ok) {
	      for (k3 = 0; k3 < k2; k3++) {
		delete (*points)[k3];
	      }
	      delete [] (*points);
	      *points = 0;
            }
	    else {
	      (*points)[k2] = tmp_point;
	    }
	  }
	  catch (...) {
	    ok = 0;
	    for (k3 = 0; k3 < k2; k3++) {
	      delete (*points)[k3];
            }
	    delete [] (*points);
	    *points = 0;
	  }
	}
      }
    }
  }
  else {
    *points = 0;
    for (k2 = 0; k2 < 3; k2++) {
      grid_size[k2] = 0;
    }
  }
}

int Gridder3D::findRangeOfPossibleChange (const Points3D **points,
  int *grid_size, float *mins, float *maxs) const
{
  assert (points &&
    grid_size[0] > 0 && grid_size[0] == _grid_size[0] &&
    grid_size[1] > 0 && grid_size[1] == _grid_size[1] &&
    grid_size[2] > 0 && grid_size[2] == _grid_size[2] &&
    mins && maxs                                        );

  int retval = 0;
  int k2, k3, i_3D[3], mn_indx, mx_indx;
  float tmin, tmax;

  for (k2 = 0; k2 < _index->totalSize(); k2++) {
    if (compar(k2,points[k2])) {
      _index->from1D (k2, i_3D);
      if (!retval) {
	for (k3 = 0; k3 < 3; k3++) {
// pick the min and max index two reaches beyond due to the influence a
//   cell can have on its neighbor
	  mn_indx = i_3D[k3] > 1
	    ? i_3D[k3] - 2
	    : i_3D[k3] > 0
	    ? i_3D[k3] - 1
	    : i_3D[k3];
	  mx_indx = i_3D[k3] < grid_size[k3] - 2
	    ? i_3D[k3] + 2
	    : i_3D[k3] < grid_size[k3] - 1
	    ? i_3D[k3] + 1
	    : i_3D[k3];
	  mins[k3] = gridLocation (_min[k3], _reach[k3], mn_indx, MIN);
	  maxs[k3] = gridLocation (_min[k3], _reach[k3], mx_indx, MAX);
	}
      }
      else {
	for (k3 = 0; k3 < 3; k3++) {
	  mn_indx = i_3D[k3] > 0
	    ? i_3D[k3] - 1
	    : i_3D[k3];
	  mx_indx = i_3D[k3] < grid_size[k3] - 1
	    ? i_3D[k3] + 1
	    : i_3D[k3];
	  tmin = gridLocation (_min[k3], _reach[k3], mn_indx, MIN);
	  tmax = gridLocation (_min[k3], _reach[k3], mx_indx, MAX);
	  if (tmin < mins[k3]) mins[k3] = tmin;
	  if (tmax > maxs[k3]) maxs[k3] = tmax;
	}
      }
      retval++;
    }
  }
  return retval; // # of cells with changes
}

int Gridder3D::extractValues (float *values, float *mins, float *maxs,
  float *incs) const
{
  assert (values && mins && maxs && incs);

  int i_num[3];
  float locs[3];

  int k2;
  for (k2 = 0; k2 < 3; k2++) {
    assert ((maxs[k2]-mins[k2])/incs[k2] >= 0.0);
    i_num[k2] = (int)((double)(maxs[k2] - mins[k2]) / incs[k2] + 1.5); 
  }

  int k3, k4, retval = 0;
  for (k2 = 0, locs[0] = mins[0]; k2 < i_num[0];
    k2++, locs[0] += incs[0]) {
    for (k3 = 0, locs[1] = mins[1]; k3 < i_num[1];
      k3++, locs[1] += incs[1]) {
      for (k4 = 0, locs[2] = mins[2]; k4 < i_num[2];
        k4++, locs[2] += incs[2]) {
  // the order of the output array coincides with the order
  // of the extrema and location arrays (beginning with the 0th
  // direction, then 1st, and then 2nd)
	values[retval] = getVal (locs);
	retval++;
      }
    }
  }
  return retval;
}

int Gridder3D::gridIndex(float min, float reach, float loc) const
{
	return (int) floor((double) ((loc - min) / reach));
}

float Gridder3D::gridLocation (float min, float reach, int index,
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

int Gridder3D::sameLocation (const float *loc, float *sum) const
{
  int retval = 0;
  *sum = 0.0F;

  int i_3D[3], i_1D;
  int k2;
  for (k2 = 0; k2 < 3; k2++) {
    i_3D[k2] = gridIndex (_min[k2], _reach[k2],  loc[k2]);
    if (i_3D[k2] < 0 || i_3D[k2] > _grid_size[k2]-1) break;
  }

  if (k2 == 3) {
    i_1D = _index->to1D (i_3D);
    assert (i_1D > -1);
    Points3D *points = _points[i_1D];
    float loc_chk[3];
    float normalized_diff;

// Use normalized_diff >= eps for not equal instead of !=
// to avoid undeflow leading to divide by zero later.
    static const float eps = 1.0e-6;

    int k3;
    for (k2 = 0; points && k2 < points->num(); k2++) {
      points->getLoc (k2, loc_chk);

      for (k3 = 0; k3 < 3; k3++) {
	normalized_diff = (loc[k3] - loc_chk[k3]) / _reach[k3];

	if ((normalized_diff >  eps) ||
	    (normalized_diff < -eps)   ) {
	  break;
	}
      }

      if (k3 == 3) {
	*sum += points->getVal (k2);
	retval++;
      }
    }
  }
  return retval;
}

int Gridder3D::withinReach (const float *loc, float *weighted_sum,
  float *factor_sum) const
{
  int retval = 0;
  *weighted_sum = *factor_sum = 0.0F;

  int k2, i_min[3], i_max[3], bin_index;
  float min_dist2;

  for (k2 = 0; k2 < 3; k2++) {
    bin_index = gridIndex (_min[k2], _reach[k2], loc[k2]);
    if (bin_index < 1) {
      if (bin_index < 0) break;
      i_min[k2] = bin_index;
    }
    else {
      i_min[k2] = bin_index - 1;
    }
    if (bin_index > _grid_size[k2]-2) {
      if (bin_index > _grid_size[k2]-1) break;
      i_max[k2] = bin_index;
    }
    else {
      i_max[k2] = bin_index + 1;
    }
  }

  int i_3D[3], i_1D;
  float loc_chk[3], dist2, factor, min_dist;
  Points3D *points;
  int k3, k4, k5;
  if (k2 == 3) {
    for (k2 = i_min[0]; k2 <= i_max[0]; k2++) {
      i_3D[0] = k2;
      for (k3 = i_min[1]; k3 <= i_max[1]; k3++) {
	i_3D[1] = k3;
        for (k4 = i_min[2]; k4 <= i_max[2]; k4++) {
	  i_3D[2] = k4;
	  i_1D = _index->to1D (i_3D);
	  assert (i_1D > -1);
	  points = _points[i_1D];
	  for (k5 = 0; points && k5 < points->num(); k5++) {

 	    points->getLoc (k5, loc_chk);
	    dist2 = normalizedDistanceSquared (loc, loc_chk);

	    if (dist2 <= 1.0F) {
	      assert (dist2 > 0.0F);
	      if (_stype == NN) {
		if (!retval || dist2 < min_dist2) {
		  min_dist2 = dist2;
		  *weighted_sum = points->getVal (k5);
		  *factor_sum = 1;
		  retval++;
                }
	      }
	      else /* if (_stype == WTD) */ {
		factor = 1.0F /	(float)sqrt((double)dist2);
		*weighted_sum += factor * points->getVal(k5);
		*factor_sum += factor;
		retval++;
	      }
	    }
	  }
        }
      }
    }
  }
  return retval;
}

float Gridder3D::normalizedDistanceSquared (const float *loc1,
  const float *loc2) const
{
  float retval;
  int k2;

  for (retval = 0.0F, k2 = 0; k2 < 3; k2++) {
    retval += (loc2[k2] - loc1[k2]) / _reach[k2]
	    * (loc2[k2] - loc1[k2]) / _reach[k2];
  }
  return retval;
}

int Gridder3D::compar (int i_1D, const Points3D *point) const
{
  assert (i_1D > -1);
  int retval = 0;

  Points3D *cur_point = _points[i_1D];

  if (!cur_point && !point) {
  }
  else if (cur_point && point && cur_point->num() == point->num()) {

    float given_loc[3], cur_loc[3];
    float given_val, cur_val, ave_val, dif;
    float eps = 1e-6; // same if within 1 pt per million
    int k2, k3;
    for (k2 = 0; k2 < cur_point->num(); k2++) {
          point->getLoc (k2, given_loc);
      cur_point->getLoc (k2,   cur_loc);
// compare locations normalized to reach
      for (k3 = 0; !retval && k3 < 3; k3++) {
/*        printf ("given_loc[%d] = %f, cur_loc[%d] = %f, reach[%d] = %f\n",
	  k3, given_loc[k3], k3, cur_loc[k3], k3, _reach[k3]); */
	dif = (float)fabs ((double)((given_loc[k3]-cur_loc[k3])/_reach[k3]));
	if (dif > eps) {
	  retval = 1;
	  break;
	}
      }
      if (retval) break;
// compare value normalized to average (unless average is very small!)
      given_val =     point->getVal (k2);
        cur_val = cur_point->getVal (k2);
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
  }
  else {
    retval = 1;
  }
  return retval;
}
