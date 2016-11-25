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
#include "interp/interp_2d_by_locs.hh"
#include <stdlib.h> 
#include <string.h>
#include <assert.h>

Interp2DByLocs::Interp2DByLocs (float *first, float *last, int *size,
  float nil_val, int stype, float *values, int allow_lt_2D) :
  Interpolator2D (first, last, size, nil_val, stype, values, allow_lt_2D),
  _loc_dim       (0),
  _locs_size     (0),
  _loc0_set      (0),
  _loc1_set      (0),
  _loc0_defined  (0),
  _loc1_defined  (0),
  _locs          (0),
  _loc0_values   (0),
  _loc1_values   (0),
  _prev_words    (0)
{
  setCorrectionFunction (defaultCorrectionFunction, this);
}

Interp2DByLocs::~Interp2DByLocs ()
{
  if (_locs        ) free (_locs        );
  if (_loc0_values ) free (_loc0_values );
  if (_loc1_values ) free (_loc1_values );
  if (_loc0_defined) free (_loc0_defined);
  if (_loc1_defined) free (_loc1_defined);
  if (_prev_words  ) free (_prev_words  );
}

int Interp2DByLocs::setLocs (int dim, float *locs, int *size)
{
  assert (dim > -1 && dim < 2 && locs && size[0] > 0 && size[1] > 0);
  int error = 1;
  int retval = !error;
  _loc_dim = dim;
  _loc0_set = 0;
  _loc1_set = 0;
  if (_locs) free (_locs), _locs = 0;
  _locs = (float *)malloc (sizeof(float)*size[dim]);
  if (_loc0_values) free (_loc0_values), _loc0_values = 0;
  _loc0_values = (float *)malloc (sizeof(float)*size[1-dim]);
  if (_loc1_values) free (_loc1_values), _loc1_values = 0;
  _loc1_values = (float *)malloc (sizeof(float)*size[1-dim]);
  if (_loc0_defined) free (_loc0_defined), _loc0_defined = 0;
  _loc0_defined = (int *)malloc (sizeof(float)*size[1-dim]);
  if (_loc1_defined) free (_loc1_defined), _loc1_defined = 0;
  _loc1_defined = (int *)malloc (sizeof(float)*size[1-dim]);
  if (_prev_words) free (_prev_words), _prev_words = 0;
  _prev_words = (float *)malloc (sizeof(float)*size[1-dim]);
  _prev_bytes = (unsigned char *)_prev_words;
  if (_locs && _loc0_values && _loc1_values &&
      _loc0_defined && _loc1_defined && _prev_words) {
    _locs_size = size[dim];
    memcpy (_locs, locs, sizeof(float)*_locs_size);
// sort the locs in ascending order
    int k2, k3;
    float tmp;
    for (k2 = 0; k2 < _locs_size; k2++) {
      for (k3 = k2+1; k3 < _locs_size; k3++) {
	if (_locs[k3] < _locs[k2]) {
	  tmp = _locs[k3];
	  _locs[k3] = _locs[k2];
	  _locs[k2] = tmp;
        }
      }
    }
    _last_wt1  = -2;
    _last_loc0 = -999;
    _last_loc1 = -99;
  }
  else {
    retval = error;
  }
  return retval;
}

int Interp2DByLocs::getValues (void *values, float *first, float *last,
  int *size)
{
  assert (_locs);

  int dim;
  int retval;
  assert (size[0] > 0 && size[1] > 0);
  if (size[0] > size[1]) {
    dim = 0;
  }
  else {
    dim = 1;
  }
  if (dim != 1-_loc_dim) {
// not interpolating in the dimension that can take advantage of the locs
//   array
    retval = Interpolator2D::getValues (values, first, last, size);
  }
  else {
    retval = getValuesByLoc (dim, values, first, last, size);
  }
  return retval;
}

int Interp2DByLocs::getValuesByLoc (int dim, void *values, float *first,
  float *last, int *size)
{
  int error = 1;
  float f_loc[2], l_loc[2], del, ftmp;
  int odim, isize[2], k2, k3, reverse_order, loc0, loc1, loc0_ok, loc1_ok;
  float *fvals, *tmp;
  int *itmp;
  int retval = !error;
  int offset;

  odim = 1 - dim;
  fvals = (float *)malloc (sizeof(float)*size[dim]);

  if (fvals) {
    f_loc[odim] = first[odim];
    l_loc[odim] = first[odim];
    isize[odim] = 1           ;
    f_loc[ dim] = first[ dim];
    l_loc[ dim] =  last[ dim];
    isize[ dim] =  size[ dim];
    if (size[odim] < 2) {
      del = 1.0;
      reverse_order = 0;
    }
    else {
      del = (last[odim] - first[odim]) / (size[odim] - 1);
      if (del < 0) {
	reverse_order = 1;
      }
      else {
	reverse_order = 0;
      }
    }
// find the first loc
    if (f_loc[odim] < _locs[0]) {
      loc0 = -1;
      loc1 =  0;
    }
    else {
      loc0 = _locs_size - 1;
      loc1 = _locs_size    ;
      for (k2 = 0; k2 < _locs_size-1; k2++) {
	if (f_loc[odim] >= _locs[k2] && f_loc[odim] <= _locs[k2+1]) {
	  loc0 = k2;
	  loc1 = k2 + 1;
	  break;
	}
      }
    }
    assert (loc0 != loc1);
    loc0_ok = 0;
    loc1_ok = 0;

    if (_loc0_set && _loc0 == loc0) {
      loc0_ok = 1;
      if (_loc1_set && _loc1 == loc1) {
	loc1_ok = 1;
      }
    }

    else if (_loc0_set && loc1 == _loc0) {
// make the next _loc1_values equal to the current _loc0_values
      loc1_ok = 1;
      tmp = _loc0_values;
      _loc0_values = _loc1_values;
      _loc1_values = tmp;
      itmp = _loc0_defined;
      _loc0_defined = _loc1_defined;
      _loc1_defined = itmp;
    }

    else if (_loc1_set && loc0 == _loc1) {
// make the next _loc0_values equal to the current _loc1_values
      loc0_ok = 1;
      tmp = _loc0_values;
      _loc0_values = _loc1_values;
      _loc1_values = tmp;
      itmp = _loc0_defined;
      _loc0_defined = _loc1_defined;
      _loc1_defined = itmp;
    }

    if (!loc0_ok) _loc0_set = 0;
    if (!loc1_ok) _loc1_set = 0;
    _loc0 = loc0;
    _loc1 = loc1;

    int repeat;
    unsigned char *byte_results;
    float *word_results;
    for (k2 = 0; k2 < size[odim]; k2++) {
      repeat = 0;

      if (!_loc0_set) {
	ftmp = f_loc[odim];
	if (_loc0 > -1 && _loc0 < _locs_size) {
	  f_loc[odim] = _locs[_loc0];
	  Interpolator2D::getValues (dim, _loc0_values, f_loc,
	    l_loc, isize);
	  f_loc[odim] = ftmp;
	  correctValues (_loc0_values, size[dim], LOC0);
	  setIllDefined (_loc0_defined, _loc0_values, size[dim]);
	  _loc0_set = 1;
	}
      }
      if (!_loc1_set) {
	ftmp = f_loc[odim];
	if (_loc1 > -1 && _loc1 < _locs_size) {
	  f_loc[odim] = _locs[_loc1];
	  Interpolator2D::getValues (dim, _loc1_values, f_loc,
            l_loc, isize);
	  f_loc[odim] = ftmp;
	  correctValues (_loc1_values, size[dim], LOC1);
	  setIllDefined (_loc1_defined, _loc1_values, size[dim]);
	  _loc1_set = 1;
	}
      }

      if (_loc0 < 0) {
	assert (_loc1 == 0);
	if (_last_loc1 == _loc1) {
	  repeat = 1;
	}
	else {
          for (k3 = 0; k3 < size[dim]; k3++) {
	    fvals[k3] = _loc1_values[k3];
          }
	  _last_wt1  = -2;
	  _last_loc0 = -999;
	  _last_loc1 = _loc1;
	}
      }

      else if (_loc1 > _locs_size-1) {
	assert (_loc0 == _locs_size-1);
	if (_last_loc0 == _loc0) {
	  repeat = 1;
	}
	else {
	  for (k3 = 0; k3 < size[dim]; k3++) {
	    fvals[k3] = _loc0_values[k3];
          }
	  _last_wt1  = -2;
	  _last_loc0 = _loc0;
	  _last_loc1 = -99;
	}
      }

      else {
// generate the necessary other dimensiom
	switch (_stype) {
          case NN:
          case NN_LIN:
	    repeat = getValuesByNN (f_loc[odim], fvals, size[dim]);
	    break;
          case LIN:
          case LIN_NN:
	    repeat = getValuesByLIN (f_loc[odim], fvals, size[dim]);
	    break;
          default:
	    assert (0);
	    break;
	}
      }

// increment other dimension location
      f_loc[odim] += del     ;
      l_loc[odim]  = f_loc[odim];
      if (f_loc[odim] < _locs[0]) {
	if (_loc0 == 0) {
	  tmp = _loc0_values;
	  _loc0_values = _loc1_values;
	  _loc1_values = tmp;
	  itmp = _loc0_defined;
	  _loc0_defined = _loc1_defined;
	  _loc1_defined = itmp;
	}
	else if (_loc1 != 0) {
	  _loc1_set = 0;
	}
	_loc0_set = 0;
	_loc0 = -1;
	_loc1 =  0;
      }
      else if (f_loc[odim] > _locs[_locs_size-1]) {
	if (_loc1 == _locs_size-1) {
	  tmp = _loc0_values;
	  _loc0_values = _loc1_values;
	  _loc1_values = tmp;
	  itmp = _loc0_defined;
	  _loc0_defined = _loc1_defined;
	  _loc1_defined = itmp;
	}
	else if (_loc0 != _locs_size-1) {
	  _loc0_set = 0;
	}
	_loc1_set = 0;
	loc0 = _locs_size - 1;
	loc1 = _locs_size    ;
      }
      else {
	if (reverse_order) {
	  for (k3 = _loc1; k3 > 0; k3--) {
	    if (f_loc[odim] >= _locs[k3-1] && f_loc[odim] <= _locs[k3]) {
	      if (_loc0 == k3-1 && _loc1 == k3) {
		break;
	      }
	      else if (_loc0 == k3) {
		tmp = _loc0_values;
		_loc0_values = _loc1_values;
		_loc1_values = tmp;
		itmp = _loc0_defined;
		_loc0_defined = _loc1_defined;
		_loc1_defined = itmp;
	      }
	      else {
		_loc1_set = 0;
	      }
	      _loc0_set = 0;
	      _loc0 = k3 - 1;
	      _loc1 = k3    ;
	      break;
	    }
	  }
	}
	else {
	  for (k3 = _loc0; k3 < _locs_size-1; k3++) {
	    if (f_loc[odim] >= _locs[k3] && f_loc[odim] <= _locs[k3+1]) {
	      if (_loc0 == k3 && _loc1 == k3+1) {
		break;
	      }
	      else if (_loc1 == k3) {
		tmp = _loc0_values;
		_loc0_values = _loc1_values;
		_loc1_values = tmp;
		itmp = _loc0_defined;
		_loc0_defined = _loc1_defined;
		_loc1_defined = itmp;
	      }
	      else {
		_loc0_set = 0;
	      }
	      _loc1_set = 0;
	      _loc0 = k3    ;
	      _loc1 = k3 + 1;
	      break;
	    }
	  }
	}
      }
// output the values
      switch (dim) {
        case 0:
	  offset = k2;
	  if (repeat) {
	    switch (_conversion_value_size) {
	      case 1:
		byte_results = (unsigned char *)values;
		for (k3 = 0; k3 < size[0]; k3++) {
		  byte_results[offset] = _prev_bytes[k3];
		  offset += size[1];
		}
		break;
	      case 4:
		word_results = (float *)values;
		for (k3 = 0; k3 < size[0]; k3++) {
		  word_results[offset] = _prev_words[k3];
		  offset += size[1];
		}
		break;
	      default:
		assert (0);
	    }
	  }
	  else {
	    switch (_conversion_value_size) {
	      case 1:
		byte_results = (unsigned char *)values;
		for (k3 = 0; k3 < size[0]; k3++) {
		  _conversion_function (_conversion_obj,
		    &(_prev_bytes[k3]), fvals[k3]);
		  byte_results[offset] = _prev_bytes[k3];
		  offset += size[1];
		}
		break;
	      case 4:
		word_results = (float *)values;
		for (k3 = 0; k3 < size[0]; k3++) {
		  _conversion_function (_conversion_obj,
		    &(_prev_words[k3]), fvals[k3]);
		  word_results[offset] = _prev_words[k3];
		  offset += size[1];
		}
		break;
	      default:
		assert (0);
	    }
	  }
	  break;
        case 1:
	  if (repeat) {
	    switch (_conversion_value_size) {
	      case 1:
		byte_results = (unsigned char *)values;
		memcpy (&(byte_results[k2*size[1]]),_prev_bytes,
		  (size_t)size[1]);
		break;
	      case 4:
		word_results = (float *)values;
		memcpy (&(word_results[k2*size[1]]),_prev_words,
		  (size_t)4*size[1]);
		break;
	      default:
		assert (0);
	    }
	  }
	  else {
	    offset = k2 * size[1];
	    switch (_conversion_value_size) {
	      case 1:
		byte_results = (unsigned char *)values;
		for (k3 = 0; k3 < size[1]; k3++) {
		  _conversion_function (_conversion_obj,
		    &(_prev_bytes[k3]), fvals[k3]);
		  byte_results[offset+k3] = _prev_bytes[k3];
		}
		break;
	      case 4:
		word_results = (float *)values;
		for (k3 = 0; k3 < size[1]; k3++) {
		  _conversion_function (_conversion_obj,
		    &(_prev_words[k3]), fvals[k3]);
		  word_results[offset+k3] = _prev_words[k3];
		}
		break;
	      default:
		assert (0);
	    }
	  }
      }
    }
    free (fvals);
  }
  else {
    retval = error;
  }
  return retval;
}

/* on repeat condition, nothing is done to given values array */
int Interp2DByLocs::getValuesByNN (float loc, float *values, int size)
{
  int repeat = 0;

  float wt1;
  if (_locs[_loc1] == _locs[_loc0]) {
    wt1 = 0.0;
  }
  else {
    wt1 = (loc - _locs[_loc0]) / (_locs[_loc1] - _locs[_loc0]);
  }

  if (wt1 < 0.5) {
    if (_last_wt1 >= 0.0 && _last_wt1 < 0.5 && _last_loc0 == _loc0) {
      repeat = 1;
    }
    else {
      memcpy (values, _loc0_values, sizeof(float)*size);
      _last_wt1 = wt1;
      _last_loc0 = _loc0;
    }
  }
  else {
    if (_last_wt1 >= 0.5 && _last_wt1 <= 1.0 && _last_loc1 == _loc1) {
      repeat = 1;
    }
    else {
      memcpy (values, _loc1_values, sizeof(float)*size);
      _last_wt1 = wt1;
      _last_loc1 = _loc1;
    }
  }
  return repeat;
}

/* on repeat condition, nothing is done to given values array */
int Interp2DByLocs::getValuesByLIN (float loc, float *values, int size)
{
  int repeat = 0;

  float wt1;
  if (_locs[_loc1] == _locs[_loc0]) {
    wt1 = 0.0;
  }
  else {
    wt1 = (loc - _locs[_loc0]) / (_locs[_loc1] - _locs[_loc0]);
  }

  if (_last_wt1 == wt1 && _last_loc0 == _loc0 && _last_loc1 == _loc1) {
    repeat = 1;
  }
  else {
    _last_wt1 = wt1;
    _last_loc0 = _loc0;
    _last_loc1 = _loc1;
  }

  if (!repeat) {
    float wt0 = 1 - wt1;
    if (wt1 == 0) {
      memcpy (values, _loc0_values, sizeof(float)*size);
    }
    else if (wt1 == 1) {
      memcpy (values, _loc1_values, sizeof(float)*size);
    }
    else {
      int k2;
      for (k2 = 0; k2 < size; k2++) {
	if (!_loc0_defined[k2] || !_loc1_defined[k2]) {
	  if (wt1 < 0.5) {
	    values[k2] = _loc0_values[k2];
	  }
	  else {
	    values[k2] = _loc1_values[k2];
	  }
	}
	else {
	  values[k2] = wt0 * _loc0_values[k2] + wt1 * _loc1_values[k2];
	}
      }
    }
  }
  return repeat;
}

void Interp2DByLocs::setIllDefined (int *defined, float *values, int size)
{
  int k2;
  for (k2 = 0; k2 < size; k2++) {
    if (values[k2] == _nil_val) {
      defined[k2] = 0;
    }
    else {
      defined[k2] = 1;
    }
  }
}

void Interp2DByLocs::correctValues (float *values, int size,
  int which_loc)
{
  int k2;
  for (k2 = 0; k2 < size; k2++) {
    _correction_function (_correction_obj, &(values[k2]), values[k2],
      which_loc);
  }
}

void Interp2DByLocs::defaultCorrectionFunction (void * /* obj */,
  float *result, float value, int /* which_loc */)
{
  memcpy (result, &value, sizeof(float));
}

void Interp2DByLocs::setCorrectionFunction (InterpCorrectionFunction func,
  void *obj)
{
  _correction_function   = func;
  _correction_obj        = obj;
}
