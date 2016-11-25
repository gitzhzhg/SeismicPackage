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
#include "interp/interpolator_2d.hh"
#include <stdlib.h> 
#include <string.h>
#include <assert.h>

Interpolator2D::Interpolator2D (float *first, float *last, int *size,
  float nil_val, int stype, float *values, int allow_lt_2D) :
  _stype          (stype),
  _allow_lt_2D    (allow_lt_2D),
  _block_owned    (0),
  _block_unowned  (0),
  _nil_val        (nil_val),
  _block          (0),
  _values         (0)
{
  // Make sure inputs make sense.
  assert (_stype == LIN    || _stype == NN     ||
          _stype == LIN_NN || _stype == NN_LIN   );
  int k2;
  for (k2 = 0; k2 < 2; k2++) {
    assert ( size[k2] >  0);
  }

  memcpy (_first, first, sizeof(float)*2);
  memcpy (_last ,  last, sizeof(float)*2);
  memcpy (_size ,  size, sizeof(int  )*2);

  if (values) {
    moveBlock (values);
  }

  for (k2 = 0; k2 < 2; k2++) {
    if (_size[k2] > 1) {
      _del  [k2] = (last[k2] - first[k2]) / (size[k2] - 1);
    }
    else {
      if (first[k2] == 0.0) {
	_del[k2] = 1.0;
      }
      else {
        _del[k2] = 1000 * first[k2];
      }
    }
    _loc_set[k2] = 0;
     _wt_set[k2] = 0;
     _id_set[k2] = 0;
  }
  setConversionFunction (defaultConversionFunction, this);
}

Interpolator2D::~Interpolator2D ()
{
  if (_block_owned) free (_block);
  if (_values     ) free (_values);
}

void Interpolator2D::clearBlock ()
{
  if (_block_owned) {
    assert (_block);
    free (_block), _block = 0;
    _block_owned = 0;
  }
  else if (_block_unowned) {
    _block = 0;
    _block_unowned = 0;
  }
}

void Interpolator2D::moveBlock (float *values)
{
  assert (values);
  clearBlock ();

// declare a contiguous block of memory the size of the 2D array
  assert ((_block
    = (float *)malloc(sizeof(float)*_size[0]*_size[1])) != 0);
  memcpy (_block, values, sizeof(float)*_size[0]*_size[1]);
  assignValues ();
  _block_owned = 1;
}

void Interpolator2D::pointToBlock (float *values)
{
  assert (values);
  clearBlock ();
  _block = values;
  assignValues ();
  _block_unowned = 1;
}

void Interpolator2D::getSpecs (float **values, float *first,
  float *last, int *size) const
{
  *values = _block; // pointer to block
  int k2;
  for (k2 = 0; k2 < 2; k2++) {
    first[k2] = _first[k2];
    last [k2] = _last [k2];
    size [k2] = _size [k2];
  }
}

int Interpolator2D::getValues (void *values, float *first, float *last,
  int *size) const
{
  int dim, error = 1;
  assert (size[0] > 0 && size[1] > 0);
  int retval = !error;

  if (size[0] > size[1]) {
    dim = 0;
  }
  else {
    dim = 1;
  }

  float f_loc[2], l_loc[2], del, *fvals;
  int isize[2], k2, k3, offset;
  float *word_results;
  unsigned char *byte_results;

  if ((fvals = (float *)malloc(sizeof(float)*size[dim])) != 0) {
    switch (dim) {
      case 0:
	f_loc[0] = first[0];
	l_loc[0] =  last[0];
	isize[0] =  size[0];
	f_loc[1] = first[1];
	l_loc[1] = first[1];
	isize[1] = 1       ;
	if (size[1] < 2) {
	  del = 1.0;
	}
	else {
	  del = (last[1] - first[1]) / (size[1] - 1);
	}

        for (k2 = 0; k2 < size[1]; k2++) {
	  offset = k2;
	  getValues (dim, fvals, f_loc, l_loc, isize);
	  switch (_conversion_value_size) {
	    case 1:
	      byte_results = (unsigned char *)values;
	      for (k3 = 0; k3 < size[0]; k3++) {
		_conversion_function (_conversion_obj,
		  &(byte_results[offset]), fvals[k3]);
		offset += size[1];
	      }
	      break;
	    case 4:
	      word_results = (float *)values;
	      for (k3 = 0; k3 < size[0]; k3++) {
		_conversion_function (_conversion_obj,
		  &(word_results[offset]), fvals[k3]);
		offset += size[1];
	      }
	      break;
	    default:
	      assert (0);
          }
	  f_loc[1] += del     ;
	  l_loc[1]  = f_loc[1];
        }
	break;

      case 1:
	f_loc[0] = first[0];
	l_loc[0] = first[0];
	isize[0] = 1       ;
	f_loc[1] = first[1];
	l_loc[1] =  last[1];
	isize[1] =  size[1];
	if (size[0] < 2) {
	  del = 1.0;
	}
	else {
	  del = (last[0] - first[0]) / (size[0] - 1);
	}
	offset = 0;
	for (k2 = 0; k2 < size[0]; k2++) {
	  getValues (dim, fvals, f_loc, l_loc, isize);
	  switch (_conversion_value_size) {
	    case 1:
	      byte_results = (unsigned char *)values;
	      for (k3 = 0; k3 < size[1]; k3++) {
	        _conversion_function (_conversion_obj,
		  &(byte_results[offset+k3]), fvals[k3]);
	      }
	      break;
	    case 4:
	      word_results = (float *)values;
	      for (k3 = 0; k3 < size[1]; k3++) {
	        _conversion_function (_conversion_obj,
		  &(word_results[offset+k3]), fvals[k3]);
	      }
	      break;
	    default:
	      assert (0);
	  }
	  f_loc[0] += del     ;
	  l_loc[0]  = f_loc[0];
	  offset += size[1];
	}
	break;

      default:
	assert (0);
	break;
    }
    free (fvals);
  }
  else {
    retval = error;
  }
  return retval;
}

void Interpolator2D::getValues (int dim, float *values, float *first,
  float *last, int *size) const
{
  assert (_block_owned || _block_unowned);

  float wt0, wt1, del0, del1, loc0, loc1, value0a, value0b, value1a,
    value1b;
  int id0=-100, id1=-100, jd0, jd1, kd0, kd1, k2, id0_set, id1_set,
    ill_defined;
  switch (_stype) {
    case LIN:
      switch (dim) {
        case 0:
          id1  = cellIndex (1, first[1]);
          if (id1 == _size[1]-1) id1 = _size[1] - 2;
	  if (indexInValidRange(1,&id1)) {
            wt1  = first[1] / _del[1] - id1;
            wt1  = wt1 > 0.0 ? (wt1 < 1.0 ? wt1 : 1.0) : 0.0;
	    if (size[0] < 2) {
	      del0 = 0.0;
	    }
	    else {
              del0 = (last[0] - first[0]) / (float)(size[0] - 1);
	    }
            loc0 = first[0];
	    id0_set = 0;
            for (k2 = 0; k2 < size[0]; k2++) {
              jd0 = cellIndex (0, loc0);
	      if (jd0 == _size[0]-1) jd0 = _size[0] - 2;
	      if (indexInValidRange(0,&jd0)) {
	        if (!id0_set || jd0 != id0) {
	          id0 = jd0;
		  if ((ill_defined = illDefined(id0,id1))) {
		    if (wt1 < 0.5) {
		      value0a = _values[id0  ][id1];
		      value0b = _values[id0+1][id1];
		    }
		    else {
		      value0a = _values[id0  ][id1+1];
		      value0b = _values[id0+1][id1+1];
		    }
		  }
		  else {
	            value0a =        wt1  * _values[id0  ][id1+1]
		       + ((float)1 - wt1) * _values[id0  ][id1  ];
	            value0b =        wt1  * _values[id0+1][id1+1]
		       + ((float)1 - wt1) * _values[id0+1][id1  ];
		  }
	          id0_set = 1;
                }
                wt0 = loc0 / _del[0] - id0;
                wt0 = wt0 > 0.0 ? (wt0 < 1.0 ? wt0 : 1.0) : 0.0;
		if (ill_defined) {
		  if (wt0 < 0.5) {
		    values[k2] = value0a;
		  }
		  else {
		    values[k2] = value0b;
		  }
		}
		else {
                  values[k2] = wt0 * value0b + ((float)1 - wt0) * value0a;
	        }
	      }
              loc0 += del0;
	    }
          }
          else {
	    for (k2 = 0; k2 < size[1]; k2++) {
	      values[k2] = _nil_val;
	    }
          }
	  break;
          
        case 1:
          id0  = cellIndex (0, first[0]);
          if (id0 == _size[0]-1) id0 = _size[0] - 2;
	  if (indexInValidRange(0,&id0)) {
            wt0  = first[0] / _del[0] - id0;
            wt0  = wt0 > 0.0 ? (wt0 < 1.0 ? wt0 : 1.0) : 0.0;
	    if (size[1] < 2) {
	      del1 = 0.0;
	    }
	    else {
              del1 = (last[1] - first[1]) / (float)(size[1] - 1);
	    }
            loc1 = first[1];
	    id1_set = 0;
            for (k2 = 0; k2 < size[1]; k2++) {
              jd1 = cellIndex (1, loc1);
	      if (jd1 == _size[1]-1) jd1 = _size[1] - 2;
	      if (indexInValidRange(1,&jd1)) {
	        if (!id1_set || jd1 != id1) {
	          id1 = jd1;
		  if ((ill_defined = illDefined(id0,id1))) {
		    if (wt0 < 0.5) {
		      value1a = _values[id0][id1  ];
		      value1b = _values[id0][id1+1];
		    }
		    else {
		      value1a = _values[id0+1][id1  ];
		      value1b = _values[id0+1][id1+1];
		    }
		  }
		  else {
	            value1a =        wt0  * _values[id0+1][id1  ]
	               + ((float)1 - wt0) * _values[id0  ][id1  ];
	            value1b =        wt0  * _values[id0+1][id1+1]
		       + ((float)1 - wt0) * _values[id0  ][id1+1];
		  }
	          id1_set = 1;
                }
                wt1 = loc1 / _del[1] - id1;
                wt1 = wt1 > 0.0 ? (wt1 < 1.0 ? wt1 : 1.0) : 0.0;
		if (ill_defined) {
		  if (wt1 < 0.5) {
		    values[k2] = value1a;
		  }
		  else {
		    values[k2] = value1b;
		  }
		}
		else {
                  values[k2] = wt1 * value1b + ((float)1 - wt1) * value1a;
		}
	      }
              loc1 += del1;
	    }
          }
          else {
	    for (k2 = 0; k2 < size[1]; k2++) {
	      values[k2] = _nil_val;
	    }
          }
          break;

        default:
          assert (0);
          break;
      }
      break;

    case NN:
      switch (dim) {
        case 0:
          id1  = cellIndex (1, first[1]);
          if (id1 == _size[1]-1) id1 = _size[1] - 2;
	  if (indexInValidRange(1,&id1)) {
            wt1  = first[1] / _del[1] - id1;
            wt1  = wt1 > 0.0 ? (wt1 < 1.0 ? wt1 : 1.0) : 0.0;
	    if (wt1 < 0.5) {
	      kd1 = id1;
	    }
	    else {
	      kd1 = id1 + 1;
	    }
	    if (size[0] < 2) {
	      del0 = 0.0;
	    }
	    else {
              del0 = (last[0] - first[0]) / (float)(size[0] - 1);
	    }
            loc0 = first[0];
            for (k2 = 0; k2 < size[0]; k2++) {
              jd0 = cellIndex (0, loc0);
	      if (jd0 == _size[0]-1) jd0 = _size[0] - 2;
	      if (indexInValidRange(0,&jd0)) {
                wt0 = loc0 / _del[0] - jd0;
                wt0 = wt0 > 0.0 ? (wt0 < 1.0 ? wt0 : 1.0) : 0.0;
		if (wt0 < 0.5) {
		  kd0 = jd0;
		}
		else {
		  kd0 = jd0 + 1;
		}
                values[k2] = _values[kd0][kd1];
	      }
              loc0 += del0;
	    }
          }
          else {
	    for (k2 = 0; k2 < size[1]; k2++) {
	      values[k2] = _nil_val;
	    }
          }
          break;

        case 1:
          id0  = cellIndex (0, first[0]);
          if (id0 == _size[0]-1) id0 = _size[0] - 2;
	  if (indexInValidRange(0,&id0)) {
            wt0  = first[0] / _del[0] - id0;
            wt0  = wt0 > 0.0 ? (wt0 < 1.0 ? wt0 : 1.0) : 0.0;
	    if (wt0 < 0.5) {
	      kd0 = id0;
	    }
	    else {
	      kd0 = id0 + 1;
	    }
	    if (size[1] < 2) {
	      del1 = 0.0;
	    }
	    else {
              del1 = (last[1] - first[1]) / (float)(size[1] - 1);
	    }
            loc1 = first[1];
            for (k2 = 0; k2 < size[1]; k2++) {
              jd1 = cellIndex (1, loc1);
	      if (jd1 == _size[1]-1) jd1 = _size[1] - 2;
	      if (indexInValidRange(1,&jd1)) {
                wt1 = loc1 / _del[1] - jd1;
                wt1 = wt1 > 0.0 ? (wt1 < 1.0 ? wt1 : 1.0) : 0.0;
		if (wt1 < 0.5) {
		  kd1 = jd1;
		}
		else {
		  kd1 = jd1 + 1;
		}
                values[k2] = _values[kd0][kd1];
	      }
              loc1 += del1;
	    }
          }
          else {
	    for (k2 = 0; k2 < size[1]; k2++) {
	      values[k2] = _nil_val;
	    }
          }
          break;

        default:
          assert (0);
          break;
      }
      break;

    case LIN_NN:
      switch (dim) {
        case 0:
          id1  = cellIndex (1, first[1]);
          if (id1 == _size[1]-1) id1 = _size[1] - 2;
	  if (indexInValidRange(1,&id1)) {
            wt1  = first[1] / _del[1] - id1;
            wt1  = wt1 > 0.0 ? (wt1 < 1.0 ? wt1 : 1.0) : 0.0;
	    if (wt1 < 0.5) {
	      kd1 = id1;
            }
	    else {
	      kd1 = id1 + 1;
	    }
	    if (size[0] < 2) {
	      del0 = 0.0;
	    }
	    else {
              del0 = (last[0] - first[0]) / (float)(size[0] - 1);
	    }
            loc0 = first[0];
	    id0_set = 0;
            for (k2 = 0; k2 < size[0]; k2++) {
              jd0 = cellIndex (0, loc0);
	      if (jd0 == _size[0]-1) jd0 = _size[0] - 2;
	      if (indexInValidRange(0,&jd0)) {
	        if (!id0_set || jd0 != id0) {
	          id0 = jd0;
		  ill_defined = illDefined (id0, id1);
	          value0a = _values[id0  ][kd1];
	          value0b = _values[id0+1][kd1];
	          id0_set = 1;
                }
                wt0 = loc0 / _del[0] - id0;
                wt0 = wt0 > 0.0 ? (wt0 < 1.0 ? wt0 : 1.0) : 0.0;
		if (ill_defined) {
		  if (wt0 < 0.5) {
		    values[k2] = value0a;
		  }
		  else {
		    values[k2] = value0b;
		  }
		}
		else {
                  values[k2] = wt0 * value0b + ((float)1 - wt0) * value0a;
		}
	      }
              loc0 += del0;
	    }
          }
          else {
	    for (k2 = 0; k2 < size[1]; k2++) {
	      values[k2] = _nil_val;
	    }
          }
          break;

        case 1:
          id0  = cellIndex (0, first[0]);
          if (id0 == _size[0]-1) id0 = _size[0] - 2;
	  if (indexInValidRange(0,&id0)) {
            wt0  = first[0] / _del[0] - id0;
            wt0  = wt0 > 0.0 ? (wt0 < 1.0 ? wt0 : 1.0) : 0.0;
	    if (size[1] < 2) {
	      del1 = 0.0;
	    }
	    else {
              del1 = (last[1] - first[1]) / (float)(size[1] - 1);
	    }
            loc1 = first[1];
	    id1_set = 0;
            for (k2 = 0; k2 < size[1]; k2++) {
              jd1 = cellIndex (1, loc1);
	      if (jd1 == _size[1]-1) jd1 = _size[1] - 2;
	      if (indexInValidRange(1,&jd1)) {
	        if (!id1_set || jd1 != id1) {
	          id1 = jd1;
		  if (illDefined(id0,id1)) {
		    if (wt0 < 0.5) {
		      value1a = _values[id0][id1  ];
		      value1b = _values[id0][id1+1];
		    }
		    else {
		      value1a = _values[id0+1][id1  ];
		      value1b = _values[id0+1][id1+1];
		    }
		  }
		  else {
	            value1a =        wt0  * _values[id0+1][id1  ]
	               + ((float)1 - wt0) * _values[id0  ][id1  ];
	            value1b =        wt0  * _values[id0+1][id1+1]
		       + ((float)1 - wt0) * _values[id0  ][id1+1];
		  }
	          id1_set = 1;
                }
                wt1 = loc1 / _del[1] - id1;
                wt1 = wt1 > 0.0 ? (wt1 < 1.0 ? wt1 : 1.0) : 0.0;
		if (wt1 < 0.5) {
		  values[k2] = value1a;
                }
		else {
		  values[k2] = value1b;
		}
	      }
              loc1 += del1;
	    }
          }
          else {
	    for (k2 = 0; k2 < size[1]; k2++) {
	      values[k2] = _nil_val;
	    }
          }
          break;
        default:
          assert (0);
          break;
      }
      break;

    case NN_LIN:
      switch (dim) {
        case 0:
          id1  = cellIndex (1, first[1]);
          if (id1 == _size[1]-1) id1 = _size[1] - 2;
	  if (indexInValidRange(1,&id1)) {
            wt1  = first[1] / _del[1] - id1;
            wt1  = wt1 > 0.0 ? (wt1 < 1.0 ? wt1 : 1.0) : 0.0;
	    if (size[0] < 2) {
	      del0 = 0.0;
	    }
	    else {
              del0 = (last[0] - first[0]) / (float)(size[0] - 1);
	    }
            loc0 = first[0];
	    id0_set = 0;
            for (k2 = 0; k2 < size[0]; k2++) {
              jd0 = cellIndex (0, loc0);
	      if (jd0 == _size[0]-1) jd0 = _size[0] - 2;
	      if (indexInValidRange(0,&jd0)) {
	        if (!id0_set || jd0 != id0) {
	          id0 = jd0;
		  if (illDefined(id0,id1)) {
		    if (wt1 < 0.5) {
		      value0a = _values[id0  ][id1];
		      value0b = _values[id0+1][id1];
		    }
		    else {
		      value0a = _values[id0  ][id1+1];
		      value0b = _values[id0+1][id1+1];
		    }
		  }
		  else {
	            value0a =        wt1  * _values[id0  ][id1+1]
		       + ((float)1 - wt1) * _values[id0  ][id1  ];
	            value0b =        wt1  * _values[id0+1][id1+1]
		       + ((float)1 - wt1) * _values[id0+1][id1  ];
		  }
	          id0_set = 1;
                }
                wt0 = loc0 / _del[0] - id0;
                wt0 = wt0 > 0.0 ? (wt0 < 1.0 ? wt0 : 1.0) : 0.0;
		if (wt0 < 0.5) {
		  values[k2] = value0a;
		}
		else {
		  values[k2] = value0b;
		}
	      }
              loc0 += del0;
	    }
          }
          else {
	    for (k2 = 0; k2 < size[1]; k2++) {
	      values[k2] = _nil_val;
	    }
          }
          break;

        case 1:
          id0  = cellIndex (0, first[0]);
          if (id0 == _size[0]-1) id0 = _size[0] - 2;
	  if (indexInValidRange (0,&id0)) {
            wt0  = first[0] / _del[0] - id0;
            wt0  = wt0 > 0.0 ? (wt0 < 1.0 ? wt0 : 1.0) : 0.0;
	    if (wt0 < 0.5) {
	      kd0 = id0;
	    }
	    else {
	      kd0 = id0 + 1;
	    }
	    if (size[1] < 2) {
	      del1 = 0.0;
	    }
	    else {
              del1 = (last[1] - first[1]) / (float)(size[1] - 1);
	    }
            loc1 = first[1];
	    id1_set = 0;
            for (k2 = 0; k2 < size[1]; k2++) {
              jd1 = cellIndex (1, loc1);
	      if (jd1 == _size[1]-1) jd1 = _size[1] - 2;
	      if (indexInValidRange(1,&jd1)) {
	        if (!id1_set || jd1 != id1) {
	          id1 = jd1;
		  ill_defined = illDefined (id0, id1);
	          value1a = _values[kd0][id1  ];
	          value1b = _values[kd0][id1+1];
	          id1_set = 1;
                }
                wt1 = loc1 / _del[1] - jd1;
                wt1 = wt1 > 0.0 ? (wt1 < 1.0 ? wt1 : 1.0) : 0.0;
		if (ill_defined) {
		  if (wt1 < 0.5) {
		    values[k2] = value1a;
		  }
		  else {
		    values[k2] = value1b;
		  }
		}
		else {
                  values[k2] = wt1 * value1b + ((float)1 - wt1) * value1a;
		}
	      }
              loc1 += del1;
	    }
          }
          else {
	    for (k2 = 0; k2 < size[1]; k2++) {
	      values[k2] = _nil_val;
	    }
          }
          break;
        default:
          assert (0);
          break;
      }
      break;

    default:
      assert (0);
      break;
  }
}

void Interpolator2D::getVal (void *retval, const float *loc)
{
  assert (_block_owned || _block_unowned);

  int k2, id[2], loc_same[2], id_same[2], ill_defined, id0, id1;
  float value[2], wt[2], result;

  for (k2 = 0; k2 < 2; k2++) {

    if (_loc_set[k2]) {
// the location has previously been stored
      if (loc[k2] == _prev_loc[k2]) {
// the location is identical
        loc_same[k2] = 1;
      }
      else {
// the location is different, reset weight & remember new location
	loc_same [k2] = 0;
	_id_set  [k2] = 0;
	_wt_set  [k2] = 0;
	_prev_loc[k2] = loc[k2];
      }
    }
    else {
// the location has not been previously set, reset weight & remember
//   location
      loc_same [k2] = 0;
      _id_set  [k2] = 0;
      _wt_set  [k2] = 0;
      _prev_loc[k2] = loc[k2];
      _loc_set [k2] = 1;
    }

    if (_id_set[k2]) {
// the index has previously been stored
      if (loc_same[k2]) {
// the location is identical, no need to recalculate index
	id [k2] = _prev_id [k2];
      }
      else {
// the location has changed, calculate index
        id[k2] = cellIndex (k2, loc[k2]);
        if (id[k2] == _size[k2]-1) id[k2] = _size[k2] - 2;
        if (id[k2] == _prev_id[k2]) {
// the index is identical
	  id_same[k2] = 1;
        }
	else {
// the index has changed, remember it
	  id_same  [k2] = 0;
	  _prev_id [k2] = id [k2];
	}
      }
    }
    else {
// the index has not been previously been stored, calculate it
      id[k2] = cellIndex (k2, loc[k2]);
      if (id[k2] == _size[k2]-1) id[k2] = _size[k2] - 2;
      id_same  [k2] = 0;
      _prev_id [k2] = id [k2];
      _id_set[k2] = 1;
    }
  }

// if 0th id has changed clear the 0th values set flag
  if (!id_same[0]) _0_values_set = 0;
// if 1th id has changed clear the 1st values set flag
  if (!id_same[1]) _1_values_set = 0;

  if ((id[0] < 0 || id[0] > _size[0]-1) ||
      (id[1] < 0 || id[1] > _size[1]-1)   ) {
// location is beyond data
    result = _nil_val;
  }
  else {
// find the weights
    for (k2 = 0; k2 < 2; k2++) {
      if (!_wt_set[k2]) {
// this weight has changed or was not calculated last time
        float twt = loc[k2] / _del[k2] - id[k2];
	wt[k2] = twt > 0.0 ? (twt < 1.0 ? twt : 1.0) : 0.0;
	_prev_wt[k2] = wt[k2];
	_wt_set[k2] = 1;
      }
      else {
	wt[k2] = _prev_wt[k2];
      }
    }

    if (id_same[0] && id_same[1]) {
      ill_defined = _prev_ill_defined;
    }
    else {
            ill_defined = illDefined (id[0], id[1]);
      _prev_ill_defined = ill_defined;
    }

    if (_stype == NN || ill_defined) { // X & Y-NN
// nearest neighbor interpolation
      if (wt[0] < 0.5) {
	id0 = id[0];
      }
      else {
	id0 = id[0] + 1;
      }

      if (wt[1] < 0.5) {
	id1 = id[1];
      }
      else {
	id1 = id[1] + 1;
      }
      result = _values[id0][id1];
    }

    else if (_stype == LIN) { // X & Y-LIN
// linearly interpolate two values in the 0th dimension
      for (k2 = 0; k2 < 2; k2++) {
        if (_0_values_set) {
	  value[k2] = _prev_0_values[k2];
        }
        else {
	  value[k2] =     wt[0]  * _values[id[0]+1][id[1]+k2]
	    + ((float)1 - wt[0]) * _values[id[0]  ][id[1]+k2];
	  _prev_0_values[k2] = value[k2];
        }
      }
      _0_values_set = 1;
// use the two 0th values to interpolate in the 1st dimension
      result = wt[1] * value[1] + ((float)1 - wt[1]) * value[0];
    }
    else if (_stype == LIN_NN) { // X-LIN, Y-NN
// linearly interpolate two values in the 0th dimension
      for (k2 = 0; k2 < 2; k2++) {
        if (_0_values_set) {
	  value[k2] = _prev_0_values[k2];
        }
        else {
	  value[k2] =     wt[0]  * _values[id[0]+1][id[1]+k2]
	    + ((float)1 - wt[0]) * _values[id[0]  ][id[1]+k2];
	  _prev_0_values[k2] = value[k2];
        }
      }
      _0_values_set = 1;
// find the nearest 0th value using the weight in the 1st dimension
      id1 = (int)(wt[1] + 0.5);
      result = value[id1];
    }
    else if (_stype == NN_LIN) { // X-NN, Y-LIN
// linearly interpolate two values in the 1st dimension
      for (k2 = 0; k2 < 2; k2++) {
        if (_1_values_set) {
	  value[k2] = _prev_1_values[k2];
        }
        else {
	  value[k2] =     wt[1]  * _values[id[0]+k2][id[1]+1]
	    + ((float)1 - wt[1]) * _values[id[0]+k2][id[1]  ];
	  _prev_1_values[k2] = value[k2];
        }
      }
      _1_values_set = 1;
// find the nearest 1st value using the weight in the 0th dimension
      id0 = (int)(wt[0] + 0.5);
      result = value[id0];
    }
    else {
      assert (0); // invalid sampling type
    }
  }
  _conversion_function (_conversion_obj, (void *)retval, result);
}

int Interpolator2D::cellIndex (int dim, float coord) const
{
  return cellIndex (_first[dim], _del[dim], coord);
}

int Interpolator2D::cellIndex (float first, float del, float coord) const
{
  return (int)((coord - first) / del);
}

void Interpolator2D::assignValues ()
{
  if (_values) free (_values);
// declare a array of pointers along the X-direction
  assert ((_values = (float **)malloc(sizeof(float*)*_size[0])) != 0);
  int k2;
  for (k2 = 0; k2 < _size[0]; k2++) {
// define the pointers along the X-direction
    _values[k2] = &_block[k2*_size[1]];
  }
}

int Interpolator2D::illDefined (int id0, int id1) const
{
  int retval;

  if (id0 < 0 || id0+1 > _size[0]-1 ||
      id1 < 0 || id1+1 > _size[1]-1   ) {
    retval = 1;
  }
  else if (_values[id0  ][id1  ] == _nil_val ||
      _values[id0  ][id1+1] == _nil_val ||
      _values[id0+1][id1  ] == _nil_val ||
      _values[id0+1][id1+1] == _nil_val   ) {
    retval = 1;
  }
  else {
    retval = 0;
  }
  return retval;
}

void Interpolator2D::defaultConversionFunction (void * /* obj */,
  void *result, float value)
{
  memcpy (result, &value, sizeof(float));
}

void Interpolator2D::setConversionFunction (InterpConversionFunction func,
  void *obj, int num_bytes)
{
  _conversion_function   = func;
  _conversion_obj        = obj;
  _conversion_value_size = num_bytes;
}

int Interpolator2D::indexInValidRange (int dim, int *id) const
{
  int retval = *id > -1 && *id < _size[dim];
  if (!retval && _allow_lt_2D && _size[dim] < 2) {
    *id = 0;
    retval = 1;
  }
  return retval;
}
