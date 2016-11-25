#include "curves/weighted_curve_fitter.hh"
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
#include <stdlib.h>
#include <assert.h>

WeightedCurveFitter::WeightedCurveFitter (int type, int logarithmic) :
  CurveFitter (type),
  _logarithmic            (logarithmic),
  _max_run                (1),
  _x_run                  (0),
  _y_run                  (0),
  _runs_grtr_thn_1_exist  (0),
  _zero_runs_exist        (0)
{
}

WeightedCurveFitter::~WeightedCurveFitter ()
{
  if (_x_run) free (_x_run), _x_run = 0;
  if (_y_run) free (_y_run), _y_run = 0;
}

// the counts array is a poor man's way of implementing weigthed curve fitting
//   by allowing *counts = 0, simple curve fitting is done
int WeightedCurveFitter::doit (float *xs, float *ys, int count, int *counts)
{
  int err;
  int doit_err   = CV::NORMAL;
  int insert_err = CV::NORMAL;
  

  if (!counts) {
// simplest case
    doit_err = CurveFitter::doit (xs, ys, count);
  }

  else {
    adjustRunArrays (count, counts);

    int k2;
    if (_runs_grtr_thn_1_exist || _zero_runs_exist) {
// some zero runs or runs greater than one exist so check all the counts
      int num_of_1_runs = 0, doit_was_run = 0, k3 = 0;
      for (k2 = 0; k2 < count; k2 ++) {
        if (counts[k2] == 1) {
// a 1 run was found, keep track of how many of these are consecutive
          k3 = k2;
          num_of_1_runs++;
        }

        else {
// a break in 1 runs was found
          if (num_of_1_runs > 0) {
// if a stack of 1 runs exist then output them
            if (doit_was_run) {
// doit has already been run
	      err = CurveFitter::insert (&xs[k3], &ys[k3], num_of_1_runs);
              if (err != CV::NORMAL) {
                if (insert_err == CV::NORMAL) {
                  insert_err = err;
                }
              }
            }
            else {
// doit has not been run yet
	      err = CurveFitter::doit (&xs[k3], &ys[k3], num_of_1_runs);
              if (err != CV::NORMAL) {
                if (doit_err == CV::NORMAL) {
                  doit_err = err;
                }
              }
              doit_was_run = 1;
            }
            num_of_1_runs = 0;
          }

          if (counts[k2] > 1) {
// a run was encountered
            for (k3 = 0; k3 < counts[k2]; k3++) {
// repeat the same point as necessary
              _x_run[k3] = xs[k2];
              _y_run[k3] = ys[k2];
            }
            if (doit_was_run) {
// doit has already been run
	      err = CurveFitter::insert (_x_run, _y_run, counts[k2]);
              if (err != CV::NORMAL) {
                if (insert_err == CV::NORMAL) {
                  insert_err = err;
                }
              }
            }
            else {
// doit has not been run yet
	      err = CurveFitter::doit (_x_run, _y_run, counts[k2]);
              if (err != CV::NORMAL) {
                if (doit_err == CV::NORMAL) {
                  doit_err = err;
                }
              }
              doit_was_run = 1;
            }
          }
        }
      }
    }
    else {
// no zero runs or runs greater than one exist:  simple case
      doit_err = CurveFitter::doit (xs, ys, count);
    }
  }
  if (doit_err        != CV::NORMAL) return _status = doit_err;
  else if (insert_err != CV::NORMAL) return _status = insert_err;
  else                               return _status = CV::NORMAL;
}

int WeightedCurveFitter::remove (float *xs, float *ys, int count,
  int *counts)
{
  int err;
  int remove_err = CV::NORMAL;

  if (!counts) {
// simplest case
    remove_err = CurveFitter::remove (xs, ys, count);
  }

  else {
    adjustRunArrays (count, counts);

    int k2;
    if (_runs_grtr_thn_1_exist || _zero_runs_exist) {
// some zero runs or runs greater than one exist so check all the counts
      int num_of_1_runs = 0, k3 = 0;
      for (k2 = 0; k2 < count; k2 ++) {
        if (counts[k2] == 1) {
// a 1 run was found, keep track of how many of these are consecutive
          k3 = k2;
          num_of_1_runs++;
        }

        else {
// a break in 1 runs was found
          if (num_of_1_runs > 0) {
// if a stack of 1 runs exist then output them
            err = CurveFitter::remove (&xs[k3], &ys[k3], num_of_1_runs);
            if (err != CV::NORMAL) {
              if (remove_err == CV::NORMAL) {
                remove_err = err;
              }
            }
            num_of_1_runs = 0;
          }

          if (counts[k2] > 1) {
// a run was encountered
            for (k3 = 0; k3 < counts[k2]; k3++) {
// repeat the same point as necessary
              _x_run[k3] = xs[k2];
              _y_run[k3] = ys[k2];
            }
            err = CurveFitter::remove (_x_run, _y_run, counts[k2]);
            if (err != CV::NORMAL) {
              if (remove_err == CV::NORMAL) {
                remove_err = err;
              }
            }
          }
        }
      }
    }
    else {
// no zero runs or runs greater than one exist:  simple case
      remove_err = CurveFitter::remove (xs, ys, count);
    }
  }
  return _status = remove_err;
}

int WeightedCurveFitter::insert (float *xs, float *ys, int count,
  int *counts)
{
  int err;
  int insert_err = CV::NORMAL;

  if (!counts) {
// simplest case
    insert_err = CurveFitter::insert (xs, ys, count);
  }

  else {
    adjustRunArrays (count, counts);

    int k2;
    if (_runs_grtr_thn_1_exist || _zero_runs_exist) {
// some zero runs or runs greater than one exist so check all the counts
      int num_of_1_runs = 0, k3 = 0;
      for (k2 = 0; k2 < count; k2 ++) {
        if (counts[k2] == 1) {
// a 1 run was found, keep track of how many of these are consecutive
          k3 = k2;
          num_of_1_runs++;
        }

        else {
// a break in 1 runs was found
          if (num_of_1_runs > 0) {
// if a stack of 1 runs exist then output them
            err = CurveFitter::insert (&xs[k3], &ys[k3], num_of_1_runs);
            if (err != CV::NORMAL) {
              if (insert_err == CV::NORMAL) {
                insert_err = err;
              }
            }
            num_of_1_runs = 0;
          }

          if (counts[k2] > 1) {
// a run was encountered
            for (k3 = 0; k3 < counts[k2]; k3++) {
// repeat the same point as necessary
              _x_run[k3] = xs[k2];
              _y_run[k3] = ys[k2];
            }
            err = CurveFitter::insert (_x_run, _y_run, counts[k2]);
            if (err != CV::NORMAL) {
              if (insert_err == CV::NORMAL) {
                insert_err = err;
              }
            }
          }
        }
      }
    }
    else {
// no zero runs or runs greater than one exist:  simple case
      insert_err = CurveFitter::insert (xs, ys, count);
    }
  }
  return _status = insert_err;
}

void WeightedCurveFitter::adjustRunArrays (int count, int *counts)
{
  if (count < 1) return;
  assert (counts);

  int k2;
  _runs_grtr_thn_1_exist = 0;
  _zero_runs_exist = 0;

// find the maximum x,y pair run
  for (k2 = 0; k2 < count; k2++) {
    if (counts[k2] > _max_run) {
      if (_x_run) free (_x_run), _x_run = 0;
      if (_y_run) free (_y_run), _y_run = 0;
      _max_run = counts[k2];
      _runs_grtr_thn_1_exist = 1;
    }
    else if (counts[k2] > 1) {
      _runs_grtr_thn_1_exist = 1;
    }
    else if (counts[k2] < 1) {
      _zero_runs_exist = 1;
    }
  }

// allocate for the maximum x,y pair run
  if (_max_run > 1 && !_x_run) {
// a new maximum run must have exceeded previous maximum run
    _x_run = (float *)malloc (sizeof(float)*(size_t)_max_run);
    _y_run = (float *)malloc (sizeof(float)*(size_t)_max_run);
    assert (_x_run && _y_run);
  }
}
