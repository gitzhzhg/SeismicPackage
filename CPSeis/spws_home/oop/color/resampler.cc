// class that creates a resampler as follows
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

// produce a table f(k); k = 0,...,n-1; min <= f(k) <= max

//   it is assumed that incr = (f(n-1)-f(0))/(n-1)
//     let f(n-1) = incr * (n-1) + f(0)
//     if (f(n-1) < min || f(n-1) > max) f1 = min,  fm = max ! wrap around
//     else                              f1 = f(0), fm = f(n-1) ! interpolat

//   nm1 = (n - 1) / cycles
//   del = (fm - f1) / nm1

//   if (type == CONSTANT || incr == 0)
//     f(k) = f(0)

//   if (type == LINEAR)
//     f(k) = A*k + B; where
//     A = del
//     B = f1

//   if (type == EXPONENTIAL)
//     f(k) = A*k*k + B*k + C; where
//     A = del / [nm1-2D]
//     B = del * 2D / [2D-nm1]
//     C = f1
//     D < 0  If D == 0, then the equation has no linear term.

//   if (type == LOGARITHMIC)
//     f(k) =  A*k*k + B*k + C; where
//     A = -del / [nm1+2D]
//     B = del * 2 * (D+nm1) / [nm1+2D]
//     C = f1
//     D > 0

//   kmin = [-B +/- sqrt (B*B-4*A*(C-min))] / 2*A, where
//     0 < kmin < n-1 and real

//   kmax = [-B +/- sqrt (B*B-4*A*(C-max))] / 2*A
//     0 < kmax < n-1 and real

//   for f(k-k0)

//   k0 = [B +/- sqrt (B*B-4*A*(C-f(0)))] / 2*A 
//     0 < k0 < n-1 and real

// a simple table is created

#include "color/resampler.hh"
#include <stdlib.h>
#include <math.h>
#include <float.h>

Resampler::Resampler (float value0, float incr, int count, Type type,
  float cycles, float maximum, float minimum) :
  _incr    (incr),
  _count   (0),
  _index   (0),
  _table   (0),
  _status  (1)
{
// insure that count is at least two
  if (count < 1) _count = 1;
  else _count = count;

// insure that cycles is positive
   if (cycles < 0) cycles *= -1;

// set up to take care of the situation when the increment is greater
//   than the difference in the extrema
  int tries, havent_made_it, k3;
  float current_extreme, extrema[2];
  extrema[0] = minimum;
  extrema[1] = maximum;

// allocate the table
  _table = (float *)malloc ((size_t)(count*sizeof(float)));
  if (!_table) {
    _status = 0;
    return;
  }

  float value;
  int k2;

// insure that maximum is greater than minimum
  if (minimum > maximum) {
    value = minimum;
    minimum = maximum;
    maximum = value;
  }
  float EPSILON = 0.00001;
  float mini = minimum - EPSILON;
  float maxi = maximum + EPSILON;

// insure that the initial value is within tolerance
  if (value0 > maximum) value = maximum;
  else if (value0 < minimum) value = minimum;
  else value = value0;

// define f1 and fm
  float f1, fm;
  fm = _incr * (_count - 1) + value;
  if (cycles <= 1 && fm >= mini && fm <= maxi) {
    f1 = value;
  }
  else {
    if (_incr < 0) {
      f1 = maximum;
      fm = minimum;
    }
    else {
      f1 = minimum;
      fm = maximum;
    }
  }

  float A, B, C, D, kmin, kmax, kmn, kmx, k, k0, sqr, try1, try2, nm1, del;

// define the increment and n-1
  if (cycles == 0) {
    del = 0;
  }
  else {
    nm1 = (_count - 1) / cycles;
    if (nm1 <= 0) {
      del = 0;
    }
    else {
      del = (fm - f1) / nm1;
    }
  }

  double arg;
  int try1_ok, try2_ok;

  if (type == EXPONENTIAL && del != 0) {
    D = -0.1;
    A = del / (nm1 - 2 * D);
    B = del * 2 * D / (2 * D - nm1);
    C = f1;

    if (del < 0) {
      arg = (double)(B * B - 4 * A * (C - minimum));
      if (arg > 0) {
        sqr = (float)sqrt (arg);
        try1 = (-B - sqr) / 2 / A;
        try2 = (-B + sqr) / 2 / A;
        if ((try1 <= nm1) && (try1 >= 0)) try1_ok = 1;
        else                              try1_ok = 0;
        if ((try2 <= nm1) && (try2 >= 0)) try2_ok = 1;
        else                              try2_ok = 0;
        if (try1_ok && try2_ok) {
          if (try1 < try2) kmin = try1;
          else             kmin = try2;
        }
        else if (try1_ok)  kmin = try1;
        else if (try2_ok)  kmin = try2;
        else               kmin = nm1;
      }
      else {
        kmin = nm1;
      }
      k3 = 1;
// if the kmin is within 0.01% of the nm1, then set kmin = nm1
      if (fabs((double)((kmin - nm1) / nm1)) < (double).0001) {
        kmn = nm1 + EPSILON;
      }
      else {
        kmn = kmin + EPSILON;
      }
    }

    else {
      arg = (double)(B * B - 4 * A * (C - maximum));
      if (arg > 0) {
        sqr = (float)sqrt (arg);
        try1 = (-B - sqr) / 2 / A;
        try2 = (-B + sqr) / 2 / A;
        if ((try1 <= nm1) && (try1 >= 0)) try1_ok = 1;
        else                              try1_ok = 0;
        if ((try2 <= nm1) && (try2 >= 0)) try2_ok = 1;
        else                              try2_ok = 0;
        if (try1_ok && try2_ok) {
          if (try1 < try2) kmax = try1;
          else             kmax = try2;
        }
        else if (try1_ok)  kmax = try1;
        else if (try2_ok)  kmax = try2;
        else               kmax = nm1;
      }
      else {
        kmax = nm1;
      }
      k3 = 0;
// if the kmax is within 0.01% of the nm1, then set kmax = nm1
      if (fabs((double)((kmax - nm1) / nm1)) < (double).0001) {
        kmx = nm1 + EPSILON;
      }
      else {
        kmx = kmax + EPSILON;
      }
      kmx = kmax + EPSILON;
    }

// determine the initial condition
    if (value != f1) {
      arg = (double)(B * B - 4 * A * (C - value));
      if (arg > 0) {
        sqr = (float)sqrt (arg);
        try1 = (B - sqr) / 2 / A;
        try2 = (B + sqr) / 2 / A;
        if ((-try1 <= nm1) && (-try1 >= 0)) try1_ok = 1;
        else                                try1_ok = 0;
        if ((-try2 <= nm1) && (-try2 >= 0)) try2_ok = 1;
        else                                try2_ok = 0;
        if (try1_ok && try2_ok) {
          if (-try1 < -try2) k0 = try1;
          else               k0 = try2;
        }
        else if (try1_ok)    k0 = try1;
        else if (try2_ok)    k0 = try2;
        else                 k0 = 0;
      }
      else {
        k0 = 0;
      }
    }
    else {
      k0 = 0;
    }

    k = -k0;
    for (k2 = 0; k2 < _count; k2++) {
      if (k2 > 0) {
        if (del > 0 && k > kmx) {
          k -= kmax;
        }
        else if (del < 0 && k > kmn) {
          k -= kmin;
        }
        value = A * k * k + B * k + C;
        if (value < mini || value > maxi) {
          k3 = 1 - k3;
          value = extrema[k3];
        }
      }
      _table[k2] = value;
      k += (float)1;
    }
  }

  else if (type == LOGARITHMIC && del != 0) {
    D = 0.1;
    A = -del / (nm1 + 2 * D);
    B = del * 2 * (D + nm1) / (nm1 + 2 * D);
    C = f1;

    if (del < 0) {
      arg = (double)(B * B - 4 * A * (C - minimum));
      if (arg > 0) {
        sqr = (float)sqrt (arg);
        try1 = (-B - sqr) / 2 / A;
        try2 = (-B + sqr) / 2 / A;
        if ((try1 <= nm1) && (try1 >= 0)) try1_ok = 1;
        else                              try1_ok = 0;
        if ((try2 <= nm1) && (try2 >= 0)) try2_ok = 1;
        else                              try2_ok = 0;
        if (try1_ok && try2_ok) {
          if (try1 < try2) kmin = try1;
          else             kmin = try2;
        }
        else if (try1_ok)  kmin = try1;
        else if (try2_ok)  kmin = try2;
        else               kmin = nm1;
      }
      else {
        kmin = nm1;
      }
      k3 = 1;
// if the kmin is within 0.01% of the nm1, then set kmin = nm1
      if (fabs((double)((kmin - nm1) / nm1)) < (double).0001) {
        kmn = nm1 + EPSILON;
      }
      else {
        kmn = kmin + EPSILON;
      }
    }
    else {
      arg = (double)(B * B - 4 * A * (C - maximum));
      if (arg > 0) {
        sqr = (float)sqrt (arg);
        try1 = (-B - sqr) / 2 / A;
        try2 = (-B + sqr) / 2 / A;
        if ((try1 <= nm1) && (try1 >= 0)) try1_ok = 1;
        else                              try1_ok = 0;
        if ((try2 <= nm1) && (try2 >= 0)) try2_ok = 1;
        else                              try2_ok = 0;
        if (try1_ok && try2_ok) {
          if (try1 < try2) kmax = try1;
          else             kmax = try2;
        }
        else if (try1_ok)  kmax = try1;
        else if (try2_ok)  kmax = try2;
        else               kmax = nm1;
      }
      else {
        kmax = nm1;
      }
      k3 = 0;
// if the kmax is within 0.01% of the nm1, then set kmax = nm1
      if (fabs((double)((kmax - nm1) / nm1)) < (double).0001) {
        kmx = nm1 + EPSILON;
      }
      else {
        kmx = kmax + EPSILON;
      }
    }

// determine the initial condition
    if (value != f1) {
      arg = (double)(B * B - 4 * A * (C - value));
      if (arg > 0) {
        sqr = (float)sqrt (arg);
        try1 = (B - sqr) / 2 / A;
        try2 = (B + sqr) / 2 / A;
        if ((-try1 <= nm1) && (-try1 >= 0)) try1_ok = 1;
        else                                try1_ok = 0;
        if ((-try2 <= nm1) && (-try2 >= 0)) try2_ok = 1;
        else                                try2_ok = 0;
        if (try1_ok && try2_ok) {
          if (-try1 < -try2) k0 = try1;
          else               k0 = try2;
        }
        else if (try1_ok)    k0 = try1;
        else if (try2_ok)    k0 = try2;
        else                 k0 = 0;
      }
      else {
        k0 = 0;
      }
    }
    else {
      k0 = 0;
    }

    k = -k0;
    for (k2 = 0; k2 < _count; k2++) {
      if (k2 > 0) {
        if (del > 0 && k > kmx) {
          k -= kmax;
        }
        else if (del < 0 && k > kmn){
          k -= kmin;
        }
        value = A * k * k + B * k + C;
        if (value < mini || value > maxi) {
          k3 = 1 - k3;
          value = extrema[k3];
        }
      }
      _table[k2] = value;
      k += (float)1;
    }
  }

  else if (type == LINEAR && del != 0) {
    A = del;
    B = f1;

    if (del < 0) {
      kmin = (minimum - B) / A;
      k3 = 1;
// if the kmin is within 0.01% of the nm1, then set kmin = nm1
      if (fabs((double)((kmin - nm1) / nm1)) < (double).0001) {
        kmn = nm1 + EPSILON;
      }
      else {
        kmn = kmin + EPSILON;
      }
    }
    else {
      kmax = (maximum - B) / A;
      k3 = 0;
// if the kmax is within 0.01% of the nm1, then set kmax = nm1
      if (fabs((double)((kmax - nm1) / nm1)) < (double).0001) {
        kmx = nm1 + EPSILON;
      }
      else {
        kmx = kmax + EPSILON;
      }
    }

// determine the initial condition
    if (value != f1) {
      k0 = (B - value) / A;
    }
    else {
      k0 = 0;
    }

    k = -k0;
    for (k2 = 0; k2 < _count; k2++) {
      if (k2 > 0) {
        if (del > 0 && k > kmx) {
          k -= kmax;
        }
        else if (del < 0 && k > kmn) {
          k -= kmin;
        }
        value = A * k + B;
        if (value < mini || value > maxi) {
          k3 = 1 - k3;
          value = extrema[k3];
        }
      }
      _table[k2] = value;
      k += (float)1;
    }
  }

  else /* if (type == CONSTANT || del == 0) */ {
    for (k2 = 0; k2 < _count; k2++) _table[k2] = value;
  }
}

Resampler::~Resampler ()
{
  if (_table) free (_table), _table = 0;
}

float Resampler::value (int index)
{
  if (index < 0) _index = 0;
  else if (index >= _count) _index = _count - 1;
  else _index = index;
  return _table[_index];
}

float Resampler::nextValue ()
{
  _index++;
  if (_index >= _count) _index--;
  return _table[_index];
}

float Resampler::previousValue ()
{
  _index--;
  if (_index < 0) _index = 0;
  return _table[_index];
}

int Resampler::index (float value)
{
// always return a valid value
  int k2;

// look for exact matches
  for (k2 = 0; k2 < _count; k2++) {
    if (value == _table[k2]) {
      return k2;
    }
  }

  if (_incr < 0) {
// values are decreasing
    if      (value >  _table[0]) return 0;
    else if (value < _table[_count-1]) return _count - 1;
    else /* (value < _table[0] && value > _table[count-1]) */ {
      for (k2 = 1; k2 < _count; k2++) {
        if (value < _table[k2-1] && value >= _table[k2]) {
          return k2;
        }
      }
    }
  }
  else if (_incr > 0) {
// values are increasing
    if      (value <  _table[0]) return 0;
    else if (value > _table[_count-1]) return _count - 1;
    else /* (value > _table[0] && value < _table[_count-1]) */ {
      for (k2 = 1; k2 < _count; k2++) {
        if (value > _table[k2-1] && value < _table[k2]) {
          return k2;
        }
      }
    }
  }
  else /* if (_incr == 0) */ {
// values are constant
    return 0;
  }
  return 0;
}
