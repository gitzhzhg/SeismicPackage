#include "curves/curve_parameters.hh"
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
#include "curves/curve_fitter.hh"
#include <stdlib.h>
#include <string.h>

CurveParameters::CurveParameters () :
  _fitter  (0),
  _steps   (DEF_STEPS)
{
  initMinsAndMaxs ();
}

CurveParameters::~CurveParameters ()
{
  if (_mins) free (_mins), _mins = 0;
  if (_maxs) free (_maxs), _maxs = 0;
  if (_user) free (_user), _user = 0;
}

int CurveParameters::setFitter (CurveFitter *fitter)
{
  assert (fitter);
  _fitter = fitter;
  return getCoefficientRanges ();
}

int CurveParameters::getCoefficientRanges ()
{
  assert (_fitter);
  int err = CV::NORMAL;
  int k2, num_coefs = CV::coefficientCount (_fitter->type());
  double coef = 0;
  for (k2 = 0; err == CV::NORMAL && k2 < num_coefs; k2++) {
    if (!_user[k2]) {
      err = _fitter->coefficientRangeDefault (k2, &_mins[k2], &_maxs[k2]);
    }
    else {
// ensure that the ranges can accomodate the previously set coefficients
      err = _fitter->coefficient (k2, &coef);
      if (err == CV::NORMAL) {
        if (coef < _mins[k2]) _mins[k2] = coef;
        if (coef > _maxs[k2]) _maxs[k2] = coef;
      }
    }
  }
  return err;
}

void CurveParameters::setCoefficientRange (int index, double min,
  double max)
{
  assert (_fitter);
  assert (index > -1 &&  index < CV::coefficientCount(_fitter->type()));
  _mins[index] = min;
  _maxs[index] = max;
  _user[index] = 1;
}

double CurveParameters::coefficientMinimum (int index)
{
  assert (_fitter);
  assert (index > -1 &&  index < CV::coefficientCount(_fitter->type()));
  return _mins[index];
}

double CurveParameters::coefficientMaximum (int index)
{
  assert (_fitter);
  assert (index > -1 &&  index < CV::coefficientCount(_fitter->type()));
  return _maxs[index];
}

double CurveParameters::coefficientRangeIncrement (int index)
{
  assert (_fitter);
  assert (index > -1 &&  index < CV::coefficientCount(_fitter->type()));

  double inc = (_maxs[index] - _mins[index]) / _steps;
  return inc;
}

void CurveParameters::initMinsAndMaxs ()
{
  int    num_coefs = CV::MAX_COEFS;
  size_t count     = sizeof(double) * num_coefs;

  _mins = 0;
  _mins = (double *)malloc (count);
  assert (_mins);
  memset (_mins, (int)0, count);

  _maxs = 0;
  _maxs = (double *)malloc (count);
  assert (_mins);
  memset (_maxs, (int)0, count);

  _user = 0;
  _user = (int *)malloc (count);
  assert (_user);
  memset (_user, (int)0, count);
}
