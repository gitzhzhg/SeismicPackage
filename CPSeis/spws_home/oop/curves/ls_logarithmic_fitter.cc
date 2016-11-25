#include "curves/ls_logarithmic_fitter.hh"
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
#include <math.h>
#include <float.h>
#include <assert.h>

LSLogarithmicFitter::LSLogarithmicFitter () :
  LSLinearFitter (CV::LOGARITHMIC)
{
}

LSLogarithmicFitter::~LSLogarithmicFitter ()
{
}

// given an unscaled coefficient in the appropriate curve's system, do 
//   conversion necessary to take it to the internal (linear) system
int LSLogarithmicFitter::convertCoefficientToInternal (int index,
  double external_coefficient, double *internal_coefficient)
{
  if (index == CV::A0) {
    if (external_coefficient <= 0) return _status = CV::BAD_COEFFICIENT;
    *internal_coefficient = log (external_coefficient);
  }
  else {
    *internal_coefficient = external_coefficient;
  }
  return _status = CV::NORMAL;
}
// given an unscaled internal (linear) coefficient, do conversion necessary
//   to take it from the internal system to the appropriate curve's system
int LSLogarithmicFitter::convertCoefficientFromInternal (int index,
  double internal_coefficient, double *external_coefficient)
{
  if (index == CV::A0) {
    *external_coefficient = exp (internal_coefficient);
  }
  else {
    *external_coefficient = internal_coefficient;
  }
  return _status = CV::NORMAL;
}

int LSLogarithmicFitter::independentVariableModifier (float x,
  double *modified_x)
{
  if (x <= 0) return _status = CV::BAD_DATA_FOUND;

  *modified_x = log ((double)x);
  return _status = CV::NORMAL;
}

int LSLogarithmicFitter::independentVariableUnmodifier (float x,
  double *unmodified_x)
{
  *unmodified_x = exp ((double)x);
  return _status = CV::NORMAL;
}

void LSLogarithmicFitter::limitCoefficientRange (int index, double *min,
  double *max)
{
  if (index == CV::A0) {
    double tiny = FLT_EPSILON * FLT_EPSILON;
    if (*min < tiny) *min = 1.5 * tiny;
    if (*max < tiny) *max = 1.5 * tiny;
  }
}
