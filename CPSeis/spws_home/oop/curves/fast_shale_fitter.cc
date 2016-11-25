#include "curves/fast_shale_fitter.hh"
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
#include "curves/fixed_curve_constants.hh"
#include <math.h>
#include <float.h>
#include <assert.h>

FastShaleFitter::FastShaleFitter () :
  LSPowerLawFitter (CV::FAST_SHALE)
{
  _y0 = 1;
}

FastShaleFitter::~FastShaleFitter ()
{
}


int FastShaleFitter::coefficientRangeDefault (int index, double *min,
  double *max)
{
  if (index == CV::X0 || (index >= CV::A0 && index <= CV::A1)) {
    LSPowerLawFitter::coefficientRangeDefault (index, min, max);
  }
  else if (index == CV::Y0) {
// _y0 (p0) makes most since as 1 (for fractions) or 100 (for percents)
      *max = 100;
      double tinyp = 1.5 * FLT_EPSILON * FLT_EPSILON;
      *min = tinyp; // make it close to zero for flexibility
  }
  return _status;
}

// y is porosity
int FastShaleFitter::dependentVariableModifier (float y,
  double *modified_y)
{
  double y_translated = _y0 - (double)y;
  if (y_translated <= 0) return _status = CV::BAD_DATA_FOUND;

  *modified_y = log (y_translated);
  return _status;
}

// y is porosity
int FastShaleFitter::dependentVariableUnmodifier (float y,
  double *unmodified_y)
{
  *unmodified_y = _y0 - exp ((double)y);
  return _status = CV::NORMAL;
}

int FastShaleFitter::convertOffsetToInternal (int index,
  double external_offset, double *internal_offset)
{
  if (index == CV::X0) {
    LSPowerLawFitter::convertOffsetToInternal (index, external_offset,
      internal_offset);
  }
  else {
    if (external_offset <= 0 || external_offset > 100) {
      _status = CV::BAD_OFFSET;
    }
    else {
      *internal_offset = external_offset;
      _status = CV::NORMAL;
    }
  }
  return _status;
}
