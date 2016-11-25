#include "curves/ls_power_fitter.hh"
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

LSPowerFitter::LSPowerFitter () :
  LSGainFitter (CV::POWER)
{
}

LSPowerFitter::~LSPowerFitter ()
{
}

int LSPowerFitter::independentVariableModifier (float x,
  double *modified_x)
{
  double x_translated = (double)x - _x0;
  if (x_translated <= 0) return _status = CV::BAD_DATA_FOUND;

  *modified_x = log (x_translated);
  return _status = CV::NORMAL;
}

int LSPowerFitter::independentVariableUnmodifier (float x,
  double *unmodified_x)
{
  *unmodified_x = exp ((double)x) + _x0;
  return _status = CV::NORMAL;
}

int LSPowerFitter::dependentVariableModifier (float y,
  double *modified_y)
{
  double y_translated = (double)y - _y0;
  if (y_translated <= 0) return _status = CV::BAD_DATA_FOUND;

  *modified_y = log (y_translated);
  return _status = CV::NORMAL;
}

int LSPowerFitter::dependentVariableUnmodifier (float y,
  double *unmodified_y)
{
  *unmodified_y = exp ((double)y) + _y0;
  return _status = CV::NORMAL;
}
