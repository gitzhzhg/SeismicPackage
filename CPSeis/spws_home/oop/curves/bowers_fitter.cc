#include "curves/bowers_fitter.hh"
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

BowersFitter::BowersFitter () :
  LSPowerLawFitter (CV::BOWERS),
  _u_set  (0)
{
}

BowersFitter::~BowersFitter ()
{
}

int BowersFitter::setCoefficient (int index, double value)
{
  int err = LSPowerLawFitter::setCoefficient (index, value);

  if (index == CV::A2 && err == CV::NORMAL) _u_set = 1;

  return err;
}

int BowersFitter::coefficient (int index, double *coef)
{
  if (index == CV::A2) {
    if (!_u_set) setCoefficient (index, 1);
  }
  int err = LSPowerLawFitter::coefficient (index, coef);
  return err;
}

int BowersFitter::coefficientRangeDefault (int index, double *min,
  double *max)
{
  int err;
  if (index == CV::A2) { // unloading coefficient
    *max = 10;
    *min = 1; // fixed lower limit
    if (!_u_set) setCoefficient (CV::A2, 1);
    _status = err = CV::NORMAL;
  }
  else {
    err = LSPowerLawFitter::coefficientRangeDefault (index, min, max);
  }
  return err;
}

FixedCurveConstants *BowersFitter::curveConstants ()
{
  FixedCurveConstants *retval = LSPowerLawFitter::curveConstants ();

  if (retval) {
    double u;
    coefficient (CV::A2, &u);
    int err = retval->setConstant (CV::A2, u);
    if (err != CV::NORMAL) {
      delete retval, retval = 0;
    }
  }
  return retval;
}

double BowersFitter::internalCoefficient (int index)
{
  double retval;
  if (index == CV::A2) {  // unloading coefficient (u)
    retval = _as[2];
  }
  else {
    retval = LSPowerLawFitter::internalCoefficient (index);
  }
  return retval;
}
