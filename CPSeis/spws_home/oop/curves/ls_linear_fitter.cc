#include "curves/ls_linear_fitter.hh"
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
#include <stdlib.h>
#include <assert.h>
#include <float.h>

LSLinearFitter::LSLinearFitter (int type) :
  WeightedCurveFitter (type),
  _n  (0),
  _P  (0),
  _t  (0)
{
}

LSLinearFitter::~LSLinearFitter ()
{
  if (_n)   free (_n),   _n   = 0;
  if (_P)   free (_P),   _P   = 0;
  if (_t)   free (_t),   _t   = 0;
}

// from sums of scaled data, compute scaled coefficients
int LSLinearFitter::doFit ()
{
  double det;
  det = _count * _sx2_sum - _sx_sum * _sx_sum;

  if (fabs(det) < FLT_EPSILON*FLT_EPSILON) {
    return _status = CV::DIVIDE_BY_ZERO;
  }

  _sas[0] = (_sy_sum  * _sx2_sum - _sxy_sum * _sx_sum) / det;

  _sas[1] = (_sxy_sum * _count   - _sy_sum  * _sx_sum) / det;

  return _status = CV::NORMAL;
}

// from sums of scaled data and scaled coefficients, find the LINEAR error
//   statistics In terms of unscaled values
int LSLinearFitter::computeErrorStatistics ()
{
// clearly, the error statistics are very dependent on any
//   scaling done on x or y
  double y_sum, y2_sum, x_sum, x2_sum, xy_sum;
  double y2_gain, x2_gain, xy_gain, a1, a0, err_var;

  y_sum    = _sy_sum / _y_gain - _y_bias / _y_gain * _count;

  y2_gain  = _y_gain * _y_gain;
  y2_sum   = _sy2_sum / y2_gain
           - 2 * _y_bias / y2_gain * _sy_sum
           + _y_bias * _y_bias / y2_gain * _count;

  x_sum    = _sx_sum / _x_gain - _x_bias / _x_gain * _count;

  x2_gain  = _x_gain * _x_gain;
  x2_sum   = _sx2_sum / x2_gain
           - 2 * _x_bias / x2_gain * _sx_sum
           + _x_bias * _x_bias / x2_gain * _count;

  xy_gain  = _y_gain * _x_gain;
  xy_sum   = _sxy_sum / xy_gain
           - _y_bias / xy_gain * _sx_sum
           - _x_bias / xy_gain * _sy_sum
           + _y_bias * _x_bias / xy_gain * _count;

  a1 = internalCoefficient (CV::A1);

  a0 = internalCoefficient (CV::A0);

  if (_count < 1) {
    return _status = CV::TOO_LITTLE_DATA;
  }

  _err_ave = y_sum / _count
           - a1 * x_sum  / _count
           - a0;

  if (_count < 2) {
    _err_std = 0;
  }
  else {
    err_var  = (y2_sum
             - 2 * a1 * xy_sum
             - 2 * a0 * y_sum
             + a1 * a1 * x2_sum
             + 2 * a1 * a0 * x_sum
             + a0 * a0 * _count
             - _count * _err_ave * _err_ave)
             / (_count - 1);

    _err_std = sqrt (err_var);
  }
  return _status = CV::NORMAL;
}

int LSLinearFitter::computeOtherMeasures ()
{
  int err = computeCorrelation ();
  if (err != CV::NORMAL) return _status = err;

  computeProbabilityOfNoCorrelation ();
  return _status = CV::NORMAL;
}

// from sums of scaled data and scaled coefficients, find the correlation coef
int LSLinearFitter::computeCorrelation ()
{
  if (_count <= 0) return _status = CV::TOO_LITTLE_DATA;

// it can be shown that the correlation coefficient is independent of any
//   scaling done on x or y
  double den;

  den = _sx2_sum * _sy2_sum
      - _sy_sum / _count * _sy_sum * _sx2_sum
      - _sx_sum / _count * _sx_sum * _sy2_sum
      + _sx_sum / _count * _sx_sum * _sy_sum / _count * _sy_sum;

  if (den <= 0) {
    _r = 1;  // data has to lie perfectly horizontal or vertical
  }
  else {
    _r = (_sxy_sum - _sx_sum / _count * _sy_sum) / sqrt (den);
  }
  return _status = CV::NORMAL;
}

void LSLinearFitter::computeProbabilityOfNoCorrelation ()
{
// Following after Elements of Statistical Inference by Huntsberger and
//   Billinsley on page 233,
//   we test the hypothesis that the variables are not linearly related, that
//   is, that the correlation is zero.  We use an ordinary t-test for when
//   correlation is zero given t as follows.
//
  double den = 1 - _r * _r;
  if (den < FLT_EPSILON*FLT_EPSILON) {
    _pnr = 0;
    return;
  }
  double t = fabs (_r) * sqrt ((_count - 2) / den);

// The degrees of freedom are _count-2.  To test the hypothesis that
//   the correlation is zero we compare t above to the tables for the given
//   probability of a Type I error.
    _pnr = tDistributionProbability ((float)t, _count-2);

// the result is the probability that no correlation exists!
  _pnr = 2 * (1 - _pnr);
}

double LSLinearFitter::internalCoefficient (int index)
{
  double retval;
  if (index == CV::A1) {
    retval = _as[1] = _sas[1] * _x_gain / _y_gain;
  }
  else if (index == CV::A0) {
    retval = _as[0] = (_sas[1] * _x_bias + _sas[0] - _y_bias) / _y_gain;
  }
  else {
    assert (0); // invalid coefficient index
  }
  return retval;
}

void LSLinearFitter::initializeSums ()
{
  _sx_sum  = 0;
  _sy_sum  = 0;
  _sx2_sum = 0;
  _sxy_sum = 0;
  _sy2_sum = 0;
}

void LSLinearFitter::incrementSums (double x, double y)
{
  _sx_sum  += x;
  _sy_sum  += y;
  _sx2_sum += x * x;
  _sxy_sum += x * y;
  _sy2_sum += y * y;
}

void LSLinearFitter::decrementSums (double x, double y)
{
  _sx_sum  -= x;
  _sy_sum  -= y;
  _sx2_sum -= x * x;
  _sxy_sum -= x * y;
  _sy2_sum -= y * y;
}

// from the stored unscaled coefficients, compute the scaled coefficients
void LSLinearFitter::redoCoefficientValues ()
{
  _sas[1] = _as[1] * _y_gain / _x_gain;
  _sas[0] = _as[0] * _y_gain - _sas[1] * _x_bias + _y_bias;
}

// given the t statistic and the degrees of freedom, n; return the
//   probability interpolated from a t-distribution table
float LSLinearFitter::tDistributionProbability (float t, int n)
{
  int k2;
  if (n <= 0) return 1;
  if (!_n && !_P && !_t) {
    _n = (int *)malloc (sizeof(int)*(size_t)34);
    assert (_n);
    _P = (float *)malloc (sizeof(float)*(size_t)8);
    assert (_P);
    _t = (float *)malloc (sizeof(float)*(size_t)8*34);
    assert (_t);

// define the degrees of freedom array
    for (k2 = 0; k2 < 30; k2++) _n[k2] = k2+1;
          _n[k2] = 40;
    k2++; _n[k2] = 60;
    k2++; _n[k2] = 120;
    k2++; _n[k2] = 300; // really this is taken as infinite degrees of freedom

// define the probability array
    _P[0] = 0.6;
    _P[1] = 0.75;
    _P[2] = 0.90;
    _P[3] = 0.95;
    _P[4] = 0.975;
    _P[5] = 0.99;
    _P[6] = 0.995;
    _P[7] = 0.9995;

// define the left side of the t-distribution table
    _t[  0]=  0.325;_t[  1]=  1.000;_t[  2]=  3.078;_t[  3]=  6.314;
    _t[  8]=  0.289;_t[  9]=  0.816;_t[ 10]=  1.886;_t[ 11]=  2.920;
    _t[ 16]=  0.277;_t[ 17]=  0.765;_t[ 18]=  1.638;_t[ 19]=  2.353;
    _t[ 24]=  0.271;_t[ 25]=  0.741;_t[ 26]=  1.533;_t[ 27]=  2.132;
    _t[ 32]=  0.267;_t[ 33]=  0.727;_t[ 34]=  1.476;_t[ 35]=  2.015;
    _t[ 40]=  0.265;_t[ 41]=  0.718;_t[ 42]=  1.440;_t[ 43]=  1.943;
    _t[ 48]=  0.263;_t[ 49]=  0.711;_t[ 50]=  1.415;_t[ 51]=  1.895;
    _t[ 56]=  0.262;_t[ 57]=  0.706;_t[ 58]=  1.397;_t[ 59]=  1.860;
    _t[ 64]=  0.261;_t[ 65]=  0.703;_t[ 66]=  1.383;_t[ 67]=  1.833;
    _t[ 72]=  0.260;_t[ 73]=  0.700;_t[ 74]=  1.372;_t[ 75]=  1.812;
    _t[ 80]=  0.260;_t[ 81]=  0.697;_t[ 82]=  1.363;_t[ 83]=  1.796;
    _t[ 88]=  0.259;_t[ 89]=  0.695;_t[ 90]=  1.356;_t[ 91]=  1.782;
    _t[ 96]=  0.259;_t[ 97]=  0.694;_t[ 98]=  1.350;_t[ 99]=  1.771;
    _t[104]=  0.258;_t[105]=  0.692;_t[106]=  1.345;_t[107]=  1.761;
    _t[112]=  0.258;_t[113]=  0.691;_t[114]=  1.341;_t[115]=  1.753;
    _t[120]=  0.258;_t[121]=  0.690;_t[122]=  1.337;_t[123]=  1.746;
    _t[128]=  0.257;_t[129]=  0.689;_t[130]=  1.333;_t[131]=  1.740;
    _t[136]=  0.257;_t[137]=  0.688;_t[138]=  1.330;_t[139]=  1.734;
    _t[144]=  0.257;_t[145]=  0.688;_t[146]=  1.328;_t[147]=  1.729;
    _t[152]=  0.257;_t[153]=  0.687;_t[154]=  1.325;_t[155]=  1.725;
    _t[160]=  0.257;_t[161]=  0.686;_t[162]=  1.323;_t[163]=  1.724;
    _t[168]=  0.256;_t[169]=  0.686;_t[170]=  1.321;_t[171]=  1.717;
    _t[176]=  0.256;_t[177]=  0.685;_t[178]=  1.319;_t[179]=  1.714;
    _t[184]=  0.256;_t[185]=  0.685;_t[186]=  1.318;_t[187]=  1.711;
    _t[192]=  0.256;_t[193]=  0.684;_t[194]=  1.316;_t[195]=  1.708;
    _t[200]=  0.256;_t[201]=  0.684;_t[202]=  1.315;_t[203]=  1.706;
    _t[208]=  0.256;_t[209]=  0.684;_t[210]=  1.314;_t[211]=  1.703;
    _t[216]=  0.256;_t[217]=  0.683;_t[218]=  1.313;_t[219]=  1.701;
    _t[224]=  0.256;_t[225]=  0.683;_t[226]=  1.311;_t[227]=  1.699;
    _t[232]=  0.256;_t[233]=  0.683;_t[234]=  1.310;_t[235]=  1.697;
    _t[240]=  0.255;_t[241]=  0.681;_t[242]=  1.303;_t[243]=  1.684;
    _t[248]=  0.254;_t[249]=  0.679;_t[250]=  1.296;_t[251]=  1.671;
    _t[256]=  0.264;_t[257]=  0.677;_t[258]=  1.289;_t[259]=  1.658;
    _t[264]=  0.253;_t[265]=  0.674;_t[266]=  1.282;_t[267]=  1.645;


// define the right side of the t-distribution table
    _t[  4]= 12.706;_t[  5]= 31.821;_t[  6]= 63.657;_t[  7]=636.619;
    _t[ 12]=  4.303;_t[ 13]=  6.965;_t[ 14]=  9.925;_t[ 15]= 31.598;
    _t[ 20]=  3.182;_t[ 21]=  4.541;_t[ 22]=  5.841;_t[ 23]= 12.924;
    _t[ 28]=  2.776;_t[ 29]=  3.747;_t[ 30]=  4.604;_t[ 31]=  8.610;
    _t[ 36]=  2.571;_t[ 37]=  3.365;_t[ 38]=  4.032;_t[ 39]=  6.869;
    _t[ 44]=  2.447;_t[ 45]=  3.143;_t[ 46]=  3.707;_t[ 47]=  5.959;
    _t[ 52]=  2.365;_t[ 53]=  2.998;_t[ 54]=  3.499;_t[ 55]=  5.408;
    _t[ 60]=  2.306;_t[ 61]=  2.896;_t[ 62]=  3.355;_t[ 63]=  5.041;
    _t[ 68]=  2.262;_t[ 69]=  2.821;_t[ 70]=  3.250;_t[ 71]=  4.781;
    _t[ 76]=  2.228;_t[ 77]=  2.764;_t[ 78]=  3.169;_t[ 79]=  4.587;
    _t[ 84]=  2.201;_t[ 85]=  2.718;_t[ 86]=  3.106;_t[ 87]=  4.437;
    _t[ 92]=  2.179;_t[ 93]=  2.681;_t[ 94]=  3.055;_t[ 95]=  4.318;
    _t[100]=  2.160;_t[101]=  2.650;_t[102]=  3.012;_t[103]=  4.221;
    _t[108]=  2.145;_t[109]=  2.624;_t[110]=  2.977;_t[111]=  4.140;
    _t[116]=  2.131;_t[117]=  2.602;_t[118]=  2.947;_t[119]=  4.073;
    _t[124]=  2.120;_t[125]=  2.583;_t[126]=  2.921;_t[127]=  4.015;
    _t[132]=  2.110;_t[133]=  2.567;_t[134]=  2.898;_t[135]=  3.965;
    _t[140]=  2.101;_t[141]=  2.552;_t[142]=  2.878;_t[143]=  3.922;
    _t[148]=  2.093;_t[149]=  2.539;_t[150]=  2.861;_t[151]=  3.883;
    _t[156]=  2.086;_t[157]=  2.528;_t[158]=  2.845;_t[159]=  3.850;
    _t[164]=  2.080;_t[165]=  2.518;_t[166]=  2.831;_t[167]=  3.819;
    _t[172]=  2.074;_t[173]=  2.508;_t[174]=  2.819;_t[175]=  3.792;
    _t[180]=  2.069;_t[181]=  2.500;_t[182]=  2.807;_t[183]=  3.767;
    _t[188]=  2.064;_t[189]=  2.492;_t[190]=  2.797;_t[191]=  3.745;
    _t[196]=  2.060;_t[197]=  2.485;_t[198]=  2.787;_t[199]=  3.725;
    _t[204]=  2.056;_t[205]=  2.479;_t[206]=  2.779;_t[207]=  3.707;
    _t[212]=  2.052;_t[213]=  2.473;_t[214]=  2.771;_t[215]=  3.690;
    _t[220]=  2.048;_t[221]=  2.467;_t[222]=  2.763;_t[223]=  3.674;
    _t[228]=  2.045;_t[229]=  2.462;_t[230]=  2.756;_t[231]=  3.659;
    _t[236]=  2.042;_t[237]=  2.457;_t[238]=  2.750;_t[239]=  3.646;
    _t[244]=  2.021;_t[245]=  2.423;_t[246]=  2.704;_t[247]=  3.551;
    _t[252]=  2.000;_t[253]=  2.390;_t[254]=  2.660;_t[255]=  3.460;
    _t[260]=  1.980;_t[261]=  2.358;_t[262]=  2.617;_t[263]=  3.373;
    _t[268]=  1.960;_t[269]=  2.326;_t[270]=  2.576;_t[271]=  3.291;
  }

  int indx;
  if (n <= 30) {
    indx = n - 1;
  }
  else if (n == _n[30]) {
    indx = 30;
  }
  else if (n == _n[31]) {
    indx = 31;
  }
  else if (n == _n[32]) {
    indx = 32;
  }
  else if (n > 2*_n[32]) {
// if the degrees of freedom is more than twice as many as the next to the
//   last degrees of freedom value, then assume an infinite sample size
    indx = 33;
  }
  else {
// the degrees of freedom are between rows
    indx = -1;
  }

  int k20, k2n, k3, k4;
  float ts0, ts1;
  if (indx != -1) {
// is the statistic within the row of the table
    k20 = indx * 8;
    if (t <  _t[k20]) return _P[0];
    k2n = (indx+1) * 8;
    if (t >= _t[k2n-1]) return _P[7];

// based on the degrees of freedom, find the two points on the row that
//   sandwich the given t-statistic
    k3 = -1;
    k4 = 0;
    for (k2 = k20+1; k2 < k2n && k3 == -1; k2++) {
      k4++;
      if (t >= _t[k2-1] && t < _t[k2]) k3 = k2;
    }
    if (k3 > 0) {
      ts0 = _t[k3-1];
      ts1 = _t[k3];
    }
  }

  else {
// find the weight between rows in the table
    indx = -1;
    for (k2 = 30; k2 < 34 && indx == -1; k2++) {
      if (n > _n[k2-1] && n <= _n[k2]) indx = k2;
    }
    if (indx == -1) return 0.5;  // t-distribution table is inadequate here
//  assert (indx != -1);
    float rwt = (float)(n - _n[indx]) / (float)(_n[indx-1] - _n[indx]);

// using the row weighting, determine if the statistic is within the
//   row of the table
    k20 = (indx-1) * 8;
    k2n =  indx    * 8;

    k3 = -1;
    k4 = 0;
    float ts0, ts1;
    for (k2 = k20+1; k2 < k2n && k3 == -1; k2++) {
// based on the row weighting, compute the t-statistic range
      ts0 = rwt * _t[k2]   + (1-rwt) * _t[k2-1];
      ts1 = rwt * _t[k2+8] + (1-rwt) * _t[k2+7];

// identify the two weighted points along the row that
//   sandwich the given t-statistic
      k4++;
           if (k2 == k20+1 && t <  ts0) return _P[0];
      else if (k2 == k2n-1 && t >= ts1) return _P[7];
      else if (t >= ts0 && t < ts1) k3 = k2;
    }
  }

// interpolate the probability to return
  if (k3 == -1) return 0.5;  // t-distribution table is inadequate here
//  assert (k3 != -1);
  float wt, P;
  wt = (t - ts0) / (ts1 - ts0);
  P = wt * _P[k4] + (1-wt) * _P[k4-1];
  return P;
}
