#include "curves/curve_constants.hh"
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
#include "curves/ls_linear_fitter.hh"
#include "curves/ls_quadratic_fitter.hh"
#include "curves/ls_exponential_fitter.hh"
#include "curves/ls_power_law_fitter.hh"
#include "curves/bowers_fitter.hh"
#include "curves/fast_shale_fitter.hh"
#include "curves/slow_shale_fitter.hh"
#include "curves/ls_logarithmic_fitter.hh"
#include "curves/ls_gain_fitter.hh"
#include "curves/ls_power_fitter.hh"
#include "curves/fixed_curve_constants.hh"
#include "curves/table_fitter.hh"
#include "cprim.h"

#include <math.h>
#include <stdarg.h>
#include <assert.h>

#define NORM_MSG       "normal"
#define NO_DATA_MSG    "no valid data for the fitter was found"
#define TOO_LITTLE_MSG "not enough valid data was found to perform the fit"
#define BAD_DATA_MSG   "some of the data found was invalid for the fitter"
#define BAD_COEF_MSG   "a bad curve coefficient for the fitter was specified"
#define BAD_OFFS_MSG   "a bad data offset was specified to perform the fit"
#define DBZ_MSG        "fitting operation resulted in a divide by zero"

static const float znil = ZNIL;


struct Message {
  int status;
  char *status_message;
};

//   status                  status message

static Message MESSAGES[] =
{
  { CV::NORMAL,              NORM_MSG       },
  { CV::NO_VALID_DATA_FOUND, NO_DATA_MSG    },
  { CV::TOO_LITTLE_DATA,     TOO_LITTLE_MSG },
  { CV::BAD_DATA_FOUND,      BAD_DATA_MSG   },
  { CV::BAD_COEFFICIENT,     BAD_COEF_MSG   },
  { CV::BAD_OFFSET,          BAD_OFFS_MSG   },
  { CV::DIVIDE_BY_ZERO,      DBZ_MSG        },
};

static const char *UNKNOWN_MSG = "unknown";

static const int NUM_MESSAGES = sizeof MESSAGES / sizeof (Message);

const char *CV::statusMessage (int status)
{
  if (status < 0 || status >= NUM_MESSAGES) return UNKNOWN_MSG;
  return MESSAGES[status].status_message;
}

static const char *WHO_KNOWS = "unknown";

struct Curve {
  int curve_type;
  int coefficient_count;
  int min_data_pts_allowed;
  char *curve_name;
  char *curve_form;
  char *indep_form;
  char *depend_form;
};

static Curve CURVE_TYPES[] =
{
  { CV::LINEAR     , 4, 2, "Linear"     , "y' = a1 * x' + a0"             ,
                                          "y' = y - y0"                   ,
                                          "x' = x - x0"                     },
  { CV::QUADRATIC  , 5, 3, "Quadratic"  , "y' = a2 * x'**2 + a1 * x' + a0",
                                          "y' = y - y0"                   ,
                                          "x' = x - x0"                     },
  { CV::EXPONENTIAL, 4, 2, "Exponential", "y' = a0 * e**(a1*x')"          ,
                                          "y' = y - y0"                   ,
                                          "x' = x - x0"                     },
  { CV::POWER_LAW  , 4, 2, "Power Law"  , "y' = a0 * x'**a1"              ,
                                          "y' = y - y0"                   ,
                                          "x' = x - x0"                     },
  { CV::BOWERS     , 5, 2, "Bowers"     , "v' = a0 * s'**a1"              ,
                                          "v' = v - v0"                   ,
                                          "s' = s - s0"                     },
  { CV::FAST_SHALE , 4, 2, "Fast Shale" , "p' = a0 * v'**a1"              ,
                                          "p' = p0 - p"                   ,
                                          "v' = v - v0"                     },
  { CV::SLOW_SHALE , 4, 2, "Slow Shale" , "p' = a0 * v'**a1"              ,
                                          "p' = ln(p0/p)"                ,
                                          "v' = v - v0"                     },
  { CV::LOGARITHMIC, 4, 2, "Logarithmic", "e**y' = a0 * x'**a1"           ,
                                          "y' = y - y0"                   ,
                                          "x' = x - x0"                     },
  { CV::TABLE      , 4, 0, "Table"      , "y' = a1 * x' + a0"             ,
                                          "y' = y - y0"                   ,
                                          "x' = x - x0"                     },
  { CV::GAIN       , 3, 1, "Gain"       , "y' = a1 * x'"                  ,
                                          "y' = y - y0"                   ,
                                          "x' = x - x0"                     },
  { CV::POWER      , 3, 1, "Power"      , "y' = x'**a1"                   ,
                                          "y' = y - y0"                   ,
                                          "x' = x - x0"                     },
  { CV::BOWERS_UNLOADING, 5, 2, "Bowers Unloading",
    "y' = a0[xmax'(x'/xmax')**(1/a2)]**a1",
                                          "y' = y - y0"                   ,
                                          "x' = x - x0"                     },
};

static const int NUM_CURVE_TYPES = sizeof CURVE_TYPES / sizeof (Curve);

int CV::numCurveTypes () { return NUM_CURVE_TYPES; }
int CV::minimumValidCurveType ()
{ 
  int retval, k2;
  for (k2 = 0; k2 < NUM_CURVE_TYPES; k2++) {
    if (k2 == 0) {
      retval = CURVE_TYPES[k2].curve_type;
    }
    else if (CURVE_TYPES[k2].curve_type < retval) {
      retval = CURVE_TYPES[k2].curve_type;
    }
  }
  return retval;
}
int CV::maximumValidCurveType ()
{ 
  int retval, k2;
  for (k2 = 0; k2 < NUM_CURVE_TYPES; k2++) {
    if (k2 == 0) {
      retval = CURVE_TYPES[k2].curve_type;
    }
    else if (CURVE_TYPES[k2].curve_type > retval) {
      retval = CURVE_TYPES[k2].curve_type;
    }
  }
  return retval;
}

int CV::curveIndexFromType (int curve_type)
{
  return curve_type - minimumValidCurveType ();
}

int CV::curveTypeFromIndex (int curve_index)
{
  return curve_index + minimumValidCurveType ();
}

int CV::verifyCurveType (int curve_type)
{
  if (curve_type < minimumValidCurveType()) return FALSE;
  if (curve_type > maximumValidCurveType()) return FALSE;
  assert (curve_type ==
    CURVE_TYPES[curveIndexFromType(curve_type)].curve_type);
  return TRUE;
}

void CV::assertCurveType (int curve_type)
{
  assert (verifyCurveType(curve_type));
}

int CV::coefficientCount (int curve_type)
{
  if (verifyCurveType(curve_type) == FALSE) return 0;
  return CURVE_TYPES[curveIndexFromType(curve_type)].coefficient_count;
}

int CV::getCurveType (const char *curve_name)
{
  int k2;
  for (k2 = 0; k2 < NUM_CURVE_TYPES; k2++) {
    if (strcmp(curve_name,
      CURVE_TYPES[k2].curve_name) == 0) return curveTypeFromIndex (k2);
  }
  return UNSET_CURVE;
}

const char *CV::getCurveName (int curve_type)
{
  if (verifyCurveType(curve_type) == FALSE) return WHO_KNOWS;
  return CURVE_TYPES[curveIndexFromType(curve_type)].curve_name;
}

const char *CV::getCurveForm (int curve_type)
{
  if (verifyCurveType(curve_type) == FALSE) return WHO_KNOWS;
  return CURVE_TYPES[curveIndexFromType(curve_type)].curve_form;
}

const char *CV::getIndepForm (int curve_type)
{
  if (verifyCurveType(curve_type) == FALSE) return WHO_KNOWS;
  return CURVE_TYPES[curveIndexFromType(curve_type)].indep_form;
}

const char *CV::getDependForm (int curve_type)
{
  if (verifyCurveType(curve_type) == FALSE) return WHO_KNOWS;
  return CURVE_TYPES[curveIndexFromType(curve_type)].depend_form;
}

int CV::minimumDataPointsAllowed (int curve_type)
{
  if (verifyCurveType(curve_type) == FALSE) return 0;
  return CURVE_TYPES[curveIndexFromType(curve_type)].min_data_pts_allowed;
}

int CV::numberOfCoefficientsFit (int curve_type)
{
  return minimumDataPointsAllowed (curve_type);
}

WeightedCurveFitter *CV::createFitter (int curve_type,
  FixedCurveConstants **fixed_constants)
{
  assertCurveType (curve_type);

  WeightedCurveFitter *retval;

  switch (curve_type) {
    case CV::LINEAR     :
      retval = new LSLinearFitter ();
      break;
    case CV::QUADRATIC  :
      retval = new LSQuadraticFitter ();
      break;
    case CV::EXPONENTIAL:
      retval = new LSExponentialFitter ();
      break;
    case CV::POWER_LAW  :
      retval = new LSPowerLawFitter ();
      break;
    case CV::BOWERS     :
      retval = new BowersFitter ();
      break;
    case CV::FAST_SHALE :
      retval = new FastShaleFitter ();
      break;
    case CV::SLOW_SHALE :
      retval = new SlowShaleFitter ();
      break;
    case CV::LOGARITHMIC:
      retval = new LSLogarithmicFitter ();
      break;
    case CV::GAIN       :
      retval = new LSGainFitter ();
      break;
    case CV::POWER      :
      retval = new LSPowerFitter ();
      break;
    case CV::TABLE      :
      retval = new TableFitter ();
      break;
    default             :
      assert (0);
  }
  if (fixed_constants) {
    if (*fixed_constants) {
      if ((*fixed_constants)->curveType() == curve_type) {
// reinstate the incoming object storing the fixed constants
        (*fixed_constants)->reinstateConstants ((CurveFitter *)retval);
      }
      else {
// remove the incoming object storing the fixed constants
        delete (*fixed_constants), (*fixed_constants) = 0; // throw away obj
      }
    }
  }
  return retval;
}

int CV::maximumCoefficientIndex (int curve_type)
{
  int retval;
  switch (curve_type) {
    case CV::BOWERS:
      retval = CV::A2;
      break;
    case CV::GAIN  :
    case CV::POWER :
      retval = CV::A1;
      break;
    case CV::TABLE      :
      retval = CV::A1; 
      break;   
    case CV::LINEAR     :
    case CV::QUADRATIC  :
    case CV::EXPONENTIAL:
    case CV::POWER_LAW  :
    case CV::FAST_SHALE :
    case CV::SLOW_SHALE :
    case CV::LOGARITHMIC:
      switch (minimumDataPointsAllowed(curve_type)) {
        case 1:
          retval = CV::A0;
          break;
        case 2:
          retval = CV::A1;
          break;
        case 3:
          retval = CV::A2;
          break;
        default:
          assert (0); // unsupported number of coefficients
      }
      break;
    case CV::UNSET_CURVE:
    default:
      assert (0); // curve type not set correctly
  }
  return retval;
}

int CV::minimumCoefficientIndex (int curve_type)
{
  int retval;
  switch (curve_type) {
    case CV::GAIN  :
    case CV::POWER :
      retval = CV::A1;
      break;
    case CV::LINEAR     :
    case CV::QUADRATIC  :
    case CV::EXPONENTIAL:
    case CV::POWER_LAW  :
    case CV::BOWERS     :
    case CV::FAST_SHALE :
    case CV::SLOW_SHALE :
    case CV::LOGARITHMIC:
    case CV::TABLE      :
      retval = CV::A0;
      break;
    case CV::UNSET_CURVE:
    default:
      assert (0); // curve type not set correctly
  }
  return retval;
}

int CV::linear (int curve_type)
{
  int retval;
  switch (curve_type) {
    case CV::LINEAR     :
    case CV::EXPONENTIAL:
    case CV::POWER_LAW  :
    case CV::BOWERS     :
    case CV::FAST_SHALE :
    case CV::SLOW_SHALE :
    case CV::LOGARITHMIC:
    case CV::GAIN       :
    case CV::POWER      :
    case CV::TABLE      :
      retval = 1;
      break;
    case CV::QUADRATIC  :
      retval = 0;
      break;
    case CV::UNSET_CURVE:
    default:
      assert (0); // fitter type is invalid
  }
  return retval;
}

// coef[0]->x0, coef[1]->y0, coef[2]->a0, coef[3]->a1, coef[4]->a2
//   where the coefficient for aj is the order of x**j it is used with
//   (e.g. j = 1 => a1 & x)
// it is assumed that all coefficients are validated a'priori but not
//   necessarily x
float CV::calc (float x, float *coef, int curve_type, 
                WeightedCurveFitter *fitter)
{
  assertCurveType (curve_type);

  float retval = 0.0;
  switch (curve_type) {
    case CV::LINEAR     :
      {
        x -= coef[0];
        retval = coef[2] + coef[3] * x;
        retval += coef[1];
      }
      break;
    case CV::QUADRATIC  :
      {
	x -= coef[0];
        retval = coef[2] + coef[3] * x + coef[4] * x * x;
	retval += coef[1];
      }
      break;
    case CV::EXPONENTIAL:
      {
	x -= coef[0];
        retval = coef[2] * (float)exp ((double)(coef[3]*x));
	retval += coef[1];
      }
      break;
    case CV::POWER_LAW  :
    case CV::BOWERS     :
      {
	x -= coef[0];
        if (x < 0) {
	  retval = 0;
        }
        else {
	  retval = coef[2] * (float)pow ((double)x,(double)coef[3]);
        }
	retval += coef[1];
      }
      break;
    case CV::FAST_SHALE :
      {
	x -= coef[0];
	if (x < 0) {
	  retval = 0;
	}
	else {
	  retval = coef[2] * (float)pow ((double)x,(double)coef[3]);
	}
	retval = coef[1] - retval;
      }
      break;
    case CV::SLOW_SHALE :
      {
	x -= coef[0];
	if (x < 0) {
	  retval = 0;
	}
	else {
	  retval = coef[2] * (float)pow ((double)x,(double)coef[3]);
	}
	retval = coef[1] / (float)exp ((double)retval);
      }
      break;
    case CV::LOGARITHMIC:
      {
	x -= coef[0];
	if (x < 0) {
	  retval = log ((double)coef[2]) - coef[3] * 100.0F;
	}
	else {
	  retval = log ((double)coef[2]) + coef[3] * log ((double)x);
	}
	retval += coef[1];
      }
      break;
    case CV::GAIN      :
      {
        x -= coef[0];
        retval = coef[2] * x;
        retval += coef[1];
      }
      break;
    case CV::POWER      :
      {
	x -= coef[0];
        if (x < 0) {
	  retval = 0;
        }
        else {
	  retval = (float)pow ((double)x,(double)coef[2]);
        }
	retval += coef[1];
      }
      break;
    case CV::TABLE      :
      {
        //I was asserting on no fitter, but a user can create
        //an empty table by changing types in the calibration lists
        //assert(fitter);
        if(fitter)
          retval = tableCalc(x, fitter, True);
        else
          retval = znil;
      }
      break;
    default             :
      assert(0);
  }
  return retval;
}

// coef[0]->x0, coef[1]->y0, coef[2]->a0, coef[3]->a1, coef[4]->u
//  xp->Sigma_max
// it is assumed that all coefficients are validated a'priori but not
//   necessarily x & xp)
float CV::calc (float x, float xp, float *coef, int curve_type)
{
  assertCurveType (curve_type);

  float retval;
  switch (curve_type) {
    case CV::BOWERS_UNLOADING:
      {
	x  -= coef[0];
	xp -= coef[0];
	if (xp == 0) {
	  retval = 0;
	}
	else {
	  double norm = (double)(x / xp);
	  if (xp < 0 || norm < 0) {
	    retval = 0;
	  }
          else {
	    double normp = (double)xp * pow (norm,(double)(1/coef[4]));
	    retval = coef[2] * (float)pow (normp,(double)coef[3]);
          }
	}
	retval += coef[1];
      }
      break;
    default                  :
      assert(0);
  }
  return retval;
}

// coef[0]->x0, coef[1]->y0, coef[2]->a0, coef[3]->a1, coef[4]->a2
//   where the coefficient for aj is the order of x**j it is used with
//   (e.g. j = 1 => a1 & x)
// it is assumed that all coefficients are validated a'priori but not
//   necessarily y
float CV::backCalc (float y, float *coef, int curve_type,
                    WeightedCurveFitter *fitter)
{
  assertCurveType (curve_type);

  float retval;
  switch (curve_type) {
    case CV::LINEAR     :
      {
	y -= coef[1];
        retval = (y - coef[2]) / coef[3];
	retval += coef[0];
      }
      break;
    case CV::QUADRATIC  :
      {
	y -= coef[1];
// Name variables that look like quadratic formula.
        float a = coef[4];
        float b = coef[3];
        float c = coef[2] - y;

// Really just linear, would cause divide by 0 in quadratic formula.
        if (a == 0.0F) {
          retval = -c / b;
        }
        else {
          float rad = b * b - 4.0F * a * c;

// Return the location of the extreme for imaginary roots.
          if (rad < 0.0F) {
            retval = -b / 2.0F / a; // replace imaginary part with zero
          }
          else {
            float root1 = (-b - (float)sqrt ((double)rad)) / (2.0F * a);
            float root2 = (-b + (float)sqrt ((double)rad)) / (2.0F * a);

            switch (2 * (root1 >= 0.0F) + (root2 >= 0.0F)) {

// Both negative, use largest.
              case 0:
                retval = (root1 > root2) ? root1 : root2;
                break;

// One positive root, use it.
              case 1:
                retval = root2;
                break;
              case 2:
                retval = root1;
                break;

// Both positive, use smallest.
              case 3:
                retval = (root1 < root2) ? root1 : root2;
                break;
              default:
                assert(0);
            }
          }
        }
	retval += coef[0];
      }
      break;
    case CV::EXPONENTIAL:
      {
	y -= coef[1];
        double z = (double)(y / coef[2]);
        if (z <= 0) {
	  retval = 0;
        }
        else {
	  retval = (float)log (z) / coef[3];
        }
	retval += coef[0];
      }
      break;
    case CV::POWER_LAW  :
    case CV::BOWERS     :
      {
	y -= coef[1];
        double z = (double)(y / coef[2]);
        if (z < 0) {
	  retval = 0;
        }
        else {
	  retval = (float)pow (z, (double)(1.0F/coef[3]));
        }
	retval += coef[0];
      }
      break;
    case CV::FAST_SHALE :
      {
	y = coef[1] - y;
        double z = (double)(y / coef[2]);
        if (z < 0) {
	  retval = 0;
        }
        else {
	  retval = (float)pow (z, (double)(1.0F/coef[3]));
        }
	retval += coef[0];
      }
      break;
    case CV::SLOW_SHALE :
      {
	if (y == 0) {
	  retval = 0;
	}
	else {
	  double z0 = (double)(coef[1] / y);
	  if (z0 <= 0) {
	    retval = 0;
	  }
	  else {
	    double z1 = log (z0) / (double)coef[2];
	    if (z1 < 0) {
	      retval = 0;
	    }
	    else {
	      retval = (float)pow (z1, (double)(1.0F/coef[3]));
	    }
	  }
	}
	retval += coef[0];
      }
      break;
    case CV::LOGARITHMIC:
      {
	y -= coef[1];
	retval = (float)exp (((double)y-log((double)coef[2]))/coef[3]);
	retval += coef[0];
      }
      break;
    case CV::GAIN       :
      {
	y -= coef[1];
        retval = y  / coef[2];
	retval += coef[0];
      }
      break;
    case CV::POWER      :
      {
	y -= coef[1];
        if (y < 0) {
	  retval = 0;
        }
        else {
	  retval = (float)pow (y, (double)(1.0F/coef[3]));
        }
	retval += coef[0];
      }
      break;
    case CV::TABLE      :
      {
      //I was asserting on no fitter, but a user can create
      //an empty table by changing types in the calibration lists
      //assert(fitter);
      if(fitter)
        retval = tableCalc(y, fitter, False);
      else
        retval = znil;
      }
      break;

    default             :
      assert(0);
  }
  return retval;
}

// coef[0]->x0, coef[1]->y0, coef[2]->a0, coef[3]->a1, coef[4]->u
//  yp->Vmax
// it is assumed that all coefficients are validated a'priori but not
//   necessarily y & yp
float CV::backCalc (float y, float yp, float *coef, int curve_type)
{
  assertCurveType (curve_type);

  float retval;
  switch (curve_type) {
    case CV::BOWERS_UNLOADING:
      {
	y  -= coef[1];
	yp -= coef[1];
	if (yp == 0) {
	  retval = 0;
	}
	else {
	  double norm = (double)(y / yp);
	  double fact = (double)(yp / coef[2]);
          if (norm < 0 || fact < 0) {
	    retval = 0;
	  }
          else {
	    double normp = fact * pow (norm,(double)coef[4]);
	    retval = (float)pow (normp,(double)(1/coef[3]));
          }
	}
	retval += coef[0];
      }
      break;
    default                  :
      assert(0);
  }
  return retval;
}




//=========================================================================
//== This method handles the calc and backCalc for table type curves ======
//=========================================================================
float CV::tableCalc(float val_in, WeightedCurveFitter *fitter, int forward_calc)
{
float retval = 0.0;
int index;
int which;
int returned_which;
int match = -3;
float min_val, max_val;
float min_returned_val;
float max_returned_val;
float percent;
int incrementing = 1;
int lower, upper, mid;
float test_val, v1, v2;
SeveralFloatArrays *data = (SeveralFloatArrays *)fitter->data(); 
int num = data->numElements();


        if(!num)
           return znil;

        // Column 1 is independent variable, column 2 is dependent.
        // forward_calc True  calculates column 2 given column 1.
        // forward_calc False calculates column 1 given column 2.
	//
        if(forward_calc)
          {
          which = 1;
          returned_which = 2;
          }
        else
          {
          which = 2;
          returned_which = 1;
          }

        if(num > 1)
          {
          v1 = data->getValue(which, 0);
          v2 = data->getValue(which, 1);
          if(v1 > v2) incrementing = 0;
          }

        //Find val_in's location index
        if(incrementing)
          {
	  for (lower = 0, upper = num - 1; lower <= upper;)
	    {
		mid = (lower + upper) / 2;
		test_val = data->getValue(which, mid);

		if      (test_val < val_in)
		{
			lower = mid + 1;
		}
		else if (test_val > val_in)
		{
			upper = mid - 1;
		}
		else
		{
			break;
		}
	    }
          index = (lower > mid) ? mid + 1 : mid;
          }
        else//decrementing
          {
          for (lower = 0, upper = num - 1; lower <= upper;)
	    {
		mid = (lower + upper) / 2;
		test_val = data->getValue(which, mid);

		if      (test_val > val_in)
		{
			lower = mid + 1;
		}
		else if (test_val < val_in)
		{
			upper = mid - 1;
		}
		else
		{
			break;
		}
	    }
          index = (lower > mid) ? mid + 1 : mid;
          }


        //At the beginning
	if      (index == 0)
	{
		if (val_in == data->getValue(which, index))
		{
			match =  0;
		}
		else if (num == 1)
		{
			match = -2;
		}
		else
		{
                        float diff1, diff2;

                        if(incrementing)
                          {
			  diff1 = data->getValue(which, index);
				      - val_in;
			  diff2 = data->getValue(which,index + 1)
				      - data->getValue(which, index);
                          if (2.0F * diff1 > diff2 ||
                            val_in < data->getValue(which, index))
				match = -2;
 			  else
				match = -1;
                          }
                        else
                          {
			  diff1 = val_in
				      - data->getValue(which, index);
			  diff2 = data->getValue(which, index)
                                      - data->getValue(which,index + 1);
                          if (2.0F * diff1 < diff2 ||
                            val_in > data->getValue(which, index))
				match = -2;
			  else
				match = -1;
                          }

		}
	}

        //At the end
	else if (index == num)
	{
		index--;

		if (num == 1)
		{
			match = 2;
		}
		else
		{
                        float diff1, diff2;

                        if(incrementing)
                          {
			  diff1 = val_in
				    - data->getValue(which, index);
			  diff2 = data->getValue(which, index)
				  - data->getValue(which, index-1);

			  if (2.0F * diff1 > diff2 || 
                              val_in > data->getValue(which, index))
				match = 2;
			  else
				match = 1;
                          }
                        else
                          {
                          diff1 = data->getValue(which, index)
                                - val_in;
			  diff2 = data->getValue(which, index-1)
                                - data->getValue(which, index);
			  if (2.0F * diff1 < diff2 || 
                              val_in < data->getValue(which, index))
				match = 2;
			  else
				match = 1;
                          }
		}
	}

        //In the middle
	else
	{
		float diff1 = fabs(val_in - data->getValue(which, index - 1));
		float diff2 = fabs(val_in - data->getValue(which, index));

		if (diff1 < diff2)
		{
			index--;

			if (diff1 == 0.0F)
				match = 0;
			else
				match = 1;
		}
		else 
		{
			if (diff2 == 0.0F)
				match =  0;
			else
				match = -1;
		}
	}


  switch(match)
    {
    //If match is less than or greater than the range of data.
    //I am currently extrapolating from the end points. MLS 02/2000

    //Point requested is before the data
    case -2:
      retval = data->getValue(returned_which, 0);
      return retval;
// This break causes statement is unreachable warning on sgi.
//    break;

    //Point requested is past the data
    case  2:
      retval = data->getValue(returned_which, num - 1);
      return retval;
// This break causes statement is unreachable warning on sgi.
//    break;

    //Exact match found
    case  0:
      retval = data->getValue(returned_which, index);
      return retval;
// This break causes statement is unreachable warning on sgi.
//    break;

    //The match is greater than the indexed value
    case  1:
      min_val          = data->getValue(which, index);
      max_val          = data->getValue(which, index + 1);
      min_returned_val = data->getValue(returned_which, index);
      max_returned_val = data->getValue(returned_which, index + 1);
    break;

    //The match is less than the indexed value
    case -1:
      min_val          = data->getValue(which, index - 1);
      max_val          = data->getValue(which, index);
      min_returned_val = data->getValue(returned_which, index - 1);
      max_returned_val = data->getValue(returned_which, index);
    break;

    default:
      assert(0);
      break;
    }

  if (fitter->isLogarithmic())
  {
    // Do logarithmic interpolation.
    // Column 2 is the logarithmic column.
    // ehs 11apr00
    //
    if (which == 1)
    {
      double log_min_returned_val = log(min_returned_val);
      double log_max_returned_val = log(max_returned_val);

      percent = (val_in - min_val) / (max_val - min_val);

      retval = (float) exp(log_min_returned_val
                          + (log_max_returned_val - log_min_returned_val)
                          * (double) percent);
    }
    else
    {
      assert(which == 2);

      double log_min_val = log(min_val);
      double log_max_val = log(max_val);
      double log_val_in  = log(val_in );

      percent = (float)
                  (log_val_in - log_min_val) / (log_max_val - log_min_val);
      retval  = min_returned_val 
              + ( (max_returned_val - min_returned_val) * percent );
    }
  }
  else
  {
    percent = (val_in - min_val) / (max_val - min_val);
    retval  = min_returned_val 
            + ( (max_returned_val - min_returned_val) * percent );
  }

  return retval;
}




CurveFitters::CurveFitters ()
{
// assume all curve types are valid!
  _curve_types = new int[NUM_CURVE_TYPES];

  int k2;
  for (k2 = 0; k2 < NUM_CURVE_TYPES; k2++) {
    _curve_types[k2] = CURVE_TYPES[k2].curve_type;
  }
}

CurveFitters::~CurveFitters ()
{
  if (_curve_types) delete [] _curve_types, _curve_types = 0;
}

// -1 terminates the list of arguments
void CurveFitters::fittersToAllow (int first_curve_type, ...)
{
// initialize the valid curve types to none
  int k2;
  for (k2 = 0; k2 < NUM_CURVE_TYPES; k2++) _curve_types[k2] = -1;

  va_list args;  /* points to each unnamed arg in turn */
  va_start (args, first_curve_type);

// based on the arguments, determine which curve types are valid
  int found, k3;
  for (k2 = first_curve_type; (k2 > -1); k2 = va_arg(args,int)) {
    found = 0;
    for (k3 = 0; !found && k3 < NUM_CURVE_TYPES; k3++) {
      if (CURVE_TYPES[k3].curve_type == k2) {
        _curve_types[k3] = k2;
        found = 1;
      }
    }
    assert (found);
  }
}

int CurveFitters::first()
{
	int retval, i;

	for (retval = -1, i = 0; (retval == -1) && (i < NUM_CURVE_TYPES); i++)
	{
		retval = _curve_types[i];
	}
	assert(retval != -1);

	return retval;
}

int CurveFitters::next (int curve_type)
{
  int curve_index = fitterIndex (curve_type);
  assert (curve_index >= 0 && curve_index < NUM_CURVE_TYPES);  

  int k2, retval; 
  for (k2 = 0, retval = -1; (retval == -1)&&(k2 < NUM_CURVE_TYPES); k2++) {
    curve_index++;
    if (curve_index >= NUM_CURVE_TYPES) curve_index = 0;
    retval = _curve_types[curve_index];
  }
  assert (retval != -1);

  return retval;
}

int CurveFitters::prev (int curve_type)
{
  int curve_index = fitterIndex (curve_type);
  assert (curve_index >= 0 && curve_index < NUM_CURVE_TYPES);  

  int k2, retval; 
  for (k2 = 0, retval = -1; (retval == -1)&&(k2 < NUM_CURVE_TYPES); k2++) {
    curve_index--;
    if (curve_index < 0) curve_index = NUM_CURVE_TYPES-1;
    retval = _curve_types[curve_index];
  }
  assert (retval != -1);

  return retval;
}

int CurveFitters::fitterIndex (int curve_type)
{
  int retval = -1;
  int k2;
  for (k2 = 0; (retval == -1) && (k2 < NUM_CURVE_TYPES); k2++) {
    if (_curve_types[k2] == curve_type) retval = k2;
  }
  return retval;
}
