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
#ifndef CURVE_CONSTANTS_HH
#define CURVE_CONSTANTS_HH

class WeightedCurveFitter;
class FixedCurveConstants;

#define NUM_GUIS 9

class CV {

public:
  enum
    {LINEAR = 0      ,				// linear fitter
     QUADRATIC       ,				// quadratic fitter
     EXPONENTIAL     ,				// exponential fitter
     POWER_LAW       ,				// power law fitter
     BOWERS          ,				// power law fitter (Bower's)
     FAST_SHALE      ,				// Bower's fast shale fitter
     SLOW_SHALE      ,				// Bower's slow shale fitter
     LOGARITHMIC     ,				// logarithmic fitter
     TABLE           ,                          // uses table of points
     GAIN            ,				// linear gain fitter
     POWER           ,				// power only fitter
     BOWERS_UNLOADING,				// Bower's unloading curve
     UNSET_CURVE = -1};				// unset curve fitter

  enum
    {NORMAL = 0,				// normal return
     NO_VALID_DATA_FOUND,			// no valid data available
     TOO_LITTLE_DATA,				// not enough data for fitter
     BAD_DATA_FOUND,				// some of the data is invalid
     BAD_COEFFICIENT,				// given bad curve coefficient
     BAD_OFFSET,				// given bad data offset
     DIVIDE_BY_ZERO};				// divide by zero error

  enum
    {MAX_COEFS = 5};				// max # of curve coefficients

  enum
    {X0 = 0,					// x-offset coefficient index
     Y0,					// y-offset coefficient index
     A0,					// 1st coefficient index
     A1,					// 2nd coefficient index
     A2};					// 3rd coefficient index

  enum
    {ORIGINAL_VARIABLE = 0,			// given variable (e.g. x)
     OFFSET_VARIABLE,				// given offset (e.g. x0)
     TRANSLATED_VARIABLE};			// transltd variable (e.g. x')

  static const char *statusMessage		// return status message
    (int status);				//   error status identifier

  static int numCurveTypes ();			// return # of curve types

  static int minimumValidCurveType ();		// return min curve type

  static int maximumValidCurveType ();		// return max curve type

  static int verifyCurveType			// rtn FALSE for bad curve typ
    (int curve_type);				//   given curve type

  static void assertCurveType			// assert for bad curve typ
    (int curve_type);				//   given curve type

  static int coefficientCount			// rtn # of coefficients
    (int curve_type);				//   given curve type

  static int getCurveType			// rtn curve type
    (const char *curve_name);			//   given curve name

  static const char *getCurveName		// rtn curve name
    (int curve_type);				//   given curve type

  static const char *getCurveForm		// rtn curve formula
    (int curve_type);				//   given curve type

  static const char *getIndepForm		// rtn independent var formula
    (int curve_type);				//   given curve type

  static const char *getDependForm		// rtn dependent var formula
    (int curve_type);				//   given curve type

  static WeightedCurveFitter *createFitter	// create curve fitter
    (int curve_type,				//   given curve type
     FixedCurveConstants **fixed_constants = 0);//   given fixed constants obj

  static int minimumDataPointsAllowed		// rtn min # of data pts allwd
    (int curve_type);				//   given curve type

  static int numberOfCoefficientsFit		// rtn # of coefficents fit
    (int curve_type);				//   given curve type

  static int maximumCoefficientIndex		// rtn index of max coef
    (int curve_type);				//   given curve type

  static int minimumCoefficientIndex		// rtn index of min coef
    (int curve_type);				//   given curve type

  static int linear				// rtn 1 if 2 pnt fit curve
    (int curve_type);				//   given curve type

  static float calc				// calculate dependent var y
    (float x,					//   given independent var
     float *coef,				//   given array of coefs
     int curve_type,                            //   given curve type
     WeightedCurveFitter *fitter =  	        //   needed for table types
     (WeightedCurveFitter *) 0);

  static float calc				// calculate dependent var y
    (float x,					//   given independent var
     float xp,					//   2nd given independent var
     float *coef,				//   given array of coefs
     int curve_type);				//   given curve type

  static float backCalc				// calculate independent var x
    (float y,					//   given dependent var
     float *coef,				//   given array of coefs
     int curve_type,				//   given curve type
     WeightedCurveFitter *fitter =      	//   needed for table types
     (WeightedCurveFitter *) 0);

  static float backCalc				// calculate independent var x
    (float y,					//   given dependent var
     float yp,					//   2nd given dependent var
     float *coef,				//   given array of coefs
     int curve_type);				//   given curve type

// Changed the tableCalc parameter name back_calc to forward_calc because
// this what is really going on.  The parameter is True when tableCalc is
// called from calc and False when called from backCalc.  This enforces that for
// tables the first column is the independent variable and the second column is
// the dependent variable.  By changing the parameter name instead of the
// coding, we avoid some retesting.
// ehs --- 16aug00
//
  static float tableCalc                        // calculates like the above
    (float x,                                   //   calc and backCalc on
     WeightedCurveFitter *fitter,               //   table type curves
//   int back_calc = 0);
     int forward_calc = 0);
                            

private:
  CV () {}					// constructor - not needed

                                                // Note: needed to make the
                                                // destructor protected to 
                                                // suppress Linux compiler
                                                // warning that CV has a
                                                // private destructor with no
protected:                                      // friends
  virtual ~CV () {}				// destructor - not needed

private:
  static int curveIndexFromType			// return curve index
    (int curve_type);				//   given curve type

  static int curveTypeFromIndex			// return curve type
    (int curve_type);				//   given curve index

};

class CurveFitters {

public:
  CurveFitters ();				// constructor

  ~CurveFitters ();				// destructor

  void fittersToAllow				// establish which fitters
    (int first_curve_type,			//   first curve type
     ...);					//   end curve types w/ -1

  int first();					// first curve type in list

  int next					// next curve type in list
    (int curve_type);				//   current curve type

  int prev					// next curve type in list
    (int curve_type);				//   current curve type

private:
  int fitterIndex				// index forgive curve type
    (int curve_type);				//   given curve type

  int
    *_curve_types,				// array of curve type
    _curve_count;				// number of curve types

};

#endif
