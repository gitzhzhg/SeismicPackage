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
// y = f(a[i],x**i); for i = 0,1,...,n.

#ifndef CURVE_FITTER_HH
#define CURVE_FITTER_HH

#include "curves/curve_constants.hh"
#include "oprim/several_float_arrays.hh"


class FixedCurveConstants;



class TableData : public SeveralFloatArrays {

  public:
    TableData();
};




class CurveFitter {

public:
  virtual ~CurveFitter ();			// destructor

  int doit				        // replace arrays
    (float *xs,					//   independent array
     float *ys,					//   dependent array
     int count);				//   new array size

  int remove					// remove data from sums
    (float *xs,					//   independent array
     float *ys,					//   dependent array
     int count);				//   # of data vlus to remove

  int insert					//  insert data to sums
    (float *xs,					//   independent array
     float *ys,					//   dependent array
     int count);				//   # of data vlus to insert

  virtual int computeSums			// compute sums for fitting
    (float *xs,					//   independent array
     float *ys,					//   dependent array
     int count);				//   # of data vlus to remove

  virtual int setCoefficient			// set an unscaled coefficient
    (int index,					//   given index of coefficnt
     double coefficient);			//   given unscld xtrnl coef

  int redoCoefficients ();			// call to redo scaled coeffs

  int type ()					// return this type of fitter
    { return _type; }

  double errorAverage ()			// return average error
    { return _err_ave; }

  double errorStandardDeviation ()		// return error stndrd devtn
    { return _err_std; }

  int errorStatus ()				// rtrn err status of last op
    { return _status; }

  virtual int doFit () = 0;			// find curve coeffs (0-OK)

  virtual int computeErrorStatistics () = 0;	// find error statistics

  virtual int computeOtherMeasures ();		// find other stat measures

  virtual int coefficient			// return an unscaled coeffcnt
    (int index,					//   given index of coefficnt
     double *coef);				//   rtnd unscaled coefficient

  virtual int coefficientRangeDefault		// return def coef range 
    (int index,					//   given index of coefficnt
     double *min,				//   rtnd minimum coef value
     double *max);				//   rtnd maximum coef value

  virtual FixedCurveConstants *curveConstants ();// return fixed curve consts

  TableData *data() {return _data;}             // 

protected:
  CurveFitter					// constructor
    (int type = CV::LINEAR);			//   type of curve

  virtual int convertOffsetToInternal		// cnvert to internal offset
    (int index,					//   given index of offset
     double external_offset,			//   gvn external offset value
     double *internal_offset);			//   rtnd internal offset valu

  virtual int convertOffsetFromInternal		// cnvert from internal offset
    (int index,					//   given index of offset
     double internal_offset,			//   gvn internal offset value
     double *external_offset);			//   rtnd external offset valu

  virtual int convertCoefficientToInternal	// cnvrt to unscld ntrnl coef
    (int index,					//   given index of coefficnt
     double external_coefficient,		//   gvn xtrnl unscld coef vlu
     double *internal_coefficient);		//   rtnd ntrnl unscld coef vl

  virtual int convertCoefficientFromInternal	// cnvrt frm unscld ntrnl coef
    (int index,					//   given index of coefficnt
     double internal_coefficient,		//   gvn ntrnl unscld coef vlu
     double *external_coefficient);		//   rtnd xtrnl unscld coef vl

  virtual double internalCoefficient		// rtn internal coefficient
    (int index) = 0;				//   given index of coefficnt

  virtual void limitCoefficientRange		// return limited coef range
    (int /* index */,				//   given index of coefficnt
     double * /* min */,			//   rtnd minimum coef value
     double * /* max */) {}			//   rtnd maximum coef value

  virtual void initializeSums () = 0;		// initialize the sums

  virtual void incrementSums			// increment the sums using
    (double x,					//   independent value
     double y) = 0;				//   dependent value

  virtual void decrementSums			// decrement the sums using
    (double x,					//   independent value
     double y) = 0;				//   dependent value

  virtual int independentVariableModifier	// indepndnt variable modifier
    (float x,					//   independent value
     double *modified_x);			//   modified independent valu

  virtual int independentVariableUnmodifier	// indepndnt var inverse mod
    (float x,					//   independent value
     double *unmodified_x);			//   unmodified independent vl

  virtual int dependentVariableModifier		// dependent variable modifier
    (float y,					//   dependent value
     double *modified_y);			//   modified dependent value

  virtual int dependentVariableUnmodifier	// dependent var inverse mod
    (float y,					//   dependent value
     double *unmodified_y);			//   unmodified dependent valu

  virtual void redoCoefficientValues () = 0;	// redo scaled coefficients

  virtual int coefficientIndex			// rtn coefficient array index
    (int index);				//   given external index

  double
    *_as,					// unscaled coefficients
    *_sas,					// scaled coefficients
    _err_ave,					// error average
    _err_std,					// error stnd dev
    _x_gain,					// independent scale gain
    _x_bias,					// independent scale bias
    _y_gain,					// dependent scale gain
    _y_bias,					// dependent scale bias
    _x0,					// independent unscaled offset
    _y0;					// dependent unscaled offset

  int
    _x0_set,					// 0 -> _x0 has not been set
    _y0_set,					// 0 -> _y0 has not been set
    _type,					// type of curve
    _status,					// error status of last oper
    _count;					// sample size

    TableData   *_data;                         // data valuues

};

#endif
