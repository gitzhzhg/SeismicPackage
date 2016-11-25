//========================= COPYRIGHT NOTICE ================================
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
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//===========================================================================
//===== TableFitter derived from WeightedCurve fitter               =========
//===== This does the math etc.                                     =========
//===== M.L. Sherrill 12/99                                         =========
//===========================================================================


#include "curves/table_fitter.hh"
#include <math.h>
#include <stdlib.h>
#include <assert.h>
#include <float.h>









TableFitter::TableFitter (int type) :
  WeightedCurveFitter (type),
  _n  (0),
  _P  (0),
  _t  (0)
{
  _data = new TableData();
}

TableFitter::~TableFitter ()
{
  if (_n)   free (_n),   _n   = 0;
  if (_P)   free (_P),   _P   = 0;
  if (_t)   free (_t),   _t   = 0;
  delete _data;
}

// from sums of scaled data, compute scaled coefficients
int TableFitter::doFit ()
{
  return CV::NORMAL;
}

int TableFitter::doit (float *xs, float *ys, int count)
{

  for(long i = 0; i < count; i++)
    {
    _data->setOrAppendValue(1, i, xs[i]);
    _data->setOrAppendValue(2, i, ys[i]);  
    }
  
  return CV::NORMAL;
}


int TableFitter::coefficient (int /*index*/, double *coef)
{
  *coef = 0.0;
  return CV::NORMAL;
}




// from sums of scaled data and scaled coefficients, find the LINEAR error
//   statistics In terms of unscaled values
int TableFitter::computeErrorStatistics ()
{
// clearly, the error statistics are very dependent on any
//   scaling done on x or y

// May want to compute these later if desired on the table
// popup

  _err_ave = 0;
  _err_std = 0;
 
  
  return _status = CV::NORMAL;
}

int TableFitter::computeOtherMeasures ()
{
  return _status = CV::NORMAL;
}


double TableFitter::internalCoefficient (int /*index*/)
{
  return 0.0;
}

void TableFitter::initializeSums ()
{
}

void TableFitter::incrementSums (double /*x*/, double /*y*/)
{
}

void TableFitter::decrementSums (double /*x*/, double /*y*/)
{
}

// from the stored unscaled coefficients, compute the scaled coefficients
void TableFitter::redoCoefficientValues ()
{
}

