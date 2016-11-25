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
#include "pick/hurst.hh"

#include <math.h>
#include <stdio.h>
#include <assert.h>

#ifndef sun
#include <time.h>
#endif

#define _HURST_WINDOW		 80
#define _NUM_SUB_WINDOW_SIZES	  5
#define _SLOPE_WINDOW		 25
#define _DIFF_WINDOW		 80
#define _NUM_PAD                  0
#define _PAD    (unsigned char) 128

/*
 * All the setup for HurstBreak is copied from Tom S. break_pick & break_picks.
 */
void hurst_func(long first_trace, long last_trace, long num_traces,
	float tmincur, float tmaxcur, float picks[], float missing_pick,
	float zero_pick, unsigned char traces[], long nsamp, float tmin,
	float dt, float minSigDiff, float *minPicks, float *maxPicks)
{
#ifndef sun
	clock_t start = clock();
#endif

	HurstBreak *hurstBreak = new HurstBreak((int) nsamp,
		_HURST_WINDOW, _NUM_SUB_WINDOW_SIZES, _SLOPE_WINDOW,
		_DIFF_WINDOW, _NUM_PAD, _PAD);

	int number = (int) (last_trace - first_trace) + 1;
	int i, index1, index2;
	int j, jstart, jdone;
	double sigDiff;
	int failed = 0;
	
	int jmin = (int) ((tmincur - tmin) / dt + 0.5);
	int jmax = (int) ((tmaxcur - tmin) / dt + 0.5);
	int toLeft  = hurstBreak->getLeft ();
	int toRight = hurstBreak->getRight();

	for(i = 0; i < number; i++)
	{
		index1 = (int) first_trace - 1 + i;
		if(index1 >= 0 && index1 < num_traces &&
			picks[index1] != missing_pick)
		{
			index2 = index1 * (int) nsamp;
			jstart = (int) ((minPicks[index1] - tmin) / dt + 0.5)
				- toLeft ;
			if (jstart <  jmin   ) jstart = jmin;
			jdone  = (int) ((maxPicks[index1] - tmin) / dt + 0.5)
				+ toRight;
			if (jdone  >  jmax   ) jdone = jmax;
			if (jstart < jdone - 3)
			{
				j = hurstBreak->getPick(jdone - jstart + 1,
					&traces[index2 + jstart], &sigDiff);

				if (sigDiff < (double) minSigDiff)
				{
					picks[index1] = zero_pick;
					failed++;
				}
				else
				{
					picks[index1] =
						(j + jstart) * dt + tmin;
				}
			}
		}
	}

	delete hurstBreak;

#ifdef sun
	printf("Hurst picking:  %d traces ", number);
#else
	printf("Hurst picking:  %d traces in %f sec.", number,
		(float) (clock() - start) / (float) CLOCKS_PER_SEC);
#endif

	if (failed)
	{
		printf(" with %d traces failing\n", failed);
	}
	else
	{
		printf("\n");
	}
}

Hurst::Hurst(int maxNumPts, int windowSize, int numSubWindowSizes)
	: _maxNumPts (maxNumPts ), _windowSize(windowSize),
	  _numSubWindowSizes(numSubWindowSizes)
{
	_cum1 = new double[_maxNumPts + 1];
	_cum2 = new double[_maxNumPts + 1];

	_subWindowSize = new int[_numSubWindowSizes];
	_numSubWindows = new int[_numSubWindowSizes];

	_logSubWindowSize        = new double[_numSubWindowSizes];
	_sqrtSubWindowSizeMinus1 = new double[_numSubWindowSizes];
	_constTauValue           = new double[_numSubWindowSizes];

	int i;

	for (	_subWindowSize[_numSubWindowSizes - 1] = windowSize,
		_numSubWindows[_numSubWindowSizes - 1] = 1,
		i = _numSubWindowSizes - 2;
		i >= 0; i--)
	{
		assert(_subWindowSize[i + 1] % 2 == 0);
		_subWindowSize[i] = _subWindowSize[i + 1] / 2;
		_numSubWindows[i] = _numSubWindows[i + 1] * 2;
	}

	for (_sumX = _sumXX = 0.0, i = 0; i < _numSubWindowSizes; i++)
	{
		_logSubWindowSize[i] = log10((double) _subWindowSize[i]);
		_sumX  += _logSubWindowSize[i];
		_sumXX += _logSubWindowSize[i] * _logSubWindowSize[i];
		_sqrtSubWindowSizeMinus1[i] =
			sqrt((double) (_subWindowSize[i] - 1));
		_constTauValue[i] = sqrt((double) _subWindowSize[i])
			/ (double) _numSubWindows[i];
	}

	_tauPart = new double *[_numSubWindowSizes];

	for (i = 0; i < _numSubWindowSizes; i++)
		_tauPart[i] = new double[_maxNumPts - _subWindowSize[i] + 1];
}

Hurst::~Hurst()
{
	delete [] _cum1;
	delete [] _cum2;

	delete [] _subWindowSize          ;
	delete [] _logSubWindowSize       ;
	delete [] _numSubWindows          ;
	delete [] _constTauValue          ;
	delete [] _sqrtSubWindowSizeMinus1;

	for (int i = 0; i < _numSubWindowSizes; i++)
		delete [] _tauPart[i];

	delete [] _tauPart;
}

void Hurst::loadData(int numPts, double *y)
{
	assert(numPts <= _maxNumPts);
	_y = y;

	int i;
	for (_cum1[0] = _cum2[0] = 0.0, i = 1; i <= numPts; i++)
	{
		_cum1[i] = _cum1[i-1] + _y[i-1]          ;
		_cum2[i] = _cum2[i-1] + _y[i-1] * _y[i-1];
	}

	double cum1, cum2, mean, ss, cum, minCum, maxCum;
	int j1, j2, k;
	for (i = 0; i < _numSubWindowSizes; i++)
		for (j1 = 0, j2 = _subWindowSize[i]; j2 <= numPts; j1++, j2++)
		{
			cum1 = _cum1[j2] - _cum1[j1];
			cum2 = _cum2[j2] - _cum2[j1];

			mean = cum1 / (double) _subWindowSize[i];
			ss   = cum2 - mean * cum1;

			if (ss != 0.0)
			{
				for (minCum = maxCum = cum = 0.0, k = j1;
					k < j2;
					k++)
				{
					cum += _y[k] - mean;

					minCum = (cum < minCum) ? cum : minCum;
					maxCum = (cum > maxCum) ? cum : maxCum;
				}

				_tauPart[i][j1] = (maxCum - minCum)
					/ sqrt(ss)
					* _sqrtSubWindowSizeMinus1[i]
					/ (double) _numSubWindows[i];
			}
			else
			{
				_tauPart[i][j1] = _constTauValue[i];
			}
		}
}

double Hurst::getH(int index)
{
	double sumY, sumXY, tau, logTau;
	int i, j;

	for (sumY = sumXY = 0.0, i = 0; i < _numSubWindowSizes; i++)
	{
		for (tau = 0.0, j = index;
			j < index + _windowSize;
			j += _subWindowSize[i])
		{
			tau += _tauPart[i][j];
		}

		logTau = log10(tau);
		sumY  += logTau;
		sumXY += logTau * _logSubWindowSize[i];
	}

	return ((double) _numSubWindowSizes *  sumXY - _sumX *  sumY)
	     / ((double) _numSubWindowSizes * _sumXX - _sumX * _sumX);
}

HurstBreak::HurstBreak(int maxNumDataPts, int hurstWindow,
	int numSubWindowSizes, int slopeWindow, int diffWindow,
	int numPad, unsigned char pad)
	: _maxNumDataPts(maxNumDataPts), _hurstWindow(hurstWindow),
	  _slopeWindow  (slopeWindow  ), _diffWindow (diffWindow ),
	  _numPad       (numPad       )
{
	assert(_slopeWindow %2);		/* must be odd */
	_halfSlopeWindow = _slopeWindow / 2;

	_hurst = new Hurst(_maxNumDataPts + _numPad, _hurstWindow,
		numSubWindowSizes);

	_y = new double[_maxNumDataPts + _numPad];
	int i;
	for (i = 0; i < _numPad; i++)
		_y[i] = (double) pad;

	_he    = new double[_maxNumDataPts + _numPad - _hurstWindow + 1];

	_sumX  = new double[_maxNumDataPts + _numPad - _hurstWindow + 2];
	_sumY  = new double[_maxNumDataPts + _numPad - _hurstWindow + 2];
	_sumXY = new double[_maxNumDataPts + _numPad - _hurstWindow + 2];
	_sumXX = new double[_maxNumDataPts + _numPad - _hurstWindow + 2];
	_sumYY = new double[_maxNumDataPts + _numPad - _hurstWindow + 2];

	for (_sumX[0] = _sumXX[0] = 0.0, i = 1;
		i < _maxNumDataPts + _numPad - _hurstWindow + 2;
		i++)
	{
		_sumX [i] = _sumX [i-1] + (double) i;
		_sumXX[i] = _sumXX[i-1] + (double) i * (double) i;
	}
}

HurstBreak::~HurstBreak()
{
	delete _hurst;
	delete _y    ;
	delete _he   ;
	delete _sumX ;
	delete _sumY ;
	delete _sumXY;
	delete _sumXX;
	delete _sumYY;
}

int HurstBreak::getPick(int numPts, unsigned char *y, double *sigDiff)
{
	int retval;

	assert(numPts <= _maxNumDataPts);

	int i;
	for (i = _numPad; i < numPts + _numPad; i++)
		_y[i] = (double) y[i];

	_hurst->loadData(numPts + _numPad, _y);

	for (i = 0; i <= numPts + _numPad - _hurstWindow; i++)
		_he[i] = _hurst->getH(i);

	loadSums(numPts + _numPad - _hurstWindow + 1);

	double slope, maxSlope, difference, maxDifference;
	int maxSlopeIndex;

	for (maxDifference = maxSlope = 0.0, retval = maxSlopeIndex = -1, i = 0;
		i < numPts + _numPad - _hurstWindow - _slopeWindow + 2;
		i++)
	{
		slope = getSlope(i);

		if (slope <= 0.0)
		{
			if (maxSlopeIndex > -1)
			{
				difference = getDifference(
					maxSlopeIndex + _halfSlopeWindow,
					_diffWindow,
					numPts + _numPad - _hurstWindow + 1);

				if (retval > -1)
				{
					if (difference > maxDifference)
					{
						maxDifference = difference;
						retval = maxSlopeIndex;
					}
				}
				else
				{
					maxDifference = difference;
					retval = maxSlopeIndex;
				}

				maxSlopeIndex = -1;
			}
		}
		else
		{
			if (maxSlopeIndex > -1)
			{
				if (slope > maxSlope)
				{
					maxSlope = slope;
					maxSlopeIndex = i;
				}
			}
			else
			{
				maxSlope = slope;
				maxSlopeIndex = i;
			}
		}
	}

	if (retval >= 0)
	{
		retval = retval - _numPad + _hurstWindow + _halfSlopeWindow - 1;
		*sigDiff = maxDifference;
	}
	else
	{
		retval = 0;
		*sigDiff = 0.0;
	}

	return retval;
}

int HurstBreak::getLeft()
{
	if (_diffWindow == -1)
	{
		return -1;
	}
	else if (_diffWindow > _halfSlopeWindow)
	{
		return _hurstWindow + _diffWindow - 1;
	}
	else
	{
		return _hurstWindow + _halfSlopeWindow - 1;
	}
}

int HurstBreak::getRight()
{
	if (_diffWindow == -1)
	{
		return -1;
	}
	else if (_diffWindow > _halfSlopeWindow)
	{
		return _diffWindow;
	}
	else
	{
		return _halfSlopeWindow;
	}
}

void HurstBreak::loadSums(int numPts)
{
	int i;
	for (_sumY[0] = _sumXY[0] = _sumYY[0] = 0.0, i = 1; i <= numPts; i++)
	{
		_sumY [i] = _sumY [i-1] + _he[i-1];
		_sumXY[i] = _sumXY[i-1] + (double) i * _he[i-1];
		_sumYY[i] = _sumYY[i-1] + _he[i-1]   * _he[i-1];
	}
}
	
double HurstBreak::getSlope(int index)
{
	int ptr1 = index               ;
	int ptr2 = index + _slopeWindow;
	double sumX  = _sumX [ptr2] - _sumX [ptr1];
	double sumY  = _sumY [ptr2] - _sumY [ptr1];
	double sumXY = _sumXY[ptr2] - _sumXY[ptr1];
	double sumXX = _sumXX[ptr2] - _sumXX[ptr1];

	return ((double) _slopeWindow * sumXY - sumX * sumY)
	     / ((double) _slopeWindow * sumXX - sumX * sumX);
}

double HurstBreak::getDifference(int index, int num)
{
	double mean1 =  _sumY [index] / (double) index;
	double mean2 = (_sumY [num  ] - _sumY [index+1])
		/ (double) (num - index - 1);

	double ss1 =  _sumYY[index] - mean1 * _sumY [index];
	double ss2 = (_sumYY[num  ] - _sumYY[index+1])
	   - mean2 * (_sumY [num  ] - _sumY [index+1]);

	return (mean2 - mean1) / sqrt((ss1 + ss2) / (double) (num - 3));
}

double HurstBreak::getDifference(int index, int window, int num)
{
	if (window == -1)
		return getDifference(index, num);

	int lWindow = (index - window >=  0 ) ? index - window :  0     ;
	int rWindow = (index + window <  num) ? index + window : num - 1;
	
	double mean1 = (_sumY [index    ] - _sumY[lWindow])
		/ (double) (index   - lWindow);
	double mean2 = (_sumY [rWindow+1] - _sumY[index+1])
		/ (double) (rWindow - index  );
	
	double ss1 = (_sumYY[index    ] - _sumYY[lWindow])
	   - mean1 * (_sumY [index    ] - _sumY [lWindow]);
	double ss2 = (_sumYY[rWindow+1] - _sumYY[index+1])
	   - mean2 * (_sumY [rWindow+1] - _sumY [index+1]);
	
	return (mean2 - mean1)
		/ sqrt((ss1 + ss2) / (double) (rWindow - lWindow - 2));
}
