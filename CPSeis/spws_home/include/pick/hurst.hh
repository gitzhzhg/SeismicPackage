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
#ifndef _HURST_HH
#define _HURST_HH

void hurst_func(long first_trace, long last_trace, long num_traces,
	float tmincur, float tmaxcur, float picks[], float missing_pick,
	float zero_pick, unsigned char traces[], long nsamp, float tmin,
	float dt, float minSigDiff, float *minPicks, float *maxPicks);

class Hurst
{
	public:

		Hurst(int maxNumPts, int windowSize, int numSubWindowSizes);
		~Hurst();
		void loadData(int numPts, double *y);
		double getH(int index);

	private:

		int _maxNumPts, _windowSize, _numSubWindowSizes;
		int *_subWindowSize, *_numSubWindows;
		double *_logSubWindowSize, *_constTauValue,
			*_sqrtSubWindowSizeMinus1;
		double _sumX, _sumXX;
		double *_y;
		double *_cum1, *_cum2;
		double **_tauPart;

		Hurst()
			{ /* private, no access to default constructor */ }
		Hurst(Hurst &)
			{ /* private, no access to copy constructor */ }
		Hurst& operator=(Hurst &p)
			{ /* private, no access to = */ return p; }
};

class HurstBreak
{
	public:

		HurstBreak(int maxNumDataPts, int hurstWindow,
			int numSubWindowSizes, int slopeWindow, int diffWindow,
			int numPad, unsigned char pad);
		~HurstBreak();
		int getPick(int numPts, unsigned char *y, double *sigDiff);
		int getLeft ();
		int getRight();

	private:

		Hurst *_hurst;
		int _maxNumDataPts, _numPad;
		int _hurstWindow, _slopeWindow, _halfSlopeWindow, _diffWindow;
		double *_y, *_he, *_sumX, *_sumY, *_sumXY, *_sumXX, *_sumYY;

		void loadSums(int numPts);
		double getSlope(int index);
		double getDifference(int index,             int numPts);
		double getDifference(int index, int window, int numPts);

		HurstBreak()
			{ /* private, no access to default constructor */ }
		HurstBreak(HurstBreak &)
			{ /* private, no access to copy constructor */ }
		HurstBreak& operator=(HurstBreak &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _HURST_HH */
