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
#include <string.h>
#include "pick/fk_fan_data.hh"
#include "pick/ll_fk_data.hh"

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>

FkFanData::FkFanData(FkDataLinkedList *list, float headerValue1,
	float headerValue2, float dip[NUM_FAN_LINES], CutPass cutPass, long id)
	: FkData(list, headerValue1, headerValue2, cutPass, id)
{
	int minWN, maxWN, nyquistWN;
	float minFreq, maxFreq, freqPerSec;
	_list->getParams(&minWN, &maxWN, &nyquistWN,
		&minFreq, &maxFreq, &freqPerSec);

	for (int i = 0; i < NUM_FAN_LINES; i++)
	{
		_slope[i] = dip[i] * (float) nyquistWN * 2.0F;

		if (i != 0)
			assert(_slope[i] >= _slope[i - 1]);
	}
}

FkFanData::FkFanData(FkDataLinkedList *list, float headerValue1,
	float headerValue2, float wn[NUM_FAN_LINES], float freq[NUM_FAN_LINES],
	CutPass cutPass, long id)
	: FkData(list, headerValue1, headerValue2, cutPass, id)
{
	int minWN, maxWN, nyquistWN;
	float minFreq, maxFreq, freqPerSec;
	_list->getParams(&minWN, &maxWN, &nyquistWN,
		&minFreq, &maxFreq, &freqPerSec);

	for (int i = 0; i < NUM_FAN_LINES; i++)
	{
		assert(wn  [i] >= (float)minWN   && wn  [i] <= (float)maxWN  );
		assert(freq[i] >=        minFreq && freq[i] <=        maxFreq);

		if (freq[i] != 0.0F)
			_slope[i] = wn[i] / freq[i];
		else if (wn[i] == 0.0F)
			_slope[i] = 0.0F;
		else if (wn[i] > 0.0F)
			_slope[i] = (float) LONG_MAX;
		else
			_slope[i] = (float) LONG_MIN;
	}
			
	qsort((void *) _slope, (size_t) NUM_FAN_LINES, sizeof(float), compar);
}

int FkFanData::compar(const void *element1, const void *element2)
{
	return (*((float *) element1)  < *((float *) element2)) ? -1 :
	      ((*((float *) element1) == *((float *) element2)) ?  0 : 1);
}

FkFanData::FkFanData(FkDataLinkedList *list, float headerValue1,
	float headerValue2, FkFanData *data, long id)
	: FkData(list, headerValue1, headerValue2, data->_cutPass, id)
{
	for (int i = 0; i < NUM_FAN_LINES; i++)
		_slope[i] = (data->_slope)[i];
}

float FkFanData::getX(int i, long id)
{
	float retval;

	assert(id == _id);
	assert(i >= 0 && i <= 6);

	int minWN, maxWN, nyquistWN;
	float minFreq, maxFreq, freqPerSec;
	_list->getParams(&minWN, &maxWN, &nyquistWN,
		&minFreq, &maxFreq, &freqPerSec);

	if (i % 2)	/* 1, 3, or 5 */
	{
		retval = 0.0F;
	}
	else		/* 0, 2, 4, or 6 */
	{
		int j = i / 2;

		if (_slope[j] < 0.0F)
			retval = ((float) minWN / _slope[j] > maxFreq) ?
				_slope[j] * maxFreq : (float) minWN;
		else if (_slope[j] == 0.0F)
			retval = 0.0F;
		else
			retval = ((float) maxWN / _slope[j] > maxFreq) ?
				_slope[j] * maxFreq : (float) maxWN;
	}

	return retval;
}

float FkFanData::getY(int i, long id)
{
	float retval;

	assert(id == _id);
	assert(i >= 0 && i <= 6);

	int minWN, maxWN, nyquistWN;
	float minFreq, maxFreq, freqPerSec;
	_list->getParams(&minWN, &maxWN, &nyquistWN,
		&minFreq, &maxFreq, &freqPerSec);

	if (i % 2)	/* 1, 3, or 5 */
	{
		retval = 0.0F;
	}
	else		/* 0, 2, 4, or 6 */
	{
		int j = i / 2;

		if (_slope[j] < 0.0F)
			retval = ((float) minWN / _slope[j] > maxFreq) ?
				maxFreq : (float) minWN / _slope[j];
		else if (_slope[j] == 0.0F)
			retval = maxFreq;
		else
			retval = ((float) maxWN / _slope[j] > maxFreq) ?
				maxFreq : (float) maxWN / _slope[j];
	}

	return retval;
}

void FkFanData::replace(int index, float x, float y)
{
	assert(index >= 0 && index <= 6 && index % 2 == 0);

	int minWN, maxWN, nyquistWN;
	float minFreq, maxFreq, freqPerSec;
	_list->getParams(&minWN, &maxWN, &nyquistWN,
		&minFreq, &maxFreq, &freqPerSec);

	assert(x >= minWN   && x <= maxWN  );
	assert(y >= minFreq && y <= maxFreq);

	float newSlope;

	if (y != 0.0F)
		newSlope = x / y;
	else if (x == 0.0F)
		newSlope = 0.0F;
	else if (x > 0.0F)
		newSlope = (float) LONG_MAX;
	else
		newSlope = (float) LONG_MIN;

	index /= 2;
	int num, modIndex, i;

	/*
	 * Decreasing slope.
	 */
	if (newSlope < _slope[index])
	{
		/*
		 * Since slope is decreasing, if more than one fan
		 * has this slope use the 1st.
		 */
		for (i = index - 1; i >= 0; i--)
			if (_slope[i] == _slope[index])
				index = i;
			else if (_slope[i] < _slope[index])
				break;
			else
				assert(0);	/* Supposed to be sorted. */

		/*
		 * If new slope is smaller than other slopes which
		 * are less than the edited slope initial value,
		 * set them to new slope too.
		 */
		for (i = index - 1, num = 1, modIndex = index; i >= 0; i--)
			if (_slope[i] > newSlope)
			{
				num++;
				modIndex--;
			}
			else
			{
				break;
			}
	}
	/*
	 * Increasing slope.
	 */
	else if (newSlope > _slope[index])
	{
		int  i;

		/*
		 * Since slope is increasing, if more than one fan
		 * has this slope use the last.
		 */
		for (i = index + 1; i < NUM_FAN_LINES; i++)
			if (_slope[i] == _slope[index])
				index = i;
			else if (_slope[i] > _slope[index])
				break;
			else
				assert(0);	/* Supposed to be sorted. */

		/*
		 * If new slope is larger than other slopes which
		 * are greater than the edited slope initial value,
		 * set them to new slope too.
		 */
		for (i = index + 1, num = 1, modIndex = index;
			i < NUM_FAN_LINES;
			i++)
		{
			if (_slope[i] < newSlope)
			{
				num++;
			}
			else
			{
				break;
			}
		}
	}

	/*
	 * Need to convert modIndex and num from slope 0 - 3 to
	 * graphical fan point 0 - 6.
	 */
	modIndicesBefore(2 * modIndex, 2 * num - 1, _id);

	for (i = 0; i < num; i++)
		_slope[modIndex + i] = newSlope;

	modIndicesAfter(2 * modIndex, 2 * num - 1, _id);
	modDone(_id);
}

int FkFanData::writeRecord(FILE *stream)
{
	if (EOF == fprintf(stream, "FAN   4"))
		return -1;

	static int cut [] = { 1, 0, 0, 1 };
	assert(sizeof(cut ) / sizeof(cut [0]) == NUM_FAN_LINES);
	static int pass[] = { 0, 1, 1, 0 };
	assert(sizeof(pass) / sizeof(pass[0]) == NUM_FAN_LINES);
	int *weight;

	switch (_cutPass)
	{
		case Cut :  weight = cut ;  break;
		case Pass:  weight = pass;  break;
		default  :  assert(0);
	}

	int minWN, maxWN, nyquistWN;
	float minFreq, maxFreq, freqPerSec;
	_list->getParams(&minWN, &maxWN, &nyquistWN,
		&minFreq, &maxFreq, &freqPerSec);

	for (int i = 0; i < NUM_FAN_LINES; i++)
		if (EOF == fprintf(stream, "   %f, %d",
			_slope[i] / (float) nyquistWN / 2.0F, weight[i]))
		{
			return -1;
		}

	if (EOF == fprintf(stream, "\n"))
		return -1;

	return 0;
}
