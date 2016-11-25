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
#include "pick/ll_fk_data.hh"
#include "pick/fk_fan_data.hh"
#include "pick/fk_poly_data.hh"
#include "oprim/history.hh"

#include "cprim.h"

#include <stdio.h>
#include <assert.h>

FkDataLinkedList::FkDataLinkedList()
	: _whichHeadersSet(0), _paramsSet(0)
{
	/* just initializers */
}

FkDataLinkedList::~FkDataLinkedList()
{
	clear();
}

FkData *FkDataLinkedList::top(float hw1, float hw2, void **p)
{
	FkData *retval;
	float hv1, hv2;

	for (retval = top(p); retval; retval = next(p))
	{
		retval->getHeaderValues(&hv1, &hv2);
		if (hw1 == hv1 && hw2 == hv2)
			break;
	}

	return retval;
}

FkData *FkDataLinkedList::next(float hw1, float hw2, void **p)
{
	FkData *retval = next(p);

	if (retval)
	{
		float hv1, hv2;
		retval->getHeaderValues(&hv1, &hv2);

		if (hw1 != hv1 || hw2 != hv2)
		{
			retval = (FkData *) NULL;
			*p = (void *) NULL;
		}
	}

	return retval;
}

void FkDataLinkedList::addSorted(FkData *data)
{
	float ihv1, ihv2, hv1, hv2;
	data->getHeaderValues(&ihv1, &ihv2);

	FkType fkType = data->getFkType();

	FkData *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
	{
		ptr->getHeaderValues(&hv1, &hv2);

		if (hv1 == ihv1)
		{
			if (hv2 == ihv2)
			{
				if(fkType == Fan && ptr->getFkType() == Polygon)
					break;
			}
			else if (hv2 > ihv2)
			{
				break;
			}
		}
		else if (hv1 > ihv1)
		{
			break;
		}
	}

	if (ptr)
		addBeforeCurrent(data, &p);
	else
		add(data);
}

int FkDataLinkedList::readFile(const char *filename, char *info)
{
	assert(count() == 0);

	strcpy(info, " ");

	FILE *stream = fopen(filename, "r");

	if (!stream)
	{
		strcpy(info, "Can not open file");
		return -1;
	}

	int retval, needMore, records;
	char buff[BUFF_SIZE];
	char hw1P[10], hw2P[10];

	for (retval = needMore = records = 0;
		retval == 0 && fgets(buff, BUFF_SIZE, stream);
		records++)
	{
		if (buff[strlen(buff) - 1] != '\n')	/* record too long */
		{
			retval = -1;
			break;
		}

		switch (records)
		{
			case 0:
				assert(!_whichHeadersSet);
				if (cprim_getI(buff,
					"Header word 1", &_whichHeader1)
				 || cprim_getI(buff,
					"Header word 2", &_whichHeader2))
				{
					retval = -1;
				}
				_whichHeadersSet = 1;
				break;
			case 1:
				if (cprim_getI(buff, "Min waveno."    , &_minWN)
				 || cprim_getI(buff, "Max waveno."    , &_maxWN)
				 || cprim_getI(buff, "Nyquist waveno.",
					&_nyquistWN))
				{
					retval = -1;
				}
				break;
			case 2:
				if (cprim_getF(buff, "Min freq.", &_minFreq)
				 || cprim_getF(buff, "Max freq.", &_maxFreq)
				 || cprim_getF(buff, "Hz/sec."  , &_freqPerSec))
				{
					retval = -1;
				}
				_paramsSet = 1;
				break;
			case 3:
				sprintf(hw1P, "Header %d", _whichHeader1);
				sprintf(hw2P, "Header %d", _whichHeader2);
				if (cprim_getF(buff, hw1P, &_currentHW1)
				 || cprim_getF(buff, hw2P, &_currentHW2))
				{
					retval = -1;
				}
				else
				{
					_cutPass = Unknown;
				}
				break;
			default:
				if (needMore)
				{
					retval = getMorePoly(buff, &needMore);
				}
				else if (!strncmp(buff, hw1P, strlen(hw1P)))
				{
					retval = cprim_getF(buff, hw1P,
						&_currentHW1)
	/* bitwise if, not logical if */       | cprim_getF(buff, hw2P,
						&_currentHW2);
					_cutPass = Unknown;
				}
				else if (!strncmp(buff, "FAN "    , (size_t) 4))
				{
					retval = getFan(buff);
				}
				else if (!strncmp(buff, "POLYGON ", (size_t) 8))
				{
					retval = getPoly(buff, &needMore);
				}
				else
				{
					retval = -1;
				}
				break;
		}
	}

	if (retval == -1)
	{
		strcpy(info, "Bad values in file");
		fclose(stream);
		return -1;
	}
	else if (!feof(stream))
	{
		strcpy(info, "Error reading file");
		fclose(stream);
		return -1;
	}
	else if (needMore || records < 3)
	{
		strcpy(info, "Incomplete file");
		fclose(stream);
		return -1;
	}

	if (EOF == fclose(stream))
	{
		strcpy(info, "Error closing file");
		return -1;
	}

	return 0;
}

int FkDataLinkedList::getFan(char *buff)
{
	char *str = strtok(buff, " ");
	assert(!strcmp(str, "FAN"));

	int num;
	if (cprim_nextI((char *) NULL, &num))
		return -1;

	if (num != NUM_FAN_LINES)
		return -1;

	static float cut [] = { 1.0F, 0.0F, 0.0F, 1.0F };
	assert(sizeof(cut ) / sizeof(cut [0]) == NUM_FAN_LINES);
	static float pass[] = { 0.0F, 1.0F, 1.0F, 0.0F };
	assert(sizeof(pass) / sizeof(pass[0]) == NUM_FAN_LINES);
	float dip[NUM_FAN_LINES], weight[NUM_FAN_LINES];
	int cuts, passes, i;

	for (cuts = passes = i = 0; i < NUM_FAN_LINES; i++)
	{
		if (cprim_nextF((char *) NULL, &dip   [i]))
			return -1;

		if (cprim_nextF((char *) NULL, &weight[i]))
			return -1;

		if      (weight[i] == cut [i])
			cuts++;
		else if (weight[i] == pass[i])
			passes++;
		else
			return -1;

		if (i > 0 && dip[i] < dip[i - 1])
			return -1;
	}

	CutPass cutPass;
	if      (4 == cuts  )
	{
		assert(0 == passes);
		cutPass = Cut ;
	}
	else if (4 == passes)
	{
		assert(0 == cuts  );
		cutPass = Pass;
	}
	else
	{
		return -1;
	}

	if (_cutPass == Unknown)
	{
		_cutPass = cutPass;
	}
	else
	{
		if (_cutPass != cutPass)
		{
			printf("CUT/PASS state changes for HWs:  %f %f\n",
				_currentHW1, _currentHW2);
			return -1;
		}
	}

	FkFanData *theData = new FkFanData(this, _currentHW1, _currentHW2, dip,
		_cutPass);

	addSorted(theData);

	return 0;
}

int FkDataLinkedList::getPoly(char *buff, int *needMore)
{
	char *str = strtok(buff, " ");
	assert(!strcmp(str, "POLYGON"));

	str = strtok((char *) NULL, " ");
	CutPass cutPass;
	if      (!strcmp(str, "CUT" ))
		cutPass = Cut ;
	else if (!strcmp(str, "PASS"))
		cutPass = Pass;
	else
		return -1;

	if (_cutPass == Unknown)
	{
		_cutPass = cutPass;
	}
	else
	{
		if (_cutPass != cutPass)
		{
			printf("CUT/PASS state changes for HWs:  %f %f\n",
				_currentHW1, _currentHW2);
			return -1;
		}
	}

	if (cprim_nextI((char *) NULL, &_numPolyPts))
		return -1;

	_polyPtsCnt = 0;
	_polyWn   = new float[_numPolyPts];
	_polyFreq = new float[_numPolyPts];

	return getMorePoly((char *) NULL, needMore);
}

int FkDataLinkedList::getMorePoly(char *buff, int *needMore)
{
	int nextRetval;
	for (; _polyPtsCnt < _numPolyPts; _polyPtsCnt++)
	{
		nextRetval = cprim_nextF(buff, &_polyWn[_polyPtsCnt]);

		if (buff)
			buff = (char *) NULL;

		if      (nextRetval == -1)
		{
			return -1;
		}
		else if (nextRetval == -2)
		{
			*needMore = 1;
			return 0;
		}

		if (cprim_nextF((char *) NULL, &_polyFreq[_polyPtsCnt]))
			return -1;
	}

	FkPolyData *theData = new FkPolyData(this, _currentHW1, _currentHW2,
		_numPolyPts, _polyWn, _polyFreq, _cutPass);

	addSorted(theData);

	delete [] _polyWn  ;
	delete [] _polyFreq;

	*needMore = 0;
	return 0;
}

int FkDataLinkedList::writeFile(const char *filename, char *info)
{
	strcpy(info, " ");

	FILE *stream = fopen(filename, "w");

	if (!stream)
	{
		strcpy(info, "Can not open file");
		return -1;
	}

	if (EOF == fprintf(stream, "Header word 1 = %d   Header word 2 = %d\n",
		_whichHeader1, _whichHeader2))
	{
		strcpy(info, "Error writing file");
		fclose(stream);
		return -1;
	}
		
	if (EOF == fprintf(stream,
		"Min waveno. = %d   Max waveno. = %d   Nyquist waveno. =  %d\n",
		_minWN, _maxWN, _nyquistWN))
	{
		strcpy(info, "Error writing file");
		fclose(stream);
		return -1;
	}
		
	if (EOF == fprintf(stream,
		"Min freq. = %f   Max freq. = %f   Hz/sec. =  %f\n",
		_minFreq, _maxFreq, _freqPerSec))
	{
		strcpy(info, "Error writing file");
		fclose(stream);
		return -1;
	}

	FkData *ptr;
	void *p;
	float hv1, hv2, newHv1, newHv2;
	for (ptr = top(&p); ptr; ptr = next(&p))
	{
		if (ptr == top())
		{
			ptr->getHeaderValues(&hv1, &hv2);

			if (EOF == fprintf(stream,
				"Header %d = %f, Header %d = %f\n",
				_whichHeader1, hv1, _whichHeader2, hv2))
			{
				strcpy(info, "Error writing file");
				fclose(stream);
				return -1;
			}
		}
		else
		{
			ptr->getHeaderValues(&newHv1, &newHv2);

			if (newHv1 != hv1 || newHv2 != hv2)
			{
				hv1 = newHv1;
				hv2 = newHv2;

				if (EOF == fprintf(stream,
					"Header %d = %f, Header %d = %f\n",
					_whichHeader1, hv1, _whichHeader2, hv2))
				{
					strcpy(info, "Error writing file");
					fclose(stream);
					return -1;
				}
			}
		}

		if (-1 == ptr->writeRecord(stream))
		{
			strcpy(info, "Error writing file");
			fclose(stream);
			return -1;
		}
	}

	if (EOF == fclose(stream))
	{
		strcpy(info, "Error closing file");
		return -1;
	}

	return 0;
}

void FkDataLinkedList::clear()
{
	FkData *ptr;
	FkData *nextPtr = (FkData *) NULL;
	void *p;
	for (ptr = top(&p); ptr; ptr = nextPtr)
	{
		nextPtr = next(&p);
		remove(ptr);
		delete ptr;
	} 

	_whichHeadersSet = _paramsSet = 0;
}

void FkDataLinkedList::setWhichHeaders(int whichHeader1, int whichHeader2)
{
	/*
	 * Only one shot at this.
	 */
	assert(!_whichHeadersSet);

	_whichHeader1 = whichHeader1;
	_whichHeader2 = whichHeader2;

	_whichHeadersSet = 1;
}

void FkDataLinkedList::getWhichHeaders(int *whichHeader1, int *whichHeader2)
{
	assert(_whichHeadersSet);

	*whichHeader1 = _whichHeader1;
	*whichHeader2 = _whichHeader2;
}

void FkDataLinkedList::setParams(int minWN, int maxWN, int nyquistWN,
	float minFreq, float maxFreq, float freqPerSec)
{
	FkData *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
		ptr->preSetParams();

	_minWN      = minWN     ;
	_maxWN      = maxWN     ;
	_nyquistWN  = nyquistWN ;
	_minFreq    = minFreq   ;
	_maxFreq    = maxFreq   ;
	_freqPerSec = freqPerSec;

	assert(_minWN == -_maxWN);
	assert(_nyquistWN == _maxWN);
	assert(_minFreq == 0.0);
	assert(_maxFreq > 0.0);
	assert(_freqPerSec > 0.0);

	for (ptr = top(&p); ptr; ptr = next(&p))
		ptr->postSetParams();

	_paramsSet = 1;
}

void FkDataLinkedList::getParams(int *minWN, int *maxWN, int *nyquistWN,
	float *minFreq, float *maxFreq, float *freqPerSec)
{
	assert(_paramsSet);

	*minWN      = _minWN     ;
	*maxWN      = _maxWN     ;
	*nyquistWN  = _nyquistWN ;
	*minFreq    = _minFreq   ;
	*maxFreq    = _maxFreq   ;
	*freqPerSec = _freqPerSec;
}

int FkDataLinkedList::getGlobalParams(char *file,
	int *minWN, int *maxWN, int *nyquistWN,
	float *minFreq, float *maxFreq, float *freqPerSec,
	char **info)
{
	History *history = new History(file, "FKTR");
	if (-1 == history->constructedOK(info))
	{
		delete history;
		return -1;
	}

	int nxft;
	if (-1 == history->getInt("NXFT", &nxft))
	{
		static char *error = "NXFT not in history";
		*info = error;
		delete history;
		return -1;
	}

	int i;
	for (int power = i = 1; i < (int) sizeof(int) * 8; power *= 2, i++)
		if (nxft == power)
			break;

	if (i == (int) sizeof(int) * 8)
	{
		static char *error = "NXFT not a power of 2";
		*info = error;
		delete history;
		return -1;
	}

	             *minWN = -nxft / 2;
	*nyquistWN = *maxWN =  nxft / 2;

	*minFreq = 0.0F;

	if (-1 == history->getFloat("FMAX", maxFreq))
	{
		static char *error = "FMAX not in history";
		*info = error;
		delete history;
		return -1;
	}

	if (-1 == history->getFloat("HZPS", freqPerSec))
	{
		static char *error = "HZPS not in history";
		*info = error;
		delete history;
		return -1;
	}

	delete history;

	return 0;
}

int FkDataLinkedList::initialized()
{
	return _whichHeadersSet && _paramsSet;
}

CutPass FkDataLinkedList::getCutPass(float hw1, float hw2)
{
	CutPass retval = Unknown;

	FkData *ptr;
	void *p;
	for (ptr = top(hw1, hw2, &p); ptr; ptr = next(hw1, hw2, &p))
	{
		if (retval == Unknown)
		{
			retval = ptr->getCutPass();
		}
		else
		{
			if (retval != ptr->getCutPass())
			{
				retval = Error;
				break;
			}
		}
	}

	return retval;
}

void FkDataLinkedList::setCutPass(float hw1, float hw2, CutPass cutPass)
{
	FkData *ptr;
	void *p;
	for (ptr = top(hw1, hw2, &p); ptr; ptr = next(hw1, hw2, &p))
		ptr->setCutPass(cutPass);
}
