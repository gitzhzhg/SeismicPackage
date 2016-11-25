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
#ifndef _LL_FK_DATA_HH
#define _LL_FK_DATA_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include "pick/fk_data.hh"

#include <assert.h>
#include <iostream.h>

class FkDataElement : public Element
{
	friend class FkDataLinkedList;

	private:

		class FkData *_data;

		FkDataElement()
			{ assert(0); }
		FkDataElement(class FkData *data)
			{ _data = data; }
		~FkDataElement()
			{ /* do nothing */ }
		int operator ==(void * const data) const
			{ return((class FkData *) data == _data); }
		void print() const
			{ cout << " " << _data; }
};

class FkDataLinkedList : public BaseLinkedList
{
	private:

		enum { BUFF_SIZE = 132 };

		int _whichHeadersSet, _paramsSet;
		int _whichHeader1, _whichHeader2;
		int _minWN, _maxWN, _nyquistWN;
		float _minFreq, _maxFreq, _freqPerSec;
		float _currentHW1, _currentHW2;
		CutPass _cutPass;
		float *_polyWn, *_polyFreq;
		int _numPolyPts, _polyPtsCnt;

		int getFan       (char *buff);
		int getPoly      (char *buff, int *needMore);
		int getMorePoly  (char *buff, int *needMore);

	public:

		FkDataLinkedList();
		~FkDataLinkedList();

		void add(class FkData *data)
		{
			FkDataElement *theElement = new FkDataElement(data);
			BaseLinkedList::add((Element *) theElement);
		}
		void addBeforeCurrent(class FkData *data, void **p)
		{
			FkDataElement *theElement = new FkDataElement(data);
			BaseLinkedList::addBeforeCurrent(
				(Element *) theElement, p);
		}
		void addAfterCurrent(class FkData *data, void **p)
		{
			FkDataElement *theElement = new FkDataElement(data);
			BaseLinkedList::addAfterCurrent(
				(Element *) theElement, p);
		}
		void remove(class FkData *data)
		{
			BaseLinkedList::remove((void *) data);
		}
		class FkData *find(class FkData *data)
		{
			FkDataElement *ptr = (FkDataElement *)
				BaseLinkedList::find((void *) data);
			return (ptr ? ptr->_data : (class FkData *) NULL);
		}
		class FkData *top    (void **p = (void **) NULL)
		{
			FkDataElement *ptr = (FkDataElement *)
				BaseLinkedList::top    (p);
			return (ptr ? ptr->_data : (class FkData *) NULL);
		}
		class FkData *bottom (void **p = (void **) NULL)
		{
			FkDataElement *ptr = (FkDataElement *)
				BaseLinkedList::bottom (p);
			return (ptr ? ptr->_data : (class FkData *) NULL);
		}
		class FkData *next   (void **p = (void **) NULL)
		{
			FkDataElement *ptr = (FkDataElement *)
				BaseLinkedList::next   (p);
			return (ptr ? ptr->_data : (class FkData *) NULL);
		}
		class FkData *prev   (void **p = (void **) NULL)
		{
			FkDataElement *ptr = (FkDataElement *)
				BaseLinkedList::prev   (p);
			return (ptr ? ptr->_data : (class FkData *) NULL);
		}
		class FkData *current(void **p = (void **) NULL)
		{
			FkDataElement *ptr = (FkDataElement *)
				BaseLinkedList::current(p);
			return (ptr ? ptr->_data : (class FkData *) NULL);
		}

		void addSorted(class FkData *data);
		class FkData *top (float hw1, float hw2, void **p);
		class FkData *next(float hw1, float hw2, void **p);
		int  readFile(const char *filename, char *info);
		int writeFile(const char *filename, char *info);
		void clear();
		void setWhichHeaders(int  whichHeader1, int  whichHeader2);
		void getWhichHeaders(int *whichHeader1, int *whichHeader2);
		void setParams(int  minWN, int  maxWN, int  nyquistWN,
			float  minFreq, float  maxFreq, float  freqPerSec);
		void getParams(int *minWN, int *maxWN, int *nyquistWN,
			float *minFreq, float *maxFreq, float *freqPerSec);
		static int getGlobalParams(char *file,
			int *minWN, int *maxWN, int *nyquistWN,
			float *minFreq, float *maxFreq, float *freqPerSec,
			char **info);
		int initialized();
		CutPass getCutPass(float hw1, float hw2);
		void    setCutPass(float hw1, float hw2, CutPass cutPass);
};

#endif	/* _LL_FK_DATA_HH */
