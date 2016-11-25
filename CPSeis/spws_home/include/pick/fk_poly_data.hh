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
#ifndef _FK_POLY_DATA_HH
#define _FK_POLY_DATA_HH

#include "pick/fk_data.hh"

#include <stdio.h>
#include <assert.h>


class FkPolyData : public FkData
{
	public:

		FkPolyData(class FkDataLinkedList *list,
			float headerValue1, float headerValue2,
			int numPts, float *wn, float *freq,
			CutPass cutPass, long id = defaultId);
		FkPolyData(class FkDataLinkedList *list,
			float headerValue1, float headerValue2,
			FkPolyData *data, long id = defaultId);
		virtual ~FkPolyData();
		virtual int getNumPts(long id = defaultId)
		  { assert(id == _id);  return (_numPts) ? _numPts + 1 : 0; }
		virtual float getX(int i, long id = defaultId)
		  { assert(id == _id);  return _wn  [(i == _numPts) ? 0 : i]; }
		virtual float getY(int i, long id = defaultId)
		  { assert(id == _id);  return _freq[(i == _numPts) ? 0 : i]; }
		void insert (int index, float x, float y);
		void remove (int index                  );
		void replace(int index, float x, float y);
		virtual FkType getFkType()
		  { return Polygon; }
		virtual int writeRecord(FILE *stream);

	private:

		int _numPts;
		float *_wn, *_freq;

		FkPolyData()
			: FkData((class FkDataLinkedList *) NULL,
			  0.0F, 0.0F, (CutPass) NULL, defaultId)
			{ /* private, no access to default constructor */ }
		FkPolyData(FkPolyData &)
			: FkData((class FkDataLinkedList *) NULL,
			  0.0F, 0.0F, (CutPass) NULL, defaultId)
			{ /* private, no access to copy constructor */ }
		FkPolyData& operator=(FkPolyData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FK_POLY_DATA_HH */
