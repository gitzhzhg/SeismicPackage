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
#ifndef _FK_FAN_DATA_HH
#define _FK_FAN_DATA_HH

#include "pick/fk_data.hh"

#include <stdio.h>
#include <assert.h>

#define NUM_FAN_LINES 4

class FkFanData : public FkData
{
	public:

		FkFanData(class FkDataLinkedList *list,
			float headerValue1, float headerValue2,
			float dip[NUM_FAN_LINES],
			CutPass cutPass, long id = defaultId);
		FkFanData(class FkDataLinkedList *list,
			float headerValue1, float headerValue2,
			float wn[NUM_FAN_LINES], float freq[NUM_FAN_LINES],
			CutPass cutPass, long id = defaultId);
		FkFanData(class FkDataLinkedList *list,
			float headerValue1, float headerValue2,
			FkFanData *data, long id = defaultId);
		virtual ~FkFanData()
			{ /* do nothing */ }
		virtual int getNumPts(long id = defaultId)
			{ assert(id == _id);  return 7; }
		virtual float getX(int i, long id = defaultId);
		virtual float getY(int i, long id = defaultId);
		void replace(int index, float x, float y);
		virtual FkType getFkType()
			{ return Fan; }
		virtual int writeRecord(FILE *stream);

	private:

		float _slope[NUM_FAN_LINES];

		static int compar(const void *element1, const void *element2);

		FkFanData() : FkData((class FkDataLinkedList *) NULL,
			0.0F, 0.0F, (CutPass) NULL, defaultId)
			{ /* private, no access to default constructor */ }
		FkFanData(FkFanData &) : FkData((class FkDataLinkedList *) NULL,
			0.0F, 0.0F, (CutPass) NULL, defaultId)
			{ /* private, no access to copy constructor */ }
		FkFanData& operator=(FkFanData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FK_FAN_DATA_HH */
