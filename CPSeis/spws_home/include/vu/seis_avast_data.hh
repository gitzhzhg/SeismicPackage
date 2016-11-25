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
#ifndef AVAST_DATA_HH
#define AVAST_DATA_HH

#include "oprim/base_data.hh"

#include <assert.h>

typedef struct AvastVect {
               long  id;
               int   numPts;
               float *x;
               float *y;
               } AvastVectStruct;

class AvastData : public BaseData
{
	public:

                enum { defaultId = 0};

                AvastData(long maxIds);
                void insert(int numPts, float *x, float *y, long id = defaultId);
		virtual ~AvastData();
		virtual int getNumPts(long id = defaultId);
		virtual float getX(int i, long id = defaultId);
		virtual float getY(int i, long id = defaultId);

	private:

                long _maxIds;
                long _curId;
                AvastVectStruct *_vData;

		AvastData()
			{ /* private, no access to default constructor */ }
		AvastData(AvastData &)
			{ /* private, no access to copy constructor */ }
		AvastData& operator=(AvastData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* AVAST_DATA_HH */
