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
#ifndef _FK_DATA_HH
#define _FK_DATA_HH

#include "oprim/base_data.hh"

#include <stdio.h>

enum FkType
{
	Fan    ,
	Polygon
};

enum CutPass
{
	Unknown,
	Cut    ,
	Pass   ,
	Error
};

class FkData : public BaseData
{
	public:

		FkData(class FkDataLinkedList *list,
			float headerValue1, float headerValue2,
			CutPass cutPass, long id = defaultId)
			{
				_list         = list        ;
				_headerValue1 = headerValue1;
				_headerValue2 = headerValue2;
				_cutPass      = cutPass     ;
				_id           = id          ;
			}
		virtual ~FkData()
			{ /* do nothing */ }
		void getHeaderValues(float *headerValue1, float *headerValue2)
			{
				*headerValue1 = _headerValue1;
				*headerValue2 = _headerValue2;
			}
		void preSetParams()
			{
				modIndicesBefore(0, getNumPts(_id), _id);
			}
		void postSetParams()
			{
				modIndicesAfter(0, getNumPts(_id), _id);
				modDone(_id);
			}
		virtual FkType getFkType() = 0;
		CutPass getCutPass()
			{ return _cutPass; }
		void    setCutPass(CutPass cutPass)
			{ _cutPass = cutPass; }
		virtual int writeRecord(FILE *stream) = 0;

	protected:

		class FkDataLinkedList *_list;
		float _headerValue1, _headerValue2;
		CutPass _cutPass;
		long _id;

	private:

		FkData()
			{ /* private, no access to default constructor */ }
		FkData(FkData &)
			{ /* private, no access to copy constructor */ }
		FkData& operator=(FkData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FK_DATA_HH */
