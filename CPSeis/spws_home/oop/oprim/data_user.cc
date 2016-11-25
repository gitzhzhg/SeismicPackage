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
#include "oprim/data_user.hh"
#include "oprim/ll_base_data.hh"

#include <assert.h>

DataUser::DataUser()
{
	_baseDatas = new BaseDataLinkedList();
}

DataUser::~DataUser()
{
	void *p;

	for (BaseData *ptr = _baseDatas->top(&p);
		ptr != NULL;
		ptr = _baseDatas->next(&p))
	{
		ptr->removeUser(this);
	}

	delete _baseDatas;
}

void DataUser::dataDeleted(BaseData *baseData)
{
	_baseDatas->remove(baseData);
}

void DataUser::addData(BaseData *baseData)
{
	_baseDatas->add(baseData);

	baseData->addUser(this);
}

void DataUser::removeData(BaseData *baseData)
{
	_baseDatas->remove(baseData);

	baseData->removeUser(this);
}

void DataUser::verifyData(BaseData *baseData)
{
	void *p;

	for (BaseData *ptr = _baseDatas->top(&p);
		ptr != NULL;
		ptr = _baseDatas->next(&p))
	{
		if (ptr == baseData)
			return;
	}

	assert(0);	// Did not find it.
}
