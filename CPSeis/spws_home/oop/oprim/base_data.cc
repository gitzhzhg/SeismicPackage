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
#include "oprim/base_data.hh"
#include "oprim/data_user.hh"
#include "oprim/ll_data_user.hh"

#include <assert.h>

BaseData::BaseData()
{
	_dataUsers = new DataUserLinkedList();
}

BaseData::~BaseData()
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->dataDeleted(this);
	}

	delete _dataUsers;
}

void BaseData::addUser(DataUser *dataUser)
{
	_dataUsers->add(dataUser);
}

void BaseData::removeUser(DataUser *dataUser)
{
	_dataUsers->remove(dataUser);
}

void BaseData::modIndicesBefore(int startIndex, int numIndices, long id)
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->modIndicesBefore(this, startIndex, numIndices, id);
	}
}

void BaseData::modIndicesAfter(int startIndex, int numIndices, long id)
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->modIndicesAfter(this, startIndex, numIndices, id);
	}
}

void BaseData::modDone(long id)
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->modDone(this, id);
	}
}

void BaseData::selectBefore(long id)
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->selectBefore(this, id);
	}
}

void BaseData::selectAfter(long id)
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->selectAfter(this, id);
	}
}

void BaseData::modAttributes(int startIndex, int numIndices,
	int ignoreHold, long id)
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->modAttributes(this, startIndex, numIndices,
			ignoreHold, id);
	}
}

void BaseData::modAttributesByIndices(int *indices, int numIndices,
	int ignoreHold, long id)
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->modAttributesByIndices(this, indices, numIndices,
			ignoreHold, id);
	}
}

void BaseData::modAttributesNeedingRepair(int startIndex, int numIndices,
	long id)
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->modAttributesNeedingRepair(this, startIndex, numIndices,
			id);
	}
}

void BaseData::modAddedPnts(int startIndex, int numIndices, int ignoreHold,
	long id)
{
	void *p;

	for (DataUser *ptr = _dataUsers->top(&p);
		ptr;
		ptr = _dataUsers->next(&p))
	{
		ptr->modAddedPnts(this, startIndex, numIndices, ignoreHold, id);
	}
}
