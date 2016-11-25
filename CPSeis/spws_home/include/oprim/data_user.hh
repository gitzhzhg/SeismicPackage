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
#ifndef _DATA_USER_HH
#define _DATA_USER_HH

#include "oprim/base_data.hh"	/* only to get defaultId */

class DataUser
{
	public:

		/*
		 * These method are called by BaseData methods
		 * for editing data.
		 */
		virtual void modIndicesBefore(class BaseData * /*baseData*/,
			int /*startIndex*/, int /*numIndices*/,
			long /*id*/ = BaseData::defaultId)
			{ /* base class does nothing */ }
		virtual void modIndicesAfter (class BaseData * /*baseData*/,
			int /*startIndex*/, int /*numIndices*/,
			long /*id*/ = BaseData::defaultId)
			{ /* base class does nothing */ }
		virtual void modDone(class BaseData * /*baseData*/,
			long /*id*/ = BaseData::defaultId)
			{ /* base class does nothing */ }
		virtual void selectBefore(class BaseData * /*baseData*/,
			long /*id*/ = BaseData::defaultId)
			{ /* base class does nothing */ }
		virtual void selectAfter (class BaseData * /*baseData*/,
			long /*id*/ = BaseData::defaultId)
			{ /* base class does nothing */ }
		virtual void modAttributes(class BaseData * /*baseData*/,
			int /*startIndex*/, int /*numIndices*/,
			int /*ignoreHold*/ = 0,
			long /*id*/ = BaseData::defaultId)
			{ /* base class does nothing */ }
		virtual void modAttributesByIndices(
			class BaseData * /*baseData*/, int * /*indices*/,
			int /*numIndices*/, int /*ignoreHold*/ = 0,
			long /*id*/ = BaseData::defaultId)
			{ /* base class does nothing */ }
		virtual void modAttributesNeedingRepair(
			class BaseData * /*baseData*/, int /*startIndex*/,
			int /*numIndices*/, long /*id*/ = BaseData::defaultId)
			{ /* base class does nothing */ }
		virtual void modAddedPnts(class BaseData * /*baseData*/,
			int /*startIndex*/, int /*numIndices*/,
			int /*ignoreHold*/ = 0,
			long /*id*/ = BaseData::defaultId)
			{ /* base class does nothing */ }
		virtual ~DataUser();

		/*
		 * dataDeleted is called by BaseData destructor.
		 */
		virtual void dataDeleted(class BaseData *baseData);

	protected:

		class BaseDataLinkedList *_baseDatas;

		DataUser();

		void addData   (class BaseData *baseData);
		void removeData(class BaseData *baseData);
		void verifyData(class BaseData *baseData);

	private:

		DataUser(DataUser &)
			{ /* private, no access to copy constructor */ }
		DataUser& operator=(DataUser &p)
			{ /* private, no access to = */ return p; }
};

#endif
