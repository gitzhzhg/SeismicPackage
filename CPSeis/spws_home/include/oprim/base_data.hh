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
#ifndef _BASE_DATA_HH
#define _BASE_DATA_HH

#include <assert.h>
#include <limits.h>

class BaseData
{
	public:

/*
                aborts compilation on platform 64sgi73:
		enum { defaultId = LONG_MAX / 2 };
*/
		enum { defaultId = INT_MAX / 2 };

		/*
		 * This list of virtual functions must be expanded to
		 * accommodate all the needs of all classes of DataUsers.
		 * Only used with derived classes.
		 */
		virtual int getNumPts(long /*id*/ = defaultId)
			{ assert(0);  return 0; }
		virtual float getX(int /*i*/, long /*id*/ = defaultId)
			{ assert(0);  return 0.0; }
		virtual float getY(int /*i*/, long /*id*/ = defaultId)
			{ assert(0);  return 0.0; }
		virtual float getZ(int /*i*/, long /*id*/ = defaultId)
			{ assert(0);  return 0.0; }
		virtual int getMarkerType(int /*i*/, long /*id*/ = defaultId)
			{ assert(0);  return 0; }
		virtual int getAltMarkerColor(int /*i*/,
			long /*id*/ = defaultId)
			{ assert(0);  return 0; }
		virtual int getAltLineColor  (int /*i*/,
			long /*id*/ = defaultId)
			{ assert(0);  return 0; }
		virtual int getXOffsetType(int /*i*/, long /*id*/ = defaultId)
			{ assert(0);  return 0; }
		virtual int getYOffsetType(int /*i*/, long /*id*/ = defaultId)
			{ assert(0);  return 0; }
		virtual void *getSelected(long /*id*/ = defaultId)
			{ assert(0);  return (void *) 0; }
		virtual int getLineStyle(             long /*id*/ = defaultId)
			{ assert(0);  return 0; }
		virtual int getPenLift(int /*i*/, long /*id*/ = defaultId)
			{ assert(0);  return 0; }
		virtual BaseData *actualData()
			{ return this; }
		virtual ~BaseData();

		void addUser   (class DataUser *dataUser);
		void removeUser(class DataUser *dataUser);

	protected:

		BaseData();

		/*
		 * These protected member functions call a member function
		 * of the same name on each of the DataUsers.
		 */
		void modIndicesBefore(int startIndex, int numIndices,
			long id = defaultId);
		void modIndicesAfter (int startIndex, int numIndices,
			long id = defaultId);
		void modDone(long id = defaultId);
		void selectBefore(long id = defaultId);
		void selectAfter (long id = defaultId);
		void modAttributes(int startIndex, int numIndices,
			int ignoreHold = 0, long id = defaultId);
		void modAttributesByIndices(int *indices, int numIndices,
			int ignoreHold = 0, long id = defaultId);
		void modAttributesNeedingRepair(int startIndex, int numIndices,
			long id = defaultId);
		void modAddedPnts(int startIndex, int numIndices,
			int ignoreHold = 0, long id = defaultId);

	private:

		class DataUserLinkedList *_dataUsers;

		BaseData(BaseData &)
			{ /* private, no access to copy constructor */ }
		BaseData& operator=(BaseData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _BASE_DATA_HH */
