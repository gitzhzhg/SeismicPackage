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
#ifndef _LL_TRANS_3D_TO_2D_HH
#define _LL_TRANS_3D_TO_2D_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"

#include <iostream.h>
#include <locale.h>
#include <assert.h>

class Trans3Dto2DElement : public Element
{
	friend class Trans3Dto2DLinkedList;

	private:

		class Trans3Dto2D *_trans3Dto2D;

		Trans3Dto2DElement(class BaseData *data,
			float degreesZ, float degreesY,
			float xOffset , float xFactor ,
			float yOffset , float yFactor ,
			float zOffset , float zFactor ,
			float xExp    , float yExp    , float zExp);
		Trans3Dto2DElement()
			{ assert(0); }
		~Trans3Dto2DElement();
		int operator ==(void * const trans3Dto2D) const
		  { return((class Trans3Dto2D *) trans3Dto2D == _trans3Dto2D); }
		void print() const
			{ cout << " " << _trans3Dto2D; }
};

class Trans3Dto2DLinkedList : public BaseLinkedList
{
	public:

		Trans3Dto2DLinkedList(float degreesZ, float degreesY,
			float xOffset, float xFactor,
			float yOffset, float yFactor,
			float zOffset, float zFactor,
			float xExp = 1.0, float yExp = 1.0, float zExp = 1.0);
		~Trans3Dto2DLinkedList()
			{ /* do nothing */ }

		class Trans3Dto2D *add(class BaseData *data);
		void remove(class Trans3Dto2D *trans3Dto2D);
		class Trans3Dto2D *find(class Trans3Dto2D *trans3Dto2D,
			void **p = NULL);
		class Trans3Dto2D *top    (void **p = NULL);
		class Trans3Dto2D *bottom (void **p = NULL);
		class Trans3Dto2D *next   (void **p = NULL);
		class Trans3Dto2D *prev   (void **p = NULL);
		class Trans3Dto2D *current(void **p = NULL);

		void setAngles(float  degreesZ, float  degreesY, int doMod = 0);
		void getAngles(float *degreesZ, float *degreesY)
			{ *degreesZ = _degreesZ;  *degreesY = _degreesY; }
		void setScale(float xOffset, float xFactor, float yOffset,
			float yFactor, float zOffset, float zFactor,
			int doMod = 0);
		void getScale(float *xOffset, float *xFactor, float *yOffset,
			float *yFactor, float *zOffset, float *zFactor)
		{
			*xOffset = _xOffset;  *xFactor = _xFactor;
			*yOffset = _yOffset;  *yFactor = _yFactor;
			*zOffset = _zOffset;  *zFactor = _zFactor;
		}
		void setExpansion(float xExp, float yExp, float zExp,
			int doMod = 0);
		void getExpansion(float *xExp, float *yExp, float *zExp)
			{ *xExp = _xExp;  *yExp = _yExp;  *zExp = _zExp; }

	private:

		float _degreesZ, _degreesY;
		float _xOffset, _xFactor, _yOffset, _yFactor,
			_zOffset, _zFactor;
		float _xExp, _yExp, _zExp;

		Trans3Dto2DLinkedList()
			{ /* private, no access to default constructor */ }
		Trans3Dto2DLinkedList(Trans3Dto2DLinkedList &)
			{ /* private, no access to copy constructor */ }
		Trans3Dto2DLinkedList& operator=(Trans3Dto2DLinkedList &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _LL_TRANS_3D_TO_2D_HH */
