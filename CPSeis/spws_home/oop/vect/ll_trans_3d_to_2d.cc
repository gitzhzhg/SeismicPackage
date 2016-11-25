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
#include "vect/ll_trans_3d_to_2d.hh"
#include "vect/trans_3d_to_2d.hh"

Trans3Dto2DElement::Trans3Dto2DElement(BaseData *data,
	float degreesZ, float degreesY, float xOffset , float xFactor,
	float yOffset , float yFactor , float zOffset , float zFactor,
	float xExp    , float yExp    , float zExp)
{
	_trans3Dto2D = new Trans3Dto2D(data, degreesZ, degreesY,
		xOffset, xFactor, yOffset, yFactor, zOffset, zFactor,
		xExp, yExp, zExp);
}

Trans3Dto2DElement::~Trans3Dto2DElement()
{
	delete _trans3Dto2D;
}

Trans3Dto2DLinkedList::Trans3Dto2DLinkedList(float degreesZ, float degreesY,
	float xOffset, float xFactor, float yOffset, float yFactor,
	float zOffset, float zFactor, float xExp, float yExp, float zExp)
	: _degreesZ(degreesZ), _degreesY(degreesY),
	  _xOffset (xOffset ), _xFactor (xFactor ),
	  _yOffset (yOffset ), _yFactor (yFactor ),
	  _zOffset (zOffset ), _zFactor (zFactor ),
	  _xExp    (xExp    ), _yExp    (yExp    ),
	  _zExp    (zExp    )
{
	/* just initializers */
}

Trans3Dto2D *Trans3Dto2DLinkedList::add(BaseData *data)
{
	Trans3Dto2DElement *theElement = new Trans3Dto2DElement(data,
		_degreesZ, _degreesY, _xOffset , _xFactor,
		_yOffset , _yFactor , _zOffset , _zFactor,
		_xExp    , _yExp    , _zExp);

	BaseLinkedList::add((Element *) theElement);

	return theElement->_trans3Dto2D;
}

void Trans3Dto2DLinkedList::remove(Trans3Dto2D *trans3Dto2D)
{
	BaseLinkedList::remove((void *) trans3Dto2D);
}

Trans3Dto2D *Trans3Dto2DLinkedList::find(Trans3Dto2D *trans3Dto2D, void **p)
{
	Trans3Dto2DElement *ptr = (Trans3Dto2DElement *)
		BaseLinkedList::find((void *) trans3Dto2D, p);

	return (ptr ? ptr->_trans3Dto2D : (Trans3Dto2D *) NULL);
}

Trans3Dto2D *Trans3Dto2DLinkedList::top    (void **p)
{
	Trans3Dto2DElement *ptr = (Trans3Dto2DElement *)
		BaseLinkedList::top    (p);

	return (ptr ? ptr->_trans3Dto2D : (Trans3Dto2D *) NULL);
}

Trans3Dto2D *Trans3Dto2DLinkedList::bottom (void **p)
{
	Trans3Dto2DElement *ptr = (Trans3Dto2DElement *)
		BaseLinkedList::bottom (p);

	return (ptr ? ptr->_trans3Dto2D : (Trans3Dto2D *) NULL);
}

Trans3Dto2D *Trans3Dto2DLinkedList::next   (void **p)
{
	Trans3Dto2DElement *ptr = (Trans3Dto2DElement *)
		BaseLinkedList::next   (p);

	return (ptr ? ptr->_trans3Dto2D : (Trans3Dto2D *) NULL);
}

Trans3Dto2D *Trans3Dto2DLinkedList::prev   (void **p)
{
	Trans3Dto2DElement *ptr = (Trans3Dto2DElement *)
		BaseLinkedList::prev   (p);

	return (ptr ? ptr->_trans3Dto2D : (Trans3Dto2D *) NULL);
}

Trans3Dto2D *Trans3Dto2DLinkedList::current(void **p)
{
	Trans3Dto2DElement *ptr = (Trans3Dto2DElement *)
		BaseLinkedList::current(p);

	return (ptr ? ptr->_trans3Dto2D : (Trans3Dto2D *) NULL);
}

void Trans3Dto2DLinkedList::setAngles(float degreesZ, float degreesY,
	int doMod)
{
	_degreesZ = degreesZ;
	_degreesY = degreesY;

	Trans3Dto2D *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
		ptr->setAngles(_degreesZ, _degreesY, doMod);
}

void Trans3Dto2DLinkedList::setScale(float xOffset, float xFactor,
	float yOffset, float yFactor, float zOffset, float zFactor, int doMod)
{
	_xOffset = xOffset;
	_xFactor = xFactor;
	_yOffset = yOffset;
	_yFactor = yFactor;
	_zOffset = zOffset;
	_zFactor = zFactor;

	Trans3Dto2D *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
		ptr->setScale(xOffset, xFactor, yOffset, yFactor,
			zOffset, zFactor, doMod);
}

void Trans3Dto2DLinkedList::setExpansion(float xExp, float yExp, float zExp,
	int doMod)
{
	_xExp = xExp;
	_yExp = yExp;
	_zExp = zExp;

	Trans3Dto2D *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
		ptr->setExpansion(xExp, yExp, zExp,doMod);
}
