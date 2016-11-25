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
#ifndef _VECTOR_ELEMENT_H
#define _VECTOR_ELEMENT_H

#include "oprim/element.hh"
#include "vect/vector.hh"
#include <assert.h>
#include <iostream.h>

class VectorElement : public Element
{
	friend class VectorLinkedList;

	private:

		Vector *_vector;

		VectorElement()
			{ assert(False); }
		VectorElement(class PlotBaseLinkedList *plotList,
			const char *name,
			class BaseData *data,
			class VectorLinkedList *vectList,
			long id = BaseData::defaultId,
			const char *color = "white",
			unsigned int width = 1,
			Bool rbn = False,
			Vector::VectorStyle style = Vector::SolidLine,
			Vector::VectorMarker marker = Vector::NoMarker,
			unsigned int markerSize = 5,
			unsigned int markerLineWidth = 1,
			const char *label = (char *) NULL,
			const char *font = "fixed");
		~VectorElement();
		int operator ==(void * const vector) const
			{ return ((Vector *) vector == _vector); }
		void print() const
			{ cout << " " << _vector; }
};

#endif
