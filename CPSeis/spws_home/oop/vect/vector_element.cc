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
#include "vect/vector_element.hh"

VectorElement::VectorElement(class PlotBaseLinkedList *plotList,
	const char *name,
	class BaseData *data,
	class VectorLinkedList *vectList,
	long id,
	const char *color,
	unsigned int width,
	Bool rbn,
	Vector::VectorStyle style,
	Vector::VectorMarker marker,
	unsigned int markerSize,
	unsigned int markerLineWidth,
	const char *label,
	const char *font)
{
	_vector = new Vector(plotList, name, data, vectList, id, color, width,
		rbn, style, marker, markerSize, markerLineWidth, label, font);
}

VectorElement::~VectorElement()
{
	delete _vector;
}
