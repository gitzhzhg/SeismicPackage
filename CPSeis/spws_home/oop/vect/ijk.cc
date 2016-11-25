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
#include "vect/ijk.hh"
#include <stdio.h>
#include <assert.h>

Ijk::Ijk(float a, float b, float c) : _a(a), _b(b), _c(c)
{
	/* do nothing */
}

Ijk::Ijk(const Ijk &other)
{
	_a = other._a;
	_b = other._b;
	_c = other._c;
}

Ijk::~Ijk()
{
	/* do nothing */
}

Ijk &Ijk::operator=(const Ijk &other)
{
	_a = other._a;
	_b = other._b;
	_c = other._c;

	return *this;
}

Ijk Ijk::operator+(const Ijk &other) const
{
	return Ijk(_a + other._a, _b + other._b, _c + other._c);
}

Ijk Ijk::operator-(const Ijk &other) const
{
	return Ijk(_a - other._a, _b - other._b, _c - other._c);
}

Ijk Ijk::operator*(float factor) const
{
	return Ijk(_a * factor, _b * factor, _c * factor);
}

Ijk Ijk::operator/(float factor) const
{
	assert(factor != 0.0);

	return Ijk(_a / factor, _b / factor, _c / factor);
}

Ijk operator*(float factor, const Ijk &vector)
{
	return Ijk(vector._a * factor, vector._b * factor, vector._c * factor);
}

float Ijk::operator%(const Ijk &other) const
{
	return (_a * other._a + _b * other._b + _c * other._c);
}

void Ijk::display() const
{
	printf("%fi + %fj + %fk\n", _a, _b, _c);
}

void Ijk::get(float *a, float *b, float *c) const
{
	*a = _a;
	*b = _b;
	*c = _c;
}
