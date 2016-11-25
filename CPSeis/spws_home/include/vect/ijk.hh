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
#ifndef _IJK_HH
#define _IJK_HH

class Ijk
{
	public:

		Ijk(float a = 0.0, float b = 0.0, float c = 0.0);
		Ijk(const Ijk &other);
		~Ijk();
		Ijk &operator=(const Ijk &vector);
		Ijk operator+(const Ijk &other) const;
		Ijk operator-(const Ijk &other) const;
		Ijk operator*(float factor) const;
		Ijk operator/(float factor) const;
		friend Ijk operator*(float factor, const Ijk &vector);
		float operator%(const Ijk &other) const; /* dot product */
		void display() const;
		void get(float *a, float *b, float *c) const;

	private:

		float _a, _b, _c;
};

#endif /* _IJK_HH */
