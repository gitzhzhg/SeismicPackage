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
#ifndef _MARKER_HH
#define _MARKER_HH

#include "vect/vector.hh"
#include <Xm/Xm.h>
#include <assert.h>

class MarkerBase
{
	friend class Markers;

	public:

		virtual void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize) = 0;

	protected:

		MarkerBase()
			{ /* do nothing */ }
		virtual ~MarkerBase()
			{ /* do nothing */ }

	private:

		MarkerBase(MarkerBase &)
			{ assert(False); }      // No copy constructor.
};

class CrossMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		CrossMarker()
			{ /* do nothing */ }
		~CrossMarker()
			{ /* do nothing */ }
		CrossMarker(CrossMarker &)
			{ assert(False); }      // No copy constructor.
};

class XMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		XMarker()
			{ /* do nothing */ }
		~XMarker()
			{ /* do nothing */ }
		XMarker(XMarker &)
			{ assert(False); }      // No copy constructor.
};

class NMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		NMarker()
			{ /* do nothing */ }
		~NMarker()
			{ /* do nothing */ }
		NMarker(NMarker &)
			{ assert(False); }      // No copy constructor.
};

class HorizontalLineMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		HorizontalLineMarker()
			{ /* do nothing */ }
		~HorizontalLineMarker()
			{ /* do nothing */ }
		HorizontalLineMarker(HorizontalLineMarker &)
			{ assert(False); }      // No copy constructor.
};

class VerticalLineMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		VerticalLineMarker()
			{ /* do nothing */ }
		~VerticalLineMarker()
			{ /* do nothing */ }
		VerticalLineMarker(VerticalLineMarker &)
			{ assert(False); }      // No copy constructor.
};

class CircleMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		CircleMarker()
			{ /* do nothing */ }
		~CircleMarker()
			{ /* do nothing */ }
		CircleMarker(CircleMarker &)
			{ assert(False); }      // No copy constructor.
};

class FilledCircleMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		FilledCircleMarker()
			{ /* do nothing */ }
		~FilledCircleMarker()
			{ /* do nothing */ }
		FilledCircleMarker(FilledCircleMarker &)
			{ assert(False); }      // No copy constructor.
};

class SquareMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		SquareMarker()
			{ /* do nothing */ }
		~SquareMarker()
			{ /* do nothing */ }
		SquareMarker(SquareMarker &)
			{ assert(False); }      // No copy constructor.
};

class FilledSquareMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		FilledSquareMarker()
			{ /* do nothing */ }
		~FilledSquareMarker()
			{ /* do nothing */ }
		FilledSquareMarker(FilledSquareMarker &)
			{ assert(False); }      // No copy constructor.
};

class DiamondMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		DiamondMarker()
			{ /* do nothing */ }
		~DiamondMarker()
			{ /* do nothing */ }
		DiamondMarker(DiamondMarker &)
			{ assert(False); }      // No copy constructor.
};

class FilledDiamondMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		FilledDiamondMarker()
			{ /* do nothing */ }
		~FilledDiamondMarker()
			{ /* do nothing */ }
		FilledDiamondMarker(FilledDiamondMarker &)
			{ assert(False); }      // No copy constructor.
};

class TriangleMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		TriangleMarker()
			{ /* do nothing */ }
		~TriangleMarker()
			{ /* do nothing */ }
		TriangleMarker(TriangleMarker &)
			{ assert(False); }      // No copy constructor.
};

class FilledTriangleMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		FilledTriangleMarker()
			{ /* do nothing */ }
		~FilledTriangleMarker()
			{ /* do nothing */ }
		FilledTriangleMarker(FilledTriangleMarker &)
			{ assert(False); }      // No copy constructor.
};

class XInSquareMarker : public MarkerBase
{
	friend class Markers;

	public:

		void draw(Display *display, Drawable drawable, GC gc,
			short x, short y, short halfSize);

	private:

		XInSquareMarker()
			{ /* do nothing */ }
		~XInSquareMarker()
			{ /* do nothing */ }
		XInSquareMarker(XInSquareMarker &)
			{ assert(False); }      // No copy constructor.
};

class Markers
{
	public:

		Markers();
		~Markers();
		MarkerBase *operator[](int i)
			{ return _mrk[i]; }

	private:

		static MarkerBase **_mrk;
		static int _count;
};

#endif
