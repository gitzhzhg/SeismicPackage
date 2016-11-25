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
#include "vect/marker.hh"

void CrossMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XDrawLine(display, drawable, gc,
		(int) x, (int) (y - halfSize), (int) x, (int) (y + halfSize));

	XDrawLine(display, drawable, gc,
		(int) (x - halfSize), (int) y, (int) (x + halfSize), (int) y);
}

void XMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XDrawLine(display, drawable, gc,
		(int) (x - halfSize), (int) (y - halfSize),
		(int) (x + halfSize), (int) (y + halfSize));

	XDrawLine(display, drawable, gc,
		(int) (x - halfSize), (int) (y + halfSize),
		(int) (x + halfSize), (int) (y - halfSize));
}

void NMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XPoint pts[4];

	pts[0].x = x - halfSize;
	pts[0].y = y + halfSize;
	pts[1].x = x - halfSize;
	pts[1].y = y - halfSize;
	pts[2].x = x + halfSize;
	pts[2].y = y + halfSize;
	pts[3].x = x + halfSize;
	pts[3].y = y - halfSize;

	XDrawLines(display, drawable, gc, pts, 4, CoordModeOrigin);
}

void HorizontalLineMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XDrawLine(display, drawable, gc,
		(int) (x - halfSize), (int) y, (int) (x + halfSize), (int) y);
}

void VerticalLineMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XDrawLine(display, drawable, gc,
		(int) x, (int) (y - halfSize), (int) x, (int) (y + halfSize));
}

void CircleMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XDrawArc(display, drawable, gc,
		(int) (x - halfSize), (int) (y - halfSize),
		(unsigned int) (2 * halfSize),
		(unsigned int) (2 * halfSize),
		0, 23040);
}

void FilledCircleMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XDrawArc(display, drawable, gc,
		(int) (x - halfSize), (int) (y - halfSize),
		(unsigned int) (2 * halfSize),
		(unsigned int) (2 * halfSize),
		0, 23040);

	XFillArc(display, drawable, gc,
		(int) (x - halfSize), (int) (y - halfSize),
		(unsigned int) (2 * halfSize),
		(unsigned int) (2 * halfSize),
		0, 23040);
}

void SquareMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XDrawRectangle(display, drawable, gc,
		(int) (x - halfSize), (int) (y - halfSize),
		(unsigned int) (2 * halfSize),
		(unsigned int) (2 * halfSize));
}

void FilledSquareMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XDrawRectangle(display, drawable, gc,
		(int) (x - halfSize), (int) (y - halfSize),
		(unsigned int) (2 * halfSize),
		(unsigned int) (2 * halfSize));

	XFillRectangle(display, drawable, gc,
		(int) (x - halfSize), (int) (y - halfSize),
		(unsigned int) (2 * halfSize),
		(unsigned int) (2 * halfSize));
}

void DiamondMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XPoint pts[5];

	pts[0].x = x - halfSize;
	pts[0].y = y           ;
	pts[1].x = x           ;
	pts[1].y = y - halfSize;
	pts[2].x = x + halfSize;
	pts[2].y = y           ;
	pts[3].x = x           ;
	pts[3].y = y + halfSize;
	pts[4].x = x - halfSize;
	pts[4].y = y           ;

	XDrawLines(display, drawable, gc, pts, 5, CoordModeOrigin);
}

void FilledDiamondMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XPoint pts[5];

	pts[0].x = x - halfSize;
	pts[0].y = y           ;
	pts[1].x = x           ;
	pts[1].y = y - halfSize;
	pts[2].x = x + halfSize;
	pts[2].y = y           ;
	pts[3].x = x           ;
	pts[3].y = y + halfSize;
	pts[4].x = x - halfSize;
	pts[4].y = y           ;

	XDrawLines  (display, drawable, gc, pts, 5,         CoordModeOrigin);
	XFillPolygon(display, drawable, gc, pts, 5, Convex, CoordModeOrigin);
}

void TriangleMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XPoint pts[4];

	pts[0].x = x - halfSize;
	pts[0].y = y + halfSize;
	pts[1].x = x           ;
	pts[1].y = y - halfSize;
	pts[2].x = x + halfSize;
	pts[2].y = y + halfSize;
	pts[3].x = x - halfSize;
	pts[3].y = y + halfSize;

	XDrawLines(display, drawable, gc, pts, 4, CoordModeOrigin);
}

void FilledTriangleMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XPoint pts[4];

	pts[0].x = x - halfSize;
	pts[0].y = y + halfSize;
	pts[1].x = x           ;
	pts[1].y = y - halfSize;
	pts[2].x = x + halfSize;
	pts[2].y = y + halfSize;
	pts[3].x = x - halfSize;
	pts[3].y = y + halfSize;

	XDrawLines  (display, drawable, gc, pts, 4,         CoordModeOrigin);
	XFillPolygon(display, drawable, gc, pts, 4, Convex, CoordModeOrigin);
}

void XInSquareMarker::draw(Display *display, Drawable drawable, GC gc,
	short x, short y, short halfSize)
{
	XDrawRectangle(display, drawable, gc,
		(int) (x - halfSize), (int) (y - halfSize),
		(unsigned int) (2 * halfSize),
		(unsigned int) (2 * halfSize));

	XDrawLine(display, drawable, gc,
		(int) (x - halfSize), (int) (y - halfSize),
		(int) (x + halfSize), (int) (y + halfSize));

	XDrawLine(display, drawable, gc,
		(int) (x - halfSize), (int) (y + halfSize),
		(int) (x + halfSize), (int) (y - halfSize));
}

MarkerBase **Markers::_mrk;
int Markers::_count = 0;

Markers::Markers()
{
	if (_count++ == 0)
	{
		_mrk = new MarkerBase *[Vector::DataSpecifiedMarker];

		_mrk[Vector::CrossMarker         ] = new CrossMarker();
		_mrk[Vector::XMarker             ] = new XMarker();
		_mrk[Vector::NMarker             ] = new NMarker();
		_mrk[Vector::HorizontalLineMarker] = new HorizontalLineMarker();
		_mrk[Vector::VerticalLineMarker  ] = new VerticalLineMarker();
		_mrk[Vector::CircleMarker        ] = new CircleMarker();
		_mrk[Vector::FilledCircleMarker  ] = new FilledCircleMarker();
		_mrk[Vector::SquareMarker        ] = new SquareMarker();
		_mrk[Vector::FilledSquareMarker  ] = new FilledSquareMarker();
		_mrk[Vector::DiamondMarker       ] = new DiamondMarker();
		_mrk[Vector::FilledDiamondMarker ] = new FilledDiamondMarker();
		_mrk[Vector::TriangleMarker      ] = new TriangleMarker();
		_mrk[Vector::FilledTriangleMarker] = new FilledTriangleMarker();
		_mrk[Vector::XInSquareMarker     ] = new XInSquareMarker();
	}
}

Markers::~Markers()
{
	if (--_count == 0)
	{
		for (int i = 0; i < Vector::DataSpecifiedMarker; i++)
			delete _mrk[i];

		delete [] _mrk;
	}
}
