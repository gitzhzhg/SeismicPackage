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
#include "vect/ll_rbn_info.hh"
#include "vect/vector.hh"
#include "plot/plot_base.hh"
#include <assert.h>

void RbnInfoElement::initialize(char *color, int width)
{
	_display          = _plot->getDisplay ();
	Drawable drawable = _plot->getDrawable();
	Colormap colormap = _plot->getColormap();

	assert(_display && drawable && colormap);

	_gc = XCreateGC(_display, drawable, 0, (XGCValues *) NULL);

	unsigned long pix = Vector::getColorPixel(_display, colormap, color);
	Pixel background  = _plot->getImageBackgroundPixel();
	pix ^= (unsigned long) background;

	if (pix)
	{
		XSetForeground(_display, _gc, pix);
	}
	else
	{
		XSetForeground(_display, _gc, (unsigned long) 1);
	}

	XSetLineAttributes(_display, _gc, width,
		LineSolid, CapNotLast, JoinMiter);

	XSetFunction(_display, _gc, GXxor);

	_initialized = True;
}

void RbnInfoLinkedList::getInfo(PlotBase *plot, char *color, int width,
	XPoint ***pts, int **numPts, int **numPtsAlloc,
	int **which_draw, int **which_undraw, GC *gc)
{
	// Returning the addresses of private data like this violates
	// the oop paradyne.  But what the heck, just this once.

	RbnInfoElement *ptr;
	assert(ptr = find(plot));

	if (!ptr->initialized())
		ptr->initialize(color, width);

	*pts          =  ptr->_pts         ;
	*numPts       =  ptr->_numPts      ;
	*numPtsAlloc  =  ptr->_numPtsAlloc ;
	*which_draw   = &ptr->_which_draw  ;
	*which_undraw = &ptr->_which_undraw;
	*gc           =  ptr->_gc          ;
}
