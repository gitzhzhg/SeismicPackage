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
#ifndef _LL_RBN_INFO_H
#define _LL_RBN_INFO_H

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <assert.h>
#include <iostream.h>
#include <Xm/Xm.h>

class RbnInfoElement : public Element
{
	friend class RbnInfoLinkedList;

	private:

		class PlotBase *_plot;
		XPoint *_pts[2];
		int _numPts[2], _numPtsAlloc[2];
		int _which_draw, _which_undraw;	/* 0 or 1 */
		Bool _initialized;
		GC _gc;
		Display *_display;

		RbnInfoElement()
			{ assert(False); }
		RbnInfoElement(class PlotBase *plot)
			: _plot(plot), _which_draw(0), _which_undraw(0),
			  _initialized(False)
			{ _numPtsAlloc[0] = _numPtsAlloc[1] = 0; }
		~RbnInfoElement()
			{
				if (_numPtsAlloc[0])	delete [] _pts[0];
				if (_numPtsAlloc[1])	delete [] _pts[1];
				if (_initialized)	XFreeGC(_display, _gc);
			}
		int operator ==(void * const plot) const
			{ return((class PlotBase *) plot == _plot); }
		void print() const
			{ cout << " " << _plot; }
		Bool initialized()
			{ return _initialized; }
		void initialize(char *color, int width);
};

class RbnInfoLinkedList : public BaseLinkedList
{
	public:

		RbnInfoLinkedList()
			{ /* do nothing */ }
		~RbnInfoLinkedList()
			{ /* do nothing */ }
		void add(class PlotBase *plot)
		{
			RbnInfoElement *theElement =
				new RbnInfoElement(plot);
			BaseLinkedList::add((Element *) theElement);
		}
		void remove(class PlotBase *plot)
			{ BaseLinkedList::remove((void *) plot); }
		void getInfo(class PlotBase *plot, char *color, int width,
			XPoint ***pts, int **numPts, int **numPtsAlloc,
			int **which_draw, int **which_undraw, GC *gc);

	private:

		RbnInfoElement *find(class PlotBase *plot)
		{
			return (RbnInfoElement *)
				BaseLinkedList::find((void *) plot);
		}
		RbnInfoElement *top    ()
			{ return (RbnInfoElement *) BaseLinkedList::top    (); }
		RbnInfoElement *bottom ()
			{ return (RbnInfoElement *) BaseLinkedList::bottom (); }
		RbnInfoElement *next   ()
			{ return (RbnInfoElement *) BaseLinkedList::next   (); }
		RbnInfoElement *prev   ()
			{ return (RbnInfoElement *) BaseLinkedList::prev   (); }
		RbnInfoElement *current()
			{ return (RbnInfoElement *) BaseLinkedList::current(); }
};

#endif
