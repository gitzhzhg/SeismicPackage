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
#ifndef _LL_PLOT_INFO_H
#define _LL_PLOT_INFO_H

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <assert.h>
#include <iostream.h>
#include <Xm/Xm.h>

class PlotInfoElement : public Element
{
	friend class PlotInfoLinkedList;

	private:

		class PlotBase *_plot;
		Bool _drawn, _loadFont, _checkLabel, _edited;
		short _xMin, _yMin, _xMax, _yMax;
		XFontStruct *_fontStruct;
		Display *_display;
		int _labelLeftOffset, _labelRightOffset,
			_labelUpOffset, _labelDownOffset;
		short _labelDrawX, _labelDrawY;

		PlotInfoElement()
			{ assert(False); }
		PlotInfoElement(class PlotBase *plot);
		~PlotInfoElement();
		int operator ==(void * const plot) const
			{ return((class PlotBase *) plot == _plot); }
		void print() const
			{ cout << " " << _plot; }
};

class PlotInfoLinkedList : public BaseLinkedList
{
	public:

		PlotInfoLinkedList()
			{ /* do nothing */ }
		~PlotInfoLinkedList()
			{ /* do nothing */ }
		void add(class PlotBase *plot)
		{
			PlotInfoElement *theElement = new PlotInfoElement(plot);
			BaseLinkedList::add((Element *) theElement);
		}
		void remove(class PlotBase *plot)
			{ BaseLinkedList::remove((void *) plot); }
		void getInfo(class PlotBase *plot, Bool **drawn,
			short **xMin, short **yMin, short **xMax, short **yMax,
			XFontStruct ***fontStruct, Bool **loadFont,
			Bool **checkLabel,
			int **labelLeftOffset, int **labelRightOffset,
			int **labelUpOffset, int **labelDownOffset,
			Bool **edited = (Bool **) NULL,
			short **labelDrawX = (short    **) NULL,
			short **labelDrawY = (short    **) NULL,
			Display ***display = (Display ***) NULL);
		void loadFont()
		{
			for (PlotInfoElement *ptr = top(); ptr; ptr = next())
				ptr->_loadFont = True;
		}
		void checkLabel()
		{
			for (PlotInfoElement *ptr = top(); ptr; ptr = next())
				ptr->_checkLabel = True;
		}

	private:

		PlotInfoElement *find(class PlotBase *plot)
		{
			return (PlotInfoElement *)
				BaseLinkedList::find((void *) plot);
		}
		PlotInfoElement *top    ()
			{ return (PlotInfoElement *)BaseLinkedList::top    (); }
		PlotInfoElement *bottom ()
			{ return (PlotInfoElement *)BaseLinkedList::bottom (); }
		PlotInfoElement *next   ()
			{ return (PlotInfoElement *)BaseLinkedList::next   (); }
		PlotInfoElement *prev   ()
			{ return (PlotInfoElement *)BaseLinkedList::prev   (); }
		PlotInfoElement *current()
			{ return (PlotInfoElement *)BaseLinkedList::current(); }
};

#endif
