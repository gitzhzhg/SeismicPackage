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
#ifndef _PICK_WATCH_HH
#define _PICK_WATCH_HH

#include "plot/pick_base.hh"
#include <X11/cursorfont.h>

class PlotWatch : public PickBase
{
	public:

		PlotWatch(class PlotBase *plot)
			: PickBase(plot, "busy",
				(const char *) NULL,
				(const char *) NULL, XC_watch)
			{ /* do nothing */ }
		~PlotWatch()
			{ /* do nothing */ }

	private:

		void buttonAny(int /*x1*/, int /*x2*/, int /*y1*/, int /*y2*/,
			int /*button*/, Action /*action*/,
			Modifier /*modifier*/)
			{ doBeep(); }
		PlotWatch(const PlotWatch &) : PickBase((class PlotBase *) NULL)
			{ /* private, no access to copy constructor */ }
		PlotWatch& operator=(PlotWatch &p)
			{ /* private, no access to = */ return p; }
};

class PickWatch
{
	public:

		PickWatch();
		~PickWatch();

	private:

		int _numPlots;
		PlotWatch **_watches;

		PickWatch(const PickWatch &)
			{ /* private, no access to copy constructor */ }
		PickWatch& operator=(PickWatch &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _PICK_WATCH_HH */
