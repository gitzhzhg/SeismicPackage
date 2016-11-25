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
#include "plot/pick_watch.hh"
#include "plot/plot_base.hh"

#include <Xm/Xm.h>

PickWatch::PickWatch()
{
	PlotBase **plots;

	_numPlots = getPickPlots(&plots);

	if (_numPlots > 0)
	{
		_watches  = new PlotWatch *[_numPlots];

		for (int i = 0 ; i < _numPlots; i++)
		{
			if (plots[i]->getWidget() && PlotBase::watchOK())
			{
				_watches[i] = new PlotWatch(plots[i]);
				/*
				 * PlotBases might be on different Displays,
				 * so flush then all.
				 */
				XFlush(XtDisplay(plots[i]->getWidget()));
			}
			else
			{
				_watches[i] = (PlotWatch *) NULL;
			}
		}

		delete [] plots;
	}
}

PickWatch::~PickWatch()
{
	if (_numPlots > 0)
	{
		PlotBase *plot;

		/*
		 * Delete in reverse order as newed, since more than one
		 * PlotBase may share a mode and help display.  A little klugey.
		 */
		for (int i = _numPlots - 1; i >= 0; i--)
		{
			if (_watches[i])
			{
				plot = _watches[i]->getPlot();
				delete _watches[i];
				/*
				 * PlotBases might be on different Displays,
				 * so flush then all.
				 * We check that the widget is not NULL, 
				 * but there is no safeguard making sure
				 * that the PlotBase has not been deleted.
				 * I think it would take a SeisInform to
				 * do that.
				 */
				if (plot->getWidget())
					XFlush(XtDisplay(plot->getWidget()));
			}
		}

		delete [] _watches;
	}
}
