#include "fg2d/fg2d_pick.hh"
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
#include "fg2d/fg2d_plot.hh"
#include "vect/ll_vector.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "plot/plot_base.hh"
#include "geom/fg_constants.hh"
#include "sl/prim_support.hh"
#include "sl/shell_watch.hh"
#include "plot/pick_watch.hh"

#define FALLBACK "mouse*plot*FG2D_PICK: BTN#1: active flag\\ncntl BTN#1: select, cntl BTN #2: deselect, BTN#3: popup"

Fg2DPick::Fg2DPick(class PlotBase *plot)
	: PickBase(plot, "Chart", "FG2D_PICK", FALLBACK, XC_tcross, beep, True,
		&PrimSupport::updateEverything),
	  _fg2DPlot((Fg2DPlot *) NULL)
{
	_rbnList = new VectorLinkedList();
	_rbnList->addPlot(plot);
}

Fg2DPick::~Fg2DPick()
{
	delete _rbnList;
}

void Fg2DPick::setPlot(Fg2DPlot *fg2DPlot)
{
	_fg2DPlot = fg2DPlot;
}

void Fg2DPick::noModButtonOnePress(int x, int y)
{
	_xPress = x;
	_yPress = y;
}

void Fg2DPick::noModButtonOneMotion(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void Fg2DPick::noModButtonOneRelease(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	PickWatch  pickWatch ;
	ShellWatch shellWatch;

	if (_fg2DPlot)
	{
		if (!_fg2DPlot->setActive(_xPress, _yPress, getPlot()))
			XBell(XtDisplay(getPlot()->getWidget()), 100);
	}
	else
	{
		XBell(XtDisplay(getPlot()->getWidget()), 100);
	}
}

void Fg2DPick::cntlButtonOnePress(int x, int y)
{
	float fx = getPlot()->xWC(x);
	float fy = getPlot()->yWC(y);

	float xs[5], ys[5];

	for (int i = 0; i < 5; i++)
	{
		xs[i] = fx;
		ys[i] = fy;
	}

	_rbnData = new VectData(5, xs, ys);

	_rbnVector = _rbnList->add(_rbnData, "red", 2, True);
}

void Fg2DPick::cntlButtonOneMotion(int x1, int x2, int y1, int y2)
{
	float fx1 = getPlot()->xWC(x1);
	float fy1 = getPlot()->yWC(y1);
	float fx2 = getPlot()->xWC(x2);
	float fy2 = getPlot()->yWC(y2);

	float xs[3], ys[3];

	xs[0] = fx1;
	ys[0] = fy2;
	xs[1] = fx2;
	ys[1] = fy2;
	xs[2] = fx2;
	ys[2] = fy1;

	_rbnData->replace(1, 3, xs, ys);
}

void Fg2DPick::cntlButtonOneRelease(int x1, int x2, int y1, int y2)
{
	PickWatch  pickWatch ;
	ShellWatch shellWatch;

	cntlButtonRelease(x1, x2, y1, y2, SELECT_YES);
}

void Fg2DPick::cntlButtonRelease(int x1, int x2, int y1, int y2, char c)
{
	_rbnList->remove(_rbnVector);
	delete _rbnData;

	if (_fg2DPlot)
	{
		if (!_fg2DPlot->setSelected(x1, x2, y1, y2, getPlot(), c))
			XBell(XtDisplay(getPlot()->getWidget()), 100);
	}
	else
	{
		XBell(XtDisplay(getPlot()->getWidget()), 100);
	}
}

void Fg2DPick::cntlButtonTwoPress(int x , int y)
{
	cntlButtonOnePress(x, y);
}

void Fg2DPick::cntlButtonTwoMotion(int x1, int x2, int y1, int y2)
{
	cntlButtonOneMotion(x1, x2, y1, y2);
}

void Fg2DPick::cntlButtonTwoRelease(int x1, int x2, int y1, int y2)
{
	PickWatch  pickWatch ;
	ShellWatch shellWatch;

	cntlButtonRelease(x1, x2, y1, y2, SELECT_MAYBE);
}

void Fg2DPick::buttonAny(int /*x1*/, int /*x2*/, int /*y1*/, int /*y2*/,
	int /*button*/, Action /*action*/, Modifier /*modifier*/)
{
	XBell(XtDisplay(getPlot()->getWidget()), 100);
	ignoreActions();
}
