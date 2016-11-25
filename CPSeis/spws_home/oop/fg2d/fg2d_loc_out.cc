#include "fg2d/fg2d_loc_out.hh"
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

#include <assert.h>

Fg2DLocOut::Fg2DLocOut(Widget p, char *name, HelpCtx hctx, FieldGeometry *fg,
	SeisPlot *sp)
	: FgLocOut(p, name, hctx, fg, sp), _fg2DPlot((Fg2DPlot *) NULL)
{
	/* just initializers */
}

Fg2DLocOut::~Fg2DLocOut()
{
	/* do nothing */
}

void Fg2DLocOut::setPlot(Fg2DPlot *fg2DPlot)
{
	_fg2DPlot = fg2DPlot;
}

Boolean Fg2DLocOut::outputByOther(SeisPlot *sp, int x, int y,
	long *line_index, long *flag_index, long *shot_index,
	FgMapToFlag::SkidType *skid_type)
{
	if (_fg2DPlot)
		return _fg2DPlot->outputByXlat(sp, x, y, line_index,
				flag_index, shot_index, skid_type);
	else
		return False;
}
