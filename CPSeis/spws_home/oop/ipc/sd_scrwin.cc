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
#include "ipc/sd_scrwin.hh"
#include "ipc/sd_slave.hh"
#include <assert.h>

SlaveDisplayScrWin::SlaveDisplayScrWin(const Widget p, const char *name)
	: SLScrollWin(p,name), _sd(NULL)
{
	/* do nothing */
}

void SlaveDisplayScrWin::annotate(Widget anno_da, int x, int y,
	unsigned width, unsigned height,
	long window_x, long window_y, int which_side)
{
	if (_sd != NULL)
	{
		Dimension waWidth, waHeight;
		XtVaGetValues(workArea(),
			XmNwidth , &waWidth ,
			XmNheight, &waHeight,
			NULL);

		switch (which_side)
		{
			case SlaveDisplayScrWin::Top:
				_sd->reDisplay(anno_da,
					(int) window_x,
					0,
					width, height, x, y);
				break;
			case SlaveDisplayScrWin::Bottom:
				_sd->reDisplay(anno_da,
					(int) window_x,
					(int) waHeight - (int) bottomBorder(),
					width, height, x, y);
				break;
			case SlaveDisplayScrWin::Left:
				_sd->reDisplay(anno_da,
					0,
					(int) window_y,
					width, height, x, y);
				break;
			case SlaveDisplayScrWin::Right:
				_sd->reDisplay(anno_da,
					(int) waWidth  - (int) rightBorder() ,
					(int) window_y,
					width, height, x, y);
				break;
			default:
				assert(False);
		}
	}
}
