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
#ifndef _SD_SCRWIN_HH
#define _SD_SCRWIN_HH

#include "sl/sl_scroll_win.hh"

class SlaveDisplayScrWin : public SLScrollWin
{

	public:

		SlaveDisplayScrWin(const Widget p, const char *name);
		~SlaveDisplayScrWin()
			{ /* do nothing */ }
		virtual void annotate(Widget anno_da, int x, int y,
			unsigned width, unsigned height,
			long window_x, long window_y, int which_side );
		void setSlaveDisplay(class SlaveDisplaySlave *sd)
			{ _sd = sd; }

	private:

		class SlaveDisplaySlave *_sd;
};

#endif /* _SD_SCRWIN_HH */
