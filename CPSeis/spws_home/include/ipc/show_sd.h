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
#ifndef SHOW_SD_H
#define SHOW_SD_H

#ifndef HANDLES_ERRORS_H
#include "oprim/handles_errors.hh"
#define HANDLES_ERRORS_H
#endif

#ifndef LL_SD_H
#include "ipc/ll_sd.hh"
#define LL_SD_H
#endif

#ifndef ERROR_HANDLER_H
#include "sl/error_handler.hh"
#define ERROR_HANDLER_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

class ShowSlaveDisplay : public HandlesErrors
{
	private:

		SlaveDisplayLinkedList *_theMasterList;
		int _numSlaves;
		Widget _widget;
		enum { _MAX_PIXMAPS = 5 };	// More than 1 pixmap in case
		int _currentPixmap;		// redisplay is requested before
		Pixmap _pixmap[_MAX_PIXMAPS];	// slave has read pixmap from
						// last redisplay.
		int _numColors;
		ErrorHandler *_errorDialog;

		void makePixmap();
		int colorsOnPixmap();
		void errorNotify(int);
		static void errorAck(void *);

	public:

		ShowSlaveDisplay()	{ assert(False); }
		ShowSlaveDisplay(Widget, char **, int, char * = " Show");
		~ShowSlaveDisplay();
		void redisplay();
		SlaveDisplayLinkedList *getLinkedList()
			{ return _theMasterList; }
};

#endif
