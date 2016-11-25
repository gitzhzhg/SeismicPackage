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
#ifndef SD_SLAVE_H
#define SD_SLAVE_H

#ifndef SD_BASE_H
#include "ipc/sd_base.hh"
#define SD_BASE_H
#endif

#ifndef CM_SLAVE_H
#include "ipc/cm_slave.hh"
#define CM_SLAVE_H
#endif

#ifndef ERROR_HANDLER_H
#include "sl/error_handler.hh"
#define ERROR_HANDLER_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

class SlaveDisplaySlave : public SlaveDisplayBase
{
	private:

		class SLApp *_application;
		class SlaveDisplayScrWin *_sdScrWin;
		Bool _borders;
		int _leftBorder, _rightBorder, _topBorder, _bottomBorder;

		// Virtual in base.
		void addPixmapMessage    (long *, int);
		void deletePixmapMessage (long *, int);
		void displayPixmapMessage(long *, int);
		void pixelInfoMessage    (long *, int);
		void sizeChangeMessage   (long *, int);
		void updateRGBsMessage   (          );
		void quitMessage         (          );
		void titleMessage        (char *    );
		void numPixmapsMessage   (long *, int);
		void borderInfoMessage   (long *, int);
		void resize(int, int);

		void sameSizeWindows(Window);

		void errorHandler(HandlesErrors *, int);
		static void errorAck(void *);

	public:

		SlaveDisplaySlave();	// dummy
		SlaveDisplaySlave(Widget, int *, char **,
			class SLApp * = NULL,
			class SlaveDisplayScrWin * = NULL);
		~SlaveDisplaySlave();
		void reDisplay(Widget, int, int, unsigned, unsigned, int, int);
		void setBorders(Bool borders);
		Bool getBorders()
			{ return _borders; }
};

#endif
