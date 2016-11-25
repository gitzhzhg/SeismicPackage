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
#ifndef SD_BASE_H
#define SD_BASE_H

#ifndef HANDLES_ERRORS_H
#include "oprim/handles_errors.hh"
#define HANDLES_ERRORS_H
#endif

#ifndef CM_BASE_H
#include "ipc/cm_base.hh"
#define CM_BASE_H
#endif

#ifndef LL_PIXMAP_H
#include "ipc/ll_pixmap.hh"
#define LL_PIXMAP_H
#endif

#ifndef IPC_QUIT_H
#include "ipc/ipc_quit.hh"
#define IPC_QUIT_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

class SlaveDisplayBase : public HandlesErrors
{
	private:

		SlaveDisplayBase **_zeroWhenDestroyed;

		// For cursor.
		static const char *const _CURSOR_COLOR;

		/*
		 * On g++ _NEGATIVE_VALUE will make these enums be ints
		 * instead of unsigneds.  This is important since they
		 * are used in comparsions.
		 */
		enum {	_NEGATIVE_VALUE        = -1,
			_CURSOR_HALF_LENGTH    = 9,
			_CURSOR_HALF_WIDTH     = 1,
			_CURSOR_WORKING_PIXMAP = 4 * _CURSOR_HALF_LENGTH + 1,
			_CURSOR_PIXMAP_LENGTH  = 2 * _CURSOR_HALF_LENGTH + 1,
			_CURSOR_PIXMAP_WIDTH   = 2 * _CURSOR_HALF_WIDTH  + 1,
			_CURSOR_THRESHOLD      = 2 * _CURSOR_HALF_LENGTH    
		};

		enum { _NUM_CURSOR_PIXMAPS = 5 };
		// 0 - working, 1 - cursor horizontal, 2 - cursor vertical,
		// 3 - recover horizontal, 4 - recover vertical

		int _cursorX, _cursorY;
		Pixmap _cursorPixmap[_NUM_CURSOR_PIXMAPS];
		GC _cursorGC;
		Bool _xorCursor;
		int _exposuresPending;
		Bool _cursorMovedWhileExposurePending;
		int _saveCursorX, _saveCursorY;

		void receivedMessage(long *, int);
		void receivedMessage(char *    );	// Goes to titleMessage.

		// Cursor is handled the same on master & slave.
		void initCursor();
		void updateCursorMessage(long *, int);
		void updateCursor(int , int);
		void refreshCursor(int x, int y, int width, int height);
		void repositionCursor(int, int);
		void closeCursor();
		void getLosses(int x, int y, int xOrg, int yOrg,
			int width, int height, int *xll, int *xlw, int *xrl,
			int *xrw, int *ytl, int *ytw, int *ybl, int *ybw);

		static void eventHandler(Widget, XtPointer, XEvent *,
			Boolean *);
		void pointerMotion(XEvent *);
		void enterWindow  (XEvent *);
		void leaveWindow  (XEvent *);
		void exposure     (XEvent *);

		void exposureMessage(long *, int);

		virtual void resize(int, int) = 0;	// Pure virtual

		// Always defined in derived class, but not pure virtual
		// because only defined if needed.  These are in the base
		// class so the message handler can be in the base class.
		virtual void addPixmapMessage    (long *, int) { assert(False); }
		virtual void deletePixmapMessage (long *, int) { assert(False); }
		virtual void displayPixmapMessage(long *, int) { assert(False); }
		virtual void pixelInfoMessage    (long *, int) { assert(False); }
		virtual void sizeChangeMessage   (long *, int) { assert(False); }
		virtual void updateRGBsMessage   (          ) { assert(False); }
		virtual void quitMessage         (          ) { assert(False); }
		virtual void titleMessage        (char *    ) { assert(False); }
		virtual void numPixmapsMessage   (long *, int) { assert(False); }
		virtual void borderInfoMessage   (long *, int) { assert(False); }

		void drawXorCursor();
		void drawXorCursor(int x, int y, int width, int height);

	protected:

		Widget _localWidget;
		int _width, _height;
		ClientMessageBase *_ipc;

		Bool _exitOnDelete;
		Bool _otherGuyQuit;
		Bool _otherGuyDied;

		enum {	_UPDATE_CURSOR ,
			_ADD_PIXMAP    ,
			_DELETE_PIXMAP ,
			_DISPLAY_PIXMAP,
			_PIXEL_INFO    ,
			_SIZE_CHANGE   ,
			_UPDATE_RGBS   ,
			_TITLE         ,	// Not really needed.
			_QUIT_REQUEST  ,
			_QUIT_OK       ,
			_ERROR         ,
			_NUM_PIXMAPS   ,
			_BORDER_INFO   ,
			_EXPOSURE      
		};

		PixmapLinkedList *_pixmaps;
		IpcQuit **_quitterPtr;

		SlaveDisplayBase(Widget, SlaveDisplayBase **);
		virtual ~SlaveDisplayBase();
		void addEventHandlers();
		static void receivedIntPtr (long *, int, XtPointer);
		static void receivedCharPtr(char *,      XtPointer);
		static void resizeEH(Widget, XtPointer, XEvent *, Boolean *);

	public:

		// I might have defined addPixmap, deletePixmap, etc. as
		// virtual in the base class, but since applications will
		// know if they are the master or the slave, there is no
		// need for polymorphism.

		static const char *const _slaveApp       ;
		static const char *const _privateColormap;

		void setXorCursor(Bool xorCursor);
		Bool getXorCursor()
			{ return _xorCursor; }

		/*
		 * Made clearCursor & refreshCursor public so show could
		 * clear cursor when copying from window.  Be careful,
		 * refreshing an already refreshed cursor will screw
		 * things up.
		 */
		void clearCursor();
		void refreshCursor();
};

#endif
