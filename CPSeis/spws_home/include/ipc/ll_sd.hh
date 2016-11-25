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
#ifndef LL_SD_H
#define LL_SD_H

#ifndef LL_BASE_H
#include "oprim/ll_base.hh"
#define LL_BASE_H
#endif

#ifndef HANDLES_ERRORS_H
#include "oprim/handles_errors.hh"
#define HANDLES_ERRORS_H
#endif

#ifndef SD_ELEMENT_H
#include "ipc/sd_element.hh"
#define SD_ELEMENT_H
#endif

#ifndef SD_MASTER_H
#include "ipc/sd_master.hh"
#define SD_MASTER_H
#endif

#ifndef LL_PIXMAP_ID_H
#include "ipc/ll_pixmap_id.hh"
#define LL_PIXMAP_ID_H
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

class SlaveDisplayLinkedList : public BaseLinkedList, public HandlesErrors

{
	friend class SlaveDisplayElement;

	private:

		Widget _widget;
		int _colors;
		int _planes;
		char *_title;
		ErrorHandler *_errorDialog;
		PixmapIDLinkedList *_pixmapIDs;
		Pixmap _currentPixmapID ;
		Pixmap _currentOverlayID;
		unsigned long _currentPlaneMask;
		int _right, _left, _top, _bottom;	// sdScrWin borders
		Bool _xorCursor;

		void errorHandler(HandlesErrors *, int);

	public:

		SlaveDisplayLinkedList()
			{ assert(False); }
		SlaveDisplayLinkedList(Widget, int, int,
			char * = "Remote Display",
			int left =0, int right =0, int top =0, int bottom =0);
		~SlaveDisplayLinkedList();

		void add(char *, Bool = False);
		void remove(char *nodeName)
			{ BaseLinkedList::remove((void *) nodeName); }

		SlaveDisplayElement *find(char *nodeName)
			{ return((SlaveDisplayElement *)
				BaseLinkedList::find((void *) nodeName)); }
		SlaveDisplayElement *top    (void **p = (void **) NULL)
		  { return((SlaveDisplayElement *)BaseLinkedList::top    (p)); }
		const char *topNodeName    (void **p = (void **) NULL)
			{
				SlaveDisplayElement *ptr = top    (p);
				if (ptr)
					return((const char *) ptr->_node);
				else
					return((const char *) NULL);
			}
		SlaveDisplayElement *bottom (void **p = (void **) NULL)
		  { return((SlaveDisplayElement *)BaseLinkedList::bottom (p)); }
		const char *bottomNodeName (void **p = (void **) NULL)
			{
				SlaveDisplayElement *ptr = bottom (p);
				if (ptr)
					return((const char *) ptr->_node);
				else
					return((const char *) NULL);
			}
		SlaveDisplayElement *next   (void **p = (void **) NULL)
		  { return((SlaveDisplayElement *)BaseLinkedList::next   (p)); }
		const char *nextNodeName    (void **p = (void **) NULL)
			{
				SlaveDisplayElement *ptr = next   (p);
				if (ptr)
					return((const char *) ptr->_node);
				else
					return((const char *) NULL);
			}
		SlaveDisplayElement *prev   (void **p = (void **) NULL)
		  { return((SlaveDisplayElement *)BaseLinkedList::prev   (p)); }
		const char *prevNodeName    (void **p = (void **) NULL)
			{
				SlaveDisplayElement *ptr = prev   (p);
				if (ptr)
					return((const char *) ptr->_node);
				else
					return((const char *) NULL);
			}
		SlaveDisplayElement *current(void **p = (void **) NULL)
		  { return((SlaveDisplayElement *)BaseLinkedList::current(p)); }
		const char *currentNodeName(void **p = (void **) NULL)
			{
				SlaveDisplayElement *ptr = current(p);
				if (ptr)
					return((const char *) ptr->_node);
				else
					return((const char *) NULL);
			}

		void addPixmap    (Pixmap);
		void deletePixmap (Pixmap);
		void displayPixmap(Pixmap, unsigned long = 0);
		void updateRGBs();
		void numPixmaps(int);
		void setExitOnDelete();
		void deleteAndExit();
		void setBorder(int leftBorder, int rightBorder,
			int topBorder, int bottomBorder);
		void getBorder(int *left, int *right, int *top, int *bottom)
		{ *left =_left; *right =_right; *top =_top; *bottom =_bottom; }
		void setXorCursor(Bool xorCursor);
		Bool getXorCursor()
			{ return _xorCursor; }
		void clearCursor();
		void refreshCursor();
};
#endif
