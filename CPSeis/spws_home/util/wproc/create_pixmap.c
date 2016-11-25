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
#include <Xm/Xm.h>
#include <X11/Xproto.h>

static Bool createPixmapError;
static int (*oldErrorHandler)(Display *, XErrorEvent *);

static int createPixmapErrorHandler(Display *display, XErrorEvent *event);

Pixmap wprocCreatePixmap(Display *display, Window window,
	unsigned int width, unsigned int height, unsigned int depth)
{
	Pixmap retPixmap;

	oldErrorHandler   = XSetErrorHandler(createPixmapErrorHandler);
	createPixmapError = False;

	retPixmap = XCreatePixmap(display, window, width, height, depth);

	XSync(display, False);

	if (createPixmapError)
		retPixmap = (Pixmap) NULL;

	XSetErrorHandler(oldErrorHandler);

	return(retPixmap);
}

static int createPixmapErrorHandler(Display *display, XErrorEvent *event)
{
	if (event->error_code == BadAlloc
		&& event->request_code == X_CreatePixmap)
	{
		createPixmapError = True;
	}
	else
	{
		(*oldErrorHandler)(display, event);
	}

	return(0);
}
