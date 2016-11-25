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

static Bool putImageError;
static int (*oldErrorHandler)(Display *, XErrorEvent *);

static int putImageErrorHandler(Display *display, XErrorEvent *event)
{
	if (event->error_code == BadLength
		&& event->request_code == X_PutImage)
	{
		putImageError = True;
	}
	else
	{
		(*oldErrorHandler)(display, event);
	}

	return 0;
}

void wpPutImage(Display *display, Drawable drawable, GC gc, XImage *image,
	int src_x, int src_y, int dest_x, int dest_y,
	unsigned width, unsigned height)
{
	int offset = (int) width;
	int x_src, x_dest;
	oldErrorHandler = XSetErrorHandler(putImageErrorHandler);

	do
	{
		for (x_src = src_x, x_dest = dest_x;
			x_src < src_x + width;
			x_src += offset, x_dest += offset)
		{
			putImageError = False;

			XPutImage(display, drawable, gc, image,
				x_src, src_y, x_dest, dest_y,
				(unsigned) ((int) width + src_x - x_src < offset
					? (int) width + src_x - x_src
					: offset),
				height);

			XSync(display, False);

			if (putImageError)
				break;
		}

	} while(offset /= 2, putImageError);

	XSetErrorHandler(oldErrorHandler);
}
