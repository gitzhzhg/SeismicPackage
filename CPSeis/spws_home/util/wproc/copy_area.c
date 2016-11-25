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
#include <assert.h>
#include <Xm/Xm.h>

void wpCopyArea(Display *display, Drawable src, Drawable dst,
	GC gc, int srcX, int srcY, int width, int height, int dstX, int dstY)
{
	XGCValues values;
	XEvent event;
	GC locGc = (GC) NULL;

	assert(XGetGCValues(display, gc, GCGraphicsExposures, &values));

	if (!values.graphics_exposures)
		XSetGraphicsExposures(display, gc, True);

	XCopyArea(display, src, dst, gc, srcX, srcY, width, height,
		dstX, dstY);

	for (;;)
	{
		if (XCheckTypedWindowEvent(display, dst, NoExpose, &event))
			break;

		if (XCheckTypedWindowEvent(display, dst,
			GraphicsExpose, &event))
		{
			if (!locGc)
			{
				locGc = XCreateGC(display, dst,
					0, (XGCValues *) NULL);

				XSetForeground(display, locGc,
					BlackPixelOfScreen(
					DefaultScreenOfDisplay(display)));
			}

			XFillRectangle(display, dst, locGc,
				event.xgraphicsexpose.x,
				event.xgraphicsexpose.y,
				(unsigned int) event.xgraphicsexpose.width,
				(unsigned int) event.xgraphicsexpose.height);

			if (event.xgraphicsexpose.count == 0)
			{
				XFreeGC(display, locGc);
				break;
			}
		}
	}

	if (!values.graphics_exposures)
		XSetGraphicsExposures(display, gc, False);
}
