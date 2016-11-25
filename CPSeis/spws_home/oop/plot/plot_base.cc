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
#include "wproc.h"
#include "plot/plot_base.hh"


Bool PlotBase::_watchOK = True;

Display *PlotBase::getDisplay()
{
    Widget w = getWidget();
    return (w ? XtDisplay(w) : (Display *) NULL);
}

Drawable PlotBase::getDrawable()
{
   Widget w = getWidget();
   return (w ? (Drawable) XtWindow(w) : (Drawable) 0);
}

Colormap PlotBase::getColormap()
{
   Widget w = getWidget();
   if (w) {
	Colormap colormap;
	XtVaGetValues(w, XmNcolormap, &colormap, NULL);
        return colormap;
   }
   else {
        return (Colormap) 0;
   }
}

void PlotBase::getClipArea(int *x, int *y, int *width, int *height)
{
   getExposedArea(x, y, width, height);
}

void PlotBase::getVisibleArea(int *x, int *y, int *width, int *height)
{
   Widget w = getWidget();
   if (w) wpGetVisArea(w, x, y, width, height);
   else   *x = *y = *width = *height = 0;
}

void PlotBase::getExposedArea(int *x, int *y, int *width, int *height)
{
	Drawable drawable = getDrawable();

	if (drawable)
	{
		XWindowAttributes attr;

		switch (_backingStore)
		{
			/*
			 * Unknown BS, check it and recursively call yourself.
			 */
			case unknownBS:
				checkBackingStore(drawable);
				getExposedArea(x, y, width, height);
				break;
			/*
			 * No BS, same exposed area same as visible area.
			 */
			case noBS     :
				getVisibleArea(x, y, width, height);
				break;
			/*
			 * Using BS, treat whole window as exposed.
			 */
			case usingBS  :
				*x = *y = 0;
				assert(XGetWindowAttributes(getDisplay(),
					(Window) drawable, &attr));
				*width  = attr.width ;
				*height = attr.height;
				break;
			default:
				assert(False);
		}
	}
	else
	{
		*x = *y = *width = *height = 0;
	}
}

void PlotBase::getVisibleClipArea(float *x1, float *y1, 
                                          float *x2, float *y2)
{
   int xVis, yVis, widthVis, heightVis;
   int xClp, yClp, widthClp, heightClp;

   getVisibleArea(&xVis, &yVis, &widthVis, &heightVis);
   getClipArea   (&xClp, &yClp, &widthClp, &heightClp);

   int x = (xVis > xClp) ? xVis : xClp;
   int y = (yVis > yClp) ? yVis : yClp;
   int width  = (xVis + widthVis  < xClp + widthClp )
                     ? widthVis  + xVis - x : widthClp  + xClp - x;
   int height = (yVis + heightVis < yClp + heightClp)
                     ? heightVis + yVis - y : heightClp + yClp - y;

   *x1= xWC(x);
   *y1= yWC(y);
   *x2= xWC(x+width);
   *y2= yWC(y+height);
}

void PlotBase::getSize(int *width, int *height)
{
	XWindowAttributes attr;
	Drawable drawable = getDrawable();

	if (drawable)
	{
		assert(XGetWindowAttributes(getDisplay(), (Window) drawable,
			&attr));
		*width  = attr.width ;
		*height = attr.height;
	}
	else
	{
		*width = *height = 0;
	}
}

GC PlotBase::getScratchGC()
{
  if (!_scratchGC) {
       _scratchGCDisplay = getDisplay();
       assert(_scratchGCDisplay);
       _scratchGC = XCreateGC(_scratchGCDisplay, 
                              getDrawable(), 0, (XGCValues *) NULL);
  }
  return _scratchGC;
}

void PlotBase::checkBackingStore(Drawable drawable)
{
	assert((unknownBS == _backingStore) && drawable);

	XWindowAttributes attr;

	assert(XGetWindowAttributes(getDisplay(), (Window) drawable, &attr));

	switch (attr.backing_store)
	{
		case NotUseful :
			_backingStore = noBS;
			break;
		case WhenMapped:
		case Always    :
			switch (DoesBackingStore(attr.screen))
			{
				case NotUseful :
					_backingStore = noBS   ;
					break;
				case WhenMapped:
				case Always    :
					_backingStore = usingBS;
					break;
				default:
					assert(False);
			}
			break;
		default:
			assert(False);
	}
}

Bool PlotBase::hasBackingStore()
{
	Bool retval;
	Drawable drawable;

	switch (_backingStore)
	{
		case unknownBS:
			drawable = getDrawable();
			assert(drawable);
			checkBackingStore(drawable);
			retval = hasBackingStore();	/* recursive */
			break;
		case noBS     :
			retval = False;
			break;
		case usingBS  :
			retval = True ;
			break;
		default:
			assert(False);
	}

	return retval;
}

Pixel PlotBase::getImageBackgroundPixel()
{
	Pixel retval;

	Widget widget = getWidget();

	if (widget)
		XtVaGetValues(widget,
			XmNbackground, &retval,
			NULL);
	else
		retval = (Pixel) 0;

	return retval;
}

Pixel PlotBase::getImageForegroundPixel()
{
	Pixel retval;

	Widget widget = getWidget();

	if (widget)
		XtVaGetValues(widget,
			XmNforeground, &retval,
			NULL);
	else
		retval = (Pixel) 0;

	return retval;
}

void PlotBase::setBS(Bool bs)
{
	if (bs)
		_backingStore = usingBS;
	else
		_backingStore = noBS   ;
}

Bool PlotBase::watchOK()
{
	return _watchOK;
}

void PlotBase::setWatchOK(Bool set)
{
	_watchOK = set;
}

void PlotBase::MM2WC(float *x, float *y)
{
	/*
	 * Assumes world coordinate system is linear.
	 */
	Widget w = getWidget();
	assert(w);

	Screen *screen = XtScreen(w);

	int xDC_min, yDC_min, xDC_width, yDC_height;
	getExposedArea(&xDC_min, &yDC_min, &xDC_width, &yDC_height);

	float  width_mm = (float) xDC_width  / (float) WidthOfScreen (screen)
		* (float) WidthMMOfScreen (screen);

	float height_mm = (float) yDC_height / (float) HeightOfScreen(screen)
		* (float) HeightMMOfScreen(screen);

	float xWC_min = xWC(xDC_min);
	/*
	 * If I substracted 1 from xDC_min + xDC_width,
	 * xWC_max would be the distance from the middle of the 1st pixel
	 * to the middle of the last pixel.  I think the WidthMMOfScreen
	 * is from the left of the 1st pixel to the right of the last
	 * pixel, so not subtracting 1 is more correct.
	 */
	float xWC_max = xWC(xDC_min + xDC_width );

	*x = (xWC_max - xWC_min) /  width_mm;
	*x = (*x < 0.0F) ? *x = -(*x) : *x;

	float yWC_min = yWC(yDC_min);
	float yWC_max = yWC(yDC_min + yDC_height);

	*y = (yWC_max - yWC_min) / height_mm;
	*y = (*y < 0.0F) ? *y = -(*y) : *y;
}
