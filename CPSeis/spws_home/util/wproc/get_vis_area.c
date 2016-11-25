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
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>
#include <assert.h>

/*
 * Not in scrolled window or in scrolled window in unsupported manner.
 */
static void getVisible(Widget da, int *x, int *y, int *width, int *height)
{
	Dimension widthDa, heightDa;

	XtVaGetValues(da,
		XmNwidth , &widthDa ,
		XmNheight, &heightDa,
		NULL);

	*x = 0;
	*y = 0;
	*width  = ((int) widthDa  < WidthOfScreen (XtScreen(da))) ?
			(int) widthDa  : WidthOfScreen (XtScreen(da));
	*height = ((int) heightDa < HeightOfScreen(XtScreen(da))) ?
			(int) heightDa : HeightOfScreen(XtScreen(da));
}

/*
 * In scrolled window with no clip window.
 */
static void getVisibleNoCW(Widget da, Widget sw, int *x, int *y,
	int *width, int *height)
{
	Position xDa, yDa;
	Dimension widthDa, heightDa, widthSw, heightSw;

	XtVaGetValues(da,
		XmNx     , &xDa     ,
		XmNy     , &yDa     ,
		XmNwidth , &widthDa ,
		XmNheight, &heightDa,
		NULL);

	assert(xDa == 0 && yDa == 0);

	XtVaGetValues(sw,
		XmNwidth , &widthSw ,
		XmNheight, &heightSw,
		NULL);

	*x = 0;
	*y = 0;
	*width  = (int) ((widthDa  < widthSw ) ? widthDa  : widthSw );
	*height = (int) ((heightDa < heightSw) ? heightDa : heightSw);
}

/*
 * In scrolled window with clip window.
 */
static void getVisibleCW(Widget da, Widget cw, int *x, int *y,
	int *width, int *height)
{
	Position xDa, yDa;
	Dimension widthDa, heightDa, widthCw, heightCw;

	XtVaGetValues(da,
		XmNx     , &xDa     ,
		XmNy     , &yDa     ,
		XmNwidth , &widthDa ,
		XmNheight, &heightDa,
		NULL);

	XtVaGetValues(cw,
		XmNwidth , &widthCw ,
		XmNheight, &heightCw,
		NULL);

	*x = (int) -xDa;
	*y = (int) -yDa;
	*width  = (int) ((widthDa  < widthCw ) ? widthDa  : widthCw );
	*height = (int) ((heightDa < heightCw) ? heightDa : heightCw);
}

void wpGetVisArea(Widget da, int *x, int *y, int *width, int *height)
{
	int up;
	Widget sw, cw;

	if (XtWindow(da))
	{
		assert(XtIsSubclass(da, xmDrawingAreaWidgetClass));

		for (up = 1, sw = XtParent(da);
			sw != (Widget) NULL;
			up++, sw = XtParent(sw))
		{
			if (XtIsSubclass(sw, xmScrolledWindowWidgetClass))
				break;
		}

		if (sw == (Widget) NULL)
			up = 0;

		switch (up)
		{
			case 0:
				getVisible(da, x, y, width, height);
				break;
			case 1:
				getVisibleNoCW(da, sw, x, y, width, height);
				break;
			case 2:
				XtVaGetValues(sw,
					XmNclipWindow, &cw,
					NULL);
				if (cw == XtParent(da))
				{
					getVisibleCW(da, cw, x, y,
						width, height);
					break;	/* break is in if block */
				}
			default:
				getVisible(da, x, y, width, height);
				break;
		}
	}
	else
	{
		*x      = 0;
		*y      = 0;
		*width  = 0;
		*height = 0;
	}
}
