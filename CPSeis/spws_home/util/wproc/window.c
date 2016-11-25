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
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "window_p.h"
#include <string.h>		/* ehs   07Jan94 */

static void Realize(Widget, XtValueMask*, XSetWindowAttributes *);
static Boolean SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static void GetValuesHook(Widget, ArgList, Cardinal *);	/* ehs   07Jan94 */

static void SetSize(WindowWidget);		/* ehs   07Jan94 */

static XtResource resources[] = {
#define offset(field) XtOffset(WindowWidget, window.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
    { XiNwindowID, XiCWindowID, XtRWindow, sizeof(Window),
      offset(window_id), XtRImmediate, 0 },
#undef offset
};

#define WINDOW_CLASS_REC { \
  { /* core fields */ \
    /* superclass             */        (WidgetClass) &widgetClassRec, \
    /* class_name             */        "Window", \
    /* widget_size            */        sizeof(WindowRec), \
    /* class_initialize       */        NULL, \
    /* class_part_initialize  */        NULL, \
    /* class_inited           */        FALSE, \
    /* initialize             */        NULL, \
    /* initialize_hook        */        NULL, \
    /* realize                */        Realize, \
    /* actions                */        NULL, \
    /* num_actions            */        0, \
    /* resources              */        resources, \
    /* num_resources          */        XtNumber(resources), \
    /* xrm_class              */        NULLQUARK, \
    /* compress_motion        */        TRUE, \
    /* compress_exposure      */        TRUE, \
    /* compress_enterleave    */        TRUE, \
    /* visible_interest       */        FALSE, \
    /* destroy                */        NULL, \
    /* resize                 */        NULL, \
    /* expose                 */        NULL, \
    /* set_values             */        SetValues, \
    /* set_values_hook        */        NULL, \
    /* set_values_almost      */        XtInheritSetValuesAlmost, \
    /* get_values_hook        */        GetValuesHook, /* ehs   07Jan94 */ \
    /* accept_focus           */        NULL, \
    /* version                */        XtVersion, \
    /* callback_private       */        NULL, \
    /* tm_table               */        NULL, \
    /* query_geometry         */        XtInheritQueryGeometry, \
    /* display_accelerator    */        XtInheritDisplayAccelerator, \
    /* extension              */        NULL \
  }, \
  { /* window fields          */ \
    /* empty                  */        0 \
  } \
}

#ifndef VMS
WindowClassRec windowClassRec = WINDOW_CLASS_REC;
WidgetClass xiWindowWidgetClass = (WidgetClass)&windowClassRec;
#else
#ifdef __GNUC__
GLOBALDEF(WindowClassRec, windowClassRec, WINDOW_CLASS_REC);
GLOBALDEF(WidgetClass, xiWindowWidgetClass, (WidgetClass)&windowClassRec);
#else
externaldef(windowclassrec) WindowClassRec windowClassRec = WINDOW_CLASS_REC;
externaldef(xiwindowwidgetclass) WidgetClass xiWindowWidgetClass
	= (WidgetClass)&windowClassRec;
#endif /* __GNUC__ */
#endif /* VMS */

static void
Realize (Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
    WindowWidget windoww = (WindowWidget) w;

    windoww->core.window = windoww->window.window_id;

    SetSize(windoww);		/* ehs   07Jan94 */
}

static Boolean
SetValues(Widget current, Widget request, Widget new,
      ArgList args, Cardinal * num_args)
{
    WindowWidget old_ww = (WindowWidget) current;
    WindowWidget new_ww = (WindowWidget) new;

    if (new_ww->window.window_id != (Window) None)
    {
    new_ww->window.window_id = old_ww->window.window_id;
    XtAppWarningMsg(XtWidgetToApplicationContext(new),
            "Bad Value",
            NULL,
            "xiWindowWidget",
            "XmNwindow resource may only be set to NULL.",
            NULL, 0);
    }
    new_ww->core.window = new_ww->window.window_id;

    SetSize(new_ww);		/* ehs   07Jan94 */

    return(False); /* we NEVER draw ourselves */
}

/* ehs   07Jan94 */
static void GetValuesHook(Widget w, ArgList args, Cardinal *argcnt)
{
	int i;

	SetSize((WindowWidget) w);

	for (i = 0; i < (int) *argcnt; i++)
	{
		if      (!strcmp(args[i].name, XmNx))
		{
			*((Position  *) args[i].value) = w->core.x;
		}
		else if (!strcmp(args[i].name, XmNy))
		{
			*((Position  *) args[i].value) = w->core.y;
		}
		else if (!strcmp(args[i].name, XmNwidth))
		{
			*((Dimension *) args[i].value) = w->core.width;
		}
		else if (!strcmp(args[i].name, XmNheight))
		{
			*((Dimension *) args[i].value) = w->core.height;
		}
		else if (!strcmp(args[i].name, XmNborderWidth))
		{
			*((Dimension *) args[i].value) = w->core.border_width;
		}
		else if (!strcmp(args[i].name, XmNdepth))
		{
			*((int       *) args[i].value) = (int) w->core.depth;
		}
	}
}

/* ehs   07Jan94 */
static void SetSize(WindowWidget w)
{
	Window dummyRoot;
	int localX, localY;
	unsigned int localWidth, localHeight, localBwidth, localDepth;
 
	XGetGeometry(XtDisplay(w),  w->core.window,
		&dummyRoot, &localX, &localY,
		&localWidth, &localHeight, &localBwidth, &localDepth);

	w->core.x		= (Position ) localX     ;
	w->core.y		= (Position ) localY     ;
	w->core.width		= (Dimension) localWidth ;
	w->core.height		= (Dimension) localHeight;
	w->core.border_width	= (Dimension) localBwidth;
	w->core.depth		= (Cardinal ) localDepth ;
}
