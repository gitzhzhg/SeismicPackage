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

typedef struct {
	int type;
	Window window;
	int count;
} wpMoreEventsStruct;

static Bool wpMoreEventsPredicate(Display *display, XEvent *event,
#if XtSpecificationRelease == 5
	XPointer arg)
#else
	char    *arg)
#endif
{
	wpMoreEventsStruct *more = (wpMoreEventsStruct *) arg;

	if (more->type == event->type && (more->window == event->xany.window
		                       || more->window == (Window) NULL))
	{
		more->count++;
	}

	return False;
}

int wpMoreEvents(Display *display, Window window, int type)
{
	XEvent junkEvent;
	wpMoreEventsStruct more;

	more.type   = type  ;
	more.window = window;
	more.count  = 0     ;

	XCheckIfEvent(display, &junkEvent, wpMoreEventsPredicate,
#if XtSpecificationRelease == 5
		(XPointer) &more);
#else
		(char   *) &more);
#endif

	if (more.count == 0)
		return False;
	else
		return True ;
}
