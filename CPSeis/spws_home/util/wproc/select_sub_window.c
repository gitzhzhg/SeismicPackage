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
/*
 * Modifed from $XConsortium: dsimple.c,v 1.12 91/05/11 21:00:35 gildea Exp $
 * ehs   10-Jan-94
 */

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <assert.h>

/*
 * Routine to let user select a window using the mouse
 */

Window Select_Window_With_Pos(dpy, retX, retY)
     Display *dpy;
     int *retX, *retY;
{
  int status;
  Cursor cursor;
  XEvent event;
/*Window target_win = None, root = RootWindow(dpy,screen); */
  Window target_win = None, root = RootWindow(dpy, DefaultScreen(dpy));
  int buttons = 0;
  Window getRoot;
  int x=0, y=0;
  int getX, getY;
  unsigned int getWidth, getHeight, getBwidth, getDepth;

  /* Make the target cursor */
  cursor = XCreateFontCursor(dpy, XC_crosshair);

  /* Grab the pointer using target cursor, letting it room all over */
  status = XGrabPointer(dpy, root, False,
			ButtonPressMask|ButtonReleaseMask, GrabModeSync,
			GrabModeAsync, root, cursor, CurrentTime);
/*if (status != GrabSuccess) Fatal_Error("Can't grab the mouse."); */
  assert(status == GrabSuccess);

  /* Let the user select a window... */
  while ((target_win == None) || (buttons != 0)) {
    /* allow one more event */
    XAllowEvents(dpy, SyncPointer, CurrentTime);
    XWindowEvent(dpy, root, ButtonPressMask|ButtonReleaseMask, &event);
    switch (event.type) {
    case ButtonPress:
      if (target_win == None) {
	target_win = event.xbutton.subwindow; /* window selected */
	if (target_win == None) target_win = root;
	x = event.xbutton.x;
	y = event.xbutton.y;
      }
      buttons++;
      break;
    case ButtonRelease:
      if (buttons > 0) /* there may have been some down before we started */
	buttons--;
       break;
    }
  } 

  XUngrabPointer(dpy, CurrentTime);      /* Done with pointer */

  if (target_win == root)
  {
    *retX = x;
    *retY = y;
  }
  else
  {
    XGetGeometry(dpy, target_win, &getRoot, &getX, &getY, &getWidth,
      &getHeight, &getBwidth, &getDepth);
    *retX = x - getX;
    *retY = y - getY;
  }

  return(target_win);
}

Window Find_Child(Display *dpy, Window window, int x, int y)
{
	Window root, parent, getRoot;
	Window *children;
	int i, getX, getY;
	unsigned int num_children, getWidth, getHeight, getBwidth, getDepth;
	Window retWindow = window;

	XQueryTree(dpy, window, &root, &parent, &children,
		&num_children);

	/* Should not need this, but strange things sometimes happen. */
	if (window == root)
		return(window);
	
	for (i = 0; i < (int) num_children; i++)
	{
		XGetGeometry(dpy, children[i], &getRoot, &getX, &getY,
			&getWidth, &getHeight, &getBwidth, &getDepth);

		if (x >= getX && x < getX + (int) getWidth &&
		    y >= getY && y < getY + (int) getHeight)
		{
			retWindow = Find_Child(dpy, children[i],
				x - getX, y - getY);
			break;
		}
	}

	if (num_children)
		XFree((char *) children);

	return(retWindow);
}

Window Select_Sub_Window(Display *dpy)
{
	int x, y;

	Window parent = Select_Window_With_Pos(dpy, &x, &y);

	return(Find_Child(dpy, parent, x, y));
}
