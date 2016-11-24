/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* MISC: $Revision: 1.6 $ ; $Date: 2011/11/21 17:05:31 $	*/

/*********************** self documentation **********************/
/*****************************************************************************
MISC - Miscellaneous X-Toolkit functions

XtcwpDrawString90	Draw a string rotated 90 degrees counter-clockwise

******************************************************************************
Function Prototype:
void XtcwpDrawString90 (Display *dpy, Drawable d, GC gc,
	int x, int y, char *string, int count);

******************************************************************************
Input:
dpy		X display
d		X drawable
gc		X graphics context
x,y		coordinates of baseline starting position of the string
string		array[count] of characters to be drawn
count		number of characters in string

******************************************************************************
Author:  Dave Hale, Advance Geophysical, 06/03/93
*****************************************************************************/
/**************** end self doc ********************************/


#include <Xtcwp/Xtcwp.h>


/* An X error handler that does nothing */
static int doNothingErrorHandler (Display* dpy, XErrorEvent* event)
{
	if(event - event) doNothingErrorHandler(dpy,event); /* keep compiler happy */
	return EXIT_SUCCESS;

}

void XtcwpDrawString90 (Display *dpy, Drawable d, GC gc,
	int x, int y, char *string, int count)
/*****************************************************************************
Draw a string rotated 90 degrees counter-clockwise
******************************************************************************
Input:
dpy		X display
d		X drawable
gc		X graphics context
x,y		coordinates of baseline starting position of the string
string		array[count] of characters to be drawn
count		number of characters in string
******************************************************************************
Author:  Dave Hale, Advance Geophysical, 06/03/93
*****************************************************************************/
{
	int sa,sd,sw,sh,rw,rh,i,j;
	unsigned int width,height,depth;
	XFontStruct *fs;
	XCharStruct cs;
	Pixmap hpix;
	XImage *himage,*vimage;
	int idummy;
	unsigned int uidummy;
	Window wdummy;
	int (*eh)(Display*,XErrorEvent*);

	/* If count is too small */
	if (count<1) return;

	/* Width, height, and depth of drawable */
	XGetGeometry(dpy,d,&wdummy,&idummy,&idummy,&width,&height,
		&uidummy,&depth);

	/* String width and height before and after rotation */
	fs = XQueryFont(dpy,XGContextFromGC(gc));
	XTextExtents(fs,string,count,&idummy,&idummy,&idummy,&cs);
	sa = cs.ascent;
	sd = cs.descent;
	sw = cs.width;
	sh = cs.ascent+cs.descent;
	rw = sh;
	rh = sw;

	/* If rotated string will not be entirely inside drawable, return */
	if (x-sa<0 || x+sd>width-1 || y-sw<0 || y>height-1) {
		XFreeFontInfo(NULL,fs,1);
		return;
	}

	/* Set error handler to catch BadMatch errors from GetImage,
	 * if Drawable is a Window, when rotated string would not
	 * be viewable or entirely on the screen.  No way to prevent
	 * this error, since we can't tell if the Drawable is a Pixmap 
	 * or a Window. */
	eh = XSetErrorHandler(doNothingErrorHandler);

	/* Get vertical XImage from Drawable */
	vimage = XGetImage(dpy,d,x-sa,y-sw,rw,rh,AllPlanes,ZPixmap);

	/* Restore previous error handler */
	XSetErrorHandler(eh);

	/* If any problem in XGetImage, free font info and return */
	if (vimage==NULL) {
	  XFreeFontInfo(NULL,fs,1);
	  return;
	}

	/* Rotate vertical image to horizontal Pixmap */
	hpix = XCreatePixmap(dpy,d,sw,sh,depth);
	himage = XGetImage(dpy,hpix,0,0,sw,sh,AllPlanes,ZPixmap);
	for (i=0; i<sw; ++i)
		for (j=0; j<sh; ++j)
			XPutPixel(himage,i,j,XGetPixel(vimage,j,sw-1-i));
	XPutImage(dpy,hpix,gc,himage,0,0,0,0,sw,sh);

	/* Draw string in horizontal Pixmap */
	XDrawString(dpy,hpix,gc,0,sa,string,count);

	/* Rotate horizontal Pixmap to vertical XImage */
	XDestroyImage(himage);
	himage = XGetImage(dpy,hpix,0,0,sw,sh,AllPlanes,ZPixmap);
	XFreePixmap(dpy,hpix);
	for (i=0; i<sw; ++i)
		for (j=0; j<sh; ++j)
			XPutPixel(vimage,j,sw-1-i,XGetPixel(himage,i,j));
	XDestroyImage(himage);

	/* Put vertical XImage to Drawable */
	XPutImage(dpy,d,gc,vimage,0,0,x-sa,y-sw,rw,rh);
	XDestroyImage(vimage);

	/* Free font information */
	XFreeFontInfo(NULL,fs,1);
}
