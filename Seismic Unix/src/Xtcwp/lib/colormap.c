/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* COLORMAP: $Revision: 1.11 $ ; $Date: 2011/11/21 17:05:31 $	*/

/*********************** self documentation **********************/
/*****************************************************************************
COLORMAP - Functions to manipulate X colormaps:

XtcwpCreateRGBDefaultMap	create XA_RGB_DEFAULT_MAP property of root
				window if it does not already exist
XtcwpGetFirstPixel		return first pixel in range of contiguous
				pixels in XA_RGB_DEFAULT_MAP
XtcwpGetLastPixel		return last pixel in range of contiguous
				pixels in XA_RGB_DEFAULT_MAP
XtcwpCreateRGBColormap	create a colormap with an RGB color scale in
			contiguous cells
XtcwpCreateGrayColormap	create a colormap with a gray scale in contiguous cells
XtcwpCreateHueColormap	create a colormap with varying hues (blue to red)
			in contiguous cells
XtcwpCreateSatColormap	create a colormap with varying saturations in
			contiguous cells

******************************************************************************
Function Prototypes:
Status XtcwpCreateRGBDefaultMap (Display *dpy, XStandardColormap *scmap);
unsigned long XtcwpGetFirstPixel (Display *dpy);
unsigned long XtcwpGetLastPixel (Display *dpy);
Colormap XtcwpCreateRGBColormap (Display *dpy, Window win);
Colormap XtcwpCreateGrayColormap (Display *dpy, Window win);
Colormap XtcwpCreateHueColormap (Display *dpy, Window win);
Colormap XtcwpCreateSatColormap (Display *dpy, Window win,
	float fhue, float lhue, float wfrac, float bright)

******************************************************************************
XtcwpCreateRGBDefaultMap:
Input:
dpy		display

Output:
scmap		the standard colormap structure

******************************************************************************
XtcwpGetFirstPixel, XtcwpGetLastPixel:
Input:
dpy		display

******************************************************************************
XtcwpCreateRGBColormap, XtcwpCreateGrayColormap, XtcwpCreateHueColormap:
Input:
dpy		display
win		window

******************************************************************************
XtcwpCreateSatColormap:
Input:
dpy		display
win		window
fhue		first hue in colormap (saturation=1)
lhue		last hue in colormap (saturation=1)
wfrac		fractional position of white within the colormap (saturation=0)
bright		brightness

******************************************************************************
Notes:
PROBLEM
-------

Most mid-range display devices today support what X calls
the "PseudoColor visual".  Typically, only 256 colors (or gray
levels) may be displayed simultaneously.  Although these 256 colors
may be chosen from a much larger (4096 or more) set of available
colors, only 256 colors can appear on a display at one time.

These 256 colors are indexed by pixel values in a table called
the colormap.  Each window can have its own colormap, but only
one colormap can be installed in the display hardware at a time.
(Again, only 256 colors may be displayed at one time.)  The window 
manager is responsible for installing a window's colormap when that 
window becomes the key window.

Many of the applications we are likely to write require a large,
contiguous range of pixels (entries in the colormap).  In this
range, we must be able to:
(1) given a color (or gray), determine the corresponding pixel.
(2) given a pixel, determine the corresponding color (or gray).
An example would be an imaging application that uses a gray scale
to display images in shades of gray between black and white.
Such applications are also likely to require a few additional colors
for drawing axes, text, etc.

The problem is to coordinate the use of the limited number of
256 simultaneous colors so that windows for different applications 
appear reasonable, even when their particular colormaps are not
installed in the display hardware.  For example, we might expect 
an analog xclock's hands to be visible even when xclock's window
is not the key window, when its colormap is not installed.

We should ensure that the range of contiguous pixels used by one
application (perhaps for imaging) does not conflict with the pixels
used by other applications to draw text, clock hands, etc.


SOLUTION
--------

Applications that do not require special colormaps should simply
use the default colormap inherited from the root window when new
top-level windows are created.

Applications that do require a special colormap MUST create their
own colormap.  They must not assume that space will be available
in the default colormap for a contiguous range of read/write pixels,
because the server or window manager may have already allocated
these pixels as read-only.  Even if sufficient pixels are available
in the default colormap, they should not be allocated by a single
application.  The default colormap should be used only for windows
requiring a limited number of typical colors, such as red, yellow, etc.

Applications that require a contiguous range of read/write pixels
should allocate these pixels in their window's private colormaps.
They should determine which contiguous pixels to allocate from 
parameters in the standard colormap XA_RGB_DEFAULT_MAP.  In particular,
the first pixel in the range of contiguous pixels should be 
	base_pixel
and the last pixel in the range should be 
	base_pixel+red_max*red_mult+green_max*green_mult+blue_max*blue_mult,
where base_pixel, red_max, etc. are members in the XStandardColormap
structure.  On an 8-bit display, this range will typically provide 216
contiguous pixels, which may be set to a gray scale, color scale, or
whatever.  This leaves 40 colors for drawing text, axes, etc.

If the XA_RGB_DEFAULT_MAP does not exist, it should be created to 
consist of various colors composed of an equal number of reds, 
greens, and blues.  For example, if 216 colors are to be allocated,
then red_max=green_max=blue_max=5, red_mult=36, green_mult=6, and
blue_mult=1.  Because of the difficulty in forcing a particular 
pixel to correspond to a particular color in read-only color cells,
these 216 colors will likely be read/write color cells unless
created by the X server.  In any case, these 216 colors should not
be modified by any application.  In creating custom colormaps, the
only use of XA_RGB_DEFAULT_MAP should be in determining which 216
pixels to allocate for contiguous pixels.

In creating a custom colormap for a window, the application should
initialize this colormap to the colors already contained in the
window's colormap, which was inherited initially from its parent.
This will ensure that typical colors already allocated by other
applications will be consistent with pixels used by the application
requiring the custom colormap.  Ideally, windows might have
different colormaps, but the only differences would be in the
range of contiguous colors used for imaging, rendering, etc.
Ideally, the pixels corresponding to colors used to draw text, 
axes, etc. would be consistent for all windows.

Unfortunately, it is impractical to maintain complete consistency 
among various private colormaps.  For example, suppose a custom
colormap is created for a window before other applications have
had the opportunity to allocate their colors from the default
colormap.  Then, when the window with the custom colormap becomes
the key window, the windows of the other applications may be 
displayed with false colors, since the colormap of the key window
may not contain the true colors.  The colors used by the other 
applications did not exist when the custom colormap was created.
One solution to this problem might be to initially allocate a
set of "common" colors in the default colormap before launching
any applications.  This will increase the likelihood that typical
colors will be consistent among various colormaps.

Functions are provided below to
(1) create the standard colormap XA_RGB_DEFAULT_MAP, if it does not exist,
(2) determine the first and last pixels in the contiguous range of pixels,
(3) create some common private colormaps - gray scale, hue scale, etc.

XtcwpCreateRGBDefaultMap:
This function returns 0 if the XA_RGB_DEFAULT_MAP property does not exist
and cannot be created.  At least 8 contiguous color cells must be free
in the default colormap to create the XA_RGB_DEFAULT_MAP.  If created, the
red_max, green_max, and blue_max values returned in scmap will be equal.

XtcwpGetFirstPixel, XtcwpGetLastPixel:
If it does not already exist, XA_RGB_DEFAULT_MAP will be created.
If XA_RGB_DEFAULT_MAP does not exist and cannot be created, then
this function returns 0.

XtcwpCreateRGBColormap, XtcwpCreateGrayColormap, XtcwpCreateHueColormap,
XtcwpCreateSatColormap:
The returned colormap is only created; the window's colormap attribute
is not changed, and the colormap is not installed by this function.
The returned colormap is a copy of the window's current colormap, but 
with an RGB color scale allocated in the range of contiguous cells
determined by XA_RGB_DEFAULT_MAP.  If it does not already exist,
XA_RGB_DEFAULT_MAP will be created.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/30/90
*****************************************************************************/
/**************** end self doc ********************************/

#include "Xtcwp/Xtcwp.h"

/* function defined and used internally */
static void hsvrgb (float h, float s, float v, float *r, float *g, float *b);

Status XtcwpCreateRGBDefaultMap (Display *dpy, XStandardColormap *scmap)
/*****************************************************************************
create XA_RGB_DEFAULT_MAP property of root window if it does not already exist
******************************************************************************
Input:
dpy		display

Output:
scmap		the standard colormap structure
******************************************************************************
Notes:
This function returns 0 if the XA_RGB_DEFAULT_MAP property does not exist
and cannot be created.  At least 8 contiguous color cells must be free
in the default colormap to create the XA_RGB_DEFAULT_MAP.  If created, the
red_max, green_max, and blue_max values returned in scmap will be equal.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/29/90
*****************************************************************************/
{
	Screen *scr=XDefaultScreenOfDisplay(dpy);
	Window root=XRootWindowOfScreen(scr);
	Colormap cmap;
	XColor color;
	int i,ncells;
	unsigned long npixels;
	unsigned long bpixel,epixel,pixel1,pixel2,imax,rmult,gmult,bmult;
	unsigned long pixel[4096];

	/* grab the server */
	XGrabServer(dpy);

	/* if XA_RGB_DEFAULT_MAP does not exist, then */
	if (!XGetStandardColormap(dpy,root,scmap,XA_RGB_DEFAULT_MAP)) {
		
		/* use default colormap */
		cmap = DefaultColormapOfScreen(scr);

		/* determine largest number of contiguous free color cells */
		ncells = CellsOfScreen(scr);
		while(ncells && 
			!XAllocColorCells(dpy,cmap,True,NULL,0,pixel,ncells))
			ncells--;
		
		/* determine beginning and ending pixel of contiguous cells */
		for (i=1,bpixel=epixel=pixel1=pixel2=pixel[0]; i<ncells; i++) {
			if (pixel[i]==pixel[i-1]+1)
				pixel2 = pixel[i];
			else
				pixel1 = pixel2 = pixel[i];
			if (pixel2-pixel1>=epixel-bpixel) {
				bpixel = pixel1;
				epixel = pixel2;
			}
		}
		
		/* number of pixels must be at least 8 */
		npixels = epixel-bpixel+1;
		if (npixels<8) {
			XUngrabServer(dpy);
			return 0;
		}
		
		/* force number of contiguous cells to be an integer cubed */
		for (i=2,imax=0; i*i*i<=npixels; i++,imax++);
		npixels = (imax+1)*(imax+1)*(imax+1);
		bpixel = epixel-npixels+1;
		
		/* free cells not in contiguous range */
		for (i=0; i<ncells; i++)
			if (pixel[i]<bpixel || pixel[i]>epixel)
				XFreeColors(dpy,cmap,&pixel[i],1,0);

		/* store colors in contiguous range of allocated cells */
		rmult = (imax+1)*(imax+1);
		gmult = imax+1;
		bmult = 1;
		for (i=0; i<npixels; i++) {
			color.pixel = bpixel+i;
			color.red = (unsigned short) (i/rmult);
			color.green = (unsigned short) ((i-color.red*rmult)/gmult);
			color.blue = (unsigned short) (i-color.red*rmult-color.green*gmult);
			color.red *= 65535/imax;
			color.green *= 65535/imax;
			color.blue *= 65535/imax;
			color.flags = DoRed|DoGreen|DoBlue;
			XStoreColor(dpy,cmap,&color);
		}
		
		/* set standard colormap */
		scmap->colormap = cmap;
		scmap->red_max = imax;
		scmap->green_max = imax;
		scmap->blue_max = imax;
		scmap->red_mult = rmult;
		scmap->green_mult = gmult;
		scmap->blue_mult = bmult;
		scmap->base_pixel = bpixel;
		XSetStandardColormap(dpy,root,scmap,XA_RGB_DEFAULT_MAP);
	}

	/* ungrab the server before returning */
	XUngrabServer(dpy);
	return 1;
}

unsigned long XtcwpGetFirstPixel (Display *dpy)
/*****************************************************************************
return first pixel in range of contiguous pixels in XA_RGB_DEFAULT_MAP
******************************************************************************
Input:
dpy		display
******************************************************************************
Notes:
If it does not already exist, XA_RGB_DEFAULT_MAP will be created.
If XA_RGB_DEFAULT_MAP does not exist and cannot be created, then
this function returns 0.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/29/90
*****************************************************************************/
{
	Screen *scr=XDefaultScreenOfDisplay(dpy);
	Window root=XRootWindowOfScreen(scr);
	XStandardColormap scmap;
	
	/* if XA_RGB_DEFAULT_MAP does not exist, create it */
	if (!XGetStandardColormap(dpy,root,&scmap,XA_RGB_DEFAULT_MAP))
		if (!XtcwpCreateRGBDefaultMap(dpy,&scmap))
			return 0;
	
	/* return first pixel in range of contiguous pixels */
	return scmap.base_pixel; 
}

unsigned long XtcwpGetLastPixel (Display *dpy)
/*****************************************************************************
return last pixel in range of contiguous pixels in XA_RGB_DEFAULT_MAP
******************************************************************************
Input:
dpy		display
******************************************************************************
Notes:
If it does not already exist, XA_RGB_DEFAULT_MAP will be created.
If XA_RGB_DEFAULT_MAP does not exist and cannot be created, then
this function returns 0.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/29/90
*****************************************************************************/
{
	Screen *scr=XDefaultScreenOfDisplay(dpy);
	Window root=XRootWindowOfScreen(scr);
	XStandardColormap scmap;
	
	/* if XA_RGB_DEFAULT_MAP does not exist, create it */
	if (!XGetStandardColormap(dpy,root,&scmap,XA_RGB_DEFAULT_MAP))
		if (!XtcwpCreateRGBDefaultMap(dpy,&scmap))
			return 0;
	
	/* return last pixel in range of contiguous pixels */
	return scmap.base_pixel+
		scmap.red_max*scmap.red_mult+
		scmap.green_max*scmap.green_mult+
		scmap.blue_max*scmap.blue_mult;
}

Colormap XtcwpCreateRGBColormap (Display *dpy, Window win)
/*****************************************************************************
create a colormap with an RGB color scale in contiguous cells
******************************************************************************
Input:
dpy		display
win		window
******************************************************************************
Notes:
The returned colormap is only created; the window's colormap attribute
is not changed, and the colormap is not installed by this function.
The returned colormap is a copy of the window's current colormap, but 
with an RGB color scale allocated in the range of contiguous cells
determined by XA_RGB_DEFAULT_MAP.  If it does not already exist,
XA_RGB_DEFAULT_MAP will be created.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/29/90
*****************************************************************************/
{
	Screen *scr=XDefaultScreenOfDisplay(dpy);
	Window root=XRootWindowOfScreen(scr);
	Colormap cmap,wcmap;
	XStandardColormap scmap;
	XColor color;
	XWindowAttributes wa;
	unsigned long i,ncells,npixels;
	unsigned long bpixel,epixel,pixel[4096];
	
	/* determine beginning and ending pixels in contiguous range */
	bpixel = XtcwpGetFirstPixel(dpy);
	epixel = XtcwpGetLastPixel(dpy);
	if (epixel<=bpixel) return None;
	
	/* get standard colormap XA_RGB_DEFAULT_MAP */
	if (!XGetStandardColormap(dpy,root,&scmap,XA_RGB_DEFAULT_MAP))
		if (!XtcwpCreateRGBDefaultMap(dpy,&scmap))
			return None;
	
	/* determine window's current colormap */
	XGetWindowAttributes(dpy,win,&wa);
	wcmap = wa.colormap;
	
	/* create new colormap and allocate all cells read/write */
	cmap = XCreateColormap(dpy,win,DefaultVisualOfScreen(scr),AllocNone);
	ncells = CellsOfScreen(scr);
	XAllocColorCells(dpy,cmap,True,NULL,0,pixel,(unsigned int)ncells);
	
	/* copy color cells from window's colormap to new colormap */
	for (i=0; i<ncells; ++i) {
		if (i<bpixel || i>epixel) {
			color.pixel = i;
			XQueryColor(dpy,wcmap,&color);
			XFreeColors(dpy,cmap,&i,1,0);
			XAllocColor(dpy,cmap,&color);
		}
	}
	
	/* copy RGB color scale from XA_RGB_DEFAULT_MAP to new colormap */
	npixels = epixel-bpixel+1;
	for (i=0; i<npixels; ++i) {
		color.pixel = bpixel+i;
		XQueryColor(dpy,scmap.colormap,&color);
		XStoreColor(dpy,cmap,&color);
	}
	
	/* return colormap */
	return cmap;
}

Colormap XtcwpCreateGrayColormap (Display *dpy, Window win)
/*****************************************************************************
create a colormap with a gray scale in contiguous cells
******************************************************************************
Input:
dpy		display
win		window
******************************************************************************
Notes:
The returned colormap is only created; the window's colormap attribute
is not changed, and the colormap is not installed by this function.
The returned colormap is a copy of the window's current colormap, but 
with a gray scale (black to white) allocated in the range of contiguous
cells determined by XA_RGB_DEFAULT_MAP.  If it does not already exist,
XA_RGB_DEFAULT_MAP will be created.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/29/90
*****************************************************************************/
{
	Screen *scr=XDefaultScreenOfDisplay(dpy);
	Colormap cmap,wcmap;
	XColor color;
	XWindowAttributes wa;
	unsigned long i,ncells,npixels;
	unsigned long bpixel,epixel,pixel[4096];
	
	/* determine beginning and ending pixels in contiguous range */
	bpixel = XtcwpGetFirstPixel(dpy);
	epixel = XtcwpGetLastPixel(dpy);
	if (epixel<=bpixel) return None;
	
	/* determine window's current colormap */
	XGetWindowAttributes(dpy,win,&wa);
	wcmap = wa.colormap;
	
	/* create new colormap and allocate all cells read/write */
	cmap = XCreateColormap(dpy,win,DefaultVisualOfScreen(scr),AllocNone);
	ncells = CellsOfScreen(scr);
	XAllocColorCells(dpy,cmap,True,NULL,0,pixel,(unsigned int)ncells);
	
	/* copy color cells from window's colormap to new colormap */
	for (i=0; i<ncells; ++i) {
		if (i<bpixel || i>epixel) {
			color.pixel = i;
			XQueryColor(dpy,wcmap,&color);
			XFreeColors(dpy,cmap,&i,1,0);
			XAllocColor(dpy,cmap,&color);
		}
	}
	
	/* build gray scale in contiguous cells in new colormap */
	npixels = epixel-bpixel+1;
	for (i=0; i<npixels; ++i) {
		color.pixel = bpixel+i;
		color.red = (unsigned short) (65535*i/(npixels-1));
		color.green = color.red;
		color.blue = color.red;
		color.flags = DoRed|DoGreen|DoBlue;
		XStoreColor(dpy,cmap,&color);
	}
	
	/* return colormap */
	return cmap;
}

Colormap XtcwpCreateHueColormap (Display *dpy, Window win,
	float fhue, float lhue, float sat, float bright)
/*****************************************************************************
create a colormap with varying hues (user-specified) in contiguous cells
******************************************************************************
Input:
dpy		display
win		window
fhue		first hue in colormap
lhue		last hue in colormap
sat		saturation
bright		brightness
******************************************************************************
Notes:
The returned colormap is only created; the window's colormap attribute
is not changed, and the colormap is not installed by this function.
The returned colormap is a copy of the window's current colormap, but 
with varying hues (blue to red) allocated in the range of contiguous
cells determined by XA_RGB_DEFAULT_MAP.  If it does not already exist,
XA_RGB_DEFAULT_MAP will be created.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/29/90
Modified:  Craig Artley, Colorado School of Mines, 11/22/93
	   Saturation, brightness, and range of hues now user-specified.
*****************************************************************************/
{
	Screen *scr=XDefaultScreenOfDisplay(dpy);
	Colormap cmap,wcmap;
	XColor color;
	XWindowAttributes wa;
	unsigned long i,ncells,npixels;
	unsigned long bpixel,epixel,pixel[4096];
	float red=0,green=0,blue=0;

	/* determine beginning and ending pixels in contiguous range */
	bpixel = XtcwpGetFirstPixel(dpy);
	epixel = XtcwpGetLastPixel(dpy);
	if (epixel<=bpixel) return None;

	/* determine window's current colormap */
	XGetWindowAttributes(dpy,win,&wa);
	wcmap = wa.colormap;

	/* create new colormap and allocate all cells read/write */
	cmap = XCreateColormap(dpy,win,DefaultVisualOfScreen(scr),AllocNone);
	ncells = CellsOfScreen(scr);
	XAllocColorCells(dpy,cmap,True,NULL,0,pixel,(unsigned int) ncells);

	/* copy color cells from window's colormap to new colormap */
	for (i=0; i<ncells; ++i) {
		if (i<bpixel || i>epixel) {
			color.pixel = i;
			XQueryColor(dpy,wcmap,&color);
			XFreeColors(dpy,cmap,&i,1,0);
			XAllocColor(dpy,cmap,&color);
		}
	}

	/* build hues in contiguous cells in new colormap */
	npixels = epixel-bpixel+1;
	for (i=0; i<npixels; ++i) {
		color.pixel = bpixel+i;
		hsvrgb(fhue+(lhue-fhue)*((float)i)/((float)(npixels-1)),sat,bright,
			&red,&green,&blue);
		color.red = 65535*red;
		color.green = 65535*green;
		color.blue = 65535*blue;
		color.flags = DoRed|DoGreen|DoBlue;
		XStoreColor(dpy,cmap,&color);
	}

	/* return colormap */
	return cmap;
}

Colormap XtcwpCreateSatColormap (Display *dpy, Window win,
	float fhue, float lhue, float wfrac, float bright)
/*****************************************************************************
create a colormap with varying saturations in contiguous cells
******************************************************************************
Input:
dpy		display
win		window
fhue		first hue in colormap (saturation=1)
lhue		last hue in colormap (saturation=1)
wfrac		fractional position of white within the colormap (saturation=0)
bright		brightness
******************************************************************************
Notes:
The returned colormap is only created; the window's colormap attribute
is not changed, and the colormap is not installed by this function.
The returned colormap is a copy of the window's current colormap, but 
with varying hues (blue to red) allocated in the range of contiguous
cells determined by XA_RGB_DEFAULT_MAP.  If it does not already exist,
XA_RGB_DEFAULT_MAP will be created.
******************************************************************************
Author:  Craig Artley, Colorado School of Mines, 11/22/93
*****************************************************************************/
{
	Screen *scr=XDefaultScreenOfDisplay(dpy);
	Colormap cmap,wcmap;
	XColor color;
	XWindowAttributes wa;
	unsigned long i,j,ncells,npixels,nfpixels,nlpixels;
	unsigned long bpixel,epixel,pixel[4096];
	long  ltemp;
	float red=0,green=0,blue=0;

	/* determine beginning and ending pixels in contiguous range */
	bpixel = XtcwpGetFirstPixel(dpy);
	epixel = XtcwpGetLastPixel(dpy);
	if (epixel<=bpixel) return None;

	/* determine window's current colormap */
	XGetWindowAttributes(dpy,win,&wa);
	wcmap = wa.colormap;

	/* create new colormap and allocate all cells read/write */
	cmap = XCreateColormap(dpy,win,DefaultVisualOfScreen(scr),AllocNone);
	ncells = CellsOfScreen(scr);
	XAllocColorCells(dpy,cmap,True,NULL,0,pixel,(unsigned int) ncells);

	/* copy color cells from window's colormap to new colormap */
	for (i=0; i<ncells; ++i) {
		if (i<bpixel || i>epixel) {
			color.pixel = i;
			XQueryColor(dpy,wcmap,&color);
			XFreeColors(dpy,cmap,&i,1,0);
			XAllocColor(dpy,cmap,&color);
		}
	}

	/* devide colormap into 3 regions: fhues, white, and lhues */
	npixels = epixel-bpixel+1;
	ltemp = (long) (wfrac*((float) npixels));
	nfpixels = (ltemp<0) ? 0: ltemp;
	if (nfpixels>npixels-1) nfpixels = npixels-1;
	nlpixels = npixels-nfpixels-1;

	/* pixels from fhue to just under white */
	for (i=0; i<nfpixels; ++i) {
		color.pixel = bpixel+i;
		hsvrgb(fhue,((float)(nfpixels-i))/((float) nfpixels),bright,
			&red,&green,&blue);
		color.red = 65535*red;
		color.green = 65535*green;
		color.blue = 65535*blue;
		color.flags = DoRed|DoGreen|DoBlue;
		XStoreColor(dpy,cmap,&color);
	}

	/* white pixel */
	color.pixel = bpixel+i;
	hsvrgb(fhue,0.0,bright,&red,&green,&blue);
	color.red = 65535*red;
	color.green = 65535*green;
	color.blue = 65535*blue;
	color.flags = DoRed|DoGreen|DoBlue;
	XStoreColor(dpy,cmap,&color);
	++i;

	/* pixels from just under white to lhue */
	for (j=0; j<nlpixels; ++i,++j) {
		color.pixel = bpixel+i;
		hsvrgb(lhue,((float)(j+1))/((float) nlpixels),bright,&red,&green,&blue);
		color.red = 65535*red;
		color.green = 65535*green;
		color.blue = 65535*blue;
		color.flags = DoRed|DoGreen|DoBlue;
		XStoreColor(dpy,cmap,&color);
	}

	/* return colormap */
	return cmap;
}

static void hsvrgb(float h, float s, float v, float *r, float *g, float *b)
/*****************************************************************************
convert HSV color space coordinates to RGB color space
******************************************************************************
Input:
h		hue (0=1=red, 0.333=green, 0.667=blue)
s		saturation (0=white, 1=pure color)
v		value (brightness) (0=black, 1=max intensity)
******************************************************************************
Output:
r		red (0=black, 1=max red)
g		green (0=black, 1=max green)
b		blue (0=black, 1=max blue)
*****************************************************************************/
{
	float f,p,q,t;

	if (s==0.0) {
		*r = v;
		*g = v;
		*b = v;
	} else {
		while (h<0.0) h += 1.0;
		while (h>=1.0) h -= 1.0;

		h *= 6.0;
		f = h-(int)h;  /* fractional part of hue */

		p = v*(1.0-s);
		q = v*(1.0-(s*f));
		t = v*(1.0-(s*(1.0-f)));

		switch ((int)h) {
	  		case 0: *r = v; *g = t; *b = p; break;
	  		case 1: *r = q; *g = v; *b = p; break;
	  		case 2: *r = p; *g = v; *b = t; break;
	  		case 3: *r = p; *g = q; *b = v; break;
	  		case 4: *r = t; *g = p; *b = v; break;
	  		case 5: *r = v; *g = p; *b = q; break;
		}
    	}
}
	
/* test program - compile with "cc -DTEST colormap.c -lX11" */
#ifdef TEST
#include <stdio.h>
#define X 100
#define Y 100
#define WIDTH 256
#define HEIGHT 64
main()
{
	Display *dpy;
	Window root,win;
	Colormap cmap;
	XStandardColormap scmap;
	XColor color,junk;
	XImage *image;
	XEvent event;
	GC gc;
	int scr,i;
	unsigned long black,white,pmin,pmax;
	char *data;
	
	/* connect to X server */
	dpy = XOpenDisplay(NULL);
	if ((dpy=XOpenDisplay(NULL))==NULL) {
		fprintf(stderr,"Cannot open display!\n");
		exit(-1);
	}
	scr = DefaultScreen(dpy);
	root = RootWindow(dpy,scr);
	black = BlackPixel(dpy,scr);
	white = WhitePixel(dpy,scr);
	
	/* create and map window */
	win = XCreateSimpleWindow(dpy,root,X,Y,WIDTH,HEIGHT,4,black,white);
	cmap = XtcwpCreateGrayColormap(dpy,win);
	XSetWindowColormap(dpy,win,cmap);
	XMapWindow(dpy,win);
	
	/* determine range of contiguous pixels from standard colormap */
	if (!XtcwpCreateRGBDefaultMap(dpy,&scmap)) {
		fprintf(stderr,"Cannot create standard colormap!\n");
		exit(-1);
	}
	pmin = XtcwpGetFirstPixel(dpy);
	pmax = XtcwpGetLastPixel(dpy);
	
	/* create image */
	data = (char*)malloc(WIDTH*HEIGHT);
	for (i=0; i<WIDTH*HEIGHT; ++i)
		data[i] = pmin+(pmax-pmin)*(i%WIDTH)/WIDTH;
	image = XCreateImage(dpy,DefaultVisual(dpy,scr),
		DefaultDepth(dpy,scr),ZPixmap,
		0,data,WIDTH,HEIGHT,BitmapPad(dpy),WIDTH);
	gc = XCreateGC(dpy,win,0,NULL);
	XAllocNamedColor(dpy,cmap,"red",&color,&junk);
	XSetForeground(dpy,gc,color.pixel);
	
	/* set event mask */
	XSelectInput(dpy,win,ExposureMask);

	/* loop forever */
	XPutImage(dpy,win,gc,image,0,0,0,0,WIDTH,HEIGHT);
	while(True) {
		XNextEvent(dpy,&event);
		while (XCheckTypedEvent(dpy,Expose,&event));
		XPutImage(dpy,win,gc,image,0,0,0,0,WIDTH,HEIGHT);
		XDrawLine(dpy,win,gc,0,0,WIDTH,HEIGHT);
	}

	/* close display */
	XCloseDisplay(dpy);
}
#endif /* TEST */
