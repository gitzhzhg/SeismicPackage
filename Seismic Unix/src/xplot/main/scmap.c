/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SCMAP: $Revision: 1.7 $ ; $Date: 2011/11/21 17:03:15 $	*/

/*********************** self documentation **********************/
/*
 * SCMAP - set default standard color map (RGB_DEFAULT_MAP)
 *
 * Usage:   scmap
 *
 */
/**************** end self doc ********************************/


#include "par.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
	
int
main (int argc, char **argv)
{
	Display *dpy;
	Screen *scr;
	Window root;
	Colormap cmap;
	XStandardColormap scmap;
	XColor color;
	int i,ncells;
	unsigned long npixels;
	unsigned long bpixel,epixel,pixel1,pixel2,imax,rmult,gmult,bmult;
	unsigned long pixel[256],plane[1];

	/* connect to X server */
	if ((dpy=XOpenDisplay(NULL))==NULL) {
		fprintf(stderr,"Cannot connect to display %s\n",
			XDisplayName(NULL));
		exit(-1);
	}
	scr = XDefaultScreenOfDisplay(dpy);
	root = XRootWindowOfScreen(scr);

	/* if default standard colormap has not previously been set, then */
	if (!XGetStandardColormap(dpy,root,&scmap,XA_RGB_DEFAULT_MAP)) {

		/* use default colormap */
		cmap = DefaultColormapOfScreen(scr);
		printf("Default colormap ID = %ld\n",cmap);

		/* determine largest number of contiguous free color cells */
		ncells = CellsOfScreen(scr);
		while(ncells && 
			!XAllocColorCells(dpy,cmap,True,plane,0,pixel,ncells))
			ncells--;
		if (ncells==0) {
			fprintf(stderr,
				"Sorry, cannot allocate any color cells!\n");
			exit(-1);
		}
		printf("Number of free color cells = %d\n",ncells);
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
		npixels = epixel-bpixel+1;
		printf("Largest number of contiguous free cells = %ld\n",
			npixels);
		
		/* force number of contiguous cells to be an integer cubed */
		for (i=2,imax=0; i*i*i<=ncells; i++,imax++);
		npixels = (imax+1)*(imax+1)*(imax+1);
		bpixel = epixel-npixels+1;
		printf("Number of cells allocated = %ld = %ld cubed\n",
			npixels,imax+1);
			
		/* free cells not in contiguous range */
		for (i=0; i<ncells; i++)
			if (pixel[i]<bpixel || pixel[i]>epixel)
				XFreeColors(dpy,cmap,&pixel[i],1,0);

		/* store colors in contiguous range of allocated cells */
		rmult = 1;
		gmult = 0;
		bmult = 0;
		for (i=0; i<npixels; i++) {
			color.pixel = bpixel+i;
			color.red = (unsigned short) (65535*i/(npixels-1));
			color.green = color.red;
			color.blue = color.red;
			color.flags = DoRed|DoGreen|DoBlue;
			XStoreColor(dpy,cmap,&color);
		}
		/* ???
		rmult = (imax+1)*(imax+1);
		gmult = imax+1;
		bmult = 1;
		for (i=0; i<npixels; i++) {
			color.pixel = bpixel+i;
			color.red = i/rmult;
			color.green = (i-color.red*rmult)/gmult;
			color.blue = i-color.red*rmult-color.green*gmult;
			color.red *= 65535/imax;
			color.green *= 65535/imax;
			color.blue *= 65535/imax;
			color.flags = DoRed|DoGreen|DoBlue;
			XStoreColor(dpy,cmap,&color);
			}
		*/
		
		/* set standard colormap */
		printf("Setting RGB_DEFAULT_MAP with ID = %ld\n",cmap);
		scmap.colormap = cmap;
		scmap.red_max = npixels-1;
		scmap.green_max = 0;
		scmap.blue_max = 0;
		/* ???
		scmap.red_max = imax;
		scmap.green_max = imax;
		scmap.blue_max = imax;
		*/
		scmap.red_mult = rmult;
		scmap.green_mult = gmult;
		scmap.blue_mult = bmult;
		scmap.base_pixel = bpixel;
		XSetStandardColormap(dpy,root,&scmap,XA_RGB_DEFAULT_MAP);
	}

	/* get standard default colormap */
	printf("Getting RGB_DEFAULT_MAP\n");
	if (!XGetStandardColormap(dpy,root,&scmap,XA_RGB_DEFAULT_MAP)) {
		fprintf(stderr,"Cannot get standard default colormap\n"); 
		exit(-1);
	}
	cmap = scmap.colormap;

	/* install default colormap */
	printf("Installing standard default colormap with ID = %ld\n",cmap);
	XInstallColormap(dpy,cmap);
	XSetWindowColormap(dpy,root,cmap);

	/* close connection to X server */
	XCloseDisplay(dpy);
	
	return EXIT_SUCCESS;
}
