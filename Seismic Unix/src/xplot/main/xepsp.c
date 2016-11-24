/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*
 * XEPSP - X windows display of Encapsulated PostScript
 *
 * Usage:   xepsp < stdin
 *
 * Caveat: your system must have Display PostScript Graphics
 * Note:  
 * this program is a more advanced version of   xepsb. See also:  xepsp
 *
 */
/**************** end self doc ********************************/

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 03/03/92
 */

#include <stdio.h>
#include <signal.h>
#include <X11/X.h>
#include <DPS/XDPS.h>
#include <X11/Xlib.h>
#include <DPS/XDPSlib.h>
#include <DPS/dpsXclient.h>

#define LBUF	1000

/* default BoundingBox - 8.5 inches wide x 11 inches high */
#define LLX_DEFAULT 0
#define LLY_DEFAULT 0
#define URX_DEFAULT 612
#define URY_DEFAULT 792

main()
{
	int llx,lly,urx,ury,width,height,
		scrwidth,scrheight,scrwidthmm,scrheightmm;
	float xres,yres;
	char buf[LBUF];
	Display *dpy;
	int scr;
	unsigned long black,white;
	GC gcpix,gcwin;
	Window win;
	Pixmap pix;
	XEvent ev;
	DPSContext dps;

	/* open display */
	if ((dpy=XOpenDisplay(NULL))==NULL) {
		fprintf(stderr,"Cannot connect to display %s\n",
			XDisplayName(NULL));
		exit(-1);
	}
	scr = DefaultScreen(dpy);
	black = BlackPixel(dpy,scr);
	white = WhitePixel(dpy,scr);
	
	/* determine BoundingBox */
	llx = LLX_DEFAULT;
	lly = LLY_DEFAULT;
	urx = URX_DEFAULT;
	ury = URY_DEFAULT;
	fgets(buf,LBUF,stdin);
	if (strstr(buf,"EPS")!=NULL) {
		while(fgets(buf,LBUF,stdin)!=NULL) { 
			if (buf[0]!='%' || buf[1]!='%') continue;
			if (strstr(buf,"%%BoundingBox:")==buf) {
				if (strstr(buf,"(atend)")==NULL) {
					sscanf(&buf[14],"%d %d %d %d",
						&llx,&lly,&urx,&ury);
				}
				break;
			} else if (strstr(buf,"%%EndComments")==buf) {
				break;
			} else if (strstr(buf,"%%EndProlog")==buf) {
				break;
			}
		}
	}

	/* width and height in pixels */
	scrwidth = WidthOfScreen(DefaultScreenOfDisplay(dpy));
	scrheight = HeightOfScreen(DefaultScreenOfDisplay(dpy));
	scrwidthmm = WidthMMOfScreen(DefaultScreenOfDisplay(dpy));
	scrheightmm = HeightMMOfScreen(DefaultScreenOfDisplay(dpy));
	xres = (int)(25.4*scrwidth/scrwidthmm)/72.0;
	yres = (int)(25.4*scrheight/scrheightmm)/72.0;
	if (xres*(urx-llx)>scrwidth || yres*(ury-lly)>scrheight) {
		xres = (scrwidth-32.0)/(urx-llx);
		yres = (scrheight-32.0)/(ury-lly);
		xres = yres = (xres<yres)?xres:yres;
	}
	width = (urx-llx)*xres;
	height = (ury-lly)*yres;

	/* create pixmap and its gc */
	pix = XCreatePixmap(dpy,DefaultRootWindow(dpy),width,height,
		DefaultDepth(dpy,scr));
	gcpix = XCreateGC(dpy,pix,0,NULL);

	/* create and set Display PostScript context for pixmap */
	dps = XDPSCreateSimpleContext(dpy,pix,gcpix,0,height,
		DPSDefaultTextBackstop,DPSDefaultErrorProc,NULL);
	if (dps==NULL) {
		fprintf(stderr,"Cannot create DPS context\n");
		exit(-1);
	}
	DPSPrintf(dps,"\n resyncstart\n");
	DPSSetContext(dps);
	DPSFlushContext(dps);
	DPSWaitContext(dps);

	/* paint white background */
	DPSPrintf(dps,
		"gsave\n"
		"1 setgray\n"
		"0 0 %d %d rectfill\n"
		"grestore\n",
		urx-llx,ury-lly);
	
	/* translate */
	DPSPrintf(dps,"%d %d translate\n",-llx,-lly);

	/* read PostScript from standard input and render in pixmap */
	DPSPrintf(dps,"/showpage {} def\n");
	while (fgets(buf,LBUF,stdin)!=NULL)
		DPSWritePostScript(dps,buf,strlen(buf));
	DPSFlushContext(dps);
	DPSWaitContext(dps);
	
	/* create and map window */
	win = XCreateSimpleWindow(dpy,DefaultRootWindow(dpy),
		100,100,width,height,1,black,white);
	XSetStandardProperties(dpy,win,"EPS Pixmap","EPSpix",
		None,NULL,0,NULL);
	XMapWindow(dpy,win);

	/* copy pixmap to window; in pixmap, black=0 and white=1 */
	gcwin = XCreateGC(dpy,win,0,NULL);
	XCopyArea(dpy,pix,win,gcwin,0,0,width,height,0,0);

	/* main event loop */
	XSelectInput(dpy,win,
		KeyPressMask |
		ExposureMask);
	while(True) {
        	XNextEvent(dpy,&ev);
		if (ev.type==Expose) {
			while (XCheckTypedEvent(dpy,Expose,&ev));
			XCopyArea(dpy,pix,win,gcwin,0,0,width,height,0,0);
		} else if (ev.type==KeyPress) {
			break;
		}
	}

	/* clean up */
	DPSDestroySpace(DPSSpaceFromContext(dps));
	XFreePixmap(dpy,pix);
	XFreeGC(dpy,gcpix);
	XFreeGC(dpy,gcwin);
}
