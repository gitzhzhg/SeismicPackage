/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*
 * XPSP - Display conforming PostScript in an X-window
 *
 * xpsp < stdin
 *
 * Note: this is the most advanced version of xepsb and xepsp. 
 * Caveat: your  system must have Display PostScript Graphics
 *
 */
/**************** end self doc ********************************/
	

/**************************************************************************
Read Conforming PostScript from standard input and display in X window.
***************************************************************************
AUTHOR:  Dave Hale, Colorado School of Mines, 03/12/92
**************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <X11/X.h>
#include <DPS/XDPS.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <DPS/XDPSlib.h>
#include <DPS/dpsXclient.h>

/* length of buffer used to hold one line of input PostScript */
#define LBUF	1000

/* default BoundingBox - 8.5 inches wide x 11 inches high */
#define LLX_DEFAULT 0
#define LLY_DEFAULT 0
#define URX_DEFAULT 612
#define URY_DEFAULT 792

/* default Pages - should be big enough to for most documents */
#define NPAGES_DEFAULT 500

typedef struct BBoxStruct {
	int llx,lly,urx,ury;
} BBox;
typedef struct PageStruct {
	int offset;
	int length;
	int ordinal;
	char label[8];
} Page;
typedef struct PixStruct {
	Pixmap pixmap;
	GC gc;
	int width,height;
	int x,y;
} Pix;

/* usage */
char *usearr[] = {
"XPSP - X PostScript Preview",
"",
"xpsp [options] <PostScriptfile",
"",
"options:",
"-s <factor>			scale factor",
"-b <red,green,blue>		RGB of background",
"-l				landscape mode (selected automatically if",
"				input contains %%Orientation: Landscape)",
"",
"Note: your X server must support the Display PostScript X extension.",
"",
"Author:  Dave Hale, Colorado School of Mines, 03/12/92",
"",
};
void usage(void) {
	int n,i;
	n = (sizeof usearr)/(sizeof usearr[0]);
	for (i=0; i<n; ++i)
		fprintf(stderr,"%s\n",usearr[i]);
	exit(-1);
}

/* simple error exit */
void error (char *string)
{
	fprintf(stderr,"%s\n",string);
	exit(-1);
}

Pix *newPix (Pix *old, Display *dpy, BBox bbox,
	float scale, float xres, float yres)
{
	Pix *new=(Pix*)malloc(sizeof(Pix));
	new->width = (bbox.urx-bbox.llx)*scale*xres/72.0;
	new->height = (bbox.ury-bbox.lly)*scale*yres/72.0;
	new->pixmap = XCreatePixmap(dpy,DefaultRootWindow(dpy),
		new->width,new->height,
		DefaultDepth(dpy,DefaultScreen(dpy)));
	new->gc = XCreateGC(dpy,new->pixmap,0,NULL);
	if (old) {
		new->x = old->x;
		new->y = old->y;
		XFreePixmap(dpy,old->pixmap);
		XFreeGC(dpy,old->gc);
		free(old);
	} else {
		new->x = new->y = 0;
	}
	return new;
}

void initDPS (int nprolog, FILE *fp, DPSContext dps)
{
	int ndone,nread;
	char buf[LBUF];

	DPSPrintf(dps,"\nresyncstart\n");
	DPSPrintf(dps,"/showpage {} def\n");
	fseek(fp,0L,SEEK_SET);
	for (ndone=0,nread=LBUF; ndone<nprolog; ndone+=nread) {
		if (nread>nprolog-ndone) nread = nprolog-ndone;
		nread = fread(buf,sizeof(char),nread,fp);
		DPSWritePostScript(dps,buf,nread);
	}
	DPSSetContext(dps);
}

void drawPage (int ipage, Page *page, FILE *fp, DPSContext dps,
	BBox bbox, float scale, int landscape, float backrgb[3])
{
	int length,ndone,nread;
	char buf[LBUF];

	DPSPrintf(dps,"gsave\n");
	/*
	DPSPrintf(dps,"gsave clippath 1 setgray fill grestore\n");
	*/
	DPSPrintf(dps,"gsave clippath %f %f %f setrgbcolor fill grestore\n",
		backrgb[0],backrgb[1],backrgb[2]);
	DPSPrintf(dps,"%f %f scale\n",scale,scale);
	if (landscape) {
		DPSPrintf(dps,"%d %d translate\n",bbox.urx-bbox.llx,0);
		DPSPrintf(dps,"90 rotate\n");
	}
	DPSPrintf(dps,"%d %d translate\n",-bbox.llx,-bbox.lly);
	fseek(fp,page[ipage].offset,SEEK_SET);
	length = page[ipage].length;
	for (ndone=0,nread=LBUF; ndone<length; ndone+=nread) {
		if (nread>length-ndone) nread = length-ndone;
		nread = fread(buf,sizeof(char),nread,fp);
		DPSWritePostScript(dps,buf,nread);
	}
	DPSPrintf(dps,"grestore\n");
}

void panPix (Display *dpy, XEvent ev, Pix *pix, Window win, GC gc)
{
	int width,height,xpix,ypix,wpix,hpix,xpan,ypan,
		xold,yold,xpixold,ypixold,x,y;
	XEvent evb;
	XWindowAttributes wa;

	XGetWindowAttributes(dpy,win,&wa);
	width = wa.width;
	height = wa.height;
	xpixold = xpix = pix->x;
	ypixold = ypix = pix->y;
	wpix = pix->width;
	hpix = pix->height;
	xpan = width<wpix;
	ypan = height<hpix;
	if (!xpan && !ypan) return;
	xold = ev.xbutton.x;
	yold = ev.xbutton.y;
	while(True) {
		XNextEvent(dpy,&evb);
		if (evb.type==ButtonRelease) {
			break;
		} else if (evb.type==MotionNotify) {
			while (XCheckTypedEvent(dpy,MotionNotify,&evb));
			x = evb.xmotion.x;
			y = evb.xmotion.y;
			if (x==xold && y==yold) continue;
			if (xpan) {
				xpix += xold-x;
				if (xpix>wpix-width) xpix = wpix-width;
				if (xpix<0) xpix = 0;
			}
			if (ypan) {
				ypix += yold-y;
				if (ypix>hpix-height) ypix = hpix-height;
				if (ypix<0) ypix = 0;
			}
			if (xpix==xpixold && ypix==ypixold) continue;
			xold = x;
			yold = y;
			xpixold = xpix;
			ypixold = ypix;
			XCopyArea(dpy,pix->pixmap,win,gc,
				xpix,ypix,wpix-xpix,hpix-ypix,0,0);
		}
	}
	pix->x = xpix;
	pix->y = ypix;
}

int main(int argc, char *argv[])
{
	int i,j,scrwidth,scrheight,scrwidthmm,scrheightmm,
		npages,npage,ipage,landscape;
	float xres,yres,scale,backrgb[3];
	char buf[LBUF];
	FILE *ifp=stdin,*tfp;
	Page *page;
	BBox bbox;
	Pix *pix;
	Display *dpy;
	int scr;
	unsigned long black,white;
	Window win;
	GC gcwin;
	XEvent ev;
	XColor scolor,ecolor;
	DPSContext dps;

	/* set defaults */
	landscape = 0;
	scale = 1.0;
	backrgb[0] = backrgb[1] = backrgb[2] = 1.0;

	/* get command-line arguments */
	for (i=1; i<argc; ++i) {
		if (*argv[i]=='-') {
			char *p=argv[i]+2;
			char c=argv[i][1];
			switch(c) {
			case 'l':
				landscape = 1;
				break;
			case 's':
				if (*p==0 && argv[i+1]) p = argv[++i];
				if (sscanf(p,"%f",&scale)==0)
					error("Bad scale factor (-s option)!");
				break;
			case 'b':
				if (*p==0 && argv[i+1]) p = argv[++i];
				if (sscanf(p,"%s",buf)==0)
					error("Bad background (-b option)!");
				for (j=0,p=strtok(buf,","); j<3;
						++j,p=strtok(NULL,",")) {
					if (p==NULL) error("Bad -b option!");
					backrgb[j]=atof(p);
				}
				break;
			case '?':
			default:
				usage();
				break;
			}
		}
	}

	/* create temporary file to hold input */
	tfp = tmpfile();
	
	/* determine BoundingBox and Pages */
	bbox.llx = LLX_DEFAULT;
	bbox.lly = LLY_DEFAULT;
	bbox.urx = URX_DEFAULT;
	bbox.ury = URY_DEFAULT;
	npages = NPAGES_DEFAULT;
	page = (Page*)malloc(npages*sizeof(Page));
	npage = 0;
	while(fgets(buf,LBUF,ifp)!=NULL) {
		if (buf[0]=='%' && buf[1]=='%') {
			if (strstr(buf,"%%BoundingBox:")==buf) {
				if (strstr(buf,"(atend)")==NULL) {
					sscanf(&buf[14],"%d %d %d %d",
						&bbox.llx,&bbox.lly,
						&bbox.urx,&bbox.ury);
				}
			} else if (strstr(buf,"%%Orientation:")==buf) {
				if (strstr(buf,"Landscape")!=NULL)
					landscape = 1;
			} else if (strstr(buf,"%%Pages:")==buf) {
				if (strstr(buf,"(atend)")==NULL) {
					sscanf(&buf[8],"%d",&npages);
					if (npages<=0) npages = 1;
					free(page);
					page = (Page*)malloc(npages*
						sizeof(Page));
				}
			} else if (strstr(buf,"%%Page:")==buf) {
				if (npage==npages) error("Too many pages!");
				sscanf(&buf[7],"%s %d",
					page[npage].label,
					&page[npage].ordinal);
				page[npage].offset = ftell(tfp);
				page[npage].length = 0;
				if (npage>0)
					page[npage-1].length =
						page[npage].offset -
						page[npage-1].offset;
				++npage;
			} else if (strstr(buf,"%%Trailer")==buf) {
				if (npage>0) {
					page[npage-1].length =
						ftell(tfp) -
						page[npage-1].offset;
				}
			}
		}
		fputs(buf,tfp);
	}
	if (npage==0) {
		page[npage].offset = 0;
		page[npage].length = ftell(tfp);
		page[npage].ordinal = 1;
		page[npage].label[0] = '\0';
		++npage;
	}
	if (page[npage-1].length==0)
		page[npage-1].length = ftell(tfp)-page[npage-1].offset;
	
	/* if landscape mode, then rotate bounding box */
	if (landscape) {
		int temp=bbox.urx;
		bbox.urx = bbox.ury;
		bbox.ury = temp;
	}

	/* open display */
	if ((dpy=XOpenDisplay(NULL))==NULL) {
		fprintf(stderr,"Cannot connect to display %s\n",
			XDisplayName(NULL));
		exit(-1);
	}
	scr = DefaultScreen(dpy);
	black = BlackPixel(dpy,scr);
	white = WhitePixel(dpy,scr);

	/* resolution - pixels per inch */
	scrwidth = WidthOfScreen(DefaultScreenOfDisplay(dpy));
	scrheight = HeightOfScreen(DefaultScreenOfDisplay(dpy));
	scrwidthmm = WidthMMOfScreen(DefaultScreenOfDisplay(dpy));
	scrheightmm = HeightMMOfScreen(DefaultScreenOfDisplay(dpy));
	xres = (int)(25.4*scrwidth/scrwidthmm);
	yres = (int)(25.4*scrheight/scrheightmm);
	xres = yres = (xres<yres)?xres:yres;

	/* make initial pixmap and its gc */
	pix = newPix(NULL,dpy,bbox,scale,xres,yres);

	/* create and set Display PostScript context for pixmap */
	dps = XDPSCreateSimpleContext(dpy,pix->pixmap,pix->gc,0,pix->height,
		DPSDefaultTextBackstop,DPSDefaultErrorProc,NULL);
	if (dps==NULL) {
		fprintf(stderr,"Cannot create DPS context\n");
		exit(-1);
	}

	/* initialize with everything before first page */
	initDPS(page[0].offset,tfp,dps);

	/* draw first page */
	drawPage(ipage=0,page,tfp,dps,bbox,scale,landscape,backrgb);
	
	/* create and map window */
	win = XCreateSimpleWindow(dpy,DefaultRootWindow(dpy),
		100,100,pix->width,pix->height,1,black,white);
	XSetStandardProperties(dpy,win,"PostScript Preview","PSPrev",
		None,NULL,0,NULL);
	XMapWindow(dpy,win);

	/* copy pixmap to window */
	gcwin = XCreateGC(dpy,win,0,NULL);
	DPSWaitContext(dps);
	XCopyArea(dpy,pix->pixmap,win,gcwin,
		pix->x,pix->y,pix->width,pix->height,0,0);

	/* main event loop */
	XSelectInput(dpy,win,
		KeyPressMask |
		ExposureMask |
		PointerMotionMask |
		ButtonPressMask |
		ButtonReleaseMask |
		Button1MotionMask);
	while(True) {
		XNextEvent(dpy,&ev);
		if (ev.type==Expose) {
			while (XCheckTypedEvent(dpy,Expose,&ev));
			DPSWaitContext(dps);
			XCopyArea(dpy,pix->pixmap,win,gcwin,
				pix->x,pix->y,pix->width,pix->height,0,0);
		} else if (ev.type==KeyPress) {
			KeySym keysym;
			XComposeStatus keystat;
			XLookupString(&ev,NULL,0,&keysym,&keystat);
			if (keysym==XK_q) {
				break;
			} else if (keysym==XK_n) {
				++ipage;
				if (ipage>=npage) ipage = npage-1;
				drawPage(ipage,page,tfp,dps,bbox,
					scale,landscape,backrgb);
			} else if (keysym==XK_p) {
				--ipage;
				if (ipage<0) ipage = 0;
				drawPage(ipage,page,tfp,dps,bbox,
					scale,landscape,backrgb);
			} else if (keysym==XK_f) {
				ipage = 0;
				drawPage(ipage,page,tfp,dps,bbox,
					scale,landscape,backrgb);
			} else if (keysym==XK_l) {
				ipage = npage-1;
				drawPage(ipage,page,tfp,dps,bbox,
					scale,landscape,backrgb);
			} else {
				continue;
			}
			DPSWaitContext(dps);
			XCopyArea(dpy,pix->pixmap,win,gcwin,
				pix->x,pix->y,pix->width,pix->height,0,0);
		} else if (ev.type==ButtonPress) {
			if (ev.xbutton.button==Button1) {
				panPix(dpy,ev,pix,win,gcwin);
			}
		}
	}

	/* clean up */
	DPSDestroySpace(DPSSpaceFromContext(dps));
	XFreeGC(dpy,gcwin);
	XFreePixmap(dpy,pix->pixmap);
	XFreeGC(dpy,pix->gc);

	return 0;
}

